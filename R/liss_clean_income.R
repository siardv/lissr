# ============================================================================
# liss_clean_income.R, rule-driven income cleaning for merged LISS data
# ============================================================================
# detects, evaluates, and (where justified) corrects implausible household
# income values according to a declarative YAML ruleset. every decision is
# ledgered with the rule that took it, the evidence, the candidate set, and
# a plain-language justification; original values are preserved next to the
# cleaned column, and a report generator renders the methodology and every
# decision for independent inspection.
#
# division of labour: numeric kernels live in liss_clean_executors.R; this
# file resolves columns, walks households, applies the ruleset, writes the
# ledger, and renders reports. relies on `%||%` from liss_executors.R.

# ---- controlled vocabulary -------------------------------------------------

# single source of truth for valid cleaning actions per ruleset section,
# mirroring the role action_vocabulary.yml plays for merge recipes.
CLEANING_ACTIONS <- list(
  preparation_rules = c(
    "attach_background", "resolve_target_variable", "rectify_sign",
    "map_category_bounds", "backfill_age", "guard_residual_sentinels"
  ),
  detection_rules = c(
    "invalid_category_bounds", "absolute_floor", "contextual_floor",
    "personal_income_echo", "low_magnitude_scale", "scale_error",
    "category_bound_violation", "exceeds_cap", "robust_consensus",
    "extreme_robust_z", "dataset_consensus"
  ),
  correction_rules = c(
    "household_center", "category_midpoint", "scale_rectification",
    "temporal_smoothing", "donor_pool", "range_midpoint"
  ),
  finalization_rules = c(
    "hard_cap_to_na"
  )
)

# rule-level keys the cleaning engine consults or sanctions as
# documentation; anything else draws a warning-only notice at validation,
# mirroring the merge engine's mis-named-key check.
CLEANING_RULE_KEYS <- c(
  "rule_id", "action", "description", "rationale", "references",
  "enabled", "log", "params", "disposition", "scope", "stage",
  "notes", "note"
)

# ---- ruleset loading and validation ----------------------------------------

# packaged default ruleset, with an option-based fallback for development
# trees where the package is not installed.
default_income_ruleset_path <- function() {
  p <- system.file("cleaning", "income_cleaning_rules.yml", package = "lissr")
  if (nzchar(p)) return(p)
  p <- getOption("lissr.income_ruleset", "")
  if (nzchar(p) && file.exists(p)) return(p)
  cli::cli_abort(c(
    "packaged income-cleaning ruleset not found",
    "i" = "reinstall lissr, or point {.code options(lissr.income_ruleset = )} at a ruleset file"
  ))
}

#' Load an income-cleaning ruleset
#'
#' Reads and validates a declarative income-cleaning ruleset. With
#' `path = NULL` the ruleset shipped with the package
#' (`inst/cleaning/income_cleaning_rules.yml`) is used. A custom ruleset
#' lets researchers re-parameterise, disable, or extend individual
#' decision rules; [validate_cleaning_ruleset()] enforces the schema.
#'
#' @param path path to a ruleset YAML file, or `NULL` for the packaged
#'   default.
#' @return a validated ruleset object of class `liss_cleaning_ruleset`.
#' @seealso [liss_clean_income()], [validate_cleaning_ruleset()]
#' @export
liss_cleaning_ruleset <- function(path = NULL) {
  path <- path %||% default_income_ruleset_path()
  if (!file.exists(path)) {
    cli::cli_abort("ruleset file not found: {.path {path}}")
  }
  rs <- yaml::read_yaml(path)
  rs$meta$source_path <- normalizePath(path)
  val <- validate_cleaning_ruleset(rs, quiet = TRUE)
  if (!val$valid) {
    cli::cli_abort(c(
      "invalid income-cleaning ruleset: {.path {path}}",
      stats::setNames(val$errors, rep("x", length(val$errors)))
    ))
  }
  for (w in val$warnings) cli::cli_alert_warning(w)
  structure(rs, class = c("liss_cleaning_ruleset", "list"))
}

#' Validate an income-cleaning ruleset
#'
#' Checks a parsed ruleset against the income-cleaning schema v1.0.0:
#' required metadata and variable mappings, sane global constraints,
#' unique rule ids, actions drawn from the controlled vocabulary,
#' non-empty descriptions, resolvable reference keys, and numeric
#' parameter sanity. Controlled values are enforced for detection-rule
#' `disposition`, `scope`, and `stage`, the finalization disposition,
#' the `selection` anchors, consensus-detector `methods`, and nested
#' per-method `thresholds`. Unrecognized rule keys draw a warning only,
#' mirroring the merge engine's authoring check.
#'
#' @param ruleset a parsed ruleset (from [liss_cleaning_ruleset()] or
#'   `yaml::read_yaml()`).
#' @param quiet suppress cli output and only return the result.
#' @return invisibly, a list with `valid`, `errors`, `warnings`, and
#'   `n_rules`.
#' @export
validate_cleaning_ruleset <- function(ruleset, quiet = FALSE) {
  errors <- character(0)
  warnings <- character(0)
  err <- function(msg) errors <<- c(errors, msg)
  wrn <- function(msg) warnings <<- c(warnings, msg)

  meta <- ruleset$meta %||% list()
  if (!nzchar(meta$ruleset %||% "")) err("meta.ruleset is required")
  if (!nzchar(meta$ruleset_version %||% "")) err("meta.ruleset_version is required")
  sv <- meta$schema_version %||% ""
  if (!identical(sv, "1.0.0")) {
    wrn(paste0("schema_version '", sv, "' differs from the supported 1.0.0"))
  }

  vars <- ruleset$variables %||% list()
  for (req in c("target", "person_id")) {
    if (!nzchar(as.character(vars[[req]] %||% "")[1])) {
      err(paste0("variables.", req, " is required"))
    }
  }

  cons <- ruleset$constraints %||% list()
  cap <- cons$income_cap
  mn <- cons$min_income
  if (!is.numeric(cap) || length(cap) != 1 || !is.finite(cap) || cap <= 0) {
    err("constraints.income_cap must be a single positive number")
  }
  if (!is.numeric(mn) || length(mn) != 1 || !is.finite(mn) || mn <= 0) {
    err("constraints.min_income must be a single positive number")
  }
  if (is.numeric(cap) && is.numeric(mn) && length(cap) == 1 &&
      length(mn) == 1 && is.finite(cap) && is.finite(mn) && mn >= cap) {
    err("constraints.min_income must be below constraints.income_cap")
  }
  cb <- cons$category_bounds
  if (!is.null(cb)) {
    lo <- as.numeric(unlist(cb$lower))
    hi <- as.numeric(unlist(cb$upper))
    if (length(lo) != length(hi)) {
      err("constraints.category_bounds lower/upper lengths differ")
    } else if (any(hi <= lo)) {
      err("constraints.category_bounds must satisfy upper > lower elementwise")
    } else if (is.unsorted(lo)) {
      wrn("constraints.category_bounds.lower is not nondecreasing")
    }
  }

  ref_keys <- names(ruleset$references %||% list())
  numeric_params <- c(
    "threshold", "ceiling", "tolerance", "volatility_min",
    "magnitude_ceiling", "factor", "upper_factor", "lower_factor",
    "min_obs", "consensus", "k", "min_donors", "min_relative_deviation",
    "min_valid", "max_valid",
    "max_code", "multiplier", "multiplier_ceiling",
    "min_household_size", "code_share"
  )
  detection_dispositions <- c("void_bounds", "set_na", "correct", "flag")
  detection_scopes <- c("global", "household", "dataset")
  finalization_dispositions <- c("void", "winsorise", "flag")
  known_methods <- c("iqr", "mad", "zscore")
  seen_ids <- character(0)
  for (section in names(CLEANING_ACTIONS)) {
    for (r in ruleset[[section]] %||% list()) {
      rid <- r$rule_id %||% ""
      if (!nzchar(rid)) {
        err(paste0(section, ": a rule is missing rule_id"))
        next
      }
      if (rid %in% seen_ids) err(paste0("duplicate rule_id '", rid, "'"))
      seen_ids <- c(seen_ids, rid)
      act <- r$action %||% ""
      if (!act %in% CLEANING_ACTIONS[[section]]) {
        err(paste0(rid, ": unknown ", section, " action '", act, "'"))
      }
      if (!nzchar(r$description %||% "")) {
        err(paste0(rid, ": description is required"))
      }
      if (!is.logical(r$enabled %||% TRUE)) {
        err(paste0(rid, ": enabled must be logical"))
      }
      for (rf in as.character(unlist(r$references %||% list()))) {
        if (!rf %in% ref_keys) {
          wrn(paste0(rid, ": reference key '", rf,
                     "' not defined under references"))
        }
      }
      unknown <- setdiff(names(r), CLEANING_RULE_KEYS)
      if (length(unknown) > 0) {
        wrn(paste0(rid, ": unrecognized key(s) ",
                   paste(unknown, collapse = ", "),
                   " (ignored by the engine)"))
      }
      for (pn in numeric_params) {
        pv <- r$params[[pn]]
        if (is.null(pv)) next
        if (!is.numeric(pv) || length(pv) != 1 || !is.finite(pv) || pv < 0) {
          err(paste0(rid, ": params.", pn,
                     " must be a single nonnegative finite number"))
        }
      }
      # controlled values for disposition, scope, and stage
      if (identical(section, "detection_rules")) {
        disp <- as.character(r$disposition %||% "")[1]
        if (!disp %in% detection_dispositions) {
          err(paste0(rid, ": disposition '", disp, "' must be one of ",
                     paste(detection_dispositions, collapse = ", ")))
        }
        sc <- as.character(r$scope %||% "")[1]
        if (!sc %in% detection_scopes) {
          err(paste0(rid, ": scope '", sc, "' must be one of ",
                     paste(detection_scopes, collapse = ", ")))
        }
        st <- r$stage
        if (!is.null(st) && !identical(as.character(st), "preliminary")) {
          err(paste0(rid, ": stage '", st,
                     "' is not recognized (only 'preliminary' is)"))
        }
      }
      if (identical(section, "finalization_rules")) {
        fd <- as.character(r$params$disposition %||% r$disposition %||%
                             "void")[1]
        if (!fd %in% finalization_dispositions) {
          err(paste0(rid, ": disposition '", fd, "' must be one of ",
                     paste(finalization_dispositions, collapse = ", ")))
        }
      }
      # consensus-detector payloads: known methods, achievable consensus,
      # and per-method thresholds that are named positive numbers
      mets <- as.character(unlist(r$params$methods %||% list()))
      if (length(mets) > 0) {
        badm <- setdiff(mets, known_methods)
        if (length(badm) > 0) {
          err(paste0(rid, ": unknown method(s) ",
                     paste(badm, collapse = ", "), " (known: ",
                     paste(known_methods, collapse = ", "), ")"))
        }
        cons_v <- r$params$consensus
        if (is.numeric(cons_v) && length(cons_v) == 1 &&
            is.finite(cons_v) && cons_v > length(mets)) {
          err(paste0(rid, ": params.consensus (", cons_v,
                     ") exceeds the number of methods (", length(mets), ")"))
        }
      }
      th <- r$params$thresholds
      if (!is.null(th)) {
        if (!is.list(th) || is.null(names(th)) || any(!nzchar(names(th)))) {
          err(paste0(rid, ": params.thresholds must be a named list"))
        } else {
          for (tn in names(th)) {
            tvv <- th[[tn]]
            if (!is.numeric(tvv) || length(tvv) != 1 || !is.finite(tvv) ||
                tvv <= 0) {
              err(paste0(rid, ": params.thresholds.", tn,
                         " must be a single positive finite number"))
            }
          }
          if (length(mets) > 0) {
            orphan <- setdiff(names(th), mets)
            if (length(orphan) > 0) {
              wrn(paste0(rid, ": threshold(s) for method(s) not in ",
                         "params.methods: ", paste(orphan, collapse = ", ")))
            }
          }
        }
      }
    }
  }

  # candidate-selection block: the anchor is executed (not merely
  # reported), so its values are controlled here
  sel <- ruleset$selection %||% list()
  anc <- as.character(sel$anchor %||% "household_median")[1]
  if (!anc %in% c("household_median", "household_mean")) {
    err(paste0("selection.anchor '", anc,
               "' must be household_median or household_mean"))
  }
  fb <- as.character(sel$fallback_anchor %||% "range_midpoint")[1]
  if (!identical(fb, "range_midpoint")) {
    err(paste0("selection.fallback_anchor '", fb,
               "' must be range_midpoint"))
  }

  valid <- length(errors) == 0
  if (!quiet) {
    if (valid) {
      cli::cli_alert_success(
        "ruleset valid: {length(seen_ids)} rule{?s}, {length(warnings)} warning{?s}")
    } else {
      cli::cli_alert_danger("ruleset invalid: {length(errors)} error{?s}")
    }
    for (e in errors) cli::cli_alert_danger(e)
    for (w in warnings) cli::cli_alert_warning(w)
  }
  invisible(list(valid = valid, errors = errors, warnings = warnings,
                 n_rules = length(seen_ids)))
}

#' @export
print.liss_cleaning_ruleset <- function(x, ...) {
  n_all <- 0L
  n_on <- 0L
  for (section in names(CLEANING_ACTIONS)) {
    for (r in x[[section]] %||% list()) {
      n_all <- n_all + 1L
      if (isTRUE(r$enabled %||% TRUE)) n_on <- n_on + 1L
    }
  }
  cli::cli_h3("liss cleaning ruleset: {x$meta$ruleset} v{x$meta$ruleset_version}")
  cli::cli_bullets(c(
    "*" = "schema {x$meta$schema_version}, target module {x$meta$target_module %||% 'unspecified'}",
    "*" = "{n_on}/{n_all} rules enabled",
    "*" = "constraints: income_cap {fmt_plain(x$constraints$income_cap)}, min_income {fmt_plain(x$constraints$min_income)}",
    "*" = "source: {.path {x$meta$source_path %||% '(in memory)'}}"
  ))
  invisible(x)
}

# ---- overrides --------------------------------------------------------------

# apply function-level overrides onto a loaded ruleset and re-validate.
# the applied overrides are recorded in meta$overrides for the report.
apply_ruleset_overrides <- function(rs, income_cap = NULL, min_income = NULL,
                                    disable = NULL, enable_only = NULL,
                                    params = NULL, variables = NULL) {
  applied <- character(0)
  if (!is.null(income_cap)) {
    rs$constraints$income_cap <- income_cap
    applied <- c(applied, paste0("income_cap = ", fmt_plain(income_cap)))
  }
  if (!is.null(min_income)) {
    rs$constraints$min_income <- min_income
    applied <- c(applied, paste0("min_income = ", fmt_plain(min_income)))
  }
  if (!is.null(variables)) {
    for (nm in names(variables)) rs$variables[[nm]] <- variables[[nm]]
    applied <- c(applied, paste0("variables: ",
                                 paste(names(variables), collapse = ", ")))
  }

  all_ids <- character(0)
  ids_by_section <- list()
  for (section in names(CLEANING_ACTIONS)) {
    sids <- vapply(rs[[section]] %||% list(),
                   function(r) as.character(r$rule_id %||% ""), character(1))
    ids_by_section[[section]] <- sids
    all_ids <- c(all_ids, sids)
  }
  set_enabled <- function(rs, ids, value) {
    for (section in names(CLEANING_ACTIONS)) {
      rules <- rs[[section]] %||% list()
      for (i in seq_along(rules)) {
        if (rules[[i]]$rule_id %in% ids) rules[[i]]$enabled <- value
      }
      rs[[section]] <- rules
    }
    rs
  }
  if (!is.null(enable_only)) {
    unknown <- setdiff(enable_only, all_ids)
    if (length(unknown) > 0) {
      cli::cli_abort("enable_only references unknown rule id(s): {paste(unknown, collapse = ', ')}")
    }
    # scoped per section: only sections that contain a named rule are
    # restricted to the named set; the other sections keep their enabled
    # state. enable_only = "D06" therefore isolates one detection rule
    # while preparation, correction, and finalization machinery keep
    # running (the old all-section restriction silently turned a
    # detected cell's correction into a void).
    hit_sections <- names(Filter(function(sids) any(sids %in% enable_only),
                                 ids_by_section))
    for (section in hit_sections) {
      rs <- set_enabled(rs, setdiff(ids_by_section[[section]], enable_only),
                        FALSE)
    }
    rs <- set_enabled(rs, enable_only, TRUE)
    applied <- c(applied, paste0("enable_only = ",
                                 paste(enable_only, collapse = ", "),
                                 " (scoped to ",
                                 paste(hit_sections, collapse = ", "), ")"))
  }
  if (!is.null(disable)) {
    unknown <- setdiff(disable, all_ids)
    if (length(unknown) > 0) {
      cli::cli_abort("disable references unknown rule id(s): {paste(unknown, collapse = ', ')}")
    }
    rs <- set_enabled(rs, disable, FALSE)
    applied <- c(applied, paste0("disabled = ", paste(disable, collapse = ", ")))
  }
  if (!is.null(params)) {
    unknown <- setdiff(names(params), all_ids)
    if (length(unknown) > 0) {
      cli::cli_abort("params references unknown rule id(s): {paste(unknown, collapse = ', ')}")
    }
    for (section in names(CLEANING_ACTIONS)) {
      rules <- rs[[section]] %||% list()
      for (i in seq_along(rules)) {
        rid <- rules[[i]]$rule_id
        if (rid %in% names(params)) {
          rules[[i]]$params <- utils::modifyList(rules[[i]]$params %||% list(),
                                                 params[[rid]])
          applied <- c(applied, paste0("params(", rid, "): ",
                                       paste(names(params[[rid]]),
                                             collapse = ", ")))
        }
      }
      rs[[section]] <- rules
    }
  }

  val <- validate_cleaning_ruleset(rs, quiet = TRUE)
  if (!val$valid) {
    cli::cli_abort(c("overrides produce an invalid ruleset",
                     stats::setNames(val$errors, rep("x", length(val$errors)))))
  }
  rs$meta$overrides <- applied
  rs
}

# ---- variable resolution -----------------------------------------------------

# resolve the configured variable mapping against the input columns.
# `target` may be satisfied by any alias; `wave` is preference-ordered;
# a missing household id falls back to the person id with a logged note.
resolve_cleaning_variables <- function(data, vars) {
  notes <- character(0)
  fp <- function(cands) {
    cands <- as.character(unlist(cands))
    hit <- cands[cands %in% names(data)]
    if (length(hit) > 0) hit[1] else NA_character_
  }

  target <- fp(c(vars$target, vars$target_aliases))
  if (is.na(target)) {
    tried <- paste(as.character(unlist(c(vars$target, vars$target_aliases))),
                   collapse = ", ")
    cli::cli_abort("income target column not found; tried: {tried}")
  }
  if (!identical(target, as.character(vars$target)[1])) {
    notes <- c(notes, paste0("target resolved via alias '", target, "'"))
  }

  person <- fp(vars$person_id)
  if (is.na(person)) {
    cli::cli_abort("person id column {.field {vars$person_id}} not found")
  }
  household <- fp(vars$household_id)
  if (is.na(household)) {
    household <- fp(vars$household_id_fallback %||% vars$person_id)
    notes <- c(notes, paste0("household id absent; grouping by '",
                             household, "'"))
  }
  wave <- fp(vars$wave)
  if (is.na(wave)) {
    notes <- c(notes, "no wave column found; input order used within groups")
  }

  list(
    target = target,
    person = person,
    household = household,
    wave = wave,
    personal_net = fp(vars$personal_net),
    personal_gross = fp(vars$personal_gross),
    household_size = fp(vars$household_size),
    category_code = fp(vars$category_code),
    category_min = fp(vars$category_min),
    category_max = fp(vars$category_max),
    donor_keys = intersect(as.character(unlist(vars$donor_keys %||% list())),
                           names(data)),
    birth_year = fp(vars$birth_year),
    age = fp(vars$age),
    notes = notes
  )
}

# ---- small internals ---------------------------------------------------------

# numeric view of a column: haven labelled classes unwrap to their codes,
# factors and characters coerce (junk becomes NA, counted by the caller).
numeric_view <- function(x) {
  if (is.factor(x)) return(suppressWarnings(as.numeric(as.character(x))))
  if (is.character(x)) return(suppressWarnings(as.numeric(x)))
  as.numeric(unclass(x))
}

# declared SPSS user-missing metadata on a column, plus fallback codes.
declared_missing_codes <- function(x, fallback = NULL) {
  codes <- numeric(0)
  nv <- attr(x, "na_values", exact = TRUE)
  if (!is.null(nv)) codes <- c(codes, as.numeric(nv))
  rng <- attr(x, "na_range", exact = TRUE)
  if (!is.null(rng) && length(rng) == 2) rng <- as.numeric(rng) else rng <- NULL
  list(codes = unique(c(codes, as.numeric(fallback %||% numeric(0)))),
       range = rng)
}

# number formatting for evidence strings and report tables
fnum <- function(x, digits = 0) {
  ifelse(is.finite(x),
         formatC(round(x, digits), format = "f", digits = digits),
         "NA")
}

# scipen-proof rendering for pasted configuration numbers (base R turns
# 200000 into "2e+05" under default coercion)
fmt_plain <- function(x) {
  if (is.numeric(x)) format(x, scientific = FALSE, trim = TRUE) else as.character(x)
}

dist_stats <- function(x) {
  v <- finite_vals(x)
  if (length(v) == 0) return(NULL)
  list(
    n = length(v), min = min(v),
    q1 = unname(stats::quantile(v, 0.25)),
    median = stats::median(v), mean = mean(v),
    q3 = unname(stats::quantile(v, 0.75)),
    max = max(v), sd = stats::sd(v), mad = stats::mad(v)
  )
}

# interpret wave values as calendar years: values already on the year
# scale pass through, annual wavenr values are shifted by the origin.
wave_years_from <- function(wv, origin) {
  fin <- finite_vals(wv)
  if (length(fin) == 0) return(rep(NA_real_, length(wv)))
  if (min(fin) >= 1900) wv else wv + origin
}

first_rule <- function(rs, section, action) {
  for (r in rs[[section]] %||% list()) {
    if (identical(r$action, action)) return(r)
  }
  NULL
}

rule_enabled <- function(r) !is.null(r) && isTRUE(r$enabled %||% TRUE)

rp <- function(r, name, default) {
  v <- r$params[[name]]
  if (is.null(v)) default else v
}

# ---- decision ledger ---------------------------------------------------------

new_ledger <- function() {
  env <- new.env(parent = emptyenv())
  env$rows <- list()
  env$n <- 0L
  env
}

ledger_add <- function(ld, rule_id, action, applied, row, person, household,
                       wave, variable, observed, corrected,
                       valid_min = NA_real_, valid_max = NA_real_,
                       anchor = NA_real_, candidates = "",
                       candidate_source = NA_character_,
                       evidence = "", justification = "") {
  ld$n <- ld$n + 1L
  ld$rows[[ld$n]] <- list(
    decision_id = ld$n,
    rule_id = as.character(rule_id),
    action = as.character(action),
    applied = isTRUE(applied),
    row = as.integer(row),
    person_id = as.character(person),
    household_id = as.character(household),
    wave = as.numeric(wave),
    variable = as.character(variable),
    observed = as.numeric(observed),
    corrected = as.numeric(corrected),
    valid_min = as.numeric(valid_min),
    valid_max = as.numeric(valid_max),
    anchor = as.numeric(anchor),
    candidates = as.character(candidates),
    candidate_source = as.character(candidate_source),
    evidence = as.character(evidence),
    justification = as.character(justification)
  )
  invisible(ld)
}

ledger_frame <- function(ld) {
  template <- list(
    decision_id = integer(0), rule_id = character(0), action = character(0),
    applied = logical(0), row = integer(0), person_id = character(0),
    household_id = character(0), wave = numeric(0), variable = character(0),
    observed = numeric(0), corrected = numeric(0), valid_min = numeric(0),
    valid_max = numeric(0), anchor = numeric(0), candidates = character(0),
    candidate_source = character(0), evidence = character(0),
    justification = character(0)
  )
  if (ld$n == 0) {
    return(do.call(data.frame, c(template, list(stringsAsFactors = FALSE))))
  }
  cols <- lapply(names(template), function(cn) {
    unlist(lapply(ld$rows, function(r) r[[cn]]), use.names = FALSE)
  })
  names(cols) <- names(template)
  do.call(data.frame, c(cols, list(stringsAsFactors = FALSE)))
}

# ---- audit log ---------------------------------------------------------------

# engine-shaped log entries (same field set as make_log in the merge
# engine) so cleaning logs and merge logs read alike.
clean_log_entry <- function(rule_id, variable, action, rows_affected,
                            values_changed = NA_integer_,
                            na_before = NA, na_after = NA) {
  list(
    rule_id = rule_id, wave_id = "*", variable = variable, action = action,
    rows_affected = rows_affected, values_changed = values_changed,
    distinct_before = NA, distinct_after = NA,
    na_count_before = na_before, na_count_after = na_after,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3"),
    duration_ms = NA
  )
}

write_clean_jsonl <- function(entries, path) {
  lines <- vapply(entries, function(e) {
    as.character(jsonlite::toJSON(e, auto_unbox = TRUE, null = "null"))
  }, character(1))
  writeLines(lines, path)
  invisible(path)
}

# ---- background attachment (P01) ----------------------------------------------

# align the background wave key to the annual scale, keep one background
# row per person-year (latest month within the year), and left-join on
# the person id plus the aligned annual index. never joins on the
# household id. single-snapshot backgrounds join on the person id only.
attach_background_frame <- function(df, background, vars, origin, wv) {
  b <- as.data.frame(background, stringsAsFactors = FALSE)
  person <- vars$person
  if (!person %in% names(b)) {
    cli::cli_abort("background lacks the person id column {.field {person}}")
  }

  # join on type-neutral keys: haven-labelled ids on one side and plain
  # numerics on the other would make dplyr refuse the join
  df$.lissr_join_person <- as.character(unclass(df[[person]]))
  b$.lissr_join_person <- as.character(unclass(b[[person]]))

  join_desc <- person
  if ("wave" %in% names(b) || "wavenr" %in% names(b)) {
    if (!"wavenr" %in% names(b)) {
      # detect the background wave scale before deriving the annual
      # index: yyyymm keys divide by 100, year keys subtract the origin
      # directly, and small integers are already annual wavenr values.
      # blindly assuming yyyymm (the old behavior) turned year-keyed
      # backgrounds into impossible wavenr values and an all-NA join
      # that was logged as success.
      bwv <- as.numeric(b$wave)
      fin <- bwv[is.finite(bwv)]
      b$wavenr <- if (length(fin) > 0 && all(fin >= 190001)) {
        as.integer(bwv %/% 100L - origin)
      } else if (length(fin) > 0 && all(fin >= 1900 & fin <= 2100)) {
        as.integer(bwv - origin)
      } else if (length(fin) > 0 && all(fin >= 0 & fin <= 100)) {
        as.integer(bwv)
      } else {
        cli::cli_warn("background wave values fit no recognizable scale (yyyymm, calendar year, or annual wavenr); assuming yyyymm")
        as.integer(bwv %/% 100L - origin)
      }
    }
    ord <- do.call(order,
                   b[intersect(c(".lissr_join_person", "wavenr", "wave"),
                               names(b))])
    b <- b[ord, , drop = FALSE]
    b <- b[!duplicated(b[c(".lissr_join_person", "wavenr")],
                       fromLast = TRUE), , drop = FALSE]
    yrs <- wave_years_from(wv, origin)
    df$.lissr_join_wavenr <- as.integer(yrs - origin)
    b$.lissr_join_wavenr <- as.integer(b$wavenr)
    keys <- c(".lissr_join_person", ".lissr_join_wavenr")
    join_desc <- paste0(person, " + annual wavenr")
  } else {
    b <- b[!duplicated(b$.lissr_join_person, fromLast = TRUE), , drop = FALSE]
    keys <- ".lissr_join_person"
  }

  # match-rate diagnostic: how many data rows find a background row
  key_b <- do.call(paste, c(b[keys], list(sep = "\r")))
  key_d <- do.call(paste, c(df[keys], list(sep = "\r")))
  n_matched <- sum(key_d %in% key_b)

  add_cols <- setdiff(names(b), c(names(df), person, "wave", "wavenr"))
  b <- b[c(keys, add_cols)]
  n_before <- nrow(df)
  out <- dplyr::left_join(df, b, by = keys)
  if (nrow(out) != n_before) {
    cli::cli_abort("background join is not one-to-one ({nrow(out)} rows from {n_before}); deduplicate the background frame")
  }
  out$.lissr_join_person <- NULL
  out$.lissr_join_wavenr <- NULL
  list(data = out, n_added_cols = length(add_cols), join_desc = join_desc,
       n_matched = n_matched)
}

# ---- candidate generation ------------------------------------------------------

# evaluate the enabled correction rules in ruleset order for one flagged
# cell; the caller filters to the valid range and selects. generation
# order is the deterministic tie-break for equal-distance candidates.
generate_candidates <- function(gv, p, obs_v, rmin, rmax, vmin, vmax,
                                corr_rules, donor_df, donor_keys,
                                value_vec, r_glob) {
  vals <- numeric(0)
  srcs <- character(0)
  add <- function(v, s) {
    if (length(v) == 1 && is.finite(v)) {
      vals <<- c(vals, v)
      srcs <<- c(srcs, s)
    }
  }
  for (r in corr_rules) {
    if (identical(r$action, "household_center")) {
      wanted <- as.character(unlist(r$params$statistics %||%
                                      list("median", "mean")))
      oth <- finite_vals(gv[-p])
      if (length(oth) > 0) {
        if ("median" %in% wanted) add(stats::median(oth), "household_median")
        if ("mean" %in% wanted) add(mean(oth), "household_mean")
      }
    } else if (identical(r$action, "category_midpoint")) {
      if (is.finite(rmin) && is.finite(rmax)) {
        add((rmin + rmax) / 2, "category_midpoint")
      }
    } else if (identical(r$action, "scale_rectification")) {
      if (is.finite(obs_v) && obs_v > 0) {
        for (d in as.numeric(unlist(r$params$divisors %||% list(10, 100)))) {
          s <- obs_v / d
          if (s >= vmin && s <= vmax) add(s, paste0("scale_div", d))
        }
        mult <- rp(r, "multiplier", 10)
        if (obs_v < rp(r, "multiplier_ceiling", 1000)) {
          s <- obs_v * mult
          if (s >= vmin && s <= vmax) add(s, paste0("scale_x", mult))
        }
      }
    } else if (identical(r$action, "temporal_smoothing")) {
      add(wma_impute_at(gv, p, k = rp(r, "k", 2),
                        weighting = r$params$weighting %||% "linear"),
          "temporal_smoothing")
    } else if (identical(r$action, "donor_pool")) {
      if (!is.null(donor_df) && length(donor_keys) > 0) {
        donor_df[[".lissr_value"]] <- value_vec
        agg <- switch(r$params$aggregate %||% "median",
                      "mean" = mean, stats::median)
        add(donor_pool_value(donor_df[r_glob, , drop = FALSE], donor_df,
                             ".lissr_value", donor_keys, aggregate = agg,
                             min_donors = rp(r, "min_donors", 1),
                             exclude_row = r_glob),
            "donor_pool")
      }
    }
  }
  list(values = vals, sources = srcs)
}

# ---- main entry ----------------------------------------------------------------

#' Detect and correct implausible household-income values
#'
#' Applies a declarative income-cleaning ruleset to merged LISS data:
#' preparation (background attachment, sentinel guarding, sign
#' rectification, bracket-code expansion), global voiding of
#' unrecoverable values, household-level detection of scale errors,
#' bound violations, cap exceedances and robust statistical outliers,
#' constrained candidate-based correction, a hard plausibility cap, and
#' dataset-level flagging. Every decision is recorded in a ledger with
#' the responsible rule, the evidence, the admissible candidate set,
#' and a plain-language justification.
#'
#' The returned data always carries the untouched input values in
#' `<target>_observed`. In `"correct"` mode the target column holds the
#' cleaned values and `<target>_clean_status` marks each modified cell
#' with its final action and rule. In `"flag"` mode the target column is
#' left untouched and the fully simulated result is returned in
#' `<target>_proposed` (with `<target>_proposed_status`), so the entire
#' procedure can be inspected as a dry run. In `"na_only"` mode detected
#' cells are voided instead of imputed. Dataset-level flags (rule D11)
#' are annotations in `<target>_dataset_flag` and never modify values.
#'
#' Calling the function on already-cleaned data (recognizable by the
#' `<target>_observed` column) is an error, which prevents accidental
#' double cleaning.
#'
#' @param data a data frame of merged LISS income data, or a
#'   [merge_liss_module()] result (its `$data` element is used).
#' @param background optional background/demographics frame attached by
#'   rule P01 before cleaning (joined on the person id and, when the
#'   background is wave-stamped, the aligned annual wave index).
#' @param ruleset a ruleset object from [liss_cleaning_ruleset()], a
#'   path to a ruleset YAML file, or `NULL` for the packaged default.
#' @param mode `"correct"` (apply corrections), `"flag"` (dry run,
#'   propose only), or `"na_only"` (void detected cells without
#'   imputation).
#' @param income_cap,min_income optional overrides for the global
#'   plausibility constraints.
#' @param disable,enable_only optional character vectors of rule ids to
#'   switch off, or to run exclusively. `enable_only` is scoped per
#'   ruleset section: only sections that contain a named rule are
#'   restricted to the named set, so `enable_only = "D06"` isolates one
#'   detection rule while the preparation, correction, and finalization
#'   machinery keeps running.
#' @param params optional named list of per-rule parameter overrides,
#'   e.g. `list(D06 = list(volatility_min = 0.7))`.
#' @param variables optional named list overriding entries of the
#'   ruleset's variable mapping, e.g. `list(target = "ci00a339")`.
#' @param output_dir optional directory; when given, the report,
#'   decision ledger, and JSONL log are written via
#'   [liss_cleaning_report()].
#' @param verbose print progress with cli.
#' @return invisibly, a `liss_clean_result` list with elements `data`,
#'   `decisions` (the ledger), `log`, `summary`, `ruleset`, `variables`,
#'   and `mode`.
#' @seealso [liss_cleaning_ruleset()], [liss_cleaning_report()],
#'   [liss_equivalise_income()]
#' @export
liss_clean_income <- function(data, background = NULL, ruleset = NULL,
                              mode = c("correct", "flag", "na_only"),
                              income_cap = NULL, min_income = NULL,
                              disable = NULL, enable_only = NULL,
                              params = NULL, variables = NULL,
                              output_dir = NULL, verbose = TRUE) {
  t0 <- Sys.time()
  mode <- match.arg(mode)

  if (is.list(data) && !is.data.frame(data) && !is.null(data$data)) {
    data <- data$data
  }
  if (!is.data.frame(data)) {
    cli::cli_abort("`data` must be a data frame or a merge result with a $data element")
  }
  was_tibble <- inherits(data, "tbl_df")
  df <- as.data.frame(data, stringsAsFactors = FALSE)
  n <- nrow(df)

  rs <- if (inherits(ruleset, "liss_cleaning_ruleset")) {
    ruleset
  } else {
    liss_cleaning_ruleset(ruleset)
  }
  rs <- apply_ruleset_overrides(rs, income_cap, min_income, disable,
                                enable_only, params, variables)
  cons <- rs$constraints
  cap <- cons$income_cap
  mn <- cons$min_income
  origin <- cons$wavenr_origin %||% 2007

  # per-rule log switches, honored uniformly: rules with log: false are
  # excluded from the per-rule decision aggregation in the JSONL trace
  # (the decision ledger itself is never suppressed)
  rule_log <- list()
  for (section in names(CLEANING_ACTIONS)) {
    for (r in rs[[section]] %||% list()) {
      rule_log[[as.character(r$rule_id %||% "")]] <- isTRUE(r$log %||% TRUE)
    }
  }

  # light pre-resolution: target only, for the double-cleaning guard
  tv0 <- {
    cands <- as.character(unlist(c(rs$variables$target,
                                   rs$variables$target_aliases)))
    hit <- cands[cands %in% names(df)]
    if (length(hit) == 0) {
      cli::cli_abort("income target column not found; tried: {paste(cands, collapse = ', ')}")
    }
    hit[1]
  }
  obs_col <- paste0(tv0, "_observed")
  if (obs_col %in% names(df)) {
    cli::cli_abort(c(
      "column {.field {obs_col}} already present: {.field {tv0}} appears to be cleaned already",
      "i" = "run liss_clean_income() on the raw merged data, or drop the *_observed/*_clean_status columns first"
    ))
  }

  if (verbose) cli::cli_h2("income cleaning ({mode} mode)")

  ld <- new_ledger()
  logs <- list(clean_log_entry("RUN", tv0, paste0("start:", mode), n))
  applied_flag <- !identical(mode, "flag")

  # wave context is needed before P01 for the annual join alignment
  vars_pre <- resolve_cleaning_variables(df, rs$variables)
  wv <- if (!is.na(vars_pre$wave)) numeric_view(df[[vars_pre$wave]]) else rep(NA_real_, n)

  # ---- P01: attach background -------------------------------------------
  p01 <- first_rule(rs, "preparation_rules", "attach_background")
  if (!is.null(background)) {
    if (rule_enabled(p01)) {
      ab <- attach_background_frame(df, background, vars_pre, origin, wv)
      df <- ab$data
      if (ab$n_matched == 0) {
        cli::cli_warn("{p01$rule_id}: background join matched 0 of {nrow(df)} row{?s}; check the background wave keying and person ids")
      }
      if (isTRUE(p01$log %||% TRUE)) {
        logs <- c(logs, list(clean_log_entry(p01$rule_id, "*",
                                             "attach_background",
                                             ab$n_matched,
                                             values_changed = ab$n_added_cols)))
      }
      if (verbose) {
        cli::cli_alert_info("{p01$rule_id}: background attached ({ab$n_added_cols} column{?s} added; join on {ab$join_desc}; {ab$n_matched}/{nrow(df)} row{?s} matched)")
      }
    } else if (verbose) {
      cli::cli_alert_info("background supplied but attach_background is disabled; ignored")
    }
  }

  vars <- resolve_cleaning_variables(df, rs$variables)
  tv <- vars$target
  if (verbose) for (nt in vars$notes) cli::cli_alert_info(nt)

  # ---- context vectors -----------------------------------------------------
  raw <- df[[tv]]
  observed <- numeric_view(raw)
  coerce_na <- sum(!is.na(raw) & is.na(observed))
  w <- observed
  status <- rep(NA_character_, n)

  pid <- as.character(unclass(df[[vars$person]]))
  hid <- as.character(unclass(df[[vars$household]]))
  wv <- if (!is.na(vars$wave)) numeric_view(df[[vars$wave]]) else rep(NA_real_, n)
  ctx_abs <- function(col) {
    if (is.na(col)) rep(NA_real_, n) else abs(numeric_view(df[[col]]))
  }
  pnet <- ctx_abs(vars$personal_net)
  pgross <- ctx_abs(vars$personal_gross)
  hsize <- if (is.na(vars$household_size)) {
    rep(NA_real_, n)
  } else {
    numeric_view(df[[vars$household_size]])
  }

  p02 <- first_rule(rs, "preparation_rules", "resolve_target_variable")
  if (rule_enabled(p02) && isTRUE(p02$log %||% TRUE)) {
    logs <- c(logs, list(clean_log_entry(p02$rule_id, tv,
                                         "resolve_target_variable", n,
                                         values_changed = coerce_na)))
  }
  if (verbose) {
    cli::cli_alert_info("target {.field {tv}}: {sum(is.finite(w))} finite of {n} row{?s}")
    if (coerce_na > 0) {
      cli::cli_alert_warning("{coerce_na} non-numeric target value{?s} coerced to NA")
    }
  }

  # ---- P06: residual sentinel guard ---------------------------------------
  p06 <- first_rule(rs, "preparation_rules", "guard_residual_sentinels")
  if (rule_enabled(p06)) {
    dm <- declared_missing_codes(raw, cons$sentinel_codes)
    hit <- is.finite(w) & w %in% dm$codes
    if (!is.null(dm$range)) {
      hit <- hit | (is.finite(w) & w >= dm$range[1] & w <= dm$range[2])
    }
    idx <- which(hit)
    for (i in idx) {
      ledger_add(ld, p06$rule_id, "set_na", applied_flag, i, pid[i], hid[i],
                 wv[i], tv, observed = w[i], corrected = NA_real_,
                 evidence = paste0("declared or configured user-missing code ",
                                   format(w[i], scientific = FALSE)),
                 justification = "residual SPSS user-missing code swept to NA before numeric evaluation")
    }
    if (length(idx) > 0) {
      na_b <- sum(is.na(w))
      w[idx] <- NA_real_
      status[idx] <- paste0("voided:", p06$rule_id)
      if (isTRUE(p06$log %||% TRUE)) {
        logs <- c(logs, list(clean_log_entry(p06$rule_id, tv, "set_na",
                                             length(idx),
                                             values_changed = length(idx),
                                             na_before = na_b,
                                             na_after = sum(is.na(w)))))
      }
      if (verbose) {
        cli::cli_alert_info("{p06$rule_id}: {length(idx)} residual sentinel value{?s} swept to NA")
      }
    }
  }

  # ---- P03: sign rectification ---------------------------------------------
  p03 <- first_rule(rs, "preparation_rules", "rectify_sign")
  if (rule_enabled(p03)) {
    idx <- which(is.finite(w) & w < 0)
    for (i in idx) {
      ledger_add(ld, p03$rule_id, "rectify_sign", applied_flag, i, pid[i],
                 hid[i], wv[i], tv, observed = w[i], corrected = abs(w[i]),
                 evidence = paste0("observed value ", fnum(w[i]), " is negative"),
                 justification = "negative annual household income treated as a sign-entry error; absolute value applied")
    }
    if (length(idx) > 0) {
      w[idx] <- abs(w[idx])
      status[idx] <- paste0("rectified:", p03$rule_id)
      if (isTRUE(p03$log %||% TRUE)) {
        logs <- c(logs, list(clean_log_entry(p03$rule_id, tv, "rectify_sign",
                                             length(idx),
                                             values_changed = length(idx))))
      }
      if (verbose) {
        cli::cli_alert_info("{p03$rule_id}: {length(idx)} negative value{?s} sign-rectified")
      }
    }
  }

  # ---- category bounds (P04, D01) ------------------------------------------
  row_min <- if (!is.na(vars$category_min)) numeric_view(df[[vars$category_min]]) else rep(NA_real_, n)
  row_max <- if (!is.na(vars$category_max)) numeric_view(df[[vars$category_max]]) else rep(NA_real_, n)

  p04 <- first_rule(rs, "preparation_rules", "map_category_bounds")
  if (rule_enabled(p04) && !is.na(vars$category_code)) {
    code_vals <- numeric_view(df[[vars$category_code]])
    max_code <- rp(p04, "max_code", 7)
    code_share <- rp(p04, "code_share", 0.9)
    # classify the column on its nonzero finite values: a bracket-code
    # column is nearly all 1..max_code, a euro-bounds column has none
    # there (its nonzero values are bracket amounts), and anything in
    # between is ambiguous and declined loudly. the old max()-based
    # gate let one stray sentinel silently disable mapping for the
    # whole column, after which raw codes were misread as euro bounds.
    fin_nz <- finite_vals(code_vals)
    fin_nz <- fin_nz[fin_nz != 0]
    share <- if (length(fin_nz) > 0) {
      mean(fin_nz >= 1 & fin_nz <= max_code)
    } else {
      0
    }
    if (share >= code_share && length(fin_nz) > 0) {
      cb <- cons$category_bounds %||% list()
      mapped <- category_bounds_from_codes(code_vals,
                                           as.numeric(unlist(cb$lower)),
                                           as.numeric(unlist(cb$upper)))
      row_min <- mapped$lower
      row_max <- mapped$upper
      n_mapped <- sum(is.finite(row_min))
      stray <- is.finite(code_vals) & code_vals != 0 &
        !(code_vals >= 1 & code_vals <= max_code)
      if (any(stray)) {
        sv <- sort(unique(code_vals[stray]))
        cli::cli_warn("{p04$rule_id}: {sum(stray)} value{?s} outside 1..{max_code} in bracket-code column {.field {vars$category_code}} treated as missing brackets (offending value{?s}: {paste(utils::head(fmt_plain(sv), 5), collapse = ', ')})")
      }
      if (isTRUE(p04$log %||% TRUE)) {
        logs <- c(logs, list(clean_log_entry(p04$rule_id, vars$category_code,
                                             "map_category_bounds", n_mapped,
                                             values_changed = sum(stray))))
      }
      if (verbose) {
        cli::cli_alert_info("{p04$rule_id}: bracket codes expanded to euro bounds on {n_mapped} row{?s}")
      }
    } else if (share > 0) {
      cli::cli_warn(c(
        "{p04$rule_id}: bracket-code mapping declined for {.field {vars$category_code}}: {round(100 * share)}% of nonzero finite values lie in 1..{max_code} (a bracket-code column needs at least {round(100 * code_share)}%)",
        "i" = "the column passes through as euro bounds; any raw codes in it will be misread by the bound checks (D01, D07)"
      ))
      if (isTRUE(p04$log %||% TRUE)) {
        logs <- c(logs, list(clean_log_entry(p04$rule_id, vars$category_code,
                                             "map_category_bounds:DECLINED",
                                             0L)))
      }
    }
    # share == 0: the column already holds euro bounds; pass through
  }

  d01 <- first_rule(rs, "detection_rules", "invalid_category_bounds")
  if (rule_enabled(d01)) {
    minv <- rp(d01, "min_valid", 0)
    maxv <- rp(d01, "max_valid", 120000)
    named_or <- function(x, fallback) {
      if (length(x) == 1 && !is.na(x)) as.character(x) else fallback
    }
    bounds_var <- paste0(named_or(vars$category_min, "category_min"), "/",
                         named_or(vars$category_max, "category_max"))
    bad <- (is.finite(row_min) & (row_min < minv | row_min > maxv)) |
      (is.finite(row_max) & (row_max < minv | row_max > maxv)) |
      (is.finite(row_min) & is.finite(row_max) & row_min > row_max)
    idx <- which(bad)
    for (i in idx) {
      off <- if (is.finite(row_min[i]) &&
                 (row_min[i] < minv || row_min[i] > maxv)) row_min[i] else row_max[i]
      ledger_add(ld, d01$rule_id, "void_bounds", TRUE, i, pid[i], hid[i],
                 wv[i],
                 variable = bounds_var,
                 observed = off, corrected = NA_real_,
                 evidence = paste0("bounds [", fnum(row_min[i]), ", ",
                                   fnum(row_max[i]), "] outside [",
                                   fnum(minv), ", ", fnum(maxv),
                                   "] or inverted"),
                 justification = "corrupt bracket metadata discarded so it cannot constrain corrections; the income value itself is untouched")
    }
    if (length(idx) > 0) {
      row_min[idx] <- NA_real_
      row_max[idx] <- NA_real_
      if (isTRUE(d01$log %||% TRUE)) {
        logs <- c(logs, list(clean_log_entry(d01$rule_id, bounds_var,
                                             "void_bounds", length(idx),
                                             values_changed = length(idx))))
      }
      if (verbose) {
        cli::cli_alert_info("{d01$rule_id}: category bounds voided on {length(idx)} row{?s}")
      }
    }
  }

  # ---- global voids (D02, D03, D04) -----------------------------------------
  for (r in rs$detection_rules %||% list()) {
    if (!rule_enabled(r)) next
    if (!identical(r$scope %||% "", "global") ||
        !identical(r$disposition %||% "", "set_na")) next

    if (identical(r$action, "absolute_floor")) {
      thr <- rp(r, "threshold", 10)
      idx <- which(is.finite(w) & w < thr)
      ev <- function(i) paste0("value ", fnum(w[i]),
                               " below the absolute floor ", fnum(thr))
      just <- "annual income below the absolute floor carries no recoverable magnitude information and is voided"
    } else if (identical(r$action, "contextual_floor")) {
      thr <- rp(r, "threshold", 100)
      # gate on pgross > 0: a zero gross personal income does not
      # contradict a near-zero household figure, so such rows are spared
      idx <- which(is.finite(w) & w <= thr & is.finite(pgross) & pgross > 0)
      ev <- function(i) paste0("value ", fnum(w[i]), " at or below ",
                               fnum(thr), " while gross personal income ",
                               fnum(pgross[i]), " is reported")
      just <- "household income at the contextual floor contradicts the reported positive personal income and is voided"
    } else if (identical(r$action, "personal_income_echo")) {
      ceil <- rp(r, "ceiling", 10000)
      tol <- rp(r, "tolerance", 100)
      min_hh <- rp(r, "min_household_size", 2)
      near_net <- is.finite(w) & is.finite(pnet) & abs(w - pnet) < tol
      near_gross <- is.finite(w) & is.finite(pgross) & abs(w - pgross) < tol
      # in a single-person household the household income legitimately
      # equals the personal income, so the echo is only evidence of a
      # keying error when the household is known to hold at least
      # min_household_size members; unknown sizes are spared
      multi <- is.finite(hsize) & hsize >= min_hh
      idx <- which(is.finite(w) & w < ceil & (near_net | near_gross) & multi)
      ev <- function(i) {
        src <- if (isTRUE(near_net[i])) {
          paste0("personal net income ", fnum(pnet[i]))
        } else {
          paste0("personal gross income ", fnum(pgross[i]))
        }
        paste0("value ", fnum(w[i]), " within +/-", fnum(tol), " of ", src,
               " in a household of ", fnum(hsize[i]))
      }
      just <- "a low household total echoing a personal income figure in a multi-person household indicates the personal amount was entered in the household field; the household value is voided"
    } else {
      next
    }

    for (i in idx) {
      ledger_add(ld, r$rule_id, "set_na", applied_flag, i, pid[i], hid[i],
                 wv[i], tv, observed = w[i], corrected = NA_real_,
                 evidence = ev(i), justification = just)
    }
    if (length(idx) > 0) {
      na_b <- sum(is.na(w))
      w[idx] <- NA_real_
      status[idx] <- paste0("voided:", r$rule_id)
      if (isTRUE(r$log %||% TRUE)) {
        logs <- c(logs, list(clean_log_entry(r$rule_id, tv, "set_na",
                                             length(idx),
                                             values_changed = length(idx),
                                             na_before = na_b,
                                             na_after = sum(is.na(w)))))
      }
      if (verbose) {
        cli::cli_alert_info("{r$rule_id} ({r$action}): {length(idx)} value{?s} voided")
      }
    }
  }

  # ---- donor frame and P05 age backfill --------------------------------------
  donor_keys <- vars$donor_keys
  donor_df <- NULL
  if (length(donor_keys) > 0) {
    donor_df <- as.data.frame(lapply(df[donor_keys], numeric_view),
                              stringsAsFactors = FALSE)
    names(donor_df) <- donor_keys

    p05 <- first_rule(rs, "preparation_rules", "backfill_age")
    if (rule_enabled(p05) && !is.na(vars$age) && vars$age %in% donor_keys &&
        !is.na(vars$birth_year) && !is.na(vars$wave)) {
      yr <- wave_years_from(wv, origin)
      gj <- numeric_view(df[[vars$birth_year]])
      carried <- stats::ave(gj, pid, FUN = function(g) {
        v <- g[is.finite(g)]
        if (length(v) > 0) v[1] else NA_real_
      })
      fill <- is.na(donor_df[[vars$age]]) & is.finite(carried) & is.finite(yr)
      donor_df[[vars$age]][fill] <- yr[fill] - carried[fill]
      if (sum(fill) > 0) {
        if (isTRUE(p05$log %||% TRUE)) {
          logs <- c(logs, list(clean_log_entry(p05$rule_id, vars$age,
                                               "backfill_age", sum(fill))))
        }
        if (verbose) {
          cli::cli_alert_info("{p05$rule_id}: age backfilled for donor matching on {sum(fill)} row{?s} (returned data untouched)")
        }
      }
    }
  }

  # ---- household stage --------------------------------------------------------
  # rows with a missing household id would otherwise silently receive no
  # household-stage cleaning (real LISS merges contain such rows); they
  # fall back to person-id grouping, and rows lacking both ids are
  # counted as skipped. the h:/p: key prefixes keep the two id spaces
  # from colliding.
  grp_key <- ifelse(!is.na(hid), paste0("h:", hid),
                    ifelse(!is.na(pid), paste0("p:", pid), NA_character_))
  n_hid_fallback <- sum(is.na(hid) & !is.na(pid))
  n_ungroupable <- sum(is.na(grp_key))
  groups <- split(seq_len(n)[!is.na(grp_key)], grp_key[!is.na(grp_key)])
  if (n_hid_fallback > 0 || n_ungroupable > 0) {
    logs <- c(logs, list(clean_log_entry(
      "GROUPING", vars$household, "household_id_fallback",
      n_hid_fallback, values_changed = n_ungroupable)))
    if (verbose) {
      if (n_hid_fallback > 0) {
        cli::cli_alert_info("{n_hid_fallback} row{?s} with a missing household id grouped by person id for the household stage")
      }
      if (n_ungroupable > 0) {
        cli::cli_alert_warning("{n_ungroupable} row{?s} with neither household nor person id received no household-stage cleaning")
      }
    }
  }
  n_groups_processed <- 0L

  d05 <- first_rule(rs, "detection_rules", "low_magnitude_scale")
  if (!rule_enabled(d05)) d05 <- NULL
  d_iter <- Filter(function(r) {
    rule_enabled(r) &&
      identical(r$scope %||% "", "household") &&
      identical(r$disposition %||% "", "correct") &&
      !identical(r$stage %||% "", "preliminary")
  }, rs$detection_rules %||% list())
  corr_rules <- Filter(function(r) {
    rule_enabled(r) && !identical(r$action, "range_midpoint")
  }, rs$correction_rules %||% list())
  c06 <- first_rule(rs, "correction_rules", "range_midpoint")
  c06_on <- rule_enabled(c06)

  # selection policy: the anchor statistic is dispatched from the ruleset's
  # selection block (household_median or household_mean), so execution and
  # the generated report can never disagree about the method used
  sel_cfg <- rs$selection %||% list()
  anchor_stat <- as.character(sel_cfg$anchor %||% "household_median")

  apply_cell <- function(i_glob, p_loc, rule, evd, gv, gmin_p, gmax_p) {
    # shared correction path for one flagged cell; returns the new value
    obs_v <- gv[p_loc]
    vmin_c <- if (is.finite(gmin_p)) gmin_p else mn
    vmax_c <- min(if (is.finite(gmax_p)) gmax_p else cap, cap)
    if (vmin_c > vmax_c) vmin_c <- vmax_c
    oth <- finite_vals(gv[-p_loc])
    anchor <- if (length(oth) > 0) {
      if (identical(anchor_stat, "household_mean")) mean(oth) else stats::median(oth)
    } else {
      (vmin_c + vmax_c) / 2
    }
    anchor_src <- if (length(oth) > 0) anchor_stat else "range_midpoint"

    if (identical(mode, "na_only")) {
      ledger_add(ld, rule$rule_id, "set_na", applied_flag, i_glob,
                 pid[i_glob], hid[i_glob], wv[i_glob], tv,
                 observed = obs_v, corrected = NA_real_,
                 valid_min = vmin_c, valid_max = vmax_c, anchor = anchor,
                 evidence = evd,
                 justification = paste0("flagged by ", rule$action,
                                        "; na_only mode voids without imputation"))
      status[i_glob] <<- paste0("voided:", rule$rule_id)
      return(NA_real_)
    }

    cnd <- generate_candidates(gv, p_loc, obs_v, gmin_p, gmax_p, vmin_c,
                               vmax_c, corr_rules, donor_df, donor_keys,
                               w, i_glob)
    flt <- filter_candidates(cnd$values, cnd$sources, vmin_c, vmax_c)
    if (length(flt$values) == 0) {
      # the range-midpoint fallback is only defensible when the household's
      # own evidence (the anchor) lies inside the admissible range; when the
      # anchor itself is out of range, every admissible value contradicts
      # the household series, so imputing the midpoint would fabricate a
      # magnitude (e.g. replacing a 10x entry error with a value 15x the
      # truth). such cells are voided instead.
      anchor_ok <- length(oth) > 0 && anchor >= vmin_c && anchor <= vmax_c
      if (c06_on && (anchor_ok || length(oth) == 0)) {
        flt <- list(values = (vmin_c + vmax_c) / 2,
                    sources = "range_midpoint")
      } else {
        just <- if (!c06_on) {
          "no admissible correction candidate and the range_midpoint fallback is disabled; value voided"
        } else {
          paste0("no admissible correction candidate, and the household ",
                 anchor_stat, " ", fnum(anchor),
                 " lies outside the admissible range [", fnum(vmin_c), ", ",
                 fnum(vmax_c),
                 "]; the range-midpoint fallback would fabricate a magnitude contradicted by the household's own series, so the value is voided")
        }
        ledger_add(ld, rule$rule_id, "set_na", applied_flag, i_glob,
                   pid[i_glob], hid[i_glob], wv[i_glob], tv,
                   observed = obs_v, corrected = NA_real_,
                   valid_min = vmin_c, valid_max = vmax_c, anchor = anchor,
                   evidence = evd, justification = just)
        status[i_glob] <<- paste0("voided:", rule$rule_id)
        return(NA_real_)
      }
    }
    sel <- select_candidate(flt$values, flt$sources, anchor)
    cand_str <- paste0(flt$sources, "=",
                       fmt_plain(signif(flt$values, 6)), collapse = "; ")
    ledger_add(ld, rule$rule_id, "correct", applied_flag, i_glob,
               pid[i_glob], hid[i_glob], wv[i_glob], tv,
               observed = obs_v, corrected = sel$value,
               valid_min = vmin_c, valid_max = vmax_c, anchor = anchor,
               candidates = cand_str, candidate_source = sel$source,
               evidence = evd,
               justification = paste0("detected by ", rule$action,
                                      "; replaced with the ", sel$source,
                                      " candidate ", fnum(sel$value),
                                      " (closest of ", length(flt$values),
                                      " admissible candidate(s) to the ",
                                      anchor_src, " ", fnum(anchor),
                                      "; constrained to [", fnum(vmin_c),
                                      ", ", fnum(vmax_c), "])"))
    status[i_glob] <<- paste0("corrected:", rule$rule_id)
    sel$value
  }

  for (g_idx in groups) {
    ord <- order(wv[g_idx], na.last = TRUE)
    idx <- g_idx[ord]
    gv <- w[idx]
    if (sum(is.finite(gv)) < 2) next
    n_groups_processed <- n_groups_processed + 1L
    gmin <- row_min[idx]
    gmax <- row_max[idx]

    # preliminary low-magnitude scaling (D05)
    if (!is.null(d05)) {
      mceil <- rp(d05, "magnitude_ceiling", 1000)
      fac <- rp(d05, "factor", 10)
      mag <- power10_magnitude(gv)
      fm <- finite_vals(mag)
      if (length(fm) > 0 && all(fm <= mceil)) {
        for (p in which(is.finite(gv) & gv > 0)) {
          v0 <- gv[p]
          scaled <- v0 * fac
          ok <- FALSE
          reason <- ""
          if (is.finite(gmin[p]) && is.finite(gmax[p])) {
            ok <- scaled >= gmin[p] && scaled <= gmax[p]
            reason <- "scaled value falls inside the row's category bounds"
          } else if (scaled <= cap && scaled >= mn) {
            oth <- finite_vals(gv[-p])
            if (length(oth) > 0) {
              med_oth <- stats::median(oth)
              ok <- abs(scaled - med_oth) < abs(v0 - med_oth)
              reason <- paste0("scaled value closer to the household median ",
                               fnum(med_oth))
            }
          }
          if (!ok) next
          i_glob <- idx[p]
          evd <- paste0("all household magnitudes at or below ", fnum(mceil),
                        "; ", reason)
          if (identical(mode, "na_only")) {
            ledger_add(ld, d05$rule_id, "set_na", applied_flag, i_glob,
                       pid[i_glob], hid[i_glob], wv[i_glob], tv,
                       observed = v0, corrected = NA_real_, evidence = evd,
                       justification = "flagged by low_magnitude_scale; na_only mode voids without imputation")
            status[i_glob] <- paste0("voided:", d05$rule_id)
            gv[p] <- NA_real_
            w[i_glob] <- NA_real_
          } else {
            ledger_add(ld, d05$rule_id, "correct", applied_flag, i_glob,
                       pid[i_glob], hid[i_glob], wv[i_glob], tv,
                       observed = v0, corrected = scaled,
                       valid_min = if (is.finite(gmin[p])) gmin[p] else mn,
                       valid_max = min(if (is.finite(gmax[p])) gmax[p] else cap,
                                       cap),
                       candidates = paste0("scale_x", fac, "=",
                                           fmt_plain(signif(scaled, 6))),
                       candidate_source = paste0("scale_x", fac),
                       evidence = evd,
                       justification = paste0("whole-household low-magnitude reporting; value scaled by ",
                                              fac,
                                              " under an explicit plausibility test (",
                                              reason, ")"))
            status[i_glob] <- paste0("corrected:", d05$rule_id)
            gv[p] <- scaled
            w[i_glob] <- scaled
          }
        }
      }
    }

    # iterative in-household detection and constrained correction
    corrected_pos <- integer(0)
    iter <- 0L
    max_iter <- length(idx)
    while (iter < max_iter) {
      iter <- iter + 1L
      vol <- local_log_volatility(gv)
      mag <- power10_magnitude(gv)
      mode_mag <- stat_mode_num(mag)
      if (!is.finite(mode_mag)) mode_mag <- 10000

      hit <- NULL
      for (r in d_iter) {
        p <- NA_integer_
        evd <- NULL
        if (identical(r$action, "scale_error")) {
          vmin_t <- rp(r, "volatility_min", 0.6)
          cand <- setdiff(which(vol >= vmin_t & is.finite(mag) &
                                  mag != mode_mag), corrected_pos)
          if (length(cand) > 0) {
            p <- cand[which.max(vol[cand])]
            evd <- paste0("local log-volatility ", vol[p], " >= ", vmin_t,
                          "; magnitude ", fnum(mag[p]),
                          " differs from the household modal magnitude ",
                          fnum(mode_mag))
          }
        } else if (identical(r$action, "category_bound_violation")) {
          uf <- rp(r, "upper_factor", 1.5)
          lf <- rp(r, "lower_factor", 0.5)
          over <- is.finite(gv) & is.finite(gmax) & gv > gmax * uf
          under <- is.finite(gv) & is.finite(gmin) & gmin > 0 & gv < gmin * lf
          cand <- setdiff(which(over | under), corrected_pos)
          if (length(cand) > 0) {
            ratios <- vapply(cand, function(q) {
              bound_deviation_ratio(gv[q], gmin[q], gmax[q])
            }, numeric(1))
            p <- cand[which.max(ratios)]
            evd <- if (isTRUE(over[p])) {
              paste0("value ", fnum(gv[p]), " exceeds ", uf,
                     " x the category upper bound ", fnum(gmax[p]))
            } else {
              paste0("value ", fnum(gv[p]), " below ", lf,
                     " x the category lower bound ", fnum(gmin[p]))
            }
          }
        } else if (identical(r$action, "exceeds_cap")) {
          cand <- setdiff(which(is.finite(gv) & gv > cap), corrected_pos)
          if (length(cand) > 0) {
            p <- cand[which.max(gv[cand])]
            evd <- paste0("value ", fnum(gv[p]),
                          " exceeds the income cap ", fnum(cap))
          }
        } else if (identical(r$action, "robust_consensus")) {
          min_obs <- rp(r, "min_obs", 4)
          vmin_t <- rp(r, "volatility_min", 0.4)
          if (sum(is.finite(gv)) >= min_obs) {
            cres <- detect_outliers_consensus(
              gv,
              methods = as.character(unlist(r$params$methods %||%
                                              list("iqr", "mad"))),
              consensus = rp(r, "consensus", 1),
              thresholds = r$params$thresholds
            )
            cand <- setdiff(which(cres$outliers & vol >= vmin_t),
                            corrected_pos)
            if (length(cand) > 0) {
              p <- cand[which.max(vol[cand])]
              by <- names(which(vapply(cres$by_method,
                                       function(m) isTRUE(m[p]),
                                       logical(1))))
              evd <- paste0("flagged by ", paste(by, collapse = "+"),
                            " on the household series; local log-volatility ",
                            vol[p], " >= ", vmin_t)
            }
          }
        } else if (identical(r$action, "extreme_robust_z")) {
          min_obs <- rp(r, "min_obs", 3)
          thr <- rp(r, "threshold", 3)
          min_rd <- rp(r, "min_relative_deviation", 0.3)
          if (sum(is.finite(gv)) >= min_obs) {
            z <- robust_zscore(gv)
            zc <- setdiff(which(is.finite(z) & abs(z) > thr), corrected_pos)
            if (length(zc) > 0) {
              # a tiny MAD in a tightly clustered household lets modest
              # fluctuations exceed the z threshold; require a material
              # deviation from the median of the other waves as well
              rd <- vapply(zc, function(q) {
                oth <- finite_vals(gv[-q])
                if (length(oth) == 0) return(Inf)
                m <- stats::median(oth)
                if (m == 0) return(Inf)
                abs(gv[q] - m) / abs(m)
              }, numeric(1))
              cand <- zc[rd >= min_rd]
              if (length(cand) > 0) {
                j <- which.max(abs(z[cand]))
                p <- cand[j]
                evd <- paste0("modified z-score ", signif(z[p], 3),
                              " beyond +/-", thr, "; value ",
                              fnum(100 * rd[rd >= min_rd][j]),
                              "% from the household median of the other waves")
              }
            }
          }
        }
        if (!is.null(evd)) {
          hit <- list(rule = r, p = p, evidence = evd)
          break
        }
      }
      if (is.null(hit)) break

      p <- hit$p
      new_v <- apply_cell(idx[p], p, hit$rule, hit$evidence, gv,
                          gmin[p], gmax[p])
      gv[p] <- new_v
      w[idx[p]] <- new_v
      corrected_pos <- c(corrected_pos, p)
    }
  }
  if (verbose) {
    cli::cli_alert_info("household stage: {n_groups_processed} group{?s} with 2+ observed values processed")
  }

  # ---- F01: hard plausibility cap ---------------------------------------------
  # disposition (ruleset key or params override): "void" sets offending
  # values to NA (the historical behavior), "winsorise" clamps over-cap
  # values to the cap while still voiding non-positive values (they carry
  # no magnitude to clamp to), and "flag" only ledgers, leaving every
  # value in place. genuine top incomes exist, so hard-voiding them
  # biases the right tail; the disposition makes that trade-off explicit
  # and reversible.
  f01 <- first_rule(rs, "finalization_rules", "hard_cap_to_na")
  if (rule_enabled(f01)) {
    f01_disp <- as.character(f01$params$disposition %||%
                               f01$disposition %||% "void")
    idx <- which(is.finite(w) & (w <= 0 | w > cap))
    n_voided_f <- 0L
    n_wins_f <- 0L
    n_flag_f <- 0L
    na_b <- sum(is.na(w))
    for (i in idx) {
      over <- w[i] > cap
      evd <- if (!over) {
        paste0("post-correction value ", fnum(w[i]), " is non-positive")
      } else {
        paste0("post-correction value ", fnum(w[i]),
               " still exceeds the income cap ", fnum(cap))
      }
      if (identical(f01_disp, "flag")) {
        ledger_add(ld, f01$rule_id, "cap_flag", TRUE, i, pid[i], hid[i],
                   wv[i], tv, observed = w[i], corrected = NA_real_,
                   evidence = evd,
                   justification = "value outside the plausible range retained under disposition 'flag'; annotated for the researcher's own treatment")
        n_flag_f <- n_flag_f + 1L
      } else if (identical(f01_disp, "winsorise") && over) {
        ledger_add(ld, f01$rule_id, "cap_winsorise", applied_flag, i, pid[i],
                   hid[i], wv[i], tv, observed = w[i], corrected = cap,
                   evidence = evd,
                   justification = "value above the income cap clamped to the cap under disposition 'winsorise', preserving the observation's rank while bounding its magnitude")
        w[i] <- cap
        status[i] <- paste0("winsorised:", f01$rule_id)
        n_wins_f <- n_wins_f + 1L
      } else {
        ledger_add(ld, f01$rule_id, "cap_na", applied_flag, i, pid[i], hid[i],
                   wv[i], tv, observed = w[i], corrected = NA_real_,
                   evidence = evd,
                   justification = "value the constrained correction stage could not bring inside the plausible range; voided rather than retained")
        w[i] <- NA_real_
        status[i] <- paste0("capped:", f01$rule_id)
        n_voided_f <- n_voided_f + 1L
      }
    }
    if (length(idx) > 0) {
      if (isTRUE(f01$log %||% TRUE)) {
        act <- paste0("hard_cap:", f01_disp)
        logs <- c(logs, list(clean_log_entry(f01$rule_id, tv, act,
                                             length(idx),
                                             values_changed = n_voided_f + n_wins_f,
                                             na_before = na_b,
                                             na_after = sum(is.na(w)))))
      }
      if (verbose) {
        msg <- switch(f01_disp,
          "flag" = "{f01$rule_id}: {n_flag_f} out-of-range value{?s} flagged (disposition 'flag'; values retained)",
          "winsorise" = "{f01$rule_id}: {n_wins_f} value{?s} winsorised to the cap, {n_voided_f} non-positive value{?s} set to NA",
          "{f01$rule_id}: {n_voided_f} unrecoverable value{?s} set to NA")
        cli::cli_alert_info(msg)
      }
    }
  }

  # ---- D11: dataset-level flags -------------------------------------------------
  ds_flag <- NULL
  d11 <- first_rule(rs, "detection_rules", "dataset_consensus")
  if (rule_enabled(d11)) {
    cres <- detect_outliers_consensus(
      w,
      methods = as.character(unlist(d11$params$methods %||%
                                      list("iqr", "mad"))),
      consensus = rp(d11, "consensus", 1),
      thresholds = d11$params$thresholds
    )
    ds_flag <- rep(NA_character_, n)
    idx <- which(cres$outliers)
    for (i in idx) {
      by <- names(which(vapply(cres$by_method,
                               function(m) isTRUE(m[i]), logical(1))))
      ds_flag[i] <- paste(by, collapse = "+")
      bstr <- paste(vapply(by, function(m) {
        b <- cres$bounds[[m]]
        paste0(m, " [", fnum(b["lower"]), ", ", fnum(b["upper"]), "]")
      }, character(1)), collapse = "; ")
      ledger_add(ld, d11$rule_id, "flag", TRUE, i, pid[i], hid[i], wv[i], tv,
                 observed = w[i], corrected = NA_real_,
                 evidence = paste0("outside ", bstr,
                                   " on the cleaned distribution"),
                 justification = "dataset-level extreme; annotated only because such values can be genuine, leaving the analytic decision to the researcher")
    }
    if (isTRUE(d11$log %||% TRUE)) {
      logs <- c(logs, list(clean_log_entry(d11$rule_id, tv, "flag",
                                           length(idx))))
    }
    if (verbose) {
      cli::cli_alert_info("{d11$rule_id}: {length(idx)} dataset-level value{?s} flagged (annotation only)")
    }
  }

  # ---- assemble output -----------------------------------------------------------
  out <- df
  out[[obs_col]] <- observed
  if (identical(mode, "flag")) {
    out[[paste0(tv, "_proposed")]] <- w
    out[[paste0(tv, "_proposed_status")]] <- status
  } else {
    newcol <- w
    lab <- attr(df[[tv]], "label", exact = TRUE)
    if (!is.null(lab)) attr(newcol, "label") <- lab
    out[[tv]] <- newcol
    out[[paste0(tv, "_clean_status")]] <- status
  }
  if (!is.null(ds_flag)) out[[paste0(tv, "_dataset_flag")]] <- ds_flag
  if (was_tibble) out <- tibble::as_tibble(out)

  # ---- summary, log tail, result --------------------------------------------------
  decisions <- ledger_frame(ld)
  if (nrow(decisions) > 0) {
    agg <- stats::aggregate(list(n = rep(1L, nrow(decisions))),
                            by = list(rule_id = decisions$rule_id,
                                      action = decisions$action),
                            FUN = sum)
    agg <- agg[order(agg$rule_id, agg$action), , drop = FALSE]
    for (k in seq_len(nrow(agg))) {
      if (isFALSE(rule_log[[agg$rule_id[k]]])) next
      logs <- c(logs, list(clean_log_entry(agg$rule_id[k], tv,
                                           paste0("decisions:", agg$action[k]),
                                           agg$n[k],
                                           values_changed = agg$n[k])))
    }
  } else {
    agg <- data.frame(rule_id = character(0), action = character(0),
                      n = integer(0), stringsAsFactors = FALSE)
  }

  cnt <- function(act) sum(decisions$action == act & decisions$applied)
  summary <- list(
    mode = mode,
    target = tv,
    n_rows = n,
    n_groups = length(groups),
    n_groups_processed = n_groups_processed,
    n_finite_before = sum(is.finite(observed)),
    n_finite_after = sum(is.finite(w)),
    n_decisions = nrow(decisions),
    n_corrected = cnt("correct"),
    n_voided = cnt("set_na"),
    n_rectified = cnt("rectify_sign"),
    n_capped = cnt("cap_na"),
    n_winsorised = cnt("cap_winsorise"),
    n_cap_flagged = sum(decisions$action == "cap_flag"),
    n_hid_fallback = n_hid_fallback,
    n_ungroupable = n_ungroupable,
    n_bounds_voided = sum(decisions$action == "void_bounds"),
    n_flagged_dataset = sum(decisions$action == "flag"),
    pct_corrected = if (sum(is.finite(observed)) > 0) {
      round(100 * cnt("correct") / sum(is.finite(observed)), 2)
    } else {
      NA_real_
    },
    by_rule = agg,
    distribution_before = dist_stats(observed),
    distribution_after = dist_stats(w),
    remaining_over_cap = sum(is.finite(w) & w > cap),
    remaining_under_min = sum(is.finite(w) & w > 0 & w < mn),
    overrides = rs$meta$overrides %||% character(0),
    notes = vars$notes,
    duration_s = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  )
  logs <- c(logs, list(clean_log_entry("RUN", tv, paste0("end:", mode), n,
                                       values_changed = summary$n_decisions,
                                       na_before = n - summary$n_finite_before,
                                       na_after = n - summary$n_finite_after)))

  res <- structure(
    list(data = out, decisions = decisions, log = logs, summary = summary,
         ruleset = rs, variables = vars, mode = mode),
    class = "liss_clean_result"
  )

  if (!is.null(output_dir)) {
    liss_cleaning_report(res, output_dir, verbose = verbose)
  }

  if (verbose) {
    if (identical(mode, "flag")) {
      cli::cli_alert_success(
        "dry run complete: {summary$n_decisions} decision{?s} ledgered as proposals; data unchanged")
    } else {
      cli::cli_alert_success(
        "income cleaning complete: {summary$n_corrected} corrected, {summary$n_voided} voided, {summary$n_rectified} rectified, {summary$n_capped} capped, {summary$n_winsorised} winsorised, {summary$n_flagged_dataset} dataset-flagged")
    }
  }
  invisible(res)
}

# ---- result methods --------------------------------------------------------------

#' @export
print.liss_clean_result <- function(x, ...) {
  s <- x$summary
  cli::cli_h3("liss income cleaning result ({s$mode} mode)")
  cli::cli_bullets(c(
    "*" = "target {.field {s$target}}: {s$n_finite_before} finite value{?s} in, {s$n_finite_after} out ({s$n_rows} row{?s}, {s$n_groups_processed} household{?s} processed)",
    "*" = "{s$n_corrected} corrected, {s$n_voided} voided, {s$n_rectified} rectified, {s$n_capped} capped, {s$n_winsorised %||% 0} winsorised, {s$n_flagged_dataset} dataset-flagged",
    "*" = "{s$n_decisions} ledgered decision{?s}; inspect $decisions or write artifacts with liss_cleaning_report()"
  ))
  invisible(x)
}

#' @export
summary.liss_clean_result <- function(object, ...) {
  s <- object$summary
  cat("\n=== Income Cleaning Summary (", s$mode, " mode) ===\n", sep = "")
  cat("Rows:", s$n_rows, " Households processed:", s$n_groups_processed, "\n")
  cat("Finite target values: ", s$n_finite_before, " -> ",
      s$n_finite_after, "\n", sep = "")
  cat("Corrected:", s$n_corrected,
      paste0("(", ifelse(is.na(s$pct_corrected), "NA", s$pct_corrected), "%)"),
      " Voided:", s$n_voided, " Rectified:", s$n_rectified,
      " Capped:", s$n_capped, " Winsorised:", s$n_winsorised %||% 0, "\n")
  cat("Dataset-level flags:", s$n_flagged_dataset, "\n")
  fmt_dist <- function(d, label) {
    if (is.null(d)) return(invisible(NULL))
    cat("\n", label, " distribution:\n", sep = "")
    cat("  n:", d$n, " min:", round(d$min), " q1:", round(d$q1),
        " median:", round(d$median), " mean:", round(d$mean), "\n")
    cat("  q3:", round(d$q3), " max:", round(d$max),
        " sd:", round(d$sd), " mad:", round(d$mad), "\n")
  }
  fmt_dist(s$distribution_before, "Observed")
  fmt_dist(s$distribution_after, "Cleaned")
  cat("==============================================\n")
  invisible(s)
}

# ---- report ------------------------------------------------------------------------

#' Write the income-cleaning report and audit artifacts
#'
#' Renders a markdown report with the run configuration, the full
#' methodology generated from the ruleset (every rule with its
#' description, rationale, parameters, and references), result
#' summaries with observed-versus-cleaned distributions, and a decision
#' appendix. Alongside the report, the complete decision ledger is
#' written as CSV and the engine-shaped audit log as JSONL.
#'
#' @param result a `liss_clean_result` from [liss_clean_income()].
#' @param output_dir directory for the artifacts (created if needed).
#'   Required, so three files are never written into the working
#'   directory by accident.
#' @param verbose print the written paths.
#' @return invisibly, a list with the `report`, `decisions`, and `log`
#'   paths.
#' @export
liss_cleaning_report <- function(result, output_dir, verbose = TRUE) {
  if (!inherits(result, "liss_clean_result")) {
    cli::cli_abort("`result` must be a liss_clean_result from liss_clean_income()")
  }
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  lcfg <- result$ruleset$logging %||% list()
  paths <- list(
    report = file.path(output_dir,
                       lcfg$report_file %||% "income_cleaning_report.md"),
    decisions = file.path(output_dir,
                          lcfg$decisions_file %||% "income_cleaning_decisions.csv"),
    log = file.path(output_dir,
                    lcfg$log_file %||% "income_cleaning_log.jsonl")
  )
  utils::write.csv(result$decisions, paths$decisions, row.names = FALSE,
                   na = "")
  write_clean_jsonl(result$log, paths$log)
  writeLines(build_cleaning_report_md(result, lcfg, basename(paths$decisions)),
             paths$report)
  if (verbose) {
    cli::cli_inform("  report: {.file {paths$report}}")
    cli::cli_inform("  decisions: {.file {paths$decisions}}")
    cli::cli_inform("  log: {.file {paths$log}}")
  }
  invisible(paths)
}

# render the markdown report body
build_cleaning_report_md <- function(result, lcfg, decisions_name) {
  rs <- result$ruleset
  s <- result$summary
  dec <- result$decisions
  refs <- rs$references %||% list()

  pkg_ver <- tryCatch(as.character(utils::packageVersion("lissr")),
                      error = function(e) "unpackaged")

  lines <- c(
    "# LISS Income Cleaning Report",
    "",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("Ruleset: ", rs$meta$ruleset, " v", rs$meta$ruleset_version,
           " (schema ", rs$meta$schema_version, ")"),
    paste0("Engine: lissr ", pkg_ver),
    paste0("Mode: ", s$mode),
    paste0("Target variable: ", s$target),
    paste0("Rows: ", s$n_rows, "; households processed: ",
           s$n_groups_processed),
    paste0("Finite target values: ", s$n_finite_before, " observed, ",
           s$n_finite_after, " after cleaning"),
    ""
  )
  if (length(s$notes) > 0) {
    lines <- c(lines, paste0("Note: ", s$notes), "")
  }

  lines <- c(lines,
    "## Configuration",
    "",
    paste0("- income_cap: ", fmt_plain(rs$constraints$income_cap)),
    paste0("- min_income: ", fmt_plain(rs$constraints$min_income)),
    paste0("- wavenr_origin: ", fmt_plain(rs$constraints$wavenr_origin %||% 2007)),
    if (length(s$overrides) > 0) {
      c("- overrides applied:", paste0("    - ", s$overrides))
    } else {
      "- overrides applied: none"
    },
    ""
  )

  # methodology, generated from the ruleset itself
  lines <- c(lines, "## Methodology", "",
             paste0("Every decision below is taken by a rule defined in the ",
                    "ruleset; disabled rules are listed for completeness and ",
                    "marked as such. Researchers can disable or ",
                    "re-parameterise any rule via liss_clean_income() or a ",
                    "custom ruleset file."),
             "")
  section_titles <- c(
    preparation_rules = "Preparation",
    detection_rules = "Detection",
    correction_rules = "Correction candidates",
    finalization_rules = "Finalization"
  )
  for (section in names(section_titles)) {
    rules <- rs[[section]] %||% list()
    if (length(rules) == 0) next
    lines <- c(lines, paste0("### ", section_titles[[section]]), "")
    for (r in rules) {
      en <- isTRUE(r$enabled %||% TRUE)
      lines <- c(lines,
                 paste0("#### ", r$rule_id, " (", r$action, ")",
                        if (!en) " [disabled]" else ""),
                 "",
                 gsub("\\s+", " ", r$description %||% ""),
                 "")
      if (nzchar(r$rationale %||% "")) {
        lines <- c(lines,
                   paste0("Rationale: ", gsub("\\s+", " ", r$rationale)), "")
      }
      pr <- r$params %||% list()
      if (length(pr) > 0) {
        pstr <- paste(vapply(names(pr), function(nm) {
          paste0(nm, " = ",
                 paste(vapply(unlist(pr[[nm]]), fmt_plain, character(1)),
                       collapse = "/"))
        }, character(1)), collapse = ", ")
        lines <- c(lines, paste0("Parameters: ", pstr), "")
      }
      rk <- as.character(unlist(r$references %||% list()))
      if (length(rk) > 0) {
        full <- vapply(rk, function(k) {
          gsub("\\s+", " ", refs[[k]] %||% k)
        }, character(1))
        lines <- c(lines, paste0("References: ",
                                 paste(full, collapse = " ")), "")
      }
    }
  }
  sel <- rs$selection %||% list()
  lines <- c(lines, "### Candidate selection", "",
             paste0("Admissible candidates are filtered to the cell's valid ",
                    "range; the candidate closest to the ",
                    sel$anchor %||% "household_median",
                    " is applied, falling back to the ",
                    sel$fallback_anchor %||% "range_midpoint",
                    " when the household offers no other observed value. ",
                    "Generation order is the deterministic tie-break."),
             "")

  # results
  lines <- c(lines, "## Results", "")
  lines <- c(lines,
             paste0("- corrected: ", s$n_corrected,
                    if (!is.na(s$pct_corrected)) {
                      paste0(" (", s$pct_corrected,
                             "% of observed finite values)")
                    } else {
                      ""
                    }),
             paste0("- voided: ", s$n_voided),
             paste0("- sign-rectified: ", s$n_rectified),
             paste0("- capped to NA: ", s$n_capped),
             paste0("- winsorised to the cap: ", s$n_winsorised %||% 0),
             paste0("- out-of-range values flagged (F01 disposition): ",
                    s$n_cap_flagged %||% 0),
             paste0("- rows grouped by person id (household id missing): ",
                    s$n_hid_fallback %||% 0,
                    "; rows with no usable group id: ",
                    s$n_ungroupable %||% 0),
             paste0("- category bounds voided: ", s$n_bounds_voided),
             paste0("- dataset-level flags (annotation only): ",
                    s$n_flagged_dataset),
             paste0("- values still above the cap: ", s$remaining_over_cap,
                    "; positive values below min_income: ",
                    s$remaining_under_min),
             "")
  if (nrow(s$by_rule) > 0) {
    lines <- c(lines, "Decisions per rule:", "",
               "| rule | action | decisions |",
               "|------|--------|-----------|")
    for (k in seq_len(nrow(s$by_rule))) {
      lines <- c(lines, paste0("| ", s$by_rule$rule_id[k], " | ",
                               s$by_rule$action[k], " | ",
                               s$by_rule$n[k], " |"))
    }
    lines <- c(lines, "")
  }
  dist_row <- function(nm, f, digits = 0) {
    b <- s$distribution_before
    a <- s$distribution_after
    paste0("| ", nm, " | ",
           if (is.null(b)) "" else fnum(f(b), digits), " | ",
           if (is.null(a)) "" else fnum(f(a), digits), " |")
  }
  lines <- c(lines, "Distribution (observed vs cleaned):", "",
             "| statistic | observed | cleaned |",
             "|-----------|----------|---------|",
             dist_row("n", function(d) d$n),
             dist_row("min", function(d) d$min),
             dist_row("q1", function(d) d$q1),
             dist_row("median", function(d) d$median),
             dist_row("mean", function(d) d$mean),
             dist_row("q3", function(d) d$q3),
             dist_row("max", function(d) d$max),
             dist_row("sd", function(d) d$sd),
             dist_row("mad", function(d) d$mad),
             "")

  # decision appendix
  max_rows <- lcfg$max_appendix_rows %||% 500
  lines <- c(lines, "## Decision appendix", "",
             paste0("Every decision is recorded in ", decisions_name,
                    " with the full candidate set, evidence, and ",
                    "justification. ",
                    if (nrow(dec) > max_rows) {
                      paste0("The first ", max_rows, " of ", nrow(dec),
                             " decisions are reproduced here.")
                    } else {
                      paste0("All ", nrow(dec),
                             " decisions are reproduced here.")
                    }),
             "")
  if (nrow(dec) > 0) {
    show <- utils::head(dec, max_rows)
    lines <- c(lines,
               "| id | rule | action | applied | person | household | wave | observed | corrected | source |",
               "|----|------|--------|---------|--------|-----------|------|----------|-----------|--------|")
    for (k in seq_len(nrow(show))) {
      lines <- c(lines, paste0(
        "| ", show$decision_id[k],
        " | ", show$rule_id[k],
        " | ", show$action[k],
        " | ", ifelse(show$applied[k], "yes", "no"),
        " | ", show$person_id[k],
        " | ", show$household_id[k],
        " | ", ifelse(is.na(show$wave[k]), "", show$wave[k]),
        " | ", ifelse(is.na(show$observed[k]), "", fnum(show$observed[k])),
        " | ", ifelse(is.na(show$corrected[k]), "", fnum(show$corrected[k])),
        " | ", ifelse(is.na(show$candidate_source[k]), "",
                      show$candidate_source[k]),
        " |"))
    }
    lines <- c(lines, "")
  }

  c(lines,
    "## Reproducibility",
    "",
    paste0("Ruleset source: ", rs$meta$source_path %||% "(in memory)"),
    "",
    "To reproduce this run:",
    "",
    "```r",
    paste0("res <- lissr::liss_clean_income(data, mode = \"", s$mode, "\")"),
    "```",
    "",
    "To disable a rule or change a threshold:",
    "",
    "```r",
    "res <- lissr::liss_clean_income(",
    "  data,",
    "  disable = c(\"D10\"),",
    "  params = list(D06 = list(volatility_min = 0.7))",
    ")",
    "```",
    "",
    paste0("Original values are preserved in ", s$target,
           "_observed; applying the ledger's observed column back over ",
           "the corrected cells reproduces the input exactly."))
}

# ---- equivalised income --------------------------------------------------------------

#' Equivalise household income
#'
#' Converts household income to a per-equivalent-adult scale for
#' cross-household comparison. The default `"weighted_sqrt"` scale
#' divides by `(adults + child_weight * children)^elasticity`, the
#' scale used by the source analysis pipelines; `"oecd_modified"`
#' divides by `1 + 0.5 * (adults - 1) + 0.3 * children`; `"sqrt"`
#' divides by the square root of household size. Rows with an invalid
#' composition (size below one, negative children, or zero adults,
#' since every scale presumes at least one adult) yield NA and are
#' counted in a warning. `household_size` and `n_children` must have
#' length 1 or the length of `income`; other lengths are an error
#' rather than being silently recycled.
#'
#' @details
#' The modified OECD scale defines children as household members under
#' 14, whereas the LISS `aantalki` variable counts children living at
#' home of any age. Passing `aantalki` therefore approximates the OECD
#' scale by treating every at-home child as under 14; when an under-14
#' count is available it should be preferred. The `"weighted_sqrt"`
#' default is calibrated to `aantalki` and unaffected.
#'
#' @param income numeric household income.
#' @param household_size total household members (LISS `aantalhh`).
#' @param n_children number of children (LISS `aantalki`; see Details
#'   for the OECD under-14 caveat).
#' @param scale equivalence scale, see details.
#' @param child_weight weight per child under `"weighted_sqrt"`.
#' @param elasticity size elasticity under `"weighted_sqrt"`.
#' @param verbose warn about invalid compositions.
#' @return numeric vector of equivalised income.
#' @examples
#' liss_equivalise_income(30000, household_size = 3, n_children = 1)
#' liss_equivalise_income(30000, 3, 1, scale = "oecd_modified")
#' @export
liss_equivalise_income <- function(income, household_size, n_children = 0,
                                   scale = c("weighted_sqrt",
                                             "oecd_modified", "sqrt"),
                                   child_weight = 0.8, elasticity = 0.5,
                                   verbose = TRUE) {
  scale <- match.arg(scale)
  out <- equivalise_income_kernel(numeric_view(income),
                                  numeric_view(household_size),
                                  numeric_view(n_children),
                                  scale = scale,
                                  child_weight = child_weight,
                                  elasticity = elasticity)
  hh <- rep_len(numeric_view(household_size), length(out))
  nc <- rep_len(numeric_view(n_children), length(out))
  bad <- !is.finite(hh) | !is.finite(nc) | hh < 1 | nc < 0 | (hh - nc) < 1
  if (verbose && any(bad)) {
    cli::cli_warn(
      "{sum(bad)} row{?s} with invalid household composition set to NA")
  }
  out
}
