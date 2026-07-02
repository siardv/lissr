# ============================================================================
# liss_merge_engine.R, unified recipe-driven merge engine for LISS panel data
# ============================================================================
# implements all 11 review improvements.
# processes any YAML recipe conforming to CANONICAL_SCHEMA.md v1.0.0 or v1.1.0 (additive).
#
# key capabilities:
#   - pre-flight schema validation with fail-fast
#   - controlled action vocabulary; unknown/empty actions rejected (#1)
#   - note_only as first-class action (#1)
#   - audit-grade JSONL logging with summary artifact (#9)
#   - expected_presence enforcement (#6)
#   - comparability contracts on boundary rules (#5)
#   - role_map resolution for semantic variable targeting (#3)
#   - taxonomy_refs support (#4)
#   - anomaly registry integration (#7)

# ============================================================================
# controlled action vocabulary
# ============================================================================
# the built-in superset below mirrors the shared action_vocabulary.yml.
# when that file is found (next to the engine or in the working dir) it is
# loaded and becomes authoritative, so the engine validator, the CLI validator,
# and CANONICAL_SCHEMA.md share one vocabulary.

`%||%` <- function(x, y) if (is.null(x)) y else x

.load_action_vocab <- function(path) {
  reg  <- yaml::yaml.load_file(path)
  secs <- c("variable_rules", "harmonization_rules",
            "boundary_rules", "drop_retain_rules")
  out  <- stats::setNames(vector("list", length(secs)), secs)
  for (s in secs)
    out[[s]] <- unique(unlist(lapply(reg$actions, function(a)
      if (s %in% (a$sections %||% character(0))) a$name else NULL)))
  out
}

VALID_ACTIONS <- list(
  variable_rules = c(
    "strip_prefix", "type_coerce", "coerce_type", "coerce_numeric",
    "rename", "rename_to_suffix", "set_label", "apply_labelled_policy",
    "strip_value_labels", "strip_labels_coerce_numeric",
    "ensure_column", "use_global_policy", "identity",
    "derive_fieldwork_month", "parse_time", "safe_integer_cast",
    "note_only"
  ),
  harmonization_rules = c(
    "recode_to_na", "recode_sentinels_to_na", "na_recode", "recode",
    "value_recode", "fix_label", "crosswalk", "conditional_label_swap",
    "strip_question_stem", "lowercase_labels", "normalize_labels",
    "label_to_string", "extend_categories", "type_coerce",
    "flag_only", "flag_absence",
    "derive_combined_party", "derive_fieldwork_month", "parse_time",
    "subtract", "note_only"
  ),
  boundary_rules = c(
    "add_era_flag", "add_flag", "add_period_flag", "split_variable",
    "structural_na", "structural_note", "filter_rows", "drop_outside",
    "crosswalk_rename", "stack_aux_files", "safe_integer_cast",
    "retain_but_flag", "retain_where_present",
    "derive_combined_party", "fix_label", "note_only"
  ),
  drop_retain_rules = c(
    "drop", "drop_outside", "retain", "retain_if_present",
    "retain_as_metadata_only", "retain_but_flag", "retain_where_present",
    "flag_only", "flag_absence", "note_only"
  )
)

# prefer the shared registry on disk; fall back to the superset above. installed
# it ships in inst/extdata; sourced standalone it sits next to the engine or in
# the working dir.
local({
  cand <- c(system.file("extdata", "action_vocabulary.yml", package = "lissr"),
            file.path(getwd(), "action_vocabulary.yml"), "action_vocabulary.yml")
  hit  <- cand[nzchar(cand) & file.exists(cand)]
  if (length(hit) >= 1)
    VALID_ACTIONS <<- tryCatch(.load_action_vocab(hit[[1]]),
                               error = function(e) VALID_ACTIONS)
})

# rule-level keys the engine and executors actually consult, as a global union
# across the four rule sections. derived by source scan of rule$<key> and
# rule[["<key>"]] accesses (the rule object is never aliased or accessed
# dynamically, so this is exhaustive); executors read resolved payloads, not
# rule keys. validate_recipe uses this set to flag unrecognized rule keys at
# authoring time. recognition only; honoring a new key requires an engine change.
RECOGNIZED_RULE_KEYS <- c(
  "action", "anomaly_ref", "assignments", "codes", "column", "columns",
  "combined_label", "comparability", "corrected_label", "crosswalk", "default",
  "derived_suffix", "description", "early_label", "eras", "exclude", "flag_column",
  "flag_name", "flag_true_waves", "flag_value_post", "flag_value_pre",
  "flag_variable", "from_value", "if_absent", "keep", "keep_values", "label_map",
  "late_label", "mapping", "new_fragment", "offset", "old_fragment",
  "output_scheme_flag", "output_variable", "output_vars", "parties_to_pool",
  "party_names_to_pool", "pattern", "phases", "post_recode", "prefix",
  "present_in_waves", "recode", "recodes", "retain", "retain_in", "rule_id",
  "scheme_column", "scope", "sentinel_values", "set_label", "source",
  "source_column", "source_variable", "sources", "stem", "stems", "suffixes",
  "suffixes_range", "swap", "target", "target_column", "target_type",
  "target_variable", "target_variables", "to_value", "transforms", "value",
  "variable", "variable_pattern", "variables", "variables_pattern", "wave",
  "waves", "waves_early", "waves_late", "waves_post", "waves_pre"
)

# sanctioned non-executed rule keys: documentation and provenance the engine
# deliberately ignores. these never warn. the minimal annotation set plus the
# boundary and coverage provenance layer.
SANCTIONED_RULE_KEYS <- c(
  # minimal annotation
  "description", "log", "note", "notes", "reason", "guidance",
  "label", "boundary", "pool", "required", "sentinel_label",
  "fill_missing_waves",
  # boundary and coverage provenance
  "absent_cw19l_only", "absent_from_waves", "absent_in", "absent_waves",
  "conservative_pooling", "cw19l_only_variables", "deleted_without_replacement",
  "discontinued_blocks", "dropped_variables", "introductions",
  "later_drops_outside_the_93", "missing_reason", "new_variables_pattern",
  "non_comparable_replacements", "nonexistent_ids_matched_by_pattern",
  "pooling_allowed", "post_check", "present_waves", "reintroduced_in_cw25r",
  "restored_wave", "review_required", "structurally_missing_waves",
  "waves_absent", "waves_absent_from", "waves_available", "waves_new",
  "waves_old", "waves_present"
)

# load the executor kernels (crosswalk, derived_variables, transform).
# installed they are a namespace file (R/liss_executors.R) and already present;
# sourced standalone they are read from disk. without them the new action
# branches and the derived-variable build fall back to their no-op behaviour.
.lissr_have_executors <- exists("dv_aggregate", mode = "function")
if (!.lissr_have_executors) {
  cand <- c(file.path(getwd(), "liss_executors.R"), "liss_executors.R")
  hit  <- cand[nzchar(cand) & file.exists(cand)]
  if (length(hit) >= 1) {
    source(hit[[1]])
    .lissr_have_executors <- TRUE
  }
}

# ============================================================================
# 1. RECIPE LOADING & PRE-FLIGHT VALIDATION
# ============================================================================

#' load and validate a canonical YAML merge recipe
#'
#' reads a YAML recipe from disk, runs the pre-flight schema validator,
#' and returns the parsed recipe list.
#'
#' @param path character. path to a YAML recipe file.
#' @return a named list representing the parsed recipe.
#' @export
load_recipe <- function(path) {
  cli::cli_inform("loading recipe: {.file {path}}")
  txt <- readLines(path, warn = FALSE, encoding = "UTF-8")
  r   <- yaml::yaml.load(paste(txt, collapse = "\n"))

  # run pre-flight validation (fail-fast)
  validate_recipe(r, path)
  r
}

#' validate a merge recipe against the canonical schema
#'
#' checks required sections, field presence, action vocabulary,
#' anomaly_ref format, and rule_id uniqueness. aborts on any violation.
#'
#' @param recipe a named list (parsed YAML recipe).
#' @param path character. file path used in error messages.
#' @return invisible `TRUE` on success (aborts otherwise).
#' @export
validate_recipe <- function(recipe, path = "<unknown>") {
  errors <- character(0)

  # required top-level sections
  required_sections <- c("meta", "global", "wave_index", "logging")
  missing_sections <- setdiff(required_sections, names(recipe))
  if (length(missing_sections) > 0)
    errors <- c(errors, paste0("missing required sections: ",
                               paste(missing_sections, collapse = ", ")))

  # required meta fields
  if ("meta" %in% names(recipe)) {
    meta_required <- c("module", "module_label", "schema_version",
                       "recipe_version", "created", "source_spec", "covered_waves")
    for (f in meta_required) {
      val <- recipe$meta[[f]]
      if (is.null(val) || (is.character(val) && length(val) == 1L && nchar(val) == 0) ||
          length(val) == 0)
        errors <- c(errors, paste0("meta$", f, " is missing or empty"))
    }
  }

  # required global fields
  if ("global" %in% names(recipe)) {
    global_required <- c("id_variable", "wave_variable", "year_variable",
                         "labelled_policy", "missing_variable_policy",
                         "strip_label_whitespace")
    for (f in global_required) {
      if (is.null(recipe$global[[f]]))
        errors <- c(errors, paste0("global$", f, " is missing"))
    }
    # enum checks
    lp <- recipe$global$labelled_policy
    if (!is.null(lp) && !(lp %in% c("to_numeric", "to_factor", "keep_labelled")))
      errors <- c(errors, paste0("global$labelled_policy invalid: '", lp, "'"))

    mp <- recipe$global$missing_variable_policy
    if (!is.null(mp) && !(mp %in% c("error", "warn_and_skip", "warn_and_create_na")))
      errors <- c(errors, paste0("global$missing_variable_policy invalid: '", mp, "'"))
  }

  # wave_index entries (guard a pattern-style non-list wave_index)
  wi <- recipe$wave_index
  if (!is.null(wi) &&
      (!is.list(wi) || (length(wi) > 0 && !all(vapply(wi, is.list, logical(1)))))) {
    errors <- c(errors, paste0("wave_index is not a list of wave entries; ",
                "pattern-style wave_index is out of scope this cycle"))
  } else {
    for (i in seq_along(wi %||% list())) {
      w <- wi[[i]]
      for (f in c("id", "year", "file_pattern")) {
        if (is.null(w[[f]]))
          errors <- c(errors, paste0("wave_index[", i, "]$", f, " is missing"))
      }
    }
  }

  # rule sections: action vocabulary + required fields
  # also collect rule-level keys the engine neither consults nor sanctions
  unknown_rule_keys <- character(0)
  rule_sections <- c("variable_rules", "harmonization_rules",
                     "boundary_rules", "drop_retain_rules")
  for (section in rule_sections) {
    rules <- recipe[[section]] %||% list()
    seen_ids <- character(0)
    for (j in seq_along(rules)) {
      rule <- rules[[j]]
      rid <- paste0(rule$rule_id %||% "", collapse = "")
      act <- paste0(rule$action %||% "", collapse = "")
      desc <- paste0(rule$description %||% "", collapse = "")

      if (nchar(rid) == 0)
        errors <- c(errors, paste0(section, "[", j, "] has empty rule_id"))
      if (nchar(act) == 0)
        errors <- c(errors, paste0(section, "[", j, "] has empty action"))
      if (nchar(desc) == 0)
        errors <- c(errors, paste0(section, "[", j, "] has empty description"))

      # check action in controlled vocabulary
      vocab <- VALID_ACTIONS[[section]]
      if (!is.null(vocab) && nchar(act) > 0 && !(act %in% vocab))
        errors <- c(errors, paste0(section, "[", j, "] unknown action: '", act, "'"))

      # check anomaly_ref format: canonical A-NN (rule sections only)
      aref <- rule$anomaly_ref
      if (!is.null(aref) && is.character(aref) && length(aref) == 1L && nchar(aref) > 0) {
        if (!grepl("^A-\\d{2,}$", aref))
          errors <- c(errors, paste0(section, "[", j,
                      "] anomaly_ref '", aref, "' not in canonical A-NN format"))
      }

      # duplicate rule_id check
      if (nchar(rid) > 0) {
        if (rid %in% seen_ids)
          errors <- c(errors, paste0(section, " has duplicate rule_id: '", rid, "'"))
        seen_ids <- c(seen_ids, rid)
      }

      # flag rule-level keys the engine does not consult and has not sanctioned
      # as documentation; advisory only, does not affect the validation outcome
      rk <- names(rule)
      if (!is.null(rk)) {
        unk <- setdiff(rk[nzchar(rk)],
                       c(RECOGNIZED_RULE_KEYS, SANCTIONED_RULE_KEYS))
        if (length(unk) > 0) {
          who <- if (nchar(rid) > 0) rid else paste0(section, "[", j, "]")
          unknown_rule_keys <- c(unknown_rule_keys,
            paste0(section, " '", who, "': ", paste(unk, collapse = ", ")))
        }
      }
    }
  }

  # validation_checks severity
  for (j in seq_along(recipe$validation_checks %||% list())) {
    chk <- recipe$validation_checks[[j]]
    sev <- paste0(chk$severity %||% "", collapse = "")
    if (nchar(sev) > 0 && !(sev %in% c("error", "warning", "info")))
      errors <- c(errors, paste0("validation_checks[", j,
                  "] invalid severity: '", sev, "'"))
  }

  # derived_variables: canonical keys rule_id + name (var_name also accepted)
  for (j in seq_along(recipe$derived_variables %||% list())) {
    dv  <- recipe$derived_variables[[j]]
    rid <- paste0(dv$rule_id %||% "", collapse = "")
    if (nchar(rid) == 0)
      errors <- c(errors, paste0("derived_variables[", j, "] has empty rule_id"))
    nm  <- paste0(dv$name %||% "", collapse = "")
    nmo <- paste0(dv$var_name %||% "", collapse = "")
    if (nchar(nm) == 0 && nchar(nmo) == 0)
      errors <- c(errors, paste0("derived_variables[", j, "] has empty name"))
  }

  # emit one non-fatal warning listing unrecognized rule keys. does not change
  # the validation outcome, control flow, return value, or merge output.
  if (length(unknown_rule_keys) > 0) {
    names(unknown_rule_keys) <- rep("*", length(unknown_rule_keys))
    cli::cli_warn(c(paste0(
      "recipe {.file {path}} has unrecognized rule-level key(s) in ",
      "{length(unknown_rule_keys)} rule(s); the engine ignores these"),
      unknown_rule_keys))
  }

  if (length(errors) > 0) {
    names(errors) <- rep("x", length(errors))
    cli::cli_abort(c("recipe has {length(errors)} schema violation(s); aborting", errors))
  }

  cli::cli_alert_success("schema validation passed for {.file {path}}")
  invisible(TRUE)
}

#' load multiple recipes
#'
#' @param ... one or more paths to YAML recipe files.
#' @return a named list of parsed recipes, keyed by module code.
#' @noRd
load_recipes <- function(...) {
  paths <- unlist(list(...))
  recipes <- purrr::map(paths, load_recipe)
  names(recipes) <- purrr::map_chr(recipes, ~ .x$meta$module)
  recipes
}

# ============================================================================
# 2. WAVE FILE DISCOVERY & LOADING
# ============================================================================

#' discover data files for each wave in a recipe (internal)
#' @noRd
discover_wave_files <- function(recipe, data_dir) {
  wave_idx <- recipe$wave_index
  files <- purrr::map(wave_idx, function(w) {
    pat <- w$file_pattern %||% paste0(w$id, "_*")
    pat_re <- if (grepl("[*?]", pat)) utils::glob2rx(pat) else pat
    found <- list.files(data_dir, pattern = pat_re, full.names = TRUE,
                        ignore.case = TRUE)

    # fallback: wave_id prefix limited to data extensions, so codebooks and
    # other sidecar files can never be swept in (handles .sav/.csv/.dta mismatch)
    if (length(found) == 0) {
      fallback_re <- paste0("^", w$id, "[_.].*\\.(sav|zsav|dta|csv)$")
      found <- list.files(data_dir, pattern = fallback_re, full.names = TRUE,
                          ignore.case = TRUE)
      if (length(found) > 0)
        cli::cli_inform("  wave {.val {w$id}}: matched via fallback pattern")
    }

    if (length(found) == 0) {
      cli::cli_warn("no file found for wave {.val {w$id}} (tried {.val {pat}})")
      return(NULL)
    }

    # separate recipe-declared aux files (supplemental, disjoint respondents)
    # from primary candidates. aux entries match by basename, extension-agnostic.
    aux_decl <- as.character(unlist(w$aux_files %||% list()))
    is_aux <- rep(FALSE, length(found))
    if (length(aux_decl) > 0) {
      base_found <- basename(found)
      is_aux <- base_found %in% aux_decl |
        tools::file_path_sans_ext(base_found) %in%
          tools::file_path_sans_ext(aux_decl)
    }
    primary <- found[!is_aux]
    aux     <- found[is_aux]

    # declared aux files resolve independently of file_pattern: an explicitly
    # named aux entry is looked up in data_dir even when the primary glob does
    # not cover it, so narrowing the pattern cannot silently drop a declaration
    if (length(aux_decl) > 0) {
      have <- c(basename(aux), tools::file_path_sans_ext(basename(aux)))
      missing_aux <- aux_decl[!(aux_decl %in% have |
                                  tools::file_path_sans_ext(aux_decl) %in% have)]
      for (ad in missing_aux) {
        hit <- list.files(data_dir, pattern = utils::glob2rx(ad),
                          full.names = TRUE, ignore.case = TRUE)
        if (length(hit) == 0) {
          ext_agnostic <- paste0(tools::file_path_sans_ext(ad), ".*")
          hit <- list.files(data_dir, pattern = utils::glob2rx(ext_agnostic),
                            full.names = TRUE, ignore.case = TRUE)
        }
        if (length(hit) > 0) {
          aux <- c(aux, hit)
        } else {
          cli::cli_warn(paste0("wave '", w$id, "': declared aux file '", ad,
                               "' not found in data_dir"))
        }
      }
      aux <- unique(aux)
      primary <- setdiff(primary, aux)
    }

    # more than one primary candidate means superseded releases or stray
    # matches; prefer the highest release version, else demand disambiguation
    if (length(primary) > 1) {
      ver <- stringr::str_match(basename(primary),
                                "[_.](\\d+(?:[._]\\d+)?)p?(?=[._])")[, 2]
      ver_num <- suppressWarnings(as.numeric(gsub("_", ".", ver)))
      if (all(!is.na(ver_num)) && sum(ver_num == max(ver_num)) == 1) {
        keep <- primary[which.max(ver_num)]
        dropped <- setdiff(basename(primary), basename(keep))
        cli::cli_warn(paste0(
          "wave '", w$id, "': ", length(primary), " files match; using highest ",
          "release version '", basename(keep), "' and ignoring: ",
          paste(dropped, collapse = ", "),
          ". remove superseded files or narrow file_pattern to silence this."))
        primary <- keep
      } else {
        cli::cli_abort(c(
          paste0("wave '", w$id, "': ", length(primary),
                 " files match and release versions cannot be ranked"),
          "i" = paste0("candidates: ",
                       paste(basename(primary), collapse = ", ")),
          "i" = "narrow file_pattern in the recipe or remove the extra files"))
      }
    }
    if (length(primary) == 0) {
      cli::cli_warn(paste0("wave '", w$id, "': only aux_files matched; ",
                           "no primary data file found, skipping wave"))
      return(NULL)
    }
    list(wave_id = w$id, year = w$year, paths = primary,
         aux_paths = aux, wave_meta = w)
  })
  purrr::compact(files)
}

#' read a single wave data file (internal)
#' @noRd
read_wave_file <- function(path) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("The 'haven' package is required to read .sav/.dta files.\n",
         "Install it with: install.packages(\"haven\")", call. = FALSE)
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("The 'readr' package is required to read .csv files.\n",
         "Install it with: install.packages(\"readr\")", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("sav", "zsav")) {
    # user_na = TRUE keeps spss user-defined missing codes (dk/refusal
    # sentinels) as values so the recipe recodes can see and govern them;
    # declarations are stashed by the labelled policy and either round-tripped
    # to the output or swept to NA at write time (never leaked as values)
    haven::read_sav(path, user_na = TRUE)
  } else if (ext == "dta") {
    haven::read_dta(path)
  } else if (ext == "csv") {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    cli::cli_abort(c(
      "unsupported data file extension {.val {ext}} for {.file {path}}",
      "i" = "supported: .sav, .zsav, .dta, .csv",
      "i" = "a codebook or other non-data file may have matched the wave pattern"
    ))
  }
}

#' harmonize column types across wave data frames before stacking (internal)
#'
#' when the same column has different types across waves (e.g. character vs
#' hms/time, numeric vs character), coerce to the most general compatible type
#' so bind_rows succeeds.
#' @noRd
harmonize_column_types <- function(dfs) {
  # collect type signatures per column across all data frames
  all_cols <- unique(unlist(lapply(dfs, names)))
  type_map <- list()

  for (col in all_cols) {
    classes <- unique(unlist(lapply(dfs, function(df) {
      if (col %in% names(df)) class(df[[col]])[1] else NULL
    })))
    if (length(classes) > 1) {
      type_map[[col]] <- classes
    }
  }

  if (length(type_map) == 0) return(dfs)

  n_conflicts <- length(type_map)
  cli::cli_inform("  resolving {n_conflicts} column type conflict(s) before stacking")

  for (col in names(type_map)) {
    classes <- type_map[[col]]
    # determine target: if any is character, use character; otherwise numeric
    target <- if ("character" %in% classes) "character" else "numeric"

    for (i in seq_along(dfs)) {
      if (!(col %in% names(dfs[[i]]))) next
      cur_class <- class(dfs[[i]][[col]])[1]
      if (cur_class == target) next

      dfs[[i]][[col]] <- tryCatch(
        if (target == "character") as.character(dfs[[i]][[col]])
        else as.numeric(dfs[[i]][[col]]),
        error = function(e) {
          cli::cli_warn("could not coerce {.var {col}} from {cur_class} to {target}")
          as.character(dfs[[i]][[col]])
        }
      )
    }
  }
  dfs
}

# ============================================================================
# 3. CORE HELPERS
# ============================================================================

#' @noRd
resolve_waves <- function(spec, all_wave_ids) {
  if (is.null(spec)) return(all_wave_ids)
  if (is.character(spec) && length(spec) == 1 && spec == "all")
    return(all_wave_ids)
  intersect(spec, all_wave_ids)
}

#' @noRd
strip_wave_prefix <- function(df, wave_id, id_vars = "nomem_encr") {
  nms <- names(df)
  prefix_re <- paste0("^", wave_id)
  new_nms <- purrr::map_chr(nms, function(nm) {
    if (nm %in% id_vars || nm == "nohouse_encr") return(nm)
    stripped <- sub(prefix_re, "", nm)
    if (nchar(stripped) == 0) return(nm)
    # prefix with "s" if name starts with a digit or underscore (invalid SPSS)
    if (grepl("^[\\d_]", stripped, perl = TRUE)) paste0("s", stripped) else stripped
  })
  names(df) <- new_nms
  df
}

#' sanitize column names for SPSS compatibility (internal)
#'
#' ensures all column names are valid SPSS variable names:
#' must start with a letter, contain only letters/digits/dots/underscores,
#' max 64 characters, cannot end with a period.
#' @noRd
sanitize_spss_names <- function(df) {
  nms <- names(df)
  new_nms <- vapply(nms, function(nm) {
    # replace disallowed characters with underscore
    nm <- gsub("[^A-Za-z0-9._]", "_", nm)
    # must start with a letter
    if (grepl("^[^A-Za-z]", nm)) nm <- paste0("v", nm)
    # cannot end with a period
    nm <- sub("\\.$", "_", nm)
    # max 64 characters
    if (nchar(nm) > 64) nm <- substr(nm, 1, 64)
    nm
  }, character(1), USE.NAMES = FALSE)
  # deduplicate
  new_nms <- make.unique(new_nms, sep = "_")
  if (!identical(nms, new_nms)) {
    changed <- which(nms != new_nms)
    cli::cli_inform("  sanitized {length(changed)} column name(s) for SPSS compatibility")
  }
  names(df) <- new_nms
  df
}

#' @noRd
apply_labelled_policy <- function(df, policy) {
  if (!requireNamespace("haven", quietly = TRUE)) return(df)
  labelled_cols <- purrr::map_lgl(df, haven::is.labelled)
  if (!any(labelled_cols)) return(df)
  switch(policy,
    "to_numeric" = {
      df[labelled_cols] <- purrr::map(df[labelled_cols], function(x) {
        labs <- attr(x, "labels")
        vlab <- attr(x, "label", exact = TRUE)
        nav  <- attr(x, "na_values", exact = TRUE)
        nar  <- attr(x, "na_range",  exact = TRUE)
        # unclass rather than zap_labels: zap_labels() converts spss
        # user-missing codes to NA, which would hide the dk/refusal sentinels
        # from the recipe recodes; the stashed declarations below let the
        # write step round-trip or sweep whatever the recipes leave behind
        vals <- as.numeric(unclass(x))
        attr(vals, "_original_labels")   <- labs
        if (!is.null(nav)) attr(vals, "_original_na_values") <- nav
        if (!is.null(nar)) attr(vals, "_original_na_range")  <- nar
        # as.numeric() strips every attribute; put the variable label back
        if (!is.null(vlab)) attr(vals, "label") <- vlab
        vals
      })
    },
    "to_factor" = {
      df[labelled_cols] <- purrr::map(df[labelled_cols], haven::as_factor)
    },
    "keep_labelled" = {},
    cli::cli_abort("unknown labelled_policy: {.val {policy}}")
  )
  df
}

#' @noRd
strip_label_whitespace <- function(df) {
  if (!requireNamespace("haven", quietly = TRUE)) return(df)
  for (i in seq_along(df)) {
    if (haven::is.labelled(df[[i]])) {
      labs <- attr(df[[i]], "labels")
      if (!is.null(labs)) {
        names(labs) <- trimws(names(labs))
        attr(df[[i]], "labels") <- labs
      }
    }
  }
  df
}

#' harvest per-column label metadata after the labelled policy ran (internal)
#'
#' called once per wave in phase 1; records the value labels stashed in
#' `_original_labels`, the spss user-missing declarations, and the variable
#' label, keyed by the post-strip column name, so `restore_value_labels()`
#' can rebuild haven labelled columns at write time.
#' @noRd
harvest_labels <- function(df, registry, wave = NA_character_) {
  for (col in names(df)) {
    labs <- attr(df[[col]], "_original_labels", exact = TRUE)
    nav  <- attr(df[[col]], "_original_na_values", exact = TRUE)
    nar  <- attr(df[[col]], "_original_na_range",  exact = TRUE)
    if (is.null(labs) && is.null(nav) && is.null(nar)) next
    registry[[col]] <- append(
      registry[[col]] %||% list(),
      list(list(labels    = labs,
                na_values = nav,
                na_range  = nar,
                wave      = wave,
                vlab      = attr(df[[col]], "label", exact = TRUE))))
  }
  invisible(registry)
}

#' restore value labels on the merged frame where it is safe (internal)
#'
#' a column is restored only when (a) every wave that carried metadata carried
#' the identical label set and user-missing declarations (so cross-era
#' recodings like the cr religion schemes are never mislabelled), (b) the
#' column is numeric, and (c) every observed value is NA, a labelled code, or
#' a declared user-missing code, so post-recode values cannot receive a stale
#' label. restored columns become haven::labelled_spss() when declarations
#' exist (round-tripping the dk/refusal distinction into the .sav) and
#' haven::labelled() otherwise.
#' @noRd
restore_value_labels <- function(merged, registry) {
  restored <- character(0)
  skipped  <- character(0)
  for (col in ls(registry)) {
    if (!(col %in% names(merged))) next
    sets <- registry[[col]]
    if (length(sets) == 0) next
    first <- sets[[1]]
    same  <- all(vapply(sets, function(s)
      identical(s$labels, first$labels) &&
        identical(s$na_values, first$na_values) &&
        identical(s$na_range,  first$na_range), logical(1)))
    x <- merged[[col]]
    if (!same || !is.numeric(x) ||
        (is.null(first$labels) && is.null(first$na_values) &&
         is.null(first$na_range))) {
      skipped <- c(skipped, col)
      next
    }
    # metadata harvested from spss string variables can be character-typed
    # (e.g. na_values "999") while the merged column is numeric; coerce
    # losslessly or skip, so a type mismatch can never abort the write phase
    to_num <- function(v) {
      if (is.null(v)) return(NULL)
      out <- suppressWarnings(as.numeric(v))
      if (anyNA(out) && !anyNA(v)) return(NA)
      names(out) <- names(v)
      out
    }
    labs <- to_num(first$labels)
    navs <- to_num(first$na_values)
    narg <- to_num(first$na_range)
    if (identical(labs, NA) || identical(navs, NA) || identical(narg, NA)) {
      skipped <- c(skipped, col)
      next
    }
    allowed <- c(as.numeric(labs %||% numeric(0)),
                 as.numeric(navs %||% numeric(0)))
    obs <- unique(x[!is.na(x)])
    in_range <- if (!is.null(narg) && length(narg) == 2)
      obs >= narg[[1]] & obs <= narg[[2]] else rep(FALSE, length(obs))
    if (length(obs) > 0 && !all(obs %in% allowed | in_range)) {
      skipped <- c(skipped, col)
      next
    }
    vlab <- attr(x, "label", exact = TRUE) %||% first$vlab
    vals <- as.numeric(x)
    res <- tryCatch({
      if (!is.null(navs) || !is.null(narg)) {
        haven::labelled_spss(vals, labels = labs, na_values = navs,
                             na_range = narg, label = vlab)
      } else {
        haven::labelled(vals, labels = labs, label = vlab)
      }
    }, error = function(e) NULL)
    if (is.null(res)) {
      skipped <- c(skipped, col)
      next
    }
    merged[[col]] <- res
    restored <- c(restored, col)
  }
  list(data = merged, restored = restored, skipped = skipped)
}

#' sweep residual user-missing codes on non-restored columns (internal)
#'
#' safety net for columns whose metadata could not be restored (era-dependent
#' label sets, recoded values): any cell still equal to a declared spss
#' user-missing code, for any wave that declared it, becomes NA rather than
#' leaking into the output as a substantive value. recipes retain first claim
#' because they run earlier and may have moved the code (e.g. cs 999 -> -9).
#' @noRd
sweep_user_missing <- function(merged, registry, cols,
                               wave_var = "wave_id", veto = list()) {
  swept <- 0L
  wave_col <- if (wave_var %in% names(merged)) merged[[wave_var]] else NULL
  col_vetoed <- function(col, sfx_waves) {
    # a column is veto-covered for a wave when a recipe exclude block named
    # its suffix for that wave; matching mirrors the harmonization-time names
    for (sfx in names(sfx_waves)) {
      if (col %in% c(paste0("s", sfx), paste0("stem_", sfx), sfx))
        return(sfx_waves[[sfx]])
    }
    character(0)
  }
  for (col in cols) {
    if (!(col %in% names(merged)) || !is.numeric(merged[[col]])) next
    sets <- registry[[col]] %||% list()
    if (length(sets) == 0) next
    x <- merged[[col]]
    hit <- rep(FALSE, length(x))
    veto_waves <- col_vetoed(col, veto)
    for (s in sets) {
      nav <- unique(stats::na.omit(suppressWarnings(as.numeric(s$na_values))))
      nar <- suppressWarnings(as.numeric(s$na_range))
      has_range <- length(nar) == 2 && !anyNA(nar)
      if (length(nav) == 0 && !has_range) next
      w <- s$wave %||% NA_character_
      # sweep only the rows of the wave that declared the code; a set
      # harvested without wave information falls back to all rows
      in_wave <- if (!is.na(w) && !is.null(wave_col)) wave_col == w
                 else rep(TRUE, length(x))
      if (!is.na(w) && w %in% veto_waves) next
      h <- in_wave & !is.na(x) & x %in% nav
      if (has_range) h <- h | (in_wave & !is.na(x) & x >= nar[[1]] & x <= nar[[2]])
      hit <- hit | h
    }
    n <- sum(hit)
    if (n > 0) {
      merged[[col]][hit] <- NA
      swept <- swept + n
    }
  }
  list(data = merged, swept = swept)
}

# ============================================================================
# 3b. ROLE-BASED VARIABLE RESOLUTION
# ============================================================================

#' resolve a variable target using role_map if available (internal)
#' @noRd
resolve_var_target <- function(target, wave_meta, df) {
  # try role_map first
  role_map <- wave_meta$role_map
  if (!is.null(role_map) && target %in% names(role_map)) {
    resolved_suffix <- role_map[[target]]
    col <- find_col(df, resolved_suffix)
    if (!is.null(col)) return(col)
  }
  # fall back to direct resolution
  find_col(df, target)
}

#' find a column matching a suffix or variable name (internal)
#' @noRd
find_col <- function(df, sfx) {
  # a null / na / empty / non-scalar suffix cannot match a column; return null
  # rather than letting `sfx %in% names(df)` collapse to logical(0), which makes
  # the downstream if raise "argument is of length zero" (e.g. a crosswalk_rename
  # entry that omits a suffix)
  if (is.null(sfx) || length(sfx) != 1L || is.na(sfx) || !nzchar(sfx)) return(NULL)
  if (sfx %in% names(df)) return(sfx)
  candidates <- c(paste0("s", sfx), paste0("stem_", sfx),
                   paste0("q", sfx), paste0("Q", sfx))
  # recipes that already carry a q/Q prefix in the target name (e.g. Q047, q112)
  # never match the s-form above; retry on the bare suffix so Q047 -> s047.
  if (grepl("^[qQ]", sfx)) {
    bare <- sub("^[qQ]", "", sfx)
    candidates <- c(candidates, paste0("s", bare), paste0("stem_", bare), bare)
  }
  for (cand in candidates) {
    if (cand %in% names(df)) return(cand)
  }
  NULL
}

#' @noRd
resolve_scope <- function(df, scope) {
  if (!requireNamespace("haven", quietly = TRUE)) return(character(0))
  if (is.character(scope) && length(scope) == 1 && scope == "all_numeric")
    return(names(df)[purrr::map_lgl(df, is.numeric)])
  if (is.character(scope) && length(scope) == 1 && scope == "all_labelled")
    return(names(df)[purrr::map_lgl(df, haven::is.labelled)])
  found <- purrr::map_chr(scope, ~ find_col(df, .x) %||% NA_character_)
  found[!is.na(found)]
}

#' @noRd
coerce_column <- function(x, target) {
  switch(target,
    "integer" = as.integer(x),
    "numeric" = , "double" = as.numeric(x),
    "character" = as.character(x),
    "logical" = as.logical(x),
    x
  )
}

# ============================================================================
# 4. AUDIT-GRADE LOGGING
# ============================================================================

#' create a structured log entry (internal)
#' @noRd
make_log <- function(rule_id, wave_id, variable, action, rows_affected,
                     values_changed = NA_integer_,
                     distinct_before = NA, distinct_after = NA,
                     na_before = NA, na_after = NA,
                     duration_ms = NA) {
  list(
    rule_id         = rule_id,
    wave_id         = wave_id,
    variable        = variable,
    action          = action,
    rows_affected   = rows_affected,
    values_changed  = values_changed,
    distinct_before = distinct_before,
    distinct_after  = distinct_after,
    na_count_before = na_before,
    na_count_after  = na_after,
    timestamp       = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3"),
    duration_ms     = duration_ms
  )
}

#' write JSONL log (internal)
#' @noRd
write_jsonl <- function(log_entries, path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The 'jsonlite' package is required for logging.\n",
         "Install it with: install.packages(\"jsonlite\")", call. = FALSE)
  }
  lines <- purrr::map_chr(log_entries, function(entry) {
    jsonlite::toJSON(entry, auto_unbox = TRUE)
  })
  writeLines(lines, path)
}

#' generate a summary artifact (internal)
#' @noRd
generate_summary <- function(merged, log_entries, recipe) {
  total_na <- sum(purrr::map_int(log_entries, function(e) {
    after <- e$na_count_after %||% NA
    before <- e$na_count_before %||% NA
    if (is.na(after) || is.na(before)) 0L else max(0L, after - before)
  }))

  total_recoded <- sum(purrr::map_int(log_entries, function(e) {
    vc <- e$values_changed %||% NA
    if (is.na(vc)) 0L else vc
  }))

  wave_counts <- if ("wave_id" %in% names(merged)) {
    as.list(table(merged$wave_id))
  } else list()

  list(
    module            = recipe$meta$module,
    total_rows        = nrow(merged),
    total_cols        = ncol(merged),
    total_waves       = length(unique(merged$wave_id)),
    total_na_created  = total_na,
    total_values_recoded = total_recoded,
    wave_row_counts   = wave_counts,
    rules_applied     = length(log_entries)
  )
}

# ============================================================================
# 5. EXPECTED-PRESENCE ENFORCEMENT
# ============================================================================

#' check expected_presence matrix against loaded wave data (internal)
#' @noRd
check_expected_presence <- function(df, wave_id, expected_presence) {
  if (is.null(expected_presence)) return(df)
  critical <- expected_presence$critical %||% list()

  for (ep in critical) {
    var <- ep$variable
    waves_spec <- ep$waves %||% "all"
    on_absence <- ep$on_absence %||% "error"

    # check if this wave is in scope
    if (!identical(waves_spec, "all") && !(wave_id %in% waves_spec))
      next

    if (!(var %in% names(df))) {
      if (on_absence == "error") {
        msg <- paste0("expected_presence: variable '", var,
                       "' absent in wave '", wave_id, "'")
        cli::cli_abort(msg)
      }
      # for warn: create NA column silently; batch warning emitted later
      df[[var]] <- NA
    }
  }
  df
}

# ============================================================================
# 6. RULE EXECUTORS
# ============================================================================

#' handle absent variable per if_absent policy (internal)
#' @noRd
handle_absent <- function(rule, suffix, wave_id) {
  policy <- rule$if_absent %||% "warn_and_create_na"
  switch(policy,
    "error" = cli::cli_abort(
      "variable {.val {suffix}} absent in wave {.val {wave_id}}"),
    "warn_and_skip" = cli::cli_warn(
      "variable {.val {suffix}} absent in wave {.val {wave_id}}, skipping"),
    "warn_and_create_na" = cli::cli_inform(
      "variable {.val {suffix}} absent in wave {.val {wave_id}}, creating NA"),
    NULL
  )
}

#' execute a single variable rule on a wave data frame (internal)
#' @noRd
exec_variable_rule <- function(df, rule, wave_id, wave_meta,
                               all_wave_ids, log_entries) {
  target_waves <- resolve_waves(rule$waves, all_wave_ids)
  if (!(wave_id %in% target_waves))
    return(list(df = df, log = log_entries))

  action <- rule$action
  rid    <- rule$rule_id
  t0     <- proc.time()

  # note_only: log and skip
  if (action == "note_only") {
    log_entries <- append(log_entries, list(
      make_log(rid, wave_id, "*", "note_only", 0L)))
    return(list(df = df, log = log_entries))
  }

  tryCatch({
    switch(action,
      "strip_value_labels" = {
        df <- strip_label_whitespace(df)
        log_entries <- append(log_entries, list(
          make_log(rid, wave_id, "*", action, nrow(df),
                   duration_ms = elapsed_ms(t0))))
      },
      "strip_prefix" = {
        df <- strip_wave_prefix(df, wave_id, c("nomem_encr", "nohouse_encr"))
        log_entries <- append(log_entries, list(
          make_log(rid, wave_id, "*", action, ncol(df),
                   duration_ms = elapsed_ms(t0))))
      },
      "apply_labelled_policy" = {
        log_entries <- append(log_entries, list(
          make_log(rid, wave_id, "*", action, 0L,
                   duration_ms = elapsed_ms(t0))))
      },
      "type_coerce" = {
        target <- rule$target_type %||% "numeric"
        targets <- rule$suffixes %||% rule$variables %||% list()
        # single variable target
        if (is.null(targets) || length(targets) == 0) {
          v <- rule$variable %||% NULL
          if (!is.null(v)) targets <- list(v)
        }
        for (sfx in targets) {
          col <- resolve_var_target(sfx, wave_meta, df)
          if (!is.null(col)) {
            before_na <- sum(is.na(df[[col]]))
            df[[col]] <- coerce_column(df[[col]], target)
            after_na <- sum(is.na(df[[col]]))
            log_entries <- append(log_entries, list(
              make_log(rid, wave_id, col, action, nrow(df),
                       na_before = before_na, na_after = after_na,
                       duration_ms = elapsed_ms(t0))))
          } else {
            handle_absent(rule, sfx, wave_id)
          }
        }
      },
      "set_label" = {
        targets <- rule$suffixes %||% rule$variables %||% list()
        for (sfx in targets) {
          col <- resolve_var_target(sfx, wave_meta, df)
          if (!is.null(col)) {
            attr(df[[col]], "label") <- rule$set_label %||% rule$corrected_label
            log_entries <- append(log_entries, list(
              make_log(rid, wave_id, col, action, 1L,
                       duration_ms = elapsed_ms(t0))))
          }
        }
      },
      "rename" = {
        mapping <- rule$mapping %||% list()
        for (old_name in names(mapping)) {
          new_name <- mapping[[old_name]]
          # resolve the source: exact name, else suffix resolution, so a bare
          # digit key like '054' matches the strip_wave_prefix form 's054'
          src_col <- if (old_name %in% names(df)) old_name else find_col(df, old_name)
          if (!is.null(src_col) && src_col %in% names(df)) {
            if (new_name != src_col && new_name %in% names(df)) {
              # target already present (e.g. an expected_presence NA placeholder):
              # fill its NAs from the source and drop the source rather than create
              # a duplicate column that name-repair turns into name...N
              keep <- df[[new_name]]
              src  <- df[[src_col]]
              keep[is.na(keep)] <- src[is.na(keep)]
              df[[new_name]] <- keep
              df[[src_col]] <- NULL
            } else if (new_name != src_col) {
              names(df)[names(df) == src_col] <- new_name
            }
            log_entries <- append(log_entries, list(
              make_log(rid, wave_id, new_name, action, nrow(df),
                       duration_ms = elapsed_ms(t0))))
          }
        }
      },
      "rename_to_suffix" = {
        # same as strip_prefix, normalise column names to suffix-only form
        df <- strip_wave_prefix(df, wave_id, c("nomem_encr", "nohouse_encr"))
        log_entries <- append(log_entries, list(
          make_log(rid, wave_id, "*", action, ncol(df),
                   duration_ms = elapsed_ms(t0))))
      },
      "coerce_numeric" = , "coerce_type" = , "strip_labels_coerce_numeric" = {
        target <- rule$target_type %||% "numeric"
        targets <- rule$suffixes %||% rule$variables %||% list()
        if (length(targets) == 0) {
          v <- rule$variable %||% NULL
          if (!is.null(v)) targets <- list(v)
        }
        if (action == "strip_labels_coerce_numeric") target <- "numeric"
        for (sfx in targets) {
          col <- resolve_var_target(sfx, wave_meta, df)
          if (!is.null(col)) {
            if (action == "strip_labels_coerce_numeric") {
              df[[col]] <- as.numeric(haven::zap_labels(df[[col]]))
            } else {
              df[[col]] <- coerce_column(df[[col]], target)
            }
            log_entries <- append(log_entries, list(
              make_log(rid, wave_id, col, action, nrow(df),
                       duration_ms = elapsed_ms(t0))))
          } else {
            handle_absent(rule, sfx, wave_id)
          }
        }
      },
      "ensure_column" = {
        col_name <- rule$variable %||% rule$column %||% NULL
        default_val <- rule$default %||% NA
        if (!is.null(col_name) && !(col_name %in% names(df))) {
          df[[col_name]] <- default_val
        }
        log_entries <- append(log_entries, list(
          make_log(rid, wave_id, col_name %||% "*", action, 1L,
                   duration_ms = elapsed_ms(t0))))
      },
      "safe_integer_cast" = {
        targets <- rule$suffixes %||% rule$variables %||% list()
        for (sfx in targets) {
          col <- resolve_var_target(sfx, wave_meta, df)
          if (!is.null(col) && is.numeric(df[[col]])) {
            df[[col]] <- as.integer(round(df[[col]]))
          }
        }
        log_entries <- append(log_entries, list(
          make_log(rid, wave_id, "*", action, length(targets),
                   duration_ms = elapsed_ms(t0))))
      },
      "use_global_policy" = , "identity" = {
        # no-op, global policy already applied in the processing flow
        log_entries <- append(log_entries, list(
          make_log(rid, wave_id, "*", action, 0L,
                   duration_ms = elapsed_ms(t0))))
      },
      "derive_fieldwork_month" = , "parse_time" = {
        # derive fieldwork month from source column(s)
        tgt <- rule[["target_column"]] %||% rule[["target"]] %||% "fieldwork_ym"
        src <- rule[["source_column"]] %||% NULL
        # handle sources list (try each candidate until one is found)
        sources <- rule[["sources"]]
        if (is.null(src) && is.list(sources)) {
          for (candidate in sources) {
            col <- find_col(df, candidate)
            if (!is.null(col)) { src <- col; break }
          }
        }
        if (!is.null(src) && length(src) == 1 && src %in% names(df)) {
          df[[tgt]] <- tryCatch(as.character(df[[src]]), error = function(e) NA_character_)
        }
        log_entries <- append(log_entries, list(
          make_log(rid, wave_id, tgt, action, nrow(df),
                   duration_ms = elapsed_ms(t0))))
      },
      # warn-and-skip for unimplemented actions
      {
        cli::cli_warn("variable_rules action {.val {action}} (rule {.val {rid}}) not yet implemented, skipping")
        log_entries <- append(log_entries, list(
          make_log(rid, wave_id, "*", paste0("SKIPPED:", action), 0L,
                   duration_ms = elapsed_ms(t0))))
      }
    )
  }, error = function(e) {
    cli::cli_warn("rule {.val {rid}} failed on wave {.val {wave_id}}: {e$message}")
    log_entries <<- append(log_entries, list(
      make_log(rid, wave_id, "*", paste0("ERROR:", action), 0L)))
  })

  list(df = df, log = log_entries)
}

#' execute a harmonization rule (internal)
#' @noRd
exec_harmonization_rule <- function(df, rule, wave_id, wave_meta,
                                    all_wave_ids, log_entries) {
  target_waves <- resolve_waves(rule$waves, all_wave_ids)
  if (!(wave_id %in% target_waves))
    return(list(df = df, log = log_entries))

  action <- rule$action
  rid    <- rule$rule_id
  t0     <- proc.time()

  if (action == "note_only" || action == "flag_only") {
    log_entries <- append(log_entries, list(
      make_log(rid, wave_id, "*", action, 0L,
               duration_ms = elapsed_ms(t0))))
    return(list(df = df, log = log_entries))
  }

  tryCatch({
    switch(action,
      "recode_to_na" = {
        # [[ ]] on 'recode': $ would partial-match 'recodes' when the key is absent
        mapping <- rule$mapping %||% rule[["recode"]] %||% list()
        scope <- rule$scope %||% rule$suffixes %||% rule$variables %||% "all_numeric"
        codes_to_na <- as.numeric(names(mapping))
        if (length(codes_to_na) == 0)
          codes_to_na <- as.numeric(rule$codes %||% rule$sentinel_values %||% numeric(0))

        # handle wave-specific recodes (CI pattern)
        recodes_list <- rule$recodes %||% list()
        if (length(recodes_list) > 0) {
          for (rr in recodes_list) {
            rr_waves <- rr$waves %||% list()
            if (!(wave_id %in% rr_waves)) next
            # ci shape: codes_to_na is a list of {code, reason}; read with [[ ]]
            # (exact) so $ partial matching does not pull codes_to_na into rr$codes
            # and hand a list to as.numeric. legacy codes/values still accepted.
            rr_cna <- rr[["codes_to_na"]] %||% rr[["codes"]] %||% rr[["values"]]
            rr_codes <- numeric(0)
            if (!is.null(rr_cna)) {
              rr_codes <- suppressWarnings(as.numeric(vapply(
                rr_cna,
                function(e) as.character(
                  if (is.list(e)) (e[["code"]] %||% e[["value"]] %||% NA) else e),
                character(1))))
              rr_codes <- rr_codes[!is.na(rr_codes)]
            }
            # per-block scope: each block names its own variables/scope; fall back
            # to the rule-level scope only when the block omits both (e.g. A-08)
            blk_scope <- rr[["scope"]] %||% rr[["variables"]] %||% scope
            target_cols <- resolve_scope(df, blk_scope)
            total_recoded <- 0L
            for (col in target_cols) {
              if (is.numeric(df[[col]])) {
                mask <- !is.na(df[[col]]) & df[[col]] %in% rr_codes
                n <- sum(mask)
                if (n > 0) { df[[col]][mask] <- NA; total_recoded <- total_recoded + n }
              }
            }
            log_entries <- append(log_entries, list(
              make_log(rid, wave_id, paste0(length(target_cols), " cols"),
                       action, total_recoded,
                       values_changed = total_recoded,
                       duration_ms = elapsed_ms(t0))))
          }
        } else {
          target_cols <- resolve_scope(df, scope)
          # wave-scoped exclude blocks (cv HR01c/HR02 carve-outs): a block names
          # suffixes and waves; a column is skipped when its suffix AND the
          # current wave both match. blocks without a waves key apply to all.
          excl_cols <- character(0)
          for (ex in (rule$exclude %||% list())) {
            if (!is.list(ex)) next
            ex_waves <- resolve_waves(ex$waves, all_wave_ids)
            if (!(wave_id %in% ex_waves)) next
            ec <- purrr::map_chr(ex$suffixes %||% ex$variables %||% list(),
                                 ~ find_col(df, .x) %||% NA_character_)
            excl_cols <- c(excl_cols, ec[!is.na(ec)])
          }
          if (length(excl_cols) > 0)
            target_cols <- setdiff(target_cols, unique(excl_cols))
          total_recoded <- 0L
          for (col in target_cols) {
            if (is.numeric(df[[col]])) {
              mask <- !is.na(df[[col]]) & df[[col]] %in% codes_to_na
              n <- sum(mask)
              if (n > 0) { df[[col]][mask] <- NA; total_recoded <- total_recoded + n }
            }
          }
          log_entries <- append(log_entries, list(
            make_log(rid, wave_id, paste0(length(target_cols), " cols"),
                     action, total_recoded,
                     values_changed = total_recoded,
                     duration_ms = elapsed_ms(t0))))
        }
      },
      "value_recode" = {
        mapping <- rule$mapping %||% list()
        # handle from_value/to_value shorthand
        if (length(mapping) == 0 && !is.null(rule$from_value)) {
          from <- as.character(rule$from_value)
          to <- if (is.null(rule$to_value)) NULL else rule$to_value
          mapping <- setNames(list(to), from)
        }
        suffixes <- rule$suffixes %||% rule$stems %||%
                    rule$variables %||% list()
        # -7 structural-sentinel guard: before a structural
        # recode writes -7, assert -7 is absent from the target; error on collision
        .to_vals <- suppressWarnings(as.numeric(unlist(mapping)))
        if (length(suffixes) == 0) {
          # auditability: a rule with no resolvable targets must still leave a trace
          log_entries <- append(log_entries, list(
            make_log(rid, wave_id, "*", paste0(action, ":NO_TARGETS"), 0L,
                     duration_ms = elapsed_ms(t0))))
        }
        for (sfx in suffixes) {
          col <- resolve_var_target(sfx, wave_meta, df)
          if (!is.null(col) && is.numeric(df[[col]])) {
            if (-7 %in% .to_vals && any(df[[col]] == -7, na.rm = TRUE))
              stop(sprintf("-7 collision in '%s': column already contains -7 (rule %s)", col, rid))
            # snapshot semantics: every mask is computed against the original
            # vector, so a map like {1: 2, 2: 3} cannot chain (1 -> 2 -> 3)
            orig <- df[[col]]
            changed <- 0L
            for (from_val in names(mapping)) {
              to_val <- mapping[[from_val]]
              mask <- !is.na(orig) & orig == as.numeric(from_val)
              n <- sum(mask)
              if (n > 0) {
                if (is.null(to_val) || identical(to_val, ".na"))
                  df[[col]][mask] <- NA
                else
                  df[[col]][mask] <- as.numeric(to_val)
                changed <- changed + n
              }
            }
            log_entries <- append(log_entries, list(
              make_log(rid, wave_id, col, action, nrow(df),
                       values_changed = changed,
                       duration_ms = elapsed_ms(t0))))
          }
        }
      },
      "fix_label" = {
        suffixes <- rule$suffixes %||% rule$stems %||%
                    rule$variables %||% list()
        if (length(suffixes) == 0) {
          log_entries <- append(log_entries, list(
            make_log(rid, wave_id, "*", "fix_label:NO_TARGETS", 0L,
                     duration_ms = elapsed_ms(t0))))
        }
        for (sfx in suffixes) {
          col <- resolve_var_target(sfx, wave_meta, df)
          if (!is.null(col)) {
            lbl <- attr(df[[col]], "label") %||% ""
            old_frag <- rule$old_fragment %||% ""
            new_frag <- rule$new_fragment %||% rule$corrected_label %||% ""
            if (nchar(old_frag) > 0)
              attr(df[[col]], "label") <- sub(old_frag, new_frag, lbl, fixed = TRUE)
            else if (nchar(new_frag) > 0)
              # set-label mode; an empty new_frag must not blank an existing label
              attr(df[[col]], "label") <- new_frag
            log_entries <- append(log_entries, list(
              make_log(rid, wave_id, col, action, 1L,
                       duration_ms = elapsed_ms(t0))))
          }
        }
      },
      "crosswalk" = {
        if (isTRUE(.lissr_have_executors)) {
          src_sfx <- (rule$variables %||% rule$suffixes %||% list())
          src_sfx <- if (length(src_sfx)) src_sfx[[1]] else NULL
          out_var <- rule$output_variable
          col <- if (!is.null(src_sfx)) resolve_var_target(src_sfx, wave_meta, df) else NULL
          if (!is.null(col) && !is.null(out_var) && col %in% names(df)) {
            x  <- df[[col]]
            cw <- rule$crosswalk %||% list()
            sc <- rule$scheme_column
            # scheme values come from a data column if present, else from the
            # per-wave wave_index entry, so a wave-constant scheme (e.g. cw's
            # edu_scheme) need not be materialized as a column.
            sc_vals <- if (is.null(sc)) NULL
                       else if (sc %in% names(df)) df[[sc]]
                       else if (!is.null(wave_meta[[sc]])) rep(wave_meta[[sc]], nrow(df))
                       else NULL
            if (!is.null(sc_vals)) {
              mapped <- crosswalk_map_scheme(x, sc_vals, cw)
              if (!is.null(rule$output_scheme_flag))
                df[[rule$output_scheme_flag]] <- sc_vals
            } else {
              m <- if (length(cw) && is.list(cw[[1]])) cw[[1]] else cw
              mapped <- crosswalk_map(x, m)
            }
            cov <- crosswalk_coverage(x, mapped)
            df[[out_var]] <- mapped
            if (cov$excess > 0)
              cli::cli_warn(paste0("crosswalk {.val {rid}} wave {.val {wave_id}}: ",
                cov$excess, " unmapped non-NA value(s) sent to NA (codes ",
                paste(cov$unmapped_codes, collapse = ","), "); coverage severity error"))
            log_entries <- append(log_entries, list(
              make_log(rid, wave_id, out_var,
                       if (cov$excess > 0) "crosswalk:COVERAGE_ERROR" else "crosswalk",
                       nrow(df), values_changed = sum(!is.na(mapped)),
                       na_before = cov$na_before, na_after = cov$na_after,
                       duration_ms = elapsed_ms(t0))))
          } else {
            log_entries <- append(log_entries, list(
              make_log(rid, wave_id, "*", "crosswalk:SKIPPED_no_target", 0L,
                       duration_ms = elapsed_ms(t0))))
          }
        } else {
          log_entries <- append(log_entries, list(
            make_log(rid, wave_id, "*", "crosswalk", 0L, duration_ms = elapsed_ms(t0))))
        }
      },
      "transform" = {
        # per-wave scalar offset; the subtract action is an alias for this
        blocks <- rule$transforms %||% list()
        vars   <- rule$variables %||% rule$suffixes %||% list()
        changed <- 0L
        for (blk in blocks) {
          blk_waves <- resolve_waves(blk$waves, all_wave_ids)
          if (!(wave_id %in% blk_waves)) next
          for (sfx in vars) {
            col <- resolve_var_target(sfx, wave_meta, df)
            if (!is.null(col) && is.numeric(df[[col]])) {
              df[[col]] <- transform_apply(df[[col]], blk$op %||% "identity", blk$value)
              changed <- changed + sum(!is.na(df[[col]]))
              n_oor <- range_check(df[[col]], blk$valid_range)
              if (n_oor > 0)
                cli::cli_warn("transform {.val {rid}} wave {.val {wave_id}}: {n_oor} value(s) outside valid_range")
            }
          }
        }
        log_entries <- append(log_entries, list(
          make_log(rid, wave_id, paste0(length(vars), " var(s)"), action, changed,
                   values_changed = changed, duration_ms = elapsed_ms(t0))))
      },
      "subtract" = {
        # folds into transform op:subtract (one offset mechanism)
        vars <- rule$variables %||% rule$suffixes %||% list()
        val  <- rule$value %||% rule$offset
        changed <- 0L
        for (sfx in vars) {
          col <- resolve_var_target(sfx, wave_meta, df)
          if (!is.null(col) && is.numeric(df[[col]])) {
            df[[col]] <- transform_apply(df[[col]], "subtract", val)
            changed <- changed + sum(!is.na(df[[col]]))
          }
        }
        log_entries <- append(log_entries, list(
          make_log(rid, wave_id, paste0(length(vars), " var(s)"),
                   "transform(subtract;deprecated_alias)", changed,
                   values_changed = changed, duration_ms = elapsed_ms(t0))))
      },
      "lowercase_labels" = {
        target_cols <- resolve_scope(df, rule$scope %||% "all_labelled")
        for (col in target_cols) {
          lbl <- attr(df[[col]], "label") %||% ""
          if (nchar(lbl) > 0) attr(df[[col]], "label") <- tolower(lbl)
        }
        log_entries <- append(log_entries, list(
          make_log(rid, wave_id, paste0(length(target_cols), " cols"),
                   action, length(target_cols),
                   duration_ms = elapsed_ms(t0))))
      },
      "strip_question_stem" = {
        # strip common prefix from value labels
        targets <- rule$suffixes %||% rule$variables %||% list()
        stem <- rule$stem %||% rule$prefix %||% ""
        for (sfx in targets) {
          col <- resolve_var_target(sfx, wave_meta, df)
          if (!is.null(col)) {
            labs <- attr(df[[col]], "labels")
            if (!is.null(labs) && nchar(stem) > 0) {
              names(labs) <- sub(paste0("^", stem), "", names(labs))
              attr(df[[col]], "labels") <- labs
            }
          }
        }
        log_entries <- append(log_entries, list(
          make_log(rid, wave_id, "*", action, length(targets),
                   duration_ms = elapsed_ms(t0))))
      },
      "na_recode" = , "recode" = {
        # general recode: mapping old->new or codes->NA
        mapping <- rule$mapping %||% list()
        scope <- rule$scope %||% rule$suffixes %||% rule$variables %||% "all_numeric"
        target_cols <- resolve_scope(df, scope)
        total_changed <- 0L
        for (col in target_cols) {
          if (is.numeric(df[[col]])) {
            # snapshot semantics: masks come from the pre-rule vector so
            # overlapping from/to sets cannot chain
            orig <- df[[col]]
            for (from_val in names(mapping)) {
              to_val <- mapping[[from_val]]
              mask <- !is.na(orig) & orig == as.numeric(from_val)
              n <- sum(mask)
              if (n > 0) {
                if (is.null(to_val) || identical(to_val, ".na"))
                  df[[col]][mask] <- NA
                else
                  df[[col]][mask] <- as.numeric(to_val)
                total_changed <- total_changed + n
              }
            }
          }
        }
        log_entries <- append(log_entries, list(
          make_log(rid, wave_id, paste0(length(target_cols), " cols"),
                   action, total_changed,
                   values_changed = total_changed,
                   duration_ms = elapsed_ms(t0))))
      },
      "normalize_labels" = {
        target_cols <- resolve_scope(df, rule$scope %||% "all_labelled")
        for (col in target_cols) {
          lbl <- attr(df[[col]], "label") %||% ""
          if (nchar(lbl) > 0) {
            lbl <- trimws(lbl)
            lbl <- gsub("\\s+", " ", lbl)
            attr(df[[col]], "label") <- lbl
          }
        }
        log_entries <- append(log_entries, list(
          make_log(rid, wave_id, paste0(length(target_cols), " cols"),
                   action, length(target_cols),
                   duration_ms = elapsed_ms(t0))))
      },
      "conditional_label_swap" = {
        # label-only: conditionally relabel value labels. inert under to_numeric
        # until the label round-trip lands (Part VI); guarded no-op then.
        targets <- rule$variables %||% rule$suffixes %||% list()
        swap    <- rule$swap %||% rule$label_map %||% list()
        touched <- 0L
        for (sfx in targets) {
          col <- resolve_var_target(sfx, wave_meta, df)
          if (!is.null(col)) {
            labs <- attr(df[[col]], "labels")
            if (!is.null(labs) && length(swap)) {
              for (k in names(swap)) names(labs)[names(labs) == k] <- swap[[k]]
              attr(df[[col]], "labels") <- labs
              touched <- touched + 1L
            }
          }
        }
        log_entries <- append(log_entries, list(
          make_log(rid, wave_id, paste0(touched, " col(s)"),
                   if (touched == 0) "conditional_label_swap:INERT_no_labels" else action,
                   touched, duration_ms = elapsed_ms(t0))))
      },
      "label_to_string" = {
        # map coded values to their string labels (unmapped -> NA, with coverage).
        # inert under to_numeric (value labels stripped); guarded no-op then.
        # accept cv's target_variables alias and derived_suffix output naming;
        # under to_numeric the live labels attribute is zapped to _original_labels,
        # so read that as the label source when labels is gone.
        targets <- rule$variables %||% rule$suffixes %||% rule$target_variables %||% list()
        out_var <- rule$output_variable
        did <- FALSE
        for (sfx in targets) {
          col <- resolve_var_target(sfx, wave_meta, df)
          if (!is.null(col)) {
            labs <- attr(df[[col]], "labels") %||% attr(df[[col]], "_original_labels")
            if (!is.null(labs)) {
              lut    <- stats::setNames(names(labs), as.character(unname(labs)))
              src    <- as.character(df[[col]])
              mapped <- unname(lut[src])
              tgt    <- if (!is.null(rule$derived_suffix)) paste0(col, rule$derived_suffix) else out_var %||% col
              if (isTRUE(.lissr_have_executors)) {
                cov <- crosswalk_coverage(suppressWarnings(as.numeric(df[[col]])),
                                          suppressWarnings(as.numeric(factor(mapped))))
                if (cov$excess > 0)
                  cli::cli_warn("label_to_string {.val {rid}}: {cov$excess} unmapped code(s) -> NA")
              }
              df[[tgt]] <- mapped
              did <- TRUE
            }
          }
        }
        log_entries <- append(log_entries, list(
          make_log(rid, wave_id, out_var %||% "*",
                   if (did) action else "label_to_string:INERT_no_labels", 0L,
                   duration_ms = elapsed_ms(t0))))
      },
      "derive_combined_party" = {
        # combine party sources into one harmonized field; runs after
        # label_to_string. inert under to_numeric (string labels absent).
        srcs    <- rule$variables %||% rule$sources %||% list()
        out_var <- rule$output_variable %||% "party_combined"
        cols    <- Filter(Negate(is.null),
                          lapply(srcs, function(s) resolve_var_target(s, wave_meta, df)))
        if (length(cols)) {
          mat <- lapply(cols, function(cc) as.character(df[[cc]]))
          combined <- Reduce(function(a, b) ifelse(!is.na(a) & nzchar(a), a, b), mat)
          df[[out_var]] <- combined
          log_entries <- append(log_entries, list(
            make_log(rid, wave_id, out_var, action, sum(!is.na(combined)),
                     duration_ms = elapsed_ms(t0))))
        } else {
          log_entries <- append(log_entries, list(
            make_log(rid, wave_id, "*", "derive_combined_party:INERT_no_sources", 0L,
                     duration_ms = elapsed_ms(t0))))
        }
      },
      "extend_categories" = , "type_coerce" = {
        # extend_categories: documentary no-op (note_no_op).
        # type_coerce in harmonization placement is not executed here (use
        # variable_rules); logged as a no-op.
        log_entries <- append(log_entries, list(
          make_log(rid, wave_id, "*", paste0(action, ":no_op"), 0L,
                   duration_ms = elapsed_ms(t0))))
      },
      # warn-and-skip for unimplemented actions
      {
        cli::cli_warn("harmonization action {.val {action}} (rule {.val {rid}}) not yet implemented, skipping")
        log_entries <- append(log_entries, list(
          make_log(rid, wave_id, "*", paste0("SKIPPED:", action), 0L,
                   duration_ms = elapsed_ms(t0))))
      }
    )
  }, error = function(e) {
    cli::cli_warn("harmonization rule {.val {rid}} failed on {.val {wave_id}}: {e$message}")
    log_entries <<- append(log_entries, list(
      make_log(rid, wave_id, "*", paste0("ERROR:", action), 0L)))
  })

  list(df = df, log = log_entries)
}

#' execute a boundary rule on the full stacked data frame (internal)
#' @noRd
exec_boundary_rule <- function(df, rule, all_wave_ids, log_entries) {
  action <- rule$action
  rid    <- rule$rule_id
  t0     <- proc.time()

  if (action == "note_only") {
    log_entries <- append(log_entries, list(
      make_log(rid, "*", "*", "note_only", 0L,
               duration_ms = elapsed_ms(t0))))
    return(list(df = df, log = log_entries))
  }

  tryCatch({
    switch(action,
      "add_era_flag" = , "add_flag" = {
        flag_var <- rule$flag_name %||% rule$flag_variable %||% paste0(rid, "_flag")
        eras <- rule$eras %||% rule$assignments %||% list()
        if (length(eras) > 0) {
          df[[flag_var]] <- NA_character_
          for (era_name in names(eras)) {
            df[[flag_var]][df$wave_id %in% eras[[era_name]]] <- era_name
          }
        } else {
          waves_pre  <- rule$waves_pre %||% list()
          waves_post <- rule$waves_post %||% list()
          df[[flag_var]] <- NA
          df[[flag_var]][df$wave_id %in% waves_pre]  <- rule$flag_value_pre %||% 0L
          df[[flag_var]][df$wave_id %in% waves_post] <- rule$flag_value_post %||% 1L
        }
        n_set <- sum(!is.na(df[[flag_var]]))
        log_entries <- append(log_entries, list(
          make_log(rid, "*", flag_var, action, n_set,
                   duration_ms = elapsed_ms(t0))))

        # emit comparability warning if applicable
        emit_comparability_warning(rule, flag_var)
      },
      "add_period_flag" = {
        flag_col <- rule$flag_column %||% paste0(rid, "_period")
        phases <- rule$phases %||% list()
        df[[flag_col]] <- NA_character_
        if (length(phases) > 0) {
          for (ph in phases) df[[flag_col]][df$wave_id %in% ph$waves] <- ph$label
        } else {
          early_waves <- rule$waves_early %||% list()
          late_waves  <- rule$waves_late %||% list()
          df[[flag_col]][df$wave_id %in% early_waves] <- rule$early_label %||% "early"
          df[[flag_col]][df$wave_id %in% late_waves]  <- rule$late_label %||% "late"
        }
        log_entries <- append(log_entries, list(
          make_log(rid, "*", flag_col, action, sum(!is.na(df[[flag_col]])),
                   duration_ms = elapsed_ms(t0))))
        emit_comparability_warning(rule, flag_col)
      },
      "split_variable" = {
        for (ov in (rule$output_vars %||% list())) {
          new_name <- ov$name
          src_col  <- find_col(df, ov$source_suffix %||% "")
          ov_waves <- ov$waves %||% list()
          df[[new_name]] <- NA
          if (!is.null(src_col) && src_col %in% names(df)) {
            mask <- df$wave_id %in% ov_waves
            df[[new_name]][mask] <- df[[src_col]][mask]
          }
          log_entries <- append(log_entries, list(
            make_log(rid, "*", new_name, action, sum(!is.na(df[[new_name]])),
                     duration_ms = elapsed_ms(t0))))
        }
        emit_comparability_warning(rule, "split_variable")
      },
      "structural_na" = {
        flag_col <- rule$flag_column %||% NULL
        if (!is.null(flag_col)) {
          true_waves <- rule$flag_true_waves %||% rule$present_in_waves %||% list()
          df[[flag_col]] <- df$wave_id %in% true_waves
        }
        log_entries <- append(log_entries, list(
          make_log(rid, "*", flag_col %||% "*", action, 0L,
                   duration_ms = elapsed_ms(t0))))
        emit_comparability_warning(rule, flag_col %||% rid)
      },
      "filter_rows" = {
        wave_target <- rule$wave %||% ""
        var_name <- rule$variable %||% ""
        keep_vals <- rule$keep_values %||% list()
        if (nchar(wave_target) > 0 && nchar(var_name) > 0) {
          col <- find_col(df, var_name)
          if (!is.null(col)) {
            before_n <- sum(df$wave_id == wave_target)
            mask <- !(df$wave_id == wave_target & !(df[[col]] %in% keep_vals))
            df <- df[mask, , drop = FALSE]
            after_n <- sum(df$wave_id == wave_target)
            log_entries <- append(log_entries, list(
              make_log(rid, wave_target, col, action, before_n - after_n,
                       duration_ms = elapsed_ms(t0))))
          }
        }
      },
      "crosswalk_rename" = {
        for (cw in (rule$crosswalk %||% list())) {
          old_sfx <- cw$old_suffix
          new_sfx <- cw$new_suffix
          harm_name <- cw$harmonized_name %||% paste0("h_", old_sfx)
          old_col <- find_col(df, old_sfx)
          new_col <- find_col(df, new_sfx)
          df[[harm_name]] <- NA
          if (!is.null(old_col) && old_col %in% names(df)) {
            mask_old <- !is.na(df[[old_col]])
            df[[harm_name]][mask_old] <- df[[old_col]][mask_old]
          }
          if (!is.null(new_col) && new_col %in% names(df)) {
            mask_new <- !is.na(df[[new_col]])
            df[[harm_name]][mask_new] <- df[[new_col]][mask_new]
          }
          log_entries <- append(log_entries, list(
            make_log(rid, "*", harm_name, "crosswalk_rename",
                     sum(!is.na(df[[harm_name]])),
                     duration_ms = elapsed_ms(t0))))
        }
        # post_recode: scoped value remap on the harmonized column(s) after the
        # rename coalesce. honors {waves_affected: [...], recode: {<from>: <to>}};
        # a null target maps to NA. matches against a snapshot so multi-entry maps
        # do not chain, and applies to every harmonized_name this rule produces.
        pr <- rule$post_recode
        if (!is.null(pr) && !is.null(pr$recode) && length(pr$recode)) {
          wa   <- as.character(unlist(pr$waves_affected %||% list()))
          rmap <- pr$recode
          sel0 <- if (length(wa)) as.character(df$wave_id) %in% wa else rep(TRUE, nrow(df))
          for (cw in (rule$crosswalk %||% list())) {
            hn <- cw$harmonized_name %||% paste0("h_", cw$old_suffix)
            if (!(hn %in% names(df))) next
            col <- df[[hn]]; orig <- col; n_pr <- 0L
            for (k in names(rmap)) {
              to  <- rmap[[k]]
              hit <- sel0 & !is.na(orig) & as.character(orig) == k
              if (any(hit)) { col[hit] <- if (is.null(to)) NA else to; n_pr <- n_pr + sum(hit) }
            }
            df[[hn]] <- col
            log_entries <- append(log_entries, list(
              make_log(rid, paste(wa, collapse = ","), hn,
                       "crosswalk_rename:post_recode", n_pr,
                       duration_ms = elapsed_ms(t0))))
          }
        }
      },
      "stack_aux_files" = {
        log_entries <- append(log_entries, list(
          make_log(rid, "*", "*", action, 0L,
                   duration_ms = elapsed_ms(t0))))
      },
      "derive_combined_party" = {
        # pool a set of party-name strings on source_variable into one combined
        # label, writing target_variable. passthrough+collapse: rows whose source
        # value is in party_names_to_pool become combined_label, all other rows
        # carry source_variable through unchanged, NA stays NA. runs in the
        # boundary phase, after label_to_string built the string source column
        # (phase 1) and waves were stacked (phase 2).
        src_var <- rule$source_variable %||% rule$source %||% "vote_actual_party"
        tgt_var <- rule$target_variable %||% rule$output_variable %||%
                   paste0(src_var, "_combined")
        pool    <- as.character(unlist(rule$party_names_to_pool %||%
                                       rule$parties_to_pool %||% list()))
        comb    <- rule$combined_label %||% "combined"
        if (src_var %in% names(df) && length(pool)) {
          src      <- as.character(df[[src_var]])
          out      <- src
          hit      <- !is.na(src) & src %in% pool
          out[hit] <- comb
          df[[tgt_var]] <- out
          log_entries <- append(log_entries, list(
            make_log(rid, "*", tgt_var, action, sum(hit),
                     duration_ms = elapsed_ms(t0))))
        } else {
          log_entries <- append(log_entries, list(
            make_log(rid, "*", tgt_var,
                     "derive_combined_party:INERT_no_source", 0L,
                     duration_ms = elapsed_ms(t0))))
        }
      },
      "structural_note" = , "retain_where_present" = ,
      "retain_but_flag" = {
        # log-and-skip for documentation / complex domain actions
        log_entries <- append(log_entries, list(
          make_log(rid, "*", "*", action, 0L,
                   duration_ms = elapsed_ms(t0))))
      },
      "safe_integer_cast" = {
        targets <- rule$suffixes %||% rule$variables %||% list()
        for (sfx in targets) {
          col <- find_col(df, sfx)
          if (!is.null(col) && is.numeric(df[[col]])) {
            df[[col]] <- as.integer(round(df[[col]]))
          }
        }
        log_entries <- append(log_entries, list(
          make_log(rid, "*", "*", action, length(targets),
                   duration_ms = elapsed_ms(t0))))
      },
      "fix_label" = {
        suffixes <- rule$suffixes %||% rule$stems %||%
                    rule$variables %||% list()
        for (sfx in suffixes) {
          col <- find_col(df, sfx)
          if (!is.null(col)) {
            lbl <- attr(df[[col]], "label") %||% ""
            old_frag <- rule$old_fragment %||% ""
            new_frag <- rule$new_fragment %||% rule$corrected_label %||% ""
            if (nchar(old_frag) > 0)
              attr(df[[col]], "label") <- sub(old_frag, new_frag, lbl, fixed = TRUE)
            else
              attr(df[[col]], "label") <- new_frag
          }
        }
        log_entries <- append(log_entries, list(
          make_log(rid, "*", "*", action, length(suffixes),
                   duration_ms = elapsed_ms(t0))))
      },
      # warn-and-skip for unimplemented actions
      {
        cli::cli_warn("boundary action {.val {action}} (rule {.val {rid}}) not yet implemented, skipping")
        log_entries <- append(log_entries, list(
          make_log(rid, "*", "*", paste0("SKIPPED:", action), 0L,
                   duration_ms = elapsed_ms(t0))))
      }
    )
  }, error = function(e) {
    cli::cli_warn("boundary rule {.val {rid}} failed: {e$message}")
    log_entries <<- append(log_entries, list(
      make_log(rid, "*", "*", paste0("ERROR:", action), 0L)))
  })

  list(df = df, log = log_entries)
}

#' emit comparability warning when contract says no_pool (internal)
#' @noRd
emit_comparability_warning <- function(rule, context) {
  contract <- rule$comparability
  if (is.null(contract)) return(invisible(NULL))
  method <- contract$method %||% "pool_ok"
  if (method == "no_pool") {
    cli::cli_warn(c(
      "!" = "comparability: {.val {context}} is non-comparable across boundary",
      "i" = "method=no_pool; rationale: {contract$rationale %||% 'see recipe'}"
    ))
  } else if (method == "pool_with_flags") {
    cli::cli_inform(c(
      "i" = "comparability: {.val {context}} requires era/period flags for valid pooling"
    ))
  }
}

#' execute drop/retain rules (internal)
#' @noRd
exec_drop_retain <- function(df, rules, log_entries) {
  for (rule in rules) {
    action <- rule$action
    rid    <- rule$rule_id
    t0     <- proc.time()

    if (action == "note_only" || action == "retain_as_metadata_only") {
      log_entries <- append(log_entries, list(
        make_log(rid, "*", "*", action, 0L,
                 duration_ms = elapsed_ms(t0))))
      next
    }

    if (action == "drop") {
      cols_to_drop <- rule$columns %||% rule$variables %||% character(0)
      pat <- rule$pattern %||% rule$variables_pattern %||% rule$variable_pattern %||% NULL
      if (!is.null(pat))
        cols_to_drop <- c(cols_to_drop, grep(pat, names(df), value = TRUE))
      existing <- intersect(cols_to_drop, names(df))
      if (length(existing) > 0) {
        df <- df[, setdiff(names(df), existing), drop = FALSE]
        log_entries <- append(log_entries, list(
          make_log(rid, "*", paste(existing, collapse = ","), "drop",
                   length(existing), duration_ms = elapsed_ms(t0))))
      }
    } else if (action == "drop_outside") {
      # two supported forms:
      #  (a) suffix-scoped wave nullification (cf A-07): for the named suffixes,
      #      null values in rows whose wave_id is not in retain_in; the column is
      #      kept for the retain_in waves. honors suffixes / suffixes_range.
      #  (b) legacy keep/pattern whitelist (keep only the listed columns).
      sfx <- as.character(rule$suffixes %||% character(0))
      rng <- rule$suffixes_range
      if (!is.null(rng) && length(rng) >= 2) {
        w <- max(nchar(as.character(rng[[1]])), nchar(as.character(rng[[2]])))
        sfx <- c(sfx, formatC(seq.int(as.integer(rng[[1]]), as.integer(rng[[2]])),
                              width = w, flag = "0"))
      }
      sfx <- unique(sfx)
      retain_in <- as.character(rule$retain_in %||% character(0))
      if (length(sfx) > 0) {
        if (!"wave_id" %in% names(df)) {
          cli::cli_warn("drop_outside (rule {.val {rid}}) needs wave_id; skipping")
        } else {
          cols <- vapply(sfx, function(s) find_col(df, s) %||% NA_character_,
                         character(1))
          absent <- sfx[is.na(cols)]
          cols   <- unname(cols[!is.na(cols)])
          if (length(absent) > 0)
            cli::cli_warn(paste0("drop_outside (rule ", rid, "): suffix(es) ",
                                 paste(absent, collapse = ","), " absent, skipped"))
          outside <- !(df$wave_id %in% retain_in)
          nulled <- 0L
          for (col in cols) {
            nulled <- nulled + sum(outside & !is.na(df[[col]]))
            df[[col]][outside] <- NA
          }
          log_entries <- append(log_entries, list(
            make_log(rid, paste(cols, collapse = ","),
                     paste0(nulled, " values to NA outside ",
                            paste(retain_in, collapse = "/")),
                     "drop_outside", length(cols), duration_ms = elapsed_ms(t0))))
        }
      } else {
        keep_cols <- rule$keep %||% rule$retain %||% character(0)
        pat <- rule$pattern %||% rule$variables_pattern %||% NULL
        if (!is.null(pat))
          keep_cols <- c(keep_cols, grep(pat, names(df), value = TRUE))
        keep_cols <- unique(c("nomem_encr", "wave_id", "wave_year", keep_cols))
        if (length(keep_cols) <= 3L) {
          cli::cli_warn(paste0("drop_outside (rule ", rid, ") has no suffixes, ",
                               "keep, or pattern; skipped to avoid dropping all columns"))
        } else {
          existing <- intersect(keep_cols, names(df))
          dropped <- setdiff(names(df), existing)
          df <- df[, existing, drop = FALSE]
          log_entries <- append(log_entries, list(
            make_log(rid, "*", paste0(length(dropped), " cols"), "drop_outside",
                     length(dropped), duration_ms = elapsed_ms(t0))))
        }
      }
    } else if (action %in% c("retain", "retain_if_present", "retain_but_flag",
                              "retain_where_present", "flag_only", "flag_absence")) {
      # retain actions are no-ops that prevent accidental drops
      log_entries <- append(log_entries, list(
        make_log(rid, "*", "*", action, 0L,
                 duration_ms = elapsed_ms(t0))))
    } else {
      cli::cli_warn("drop_retain action {.val {action}} (rule {.val {rid}}) not yet implemented, skipping")
    }
  }
  list(df = df, log = log_entries)
}

# ============================================================================
# 7. VALIDATION RUNNER (improved severity handling, #6)
# ============================================================================

#' expand item ranges into individual suffixes (internal)
#'
#' handles ranges like "020-069" and single items like "010".
#' @noRd
expand_items <- function(items) {
  if (is.null(items)) return(character(0))
  result <- character(0)
  for (item in items) {
    item <- as.character(item)
    if (grepl("^\\d+-\\d+$", item)) {
      parts <- strsplit(item, "-")[[1]]
      lo <- as.integer(parts[1])
      hi <- as.integer(parts[2])
      width <- nchar(parts[1])
      expanded <- sprintf(paste0("%0", width, "d"), seq(lo, hi))
      result <- c(result, expanded)
    } else {
      result <- c(result, item)
    }
  }
  result
}

#' resolve column name shorthands in validation checks (internal)
#'
#' maps common shorthands to actual column names in the data frame.
#' @noRd
resolve_check_cols <- function(cols, df_names) {
  # common shorthands -> actual column names
  aliases <- c(
    "wave"       = "wave_id",
    "year"       = "wave_year",
    "person"     = "nomem_encr",
    "respondent" = "nomem_encr",
    "household"  = "nohouse_encr"
  )
  cols <- unlist(cols)
  vapply(cols, function(col) {
    if (col %in% df_names) return(col)
    if (col %in% names(aliases) && aliases[col] %in% df_names) return(aliases[col])
    col
  }, character(1), USE.NAMES = FALSE)
}

run_validations <- function(df, checks, log_entries) {
  results <- list()
  error_count <- 0L

  for (chk in checks) {
    cid  <- chk$check_id %||% "?"
    sev  <- chk$severity %||% "warning"
    type <- chk$type %||% ""

    result <- tryCatch({
      switch(type,
        "structural_missingness" = {
          suffixes <- chk$suffixes %||% list()
          na_waves <- chk$waves_must_be_all_na %||% chk$waves %||% list()
          passed <- TRUE
          for (sfx in suffixes) {
            col <- find_col(df, sfx)
            if (!is.null(col) && col %in% names(df)) {
              subset <- df[df$wave_id %in% na_waves, col, drop = TRUE]
              if (any(!is.na(subset))) { passed <- FALSE; break }
            }
          }
          list(check_id = cid, passed = passed, severity = sev)
        },
        "uniqueness" = , "assert_unique" = , "n_duplicates" = {
          col <- chk$column %||% chk$key %||% "nomem_encr"
          group <- chk$within %||% chk$group_by %||% "wave_id"
          # resolve shorthand column names
          group <- resolve_check_cols(group, names(df))
          col <- resolve_check_cols(col, names(df))
          # group by key + grouping var, then find duplicates
          keys <- unique(c(col, group))
          keys <- intersect(keys, names(df))
          if (length(keys) == 0) {
            list(check_id = cid, passed = TRUE, severity = "info",
                 detail = "key columns not found in data")
          } else {
            dupes <- df |>
              dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
              dplyr::filter(dplyr::n() > 1) |> nrow()
            list(check_id = cid, passed = (dupes == 0), severity = sev,
                 detail = paste0("duplicates: ", dupes))
          }
        },
        "value_absence" = , "assert_absent_values" = {
          suffixes <- chk$suffixes %||% chk$variables %||% NULL
          if (is.null(suffixes)) suffixes <- expand_items(chk$items)
          # single-variable fallback
          if (length(suffixes) == 0) {
            single <- chk$column %||% chk$variable %||% NULL
            if (!is.null(single)) suffixes <- single
          }
          forbidden <- chk$forbidden_values %||% list()
          forbidden <- unlist(forbidden)
          # optional wave scoping: restrict the forbidden-value scan to in_waves
          # (e.g. cw V01 allows scheme-3 codes 56/57 only in cw24q/cw25r)
          in_w <- chk$in_waves %||% NULL
          row_keep <- if (!is.null(in_w) && "wave_id" %in% names(df))
            as.character(df$wave_id) %in% as.character(unlist(in_w)) else TRUE
          passed <- TRUE
          detail <- NULL
          for (sfx in suffixes) {
            col <- find_col(df, sfx)
            if (!is.null(col) && col %in% names(df)) {
              vals <- df[[col]][row_keep]
              if (any(vals %in% forbidden, na.rm = TRUE)) {
                n_bad <- sum(vals %in% forbidden, na.rm = TRUE)
                passed <- FALSE
                detail <- paste0(n_bad, " forbidden value(s) in ", col)
                break
              }
            }
          }
          list(check_id = cid, passed = passed, severity = sev,
               detail = detail)
        },
        "expected_presence" = {
          # validate specific variable exists in specific waves
          var <- chk$variable %||% ""
          waves <- chk$waves %||% list()
          passed <- TRUE
          if (nchar(var) > 0 && var %in% names(df)) {
            for (w in waves) {
              subset <- df[df$wave_id == w, var, drop = TRUE]
              if (all(is.na(subset))) { passed <- FALSE; break }
            }
          }
          list(check_id = cid, passed = passed, severity = sev)
        },
        "value_range" = , "range_check" = {
          # check that values fall within a specified range (or are NA)
          suffixes <- chk$suffixes %||% chk$variables %||% NULL
          if (is.null(suffixes)) suffixes <- expand_items(chk$items)
          lo <- chk$min %||% -Inf
          hi <- chk$max %||% Inf
          passed <- TRUE
          detail <- NULL
          for (sfx in suffixes) {
            col <- find_col(df, sfx)
            if (!is.null(col) && col %in% names(df) && is.numeric(df[[col]])) {
              vals <- df[[col]][!is.na(df[[col]])]
              bad <- sum(vals < lo | vals > hi)
              if (bad > 0) {
                passed <- FALSE
                detail <- paste0(bad, " out-of-range value(s) in ", col,
                                 " [", lo, "..", hi, "]")
                break
              }
            }
          }
          list(check_id = cid, passed = passed, severity = sev,
               detail = detail)
        },
        "na_rate" = , "na_rate_check" = {
          # check NA rate against threshold, optionally restricted by `waves` and a
          # `condition` expression evaluated against the post-derive frame. the
          # condition (e.g. questionnaire_version == 'short') selects the rows whose
          # rate is measured; cp V13 gates on the short-form respondents only.
          suffixes <- chk$suffixes %||% chk$variables %||% NULL
          if (is.null(suffixes)) suffixes <- expand_items(chk$items)
          waves <- chk$waves %||% NULL
          threshold <- chk$threshold %||% chk$max_rate %||% 1.0
          direction <- chk$direction %||% "below"
          keep <- rep(TRUE, nrow(df))
          if (!is.null(waves) && "wave_id" %in% names(df))
            keep <- keep & (as.character(df$wave_id) %in% as.character(unlist(waves)))
          cond <- chk$condition %||% NULL
          cond_ok <- TRUE
          if (!is.null(cond) && nzchar(as.character(cond))) {
            cmask <- tryCatch(eval(parse(text = cond), envir = df, enclos = baseenv()),
                              error = function(e) NULL)
            if (is.logical(cmask) && length(cmask) == nrow(df)) {
              keep <- keep & !is.na(cmask) & cmask
            } else cond_ok <- FALSE
          }
          if (!cond_ok) {
            list(check_id = cid, passed = NA, severity = sev,
                 detail = paste0("condition not evaluable: ", cond))
          } else {
            passed <- TRUE
            detail <- NULL
            for (sfx in suffixes) {
              col <- find_col(df, sfx)
              if (!is.null(col) && col %in% names(df)) {
                subset <- df[[col]][keep]
                rate <- if (length(subset)) mean(is.na(subset)) else NA_real_
                ok <- if (is.na(rate)) TRUE
                      else if (direction == "above") rate >= threshold
                      else rate <= threshold
                if (!ok) {
                  passed <- FALSE
                  detail <- paste0("NA rate ", round(rate, 4), " in ", col,
                                   " (threshold: ", direction, " ", threshold, ")")
                  break
                }
              }
            }
            list(check_id = cid, passed = passed, severity = sev, detail = detail)
          }
        },
        "wave_count" = {
          # check max waves per person
          col <- chk$column %||% chk$key %||% "nomem_encr"
          max_waves <- chk$max_waves %||% Inf
          if (col %in% names(df) && "wave_id" %in% names(df)) {
            counts <- dplyr::n_distinct(df$wave_id[!is.na(df[[col]])])
            per_person <- df |>
              dplyr::group_by(dplyr::across(dplyr::all_of(col))) |>
              dplyr::summarise(n = dplyr::n_distinct(.data$wave_id),
                               .groups = "drop")
            max_seen <- max(per_person$n)
            passed <- max_seen <= max_waves
            list(check_id = cid, passed = passed, severity = sev,
                 detail = paste0("max waves per person: ", max_seen))
          } else {
            list(check_id = cid, passed = TRUE, severity = "info",
                 detail = "key columns not found")
          }
        },
        list(check_id = cid, passed = NA, severity = sev,
             detail = paste0("type '", type, "' not implemented; SKIPPED"))
      )
    }, error = function(e) {
      list(check_id = cid, passed = NA, severity = sev,
           detail = paste0("error: ", e$message))
    })

    results <- append(results, list(result))

    status <- if (isTRUE(result$passed)) "PASS"
              else if (isFALSE(result$passed)) "FAIL" else "SKIP"
    icon <- switch(status, "PASS" = "\u2713", "FAIL" = "\u2717", "~")
    detail_str <- if (!is.null(result$detail) && status == "FAIL") {
      paste0(" -- ", result$detail)
    } else ""
    cli::cli_inform("{icon} [{sev}] {cid}: {status}{detail_str}")

    # severity=error failures are hard errors
    if (sev == "error" && isFALSE(result$passed)) {
      error_count <- error_count + 1L
    }
  }

  if (error_count > 0)
    cli::cli_warn("{error_count} validation check(s) with severity='error' FAILED")

  n_pass <- sum(vapply(results, function(r) isTRUE(r$passed), logical(1)))
  n_fail <- sum(vapply(results, function(r) isFALSE(r$passed), logical(1)))
  n_skip <- length(results) - n_pass - n_fail
  if (n_skip > 0)
    cli::cli_inform(paste0("  checks: ", n_pass, " passed, ", n_fail,
                           " failed, ", n_skip,
                           " skipped (type not implemented or not evaluable)"))

  list(results = results, log = log_entries, error_count = error_count,
       n_pass = n_pass, n_fail = n_fail, n_skip = n_skip)
}

# ============================================================================
# 8. MAIN MERGE PIPELINE
# ============================================================================

#' run the full merge pipeline for a single module
#'
#' loads wave files, applies all rules (variable, harmonization, boundary,
#' drop/retain), derives variables, runs validation checks, and writes
#' outputs (merged SAV, JSONL log, JSON summary, text report).
#'
#' @param recipe a parsed recipe list (from [load_recipe()]), or a path to a recipe file.
#' @param data_dir character. directory containing wave data files.
#' @param output_dir character. directory for output files.
#' @param strict logical. if `TRUE`, abort before writing any outputs when a
#'   validation check with `severity: error` fails; the default `FALSE`
#'   preserves the historical report-and-continue behavior.
#' @return a list (invisibly) with elements `data`, `log`, `validation`,
#'   `summary`, and `recipe`.
#' @export
merge_liss_module <- function(recipe, data_dir, output_dir = ".", strict = FALSE) {
  # accept either a parsed recipe list or a path to a recipe file
  if (is.character(recipe) && length(recipe) == 1) recipe <- load_recipe(recipe)

  mod_code  <- recipe$meta$module
  mod_label <- recipe$meta$module_label
  cli::cli_h1("merging module: {mod_label} ({toupper(mod_code)})")

  all_wave_ids <- purrr::map_chr(recipe$wave_index, ~ .x$id)
  wave_years   <- purrr::set_names(
    purrr::map_int(recipe$wave_index, ~ as.integer(.x$year)),
    all_wave_ids
  )
  # build wave_meta lookup for role resolution
  wave_meta_map <- purrr::set_names(recipe$wave_index, all_wave_ids)

  global     <- recipe$global
  id_var     <- global$id_variable %||% "nomem_encr"
  wave_var   <- global$wave_variable %||% "wave_id"
  year_var   <- global$year_variable %||% "wave_year"
  lbl_policy <- global$labelled_policy %||% "to_numeric"
  strip_ws   <- global$strip_label_whitespace %||% TRUE
  expected   <- global$expected_presence %||% NULL

  log_entries <- list()
  # per-column value-label registry for the write-time restore (round-trip)
  label_registry <- new.env(parent = emptyenv())

  # phase 1: load and pre-process each wave
  cli::cli_h2("phase 1: loading and pre-processing waves")
  wave_files <- discover_wave_files(recipe, data_dir)
  processed_waves <- list()

  for (wf in wave_files) {
    wid <- wf$wave_id
    wm  <- wave_meta_map[[wid]] %||% list()
    cli::cli_inform("  processing wave {.val {wid}} ({length(wf$paths)} file(s))")

    dfs <- purrr::map(wf$paths, read_wave_file)
    df <- if (length(dfs) == 1) dfs[[1]] else dplyr::bind_rows(dfs)

    # aux files (recipe-declared supplemental samples): stack only under an
    # enforced zero-overlap contract; shared ids mean a superseding release
    for (ap in (wf$aux_paths %||% character(0))) {
      aux_df <- read_wave_file(ap)
      if (id_var %in% names(df) && id_var %in% names(aux_df)) {
        overlap <- intersect(df[[id_var]], aux_df[[id_var]])
        if (length(overlap) > 0) {
          cli::cli_abort(c(
            paste0("wave '", wid, "': aux file '", basename(ap), "' shares ",
                   length(overlap), " respondent id(s) with the primary file"),
            "i" = paste0("aux_files must contain disjoint respondents ",
                         "(supplemental samples); a shared-id file is a ",
                         "superseding release and must not be stacked"),
            "i" = "fix file_pattern / aux_files for this wave in the recipe"))
        }
      }
      df <- dplyr::bind_rows(df, aux_df)
      cli::cli_inform("  stacked aux file {.file {basename(ap)}} (+{nrow(aux_df)} rows)")
    }

    # unconditional integrity gate: duplicated respondent ids within one wave
    # are never valid for these modules, whatever the recipe says
    if (id_var %in% names(df)) {
      n_dup <- sum(duplicated(df[[id_var]]) & !is.na(df[[id_var]]))
      if (n_dup > 0) {
        cli::cli_abort(c(
          paste0("wave '", wid, "': ", n_dup, " duplicated '", id_var,
                 "' value(s) in the loaded data"),
          "i" = paste0("check for superseded file versions or unintended ",
                       "extra matches in '", data_dir, "'")))
      }
    }

    if (strip_ws) df <- strip_label_whitespace(df)
    df <- strip_wave_prefix(df, wid, c(id_var, "nohouse_encr"))
    df <- apply_labelled_policy(df, lbl_policy)
    label_registry <- harvest_labels(df, label_registry, wid)

    df[[wave_var]] <- wid
    df[[year_var]] <- as.integer(wave_years[wid])

    # expected-presence enforcement
    df <- check_expected_presence(df, wid, expected)

    # apply variable rules
    for (rule in (recipe$variable_rules %||% list())) {
      result <- exec_variable_rule(df, rule, wid, wm, all_wave_ids, log_entries)
      df <- result$df; log_entries <- result$log
    }

    # apply harmonization rules
    for (rule in (recipe$harmonization_rules %||% list())) {
      result <- exec_harmonization_rule(df, rule, wid, wm, all_wave_ids, log_entries)
      df <- result$df; log_entries <- result$log
    }

    # auto-derive fieldwork_ym from LISS _m convention if not already present
    if (!("fieldwork_ym" %in% names(df))) {
      fm_col <- find_col(df, "_m")
      if (!is.null(fm_col) && fm_col %in% names(df)) {
        df[["fieldwork_ym"]] <- df[[fm_col]]
      }
    }

    processed_waves[[wid]] <- df
  }

  # emit batched warnings for absent expected-presence variables
  if (!is.null(expected)) {
    for (ep in (expected$critical %||% list())) {
      if ((ep$on_absence %||% "error") == "warn") {
        absent_in <- purrr::keep(names(processed_waves), function(wid) {
          !(ep$variable %in% names(processed_waves[[wid]])) ||
            all(is.na(processed_waves[[wid]][[ep$variable]]))
        })
        if (length(absent_in) > 0) {
          cli::cli_warn(paste0(
            "variable '", ep$variable, "' absent in ",
            length(absent_in), " wave(s), created as NA"))
        }
      }
    }
  }

  # phase 2: stack
  cli::cli_h2("phase 2: stacking {length(processed_waves)} waves")

  # harmonize column types across waves to prevent bind_rows failures
  if (length(processed_waves) > 1) {
    processed_waves <- harmonize_column_types(processed_waves)
  }

  merged <- dplyr::bind_rows(processed_waves)
  cli::cli_inform("  stacked: {nrow(merged)} rows x {ncol(merged)} cols")

  if (nrow(merged) == 0) {
    cli::cli_abort(c(
      "no data loaded, 0 rows after stacking",
      "i" = "check that wave files exist in {.path {data_dir}}",
      "i" = "expected file patterns like {.val {recipe$wave_index[[1]]$file_pattern}}",
      "i" = "use {.code liss_download()} to fetch data files first"
    ))
  }

  # phase 3: boundary rules
  cli::cli_h2("phase 3: boundary rules")
  for (rule in (recipe$boundary_rules %||% list())) {
    result <- exec_boundary_rule(merged, rule, all_wave_ids, log_entries)
    merged <- result$df; log_entries <- result$log
  }

  # phase 4: drop/retain
  cli::cli_h2("phase 4: drop/retain rules")
  dr <- exec_drop_retain(merged, recipe$drop_retain_rules %||% list(), log_entries)
  merged <- dr$df; log_entries <- dr$log

  # phase 5: derived variables (superset executor)
  cli::cli_h2("phase 5: derived variables")
  if (isTRUE(.lissr_have_executors)) {
    # resolve the source-suffix set applicable to a given wave; handles the
    # contract list-of-blocks shape, a flat suffix list, and ca's named
    # default/<wave> map shape.
    .dv_sfx_for_wave <- function(sources, w) {
      if (is.null(sources) || length(sources) == 0) return(character(0))
      nm <- names(sources)
      if (!is.null(nm) && any(nzchar(nm))) {
        if (w %in% nm)         return(as.character(unlist(sources[[w]])))
        if ("default" %in% nm) return(as.character(unlist(sources[["default"]])))
        return(character(0))
      }
      if (all(vapply(sources, function(e) !is.list(e), logical(1))))
        return(as.character(unlist(sources)))
      for (blk in sources) {
        if (!is.list(blk)) next
        bw <- blk$waves
        all_waves <- length(bw) == 1L && tolower(as.character(bw)) == "all"
        if (is.null(bw) || all_waves || (w %in% bw))
          return(as.character(unlist(blk$variable %||% blk$variables %||%
                                     blk$source_suffixes %||% list())))
      }
      character(0)
    }
    # per-block value_crosswalk: the block matching wave w may carry its own
    # value_crosswalk, applied only to that block's rows. mirrors the block
    # matching in .dv_sfx_for_wave so it returns the SAME block's map. enables
    # multi-era DVs whose source AND recode differ per era (cd h_rent_benefit:
    # 011/{1:1,2:0}, 086/{1:1,2:1,3:0}, 088/{1:1,2:0}; code 2 conflicts across
    # eras so no single global map works).
    .dv_xwalk_for_wave <- function(sources, w) {
      if (is.null(sources) || length(sources) == 0) return(NULL)
      nm <- names(sources)
      if (!is.null(nm) && any(nzchar(nm))) return(NULL)
      for (blk in sources) {
        if (!is.list(blk)) next
        bw <- blk$waves
        all_waves <- length(bw) == 1L && tolower(as.character(bw)) == "all"
        if (is.null(bw) || all_waves || (w %in% bw))
          return(blk$value_crosswalk %||% blk$recode)
      }
      NULL
    }
    .dv_resolve_col <- function(df, s) {
      c1 <- tryCatch(find_col(df, s), error = function(e) NULL)
      if (!is.null(c1) && length(c1) == 1 && c1 %in% names(df)) return(c1)
      if (s %in% names(df)) return(s)
      NULL
    }
    # apply a value_crosswalk that may be numeric or string-valued. an all-numeric
    # target map routes through crosswalk_map (numeric, behavior unchanged); a map
    # with any non-numeric target (cp DV08 long/short) routes through
    # crosswalk_map_chr so string targets survive (result coerces to character).
    .dv_xwalk_apply <- function(x, mapping) {
      vals <- unlist(mapping, use.names = FALSE)
      num_ok <- length(vals) > 0 &&
        all(!is.na(suppressWarnings(as.numeric(as.character(vals)))))
      if (num_ok) crosswalk_map(x, mapping) else crosswalk_map_chr(x, mapping)
    }
    waves_in_data <- unique(as.character(merged[[wave_var]]))
    for (dv in (recipe$derived_variables %||% list())) {
      nm <- dv$name %||% dv$var_name %||% ""
      if (nchar(nm) == 0 || nm %in% c(wave_var, year_var)) next
      rid    <- dv$rule_id %||% nm
      method <- dv$method %||% "sum"
      maz    <- isTRUE(dv$missing_as_zero) || identical(dv$na_rule, "zero_if_all_missing")
      otype  <- dv$output_type %||%
                (if (identical(dv$domain, "euro_amount")) "double" else NULL)
      # stack_aux_files / pending-spec DVs are held (Part VI): do not fabricate
      if (identical(dv$method, "stack_aux_files") || isTRUE(dv$pending_spec)) {
        log_entries <- append(log_entries, list(
          make_log(rid, "*", nm, "derive:PENDING_SPEC", 0L)))
        next
      }
      result  <- rep(NA_real_, nrow(merged))
      any_src <- FALSE
      for (w in waves_in_data) {
        sfx <- .dv_sfx_for_wave(dv$sources, w)
        if (length(sfx) == 0) next
        cols <- Filter(Negate(is.null), lapply(sfx, function(s) .dv_resolve_col(merged, s)))
        if (length(cols) == 0) next
        any_src <- TRUE
        rows <- which(as.character(merged[[wave_var]]) == w)
        src_list <- lapply(cols, function(cc) suppressWarnings(as.numeric(merged[[cc]][rows])))
        result[rows] <- dv_aggregate(src_list, method, maz)
        bx <- .dv_xwalk_for_wave(dv$sources, w)
        if (!is.null(bx) && length(bx))
          result[rows] <- .dv_xwalk_apply(result[rows], bx)
      }
      # wave_values: a wave -> constant map (the timing mechanism). assigns a
      # per-wave scalar with no source columns; authored maps (e.g. cd h_recall_months)
      # and migrated wave-constant DVs use it. a character value (e.g. a wave id)
      # coerces the result vector as needed; a null entry yields NA for that wave.
      wv <- dv$wave_values
      if (!is.null(wv) && length(wv)) {
        for (w in waves_in_data) {
          if (w %in% names(wv)) {
            val  <- wv[[w]]
            rows <- which(as.character(merged[[wave_var]]) == w)
            result[rows] <- if (is.null(val)) NA else val
            any_src <- TRUE
          }
        }
      }
      # waves_override: a per-wave constant map with a `default` for waves not in
      # the map (cp DV06/DV07). ".na" or a null entry yields NA; a character value
      # coerces the result vector. counts as a resolved source for the clobber guard.
      wo <- dv$waves_override
      if (!is.null(wo) && length(wo)) {
        dflt <- dv$default
        for (w in waves_in_data) {
          rows <- which(as.character(merged[[wave_var]]) == w)
          if (w %in% names(wo)) {
            val <- wo[[w]]
            if (is.null(val) || identical(as.character(val), ".na")) val <- NA
            result[rows] <- val
          } else if (!is.null(dflt)) {
            result[rows] <- dflt
          }
        }
        any_src <- TRUE
      }
      if (!is.null(dv$value_crosswalk) && length(dv$value_crosswalk))
        result <- .dv_xwalk_apply(result, dv$value_crosswalk)
      # optional per-row arithmetic post-op (transform_apply ops: add, subtract,
      # int_divide, modulo). lets a DV derive e.g. fieldwork_year = yyyymm %/% 100.
      # exact [["transform"]]: $ would partial-match a documentary `transform_ref`
      # key (ci ladder_of_life) and dispatch on its scalar value. is.list guards a
      # malformed scalar transform.
      tr <- dv[["transform"]]
      if (is.list(tr) && !is.null(tr$op))
        result <- transform_apply(result, tr$op, tr$value)
      result <- dv_coerce_output(result, otype)
      n_oor <- range_check(suppressWarnings(as.numeric(result)), dv$valid_range)
      if (n_oor > 0)
        cli::cli_warn("derived {.val {nm}}: {n_oor} value(s) outside valid_range")
      # clobber guard: a DV that resolved no sources must not overwrite a column
      # already populated upstream (e.g. a scheme crosswalk output) with all-NA.
      if (!any_src && nm %in% names(merged) && any(!is.na(merged[[nm]]))) {
        log_entries <- append(log_entries, list(
          make_log(rid, "*", nm, "derive:SKIP_KEEP_EXISTING",
                   nrow(merged), values_changed = sum(!is.na(merged[[nm]])))))
      } else {
        merged[[nm]] <- result
        if (!is.null(dv$quality_flag))
          merged[[dv$quality_flag]] <- as.integer(!is.na(result))
        log_entries <- append(log_entries, list(
          make_log(rid, "*", nm,
                   if (any_src) "derive" else "derive:NO_SOURCES_NA",
                   nrow(merged), values_changed = sum(!is.na(result)))))
      }
    }
  } else {
    for (dv in (recipe$derived_variables %||% list())) {
      vname <- dv$name %||% dv$var_name %||% ""
      if (nchar(vname) > 0 && !(vname %in% names(merged))) {
        if (vname %in% c(wave_var, year_var)) next
        merged[[vname]] <- NA
        log_entries <- append(log_entries, list(
          make_log(dv$rule_id %||% vname, "*", vname, "derive", 0L)))
      }
    }
  }

  # phase 6: validation
  cli::cli_h2("phase 6: validation checks")
  val <- run_validations(merged, recipe$validation_checks %||% list(), log_entries)
  log_entries <- val$log

  if (isTRUE(strict) && val$error_count > 0) {
    cli::cli_abort(c(
      paste0(val$error_count, " validation check(s) with severity='error' failed"),
      "i" = "strict mode: no outputs were written",
      "i" = "re-run with strict = FALSE to write outputs despite failures"))
  }

  # phase 7: write outputs
  cli::cli_h2("phase 7: writing outputs")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("The 'haven' package is required to write .sav output.\n",
         "Install it with: install.packages(\"haven\")", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The 'jsonlite' package is required for logging.\n",
         "Install it with: install.packages(\"jsonlite\")", call. = FALSE)
  }

  # restore value labels and user-missing declarations harvested in phase 1
  # where provably safe; sweep declared missing codes the recipes left behind
  # on every column that could not be restored, so no dk/refusal code ever
  # leaks into the output as a substantive value
  if (identical(lbl_policy, "to_numeric")) {
    rl <- restore_value_labels(merged, label_registry)
    merged <- rl$data
    # recipe exclude blocks are documented decisions that a declared code is
    # substantive for those suffix/wave cells; the sweep must honor them
    veto <- list()
    all_rules <- c(recipe$variable_rules %||% list(),
                   recipe$harmonization_rules %||% list())
    for (r in all_rules) {
      for (bl in (r$exclude %||% list())) {
        sfxs <- as.character(unlist(bl$suffixes %||% list()))
        wvs  <- as.character(unlist(bl$waves %||% list()))
        if (length(wvs) == 0) wvs <- names(wave_years)
        for (sfx in sfxs)
          veto[[sfx]] <- unique(c(veto[[sfx]], wvs))
      }
    }
    sw <- sweep_user_missing(merged, label_registry, rl$skipped,
                             wave_var = wave_var, veto = veto)
    merged <- sw$data
    cli::cli_inform(paste0("  value labels restored on ", length(rl$restored),
                           " column(s); skipped on ", length(rl$skipped),
                           " (recoded values or era-dependent metadata); ",
                           sw$swept, " residual user-missing cell(s) swept to NA"))
    log_entries <- append(log_entries, list(
      make_log("LABEL_RESTORE", "*",
               paste0(length(rl$restored), " restored / ",
                      length(rl$skipped), " skipped"),
               "restore_value_labels", length(rl$restored)),
      make_log("NA_SWEEP", "*", paste0(length(rl$skipped), " col(s)"),
               "sweep_user_missing", sw$swept,
               values_changed = sw$swept)))
  }

  # merged data (SPSS .sav carries the variable labels and the restored value labels)
  merged <- sanitize_spss_names(merged)
  out_sav <- file.path(output_dir, paste0(mod_code, "_merged.sav"))
  haven::write_sav(merged, out_sav)
  cli::cli_inform("  data: {.file {out_sav}}")

  # audit-grade JSONL log
  log_cfg <- recipe$logging %||% list()
  log_file <- file.path(output_dir,
    log_cfg$log_file %||% paste0(mod_code, "_merge_log.jsonl"))
  write_jsonl(log_entries, log_file)
  cli::cli_inform("  log: {.file {log_file}}")

  # summary artifact
  summary_cfg <- log_cfg$summary_artifact %||% list()
  # accept the scalar shorthand `summary_artifact: true` as well as the map form
  if (!is.list(summary_cfg)) summary_cfg <- list(enabled = isTRUE(as.logical(summary_cfg)))
  summary <- NULL
  if (isTRUE(summary_cfg$enabled)) {
    summary <- generate_summary(merged, log_entries, recipe)
    summary_file <- file.path(output_dir, paste0(mod_code, "_merge_summary.json"))
    jsonlite::write_json(summary, summary_file, pretty = TRUE, auto_unbox = TRUE)
    cli::cli_inform("  summary: {.file {summary_file}}")
  }

  # text report
  report_file <- file.path(output_dir,
    log_cfg$report_file %||% paste0(mod_code, "_merge_report.txt"))
  write_report(merged, val$results, log_entries, recipe, report_file)
  cli::cli_inform("  report: {.file {report_file}}")

  cli::cli_alert_success(
    "module {toupper(mod_code)} merge complete: {nrow(merged)} rows x {ncol(merged)} cols")

  invisible(list(
    data       = merged,
    log        = log_entries,
    validation = val$results,
    summary    = summary,
    recipe     = recipe
  ))
}

#' merge multiple modules sequentially
#'
#' validates all recipes first, then runs each module merge. modules
#' with no data files in `data_dir` are silently skipped.
#'
#' @param recipe_paths character vector of paths to YAML recipe files.
#' @param data_dir character. root data directory. per-module subdirectories
#'   are tried first (e.g. `data_dir/ch/`), falling back to `data_dir`.
#' @param output_dir character. directory for output files.
#' @param strict logical. forwarded to [merge_liss_module()].
#' @return a named list of per-module results (invisibly).
#' @export
merge_liss_modules <- function(recipe_paths, data_dir, output_dir = ".",
                               strict = FALSE) {
  recipes <- load_recipes(recipe_paths)
  results <- list()

  # pre-scan: determine which modules have data files present
  available <- character(0)
  skipped   <- character(0)
  for (mod in names(recipes)) {
    mod_data_dir <- file.path(data_dir, mod)
    if (!dir.exists(mod_data_dir)) mod_data_dir <- data_dir
    # quick check: any files matching the module prefix?
    mod_files <- list.files(mod_data_dir,
      pattern = paste0("^", mod, "\\d{2}[a-z]"),
      ignore.case = TRUE)
    if (length(mod_files) > 0) {
      available <- c(available, mod)
    } else {
      skipped <- c(skipped, mod)
    }
  }

  if (length(skipped) > 0) {
    cli::cli_alert_info(
      "skipping {length(skipped)} module(s) with no data files: {paste(skipped, collapse = ', ')}")
  }

  if (length(available) == 0) {
    cli::cli_abort(c(
      "no data files found for any module in {.path {data_dir}}",
      "i" = "download data first with {.code liss_download()}"
    ))
  }

  for (mod in available) {
    mod_data_dir <- file.path(data_dir, mod)
    if (!dir.exists(mod_data_dir)) mod_data_dir <- data_dir
    tryCatch({
      results[[mod]] <- merge_liss_module(recipes[[mod]], mod_data_dir, output_dir,
                                           strict = strict)
    }, error = function(e) {
      cli::cli_alert_danger(
        "module {.val {mod}} failed: {e$message}"
      )
      cli::cli_alert_info("continuing with remaining modules")
      results[[mod]] <<- list(
        data = NULL, log = list(), error = e$message
      )
    })
  }
  # summary
  ok <- purrr::map_lgl(results, ~ !is.null(.x$data))
  cli::cli_h2("merge batch complete")
  cli::cli_alert_success("{sum(ok)}/{length(available)} module(s) succeeded")
  if (any(!ok)) {
    failed <- names(results)[!ok]
    cli::cli_alert_warning("failed: {paste(failed, collapse = ', ')}")
  }
  if (length(skipped) > 0) {
    cli::cli_alert_info("skipped (no data): {paste(skipped, collapse = ', ')}")
  }
  invisible(results)
}

# ============================================================================
# 9. REPORT WRITER
# ============================================================================

#' @noRd
write_report <- function(merged, validation_results, log_entries, recipe, path) {
  mod <- recipe$meta$module
  lines <- c(
    paste0("LISS ", toupper(mod), " Module \u2014 Merge Report"),
    paste0("Generated: ", Sys.time()),
    paste0("Recipe version: ", recipe$meta$recipe_version),
    paste0("Schema: canonical v1.1.0 (accepts v1.0.0)"),
    "",
    paste0("Rows: ", nrow(merged)),
    paste0("Columns: ", ncol(merged)),
    paste0("Waves: ", length(unique(merged$wave_id))),
    paste0("Unique respondents: ", dplyr::n_distinct(merged$nomem_encr)),
    "",
    "--- Validation Summary ---"
  )

  for (vr in validation_results) {
    status <- if (isTRUE(vr$passed)) "PASS"
              else if (isFALSE(vr$passed)) "FAIL" else "SKIP"
    lines <- c(lines, paste0("[", vr$severity, "] ", vr$check_id, ": ", status,
                             if (!is.null(vr$detail)) paste0(" -- ", vr$detail) else ""))
  }

  # comparability warnings
  boundary_rules <- recipe$boundary_rules %||% list()
  comps <- purrr::keep(boundary_rules, ~ !is.null(.x$comparability))
  if (length(comps) > 0) {
    lines <- c(lines, "", "--- Comparability Contracts ---")
    for (br in comps) {
      cc <- br$comparability
      lines <- c(lines, paste0(
        "[", cc$status, "] ", br$rule_id, ": method=", cc$method,
        " | ", cc$rationale %||% ""))
    }
  }

  lines <- c(lines, "", paste0("Rules applied: ", length(log_entries)))
  writeLines(lines, path)
}

# ============================================================================
# 10. TIMING HELPER
# ============================================================================

#' @noRd
elapsed_ms <- function(t0) {
  round(as.numeric(proc.time() - t0)["elapsed"] * 1000, 1)
}

# ============================================================================
# 11. RECIPE HELPER
# ============================================================================

#' load a built-in merge recipe by module code
#'
#' convenience function to load one of the bundled YAML recipes shipped
#' with the package (in `inst/recipes/`).
#'
#' @param module character. two-letter module code (e.g. `"ch"`, `"cv"`).
#' @return a parsed recipe list (validated against the canonical schema).
#' @export
#' @examples
#' \dontrun{
#' recipe <- liss_recipe("ch")
#' }
liss_recipe <- function(module) {
  path <- system.file(
    "recipes", paste0(module, "_merge_recipe.yml"),
    package = "lissr", mustWork = TRUE
  )
  load_recipe(path)
}

# ============================================================================
# 12. CROSS-MODULE PANEL MERGE
# ============================================================================

#' merge multiple LISS modules into a single panel dataset
#'
#' takes the output of [merge_liss_modules()] (or a list of per-module
#' data frames) and joins them into one wide dataset keyed by respondent
#' and wave year. module-specific columns are prefixed with the module
#' code (e.g. `ch_s004`, `cv_s004`) to avoid name collisions.
#'
#' @param results either the named list returned by [merge_liss_modules()]
#'   (where each element has a `$data` tibble), or a named list of data frames
#'   directly. names should be module codes (e.g. `"ch"`, `"cv"`).
#' @param join_by character vector of columns to join on. defaults to
#'   `c("nomem_encr", "wave_year")`.
#' @param shared_cols character vector of additional columns to keep unprefixed
#'   (carried from the first module that has them). defaults to
#'   `c("nohouse_encr")`.
#' @param join_type character. type of join: `"full"` (default), `"inner"`,
#'   or `"left"` (keeps all rows from the first module).
#' @param write_to optional file path. if provided, the merged panel is written
#'   as SAV (SPSS format, preserving labels). should end in `.sav`.
#' @return a tibble with all modules joined side by side.
#' @export
#' @examples
#' \dontrun{
#' results <- merge_liss_modules(recipe_paths, data_dir = "~/Downloads/liss")
#' panel <- merge_liss_panel(results)
#' panel <- merge_liss_panel(results, write_to = "./output/liss_panel.sav")
#' }
merge_liss_panel <- function(results,
                             join_by     = c("nomem_encr", "wave_year"),
                             shared_cols = c("nohouse_encr"),
                             join_type   = c("full", "inner", "left"),
                             write_to    = NULL) {
  join_type <- match.arg(join_type)

  # extract data frames from results list
  dfs <- list()
  for (mod in names(results)) {
    d <- results[[mod]]
    if (is.data.frame(d)) {
      dfs[[mod]] <- d
    } else if (is.list(d) && !is.null(d$data) && is.data.frame(d$data)) {
      dfs[[mod]] <- d$data
    }
  }

  if (length(dfs) == 0) {
    cli::cli_abort("no valid data frames found in results")
  }

  cli::cli_h1("merging {length(dfs)} module(s) into panel dataset")

  # determine the join function
  join_fn <- switch(join_type,
    "full"  = dplyr::full_join,
    "inner" = dplyr::inner_join,
    "left"  = dplyr::left_join
  )

  # prefix columns and collect shared columns
  shared_pool <- NULL
  prefixed <- list()

  for (mod in names(dfs)) {
    df <- dfs[[mod]]

    # ensure join keys exist
    missing_keys <- setdiff(join_by, names(df))
    if (length(missing_keys) > 0) {
      cli::cli_warn("module {.val {mod}}: missing join key(s) {.val {missing_keys}}, skipping")
      next
    }

    # extract shared columns from first module that has them
    if (is.null(shared_pool)) {
      avail_shared <- intersect(shared_cols, names(df))
      if (length(avail_shared) > 0) {
        shared_pool <- df[, c(join_by, avail_shared), drop = FALSE]
        shared_pool <- dplyr::distinct(shared_pool)
      }
    }

    # columns to prefix: everything except join keys and shared cols
    keep_unprefixed <- c(join_by, shared_cols)
    cols_to_prefix <- setdiff(names(df), keep_unprefixed)

    # prefix with module code
    new_names <- paste0(mod, "_", cols_to_prefix)
    names(df)[match(cols_to_prefix, names(df))] <- new_names

    # drop shared_cols from this df (they come from shared_pool)
    drop_cols <- intersect(shared_cols, names(df))
    if (length(drop_cols) > 0) {
      df <- df[, setdiff(names(df), drop_cols), drop = FALSE]
    }

    prefixed[[mod]] <- df
    cli::cli_inform("  {.val {mod}}: {nrow(df)} rows, {length(new_names)} prefixed columns")
  }

  if (length(prefixed) == 0) {
    cli::cli_abort("no modules had valid join keys")
  }

  # sequential join
  panel <- prefixed[[1]]
  if (length(prefixed) > 1) {
    for (i in 2:length(prefixed)) {
      panel <- join_fn(panel, prefixed[[i]], by = join_by)
    }
  }

  # attach shared columns
  if (!is.null(shared_pool)) {
    panel <- join_fn(panel, shared_pool, by = join_by)
    # move shared cols right after join keys
    col_order <- c(join_by, intersect(shared_cols, names(panel)),
                   setdiff(names(panel), c(join_by, shared_cols)))
    panel <- panel[, col_order, drop = FALSE]
  }

  cli::cli_alert_success(
    "panel: {nrow(panel)} rows x {ncol(panel)} cols ({length(prefixed)} modules)")

  # optionally write
  if (!is.null(write_to)) {
    dir.create(dirname(write_to), showWarnings = FALSE, recursive = TRUE)
    panel <- sanitize_spss_names(panel)
    haven::write_sav(panel, write_to)
    cli::cli_inform("  written to {.file {write_to}}")
  }

  panel
}
