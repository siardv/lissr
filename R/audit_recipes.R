#' audit merge recipes for engine conformance
#'
#' scans recipes (by default all ten bundled ones) and reports, per module,
#' whether every rule payload conforms to its action's specification, how the
#' declared validation checks classify (executable, documentary, skip), and
#' whether wave metadata is internally consistent. this turns the scattered
#' authoring-time warnings into one corpus-level conformance report, suitable
#' for CI gating.
#'
#' for each recipe the audit reports: schema and recipe versions; wave count;
#' whether `meta$covered_waves` equals the `wave_index` ids; whether every
#' `file_pattern` is the canonical glob built from the wave id, in the
#' `ch07a_*` style (documented pins are listed, not flagged); rules whose
#' payload carries keys their action does
#' not read (per the payload registry in `action_vocabulary.yml`); and the
#' validation-check classification counts.
#'
#' @param paths character vector of recipe file paths. default `NULL` audits
#'   all bundled recipes.
#' @param quiet logical. `TRUE` suppresses the printed report.
#' @return invisibly, a list with one entry per recipe (fields `module`,
#'   `schema_version`, `recipe_version`, `n_waves`, `covered_waves_match`,
#'   `noncanonical_patterns`, `nonconforming_rules`, `checks`) plus a
#'   `totals` entry.
#' @export
#' @examples
#' \dontrun{
#' audit <- audit_liss_recipes()
#' audit$totals
#' }
audit_liss_recipes <- function(paths = NULL, quiet = FALSE) {
  if (is.null(paths)) {
    dir <- system.file("recipes", package = "lissr")
    paths <- list.files(dir, pattern = "_merge_recipe\\.yml$",
                        full.names = TRUE)
  }
  if (length(paths) == 0) cli::cli_abort("no recipe files found to audit")

  sections <- c("variable_rules", "harmonization_rules",
                "boundary_rules", "drop_retain_rules")
  out <- list()
  tot <- list(recipes = 0L, waves = 0L, rules = 0L, nonconforming = 0L,
              checks = 0L, executable = 0L, documentary = 0L, skip = 0L)

  for (p in paths) {
    r <- yaml::yaml.load_file(p)
    mod <- r$meta$module %||% basename(p)
    wave_ids <- vapply(r$wave_index %||% list(),
                       function(w) as.character(w$id %||% ""), character(1))

    covered <- as.character(unlist(r$meta$covered_waves %||% list()))
    covered_ok <- setequal(covered, wave_ids)

    noncanon <- character(0)
    for (w in (r$wave_index %||% list())) {
      pat <- as.character(w$file_pattern %||% "")
      if (!identical(pat, paste0(w$id, "_*")))
        noncanon <- c(noncanon, paste0(w$id, " '", pat, "'"))
    }

    bad_rules <- list()
    n_rules <- 0L
    for (sec in sections) {
      rules <- r[[sec]] %||% list()
      for (rule in rules) {
        n_rules <- n_rules + 1L
        act <- paste0(rule$action %||% "", collapse = "")
        unk <- .scan_rule_keys(rule, act)
        if (length(unk) > 0) {
          bad_rules[[length(bad_rules) + 1L]] <- list(
            section = sec,
            rule_id = paste0(rule$rule_id %||% "?", collapse = ""),
            action  = act,
            keys    = unk)
        }
      }
    }

    checks <- r$validation_checks %||% list()
    cls <- vapply(checks, function(c) {
      ty <- paste0(c$type %||% "", collapse = "")
      ty_canon <- if (ty %in% names(.CHECK_ALIASES)) .CHECK_ALIASES[[ty]] else ty
      if (ty_canon %in% .CANONICAL_CHECK_TYPES) "executable"
      else if (ty %in% .DOCUMENTARY_CHECK_TYPES) "documentary"
      else "skip"
    }, character(1))

    entry <- list(
      module = mod,
      schema_version = r$meta$schema_version %||% NA_character_,
      recipe_version = r$meta$recipe_version %||% NA_character_,
      n_waves = length(wave_ids),
      covered_waves_match = covered_ok,
      noncanonical_patterns = noncanon,
      nonconforming_rules = bad_rules,
      checks = list(total = length(checks),
                    executable = sum(cls == "executable"),
                    documentary = sum(cls == "documentary"),
                    skip = sum(cls == "skip"))
    )
    out[[mod]] <- entry

    tot$recipes <- tot$recipes + 1L
    tot$waves <- tot$waves + length(wave_ids)
    tot$rules <- tot$rules + n_rules
    tot$nonconforming <- tot$nonconforming + length(bad_rules)
    tot$checks <- tot$checks + length(checks)
    tot$executable <- tot$executable + sum(cls == "executable")
    tot$documentary <- tot$documentary + sum(cls == "documentary")
    tot$skip <- tot$skip + sum(cls == "skip")

    if (!quiet) {
      flag <- if (length(bad_rules) > 0)
        paste0(length(bad_rules), " nonconforming rule(s)") else "conforming"
      pat_note <- if (length(noncanon) > 0)
        paste0("; ", length(noncanon), " non-default pattern(s)") else ""
      cw_note <- if (!covered_ok) "; covered_waves MISMATCH" else ""
      cli::cli_inform(paste0(
        mod, ": ", length(wave_ids), " waves, ", n_rules, " rules (", flag,
        "), checks ", sum(cls == "executable"), "/",
        sum(cls == "documentary"), "/", sum(cls == "skip"),
        " exec/doc/skip", pat_note, cw_note))
      for (br in bad_rules) {
        cli::cli_inform(paste0("    ", br$rule_id, " (", br$action, "): ",
                               paste(br$keys, collapse = ", ")))
      }
    }
  }

  if (!quiet) {
    cli::cli_inform(paste0(
      "TOTAL: ", tot$recipes, " recipes, ", tot$waves, " waves, ",
      tot$rules, " rules (", tot$nonconforming, " nonconforming), ",
      tot$checks, " checks (", tot$executable, " executable, ",
      tot$documentary, " documentary, ", tot$skip, " skip)"))
  }

  out$totals <- tot
  invisible(out)
}
