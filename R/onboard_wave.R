#' onboard a new wave into a merge recipe
#'
#' semi-automated workflow that reads the new wave file, locates and reads the
#' actual previous wave file (resolved through the recipe's `file_pattern`,
#' with the engine's release-version disambiguation), diffs the real variable
#' suffix sets in both directions, generates a candidate `wave_index` entry,
#' checks expected-presence constraints, flags potential boundary breaks, and
#' prints an onboarding checklist.
#'
#' @param recipe_path character. path to the current canonical YAML recipe.
#' @param new_file character. path to the new wave data file (.sav, .dta, or .csv).
#' @param prev_wave_id character. wave id to diff against (e.g. `"ch24q"`).
#'   if `NULL`, the diff step is skipped.
#' @param prev_file character. optional explicit path to the previous wave
#'   file. if `NULL`, it is resolved via the recipe's `file_pattern` for
#'   `prev_wave_id` in the same directory as `new_file`.
#' @return an onboarding report list (invisibly).
#' @export
#' @examples
#' \dontrun{
#' onboard_new_wave(
#'   "ch_merge_recipe.yml",
#'   "ch25r_EN_1.0p.sav",
#'   prev_wave_id = "ch24q"
#' )
#' }
onboard_new_wave <- function(recipe_path, new_file, prev_wave_id = NULL,
                             prev_file = NULL) {

  recipe <- yaml::yaml.load_file(recipe_path)
  mod <- recipe$meta$module

  cli::cli_h1("onboarding new wave for module {toupper(mod)}")

  # step 1: read new file (auto-detect format)
  cli::cli_h2("step 1: reading new wave file")
  # read_wave_file keeps spss user-defined missing codes visible (user_na),
  # so the sentinel scan in step 6 can actually see them
  new_df <- read_wave_file(new_file)
  cli::cli_inform("  rows: {nrow(new_df)}, cols: {ncol(new_df)}")

  # extract wave id from filename
  fname <- basename(new_file)
  wave_re <- paste0(mod, "\\d{2}[a-z]")
  new_wave_id <- regmatches(fname, regexpr(wave_re, fname))
  if (length(new_wave_id) == 0) {
    cli::cli_warn("cannot extract wave id from filename; using placeholder")
    new_wave_id <- "unknown"
  }
  cli::cli_inform("  inferred wave id: {.val {new_wave_id}}")

  # step 2: variable inventory
  cli::cli_h2("step 2: variable inventory")
  new_vars <- sort(names(new_df))
  cli::cli_inform("  {length(new_vars)} variables in new wave")

  # step 3: diff vs previous wave
  report <- list(
    module = mod,
    new_wave_id = new_wave_id,
    new_file = new_file,
    rows = nrow(new_df),
    cols = ncol(new_df)
  )

  if (!is.null(prev_wave_id)) {
    cli::cli_h2("step 3: diff vs previous wave {.val {prev_wave_id}}")

    # reconstruct expected variable names from previous wave
    prev_wave <- NULL
    for (w in recipe$wave_index) {
      if (w$id == prev_wave_id) { prev_wave <- w; break }
    }
    if (is.null(prev_wave)) {
      cli::cli_warn("previous wave {.val {prev_wave_id}} not found in recipe")
    } else {
      # resolve the actual previous wave file: explicit path, or the recipe's
      # file_pattern in the new file's directory, via the engine's resolver
      # (same glob, fallback, and release-version policy as the merge itself)
      if (is.null(prev_file)) {
        hit <- discover_wave_files(list(wave_index = list(prev_wave)),
                                   dirname(new_file))
        if (length(hit) > 0) prev_file <- hit[[1]]$paths[[1]]
      }

      if (is.null(prev_file) || !file.exists(prev_file)) {
        cli::cli_warn(c(
          "previous wave file for {.val {prev_wave_id}} not found in {.path {dirname(new_file)}}",
          "i" = "pass {.arg prev_file} explicitly; diff skipped"))
        report$diff_skipped <- TRUE
      } else {
        cli::cli_inform("  previous wave file: {.file {basename(prev_file)}}")
        prev_df <- read_wave_file(prev_file)

        # real bidirectional diff on suffix sets stripped from each wave's own
        # variable names (unprefixed columns such as nomem_encr cancel out)
        strip_prefix <- function(nms, wave_id) sub(paste0("^", wave_id), "", nms)
        new_suffixes  <- strip_prefix(new_vars, new_wave_id)
        prev_suffixes <- strip_prefix(sort(names(prev_df)), prev_wave_id)

        added   <- setdiff(new_suffixes, prev_suffixes)
        removed <- setdiff(prev_suffixes, new_suffixes)

        if (length(added) > 0) {
          cli::cli_alert_info("{length(added)} suffix(es) added vs {.val {prev_wave_id}}")
          for (a in head(added, 20)) cli::cli_bullets(c(" " = a))
          if (length(added) > 20) cli::cli_inform("  ... and {length(added) - 20} more")
        }
        if (length(removed) > 0) {
          cli::cli_alert_warning("{length(removed)} suffix(es) removed vs {.val {prev_wave_id}}")
          for (r in head(removed, 20)) cli::cli_bullets(c("!" = r))
          if (length(removed) > 20) cli::cli_inform("  ... and {length(removed) - 20} more")
        }
        if (length(added) == 0 && length(removed) == 0)
          cli::cli_alert_success("suffix sets identical to {.val {prev_wave_id}}")

        report$prev_file <- prev_file
        report$added_suffixes <- added
        report$removed_suffixes <- removed
      }
    }
  } else {
    cli::cli_inform("  no previous wave specified; skipping diff")
  }

  # step 4: candidate wave_index entry
  cli::cli_h2("step 4: candidate wave_index entry")

  # infer year from wave id
  year_digits <- regmatches(new_wave_id, regexpr("\\d{2}", new_wave_id))
  inferred_year <- if (length(year_digits) > 0) {
    yr <- as.integer(year_digits)
    if (yr < 50) 2000 + yr else 1900 + yr
  } else NA

  candidate <- list(
    id = new_wave_id,
    year = inferred_year,
    file_pattern = fname,
    notes = "auto-generated by onboard_new_wave()"
  )

  # inherit extra fields from most recent wave
  if (length(recipe$wave_index) > 0) {
    last_wave <- recipe$wave_index[[length(recipe$wave_index)]]
    extra_fields <- setdiff(names(last_wave), c("id", "year", "file_pattern", "notes"))
    for (ef in extra_fields) {
      candidate[[ef]] <- last_wave[[ef]]
      cli::cli_inform("  inherited {.field {ef}}: {.val {last_wave[[ef]]}}")
    }
  }

  cli::cli_verbatim(yaml::as.yaml(list(candidate)))
  report$candidate_wave_index <- candidate

  # step 5: expected-presence check
  cli::cli_h2("step 5: expected-presence check")
  expected <- recipe$global$expected_presence
  if (!is.null(expected)) {
    critical <- expected$critical %||% list()
    for (ep in critical) {
      var <- ep$variable
      # check if critical variable exists (with or without prefix)
      found <- var %in% new_vars ||
               paste0(new_wave_id, var) %in% new_vars ||
               any(grepl(paste0(var, "$"), new_vars))
      status <- if (found) "\u2713" else "\u2717"
      cli::cli_inform("  {status} critical: {.val {var}}")
      if (!found) {
        report$missing_critical <- c(report$missing_critical %||% character(0), var)
      }
    }
  } else {
    cli::cli_inform("  no expected_presence matrix defined")
  }

  # step 6: boundary alerts
  cli::cli_h2("step 6: potential boundary alerts")

  # check for column type changes
  type_summary <- vapply(new_df, function(x) class(x)[1], character(1))
  unusual_types <- names(type_summary)[
    type_summary %in% c("hms", "difftime", "POSIXct")
  ]
  if (length(unusual_types) > 0) {
    cli::cli_alert_warning("unusual column types detected:")
    for (ut in unusual_types)
      cli::cli_bullets(c("!" = paste0(ut, " (", type_summary[ut], ")")))
    report$unusual_types <- unusual_types
  }

  # check for new sentinel patterns
  numeric_cols <- names(new_df)[vapply(new_df, is.numeric, logical(1))]
  sentinel_candidates <- list()
  known_sentinels <- c(-9, -8, 999, 9999999998, 9999999999)
  for (col in numeric_cols) {
    vals <- unique(new_df[[col]][!is.na(new_df[[col]])])
    found_sentinels <- intersect(vals, known_sentinels)
    if (length(found_sentinels) > 0)
      sentinel_candidates[[col]] <- found_sentinels
  }
  if (length(sentinel_candidates) > 0) {
    cli::cli_alert_info(
      "sentinel values detected in {length(sentinel_candidates)} column(s)"
    )
    report$sentinel_columns <- length(sentinel_candidates)
  }

  # step 7: checklist
  cli::cli_h2("step 7: onboarding checklist")
  checklist <- c(
    "[ ] add candidate wave_index entry to recipe",
    "[ ] verify file_pattern matches actual filename",
    "[ ] check role_map against new variable names",
    "[ ] review added/removed variables for boundary rules",
    "[ ] verify sentinel code regime matches current harmonization rules",
    "[ ] update taxonomy refs if party/education/income codes changed",
    "[ ] run validate_recipe() on updated recipe",
    "[ ] run regression tests to check for regressions",
    "[ ] update covered_waves in meta section"
  )
  for (item in checklist) cli::cli_inform("  {item}")
  report$checklist <- checklist

  cli::cli_alert_success("onboarding report complete")
  invisible(report)
}
