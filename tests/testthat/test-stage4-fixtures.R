# ============================================================================
# stage 4 (v1.4 development): per-module synthetic end-to-end fixtures.
# every bundled recipe merges a generated multi-wave dataset; assertions:
# the merge completes, no rule rolls back (ERROR:) or hits an unimplemented
# action (SKIPPED:), every declared flag column is non-degenerate in the
# OUTPUT, no validation check skips, and no error-severity check fails.
# plus: provenance fields, the overwrite guard, and expected_release pins.
# ============================================================================

# ---- fixture generator ------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

.collect_suffixes <- function(x, acc = character(0)) {
  # harvest plausible 3-digit suffix references from rule/check payloads
  if (is.list(x)) {
    for (k in c("suffixes", "variables", "scope", "items", "stems")) {
      v <- x[[k]]
      if (!is.null(v) && !is.list(v[[1]])) acc <- c(acc, as.character(unlist(v)))
    }
    for (el in x) if (is.list(el)) acc <- .collect_suffixes(el, acc)
  }
  acc
}

.plant_specs <- function(recipe) {
  # value constraints implied by executable checks, so the fixture passes
  # error-severity checks by construction
  allowed <- list(); present <- list(); restricted <- list()
  for (chk in (recipe$validation_checks %||% list())) {
    ty <- paste0(chk$type %||% "", collapse = "")
    cols <- as.character(unlist(chk$suffixes %||% chk$variables %||%
                                  chk$scope %||% chk$variable %||% chk$items %||% list()))
    # include every required item, including ranges and q-prefixed suffixes
    range_types <- c("value_range", "value_in_range", "assert_range", "range_check")
    if (ty %in% range_types) cols <- .expand_rng(cols)
    cols <- sub("^[qQ]([0-9]{3})$", "\\1", cols)
    if (ty %in% c("value_in_set", "value_set", "assert_values")) {
      av <- suppressWarnings(as.numeric(unlist(chk$allowed_values %||%
                                                 chk$allowed %||% list())))
      av <- av[!is.na(av)]
      if (length(av)) for (cc in cols) allowed[[cc]] <- av
    }
    if (ty %in% range_types) {
      lo <- chk$min %||% 0; hi <- chk$max %||% 3
      for (cc in cols) allowed[[cc]] <- unique(pmin(pmax(c(1, 2), lo), hi))
    }
    if (ty %in% c("value_present", "value_present_per_wave")) {
      pv <- suppressWarnings(as.numeric(chk$value))
      if (!is.na(pv)) for (cc in cols) present[[cc]] <- pv
    }
    if (ty == "value_restriction" && !is.null(chk$waves_allowed)) {
      for (cc in cols)
        restricted[[cc]] <- list(values = as.numeric(unlist(chk$value)),
                                waves = as.character(unlist(chk$waves_allowed)))
    }
  }
  list(allowed = allowed, present = present, restricted = restricted)
}

.expand_rng <- function(items) {
  out <- character(0)
  for (it in as.character(unlist(items %||% list()))) {
    if (grepl("^[0-9]+-[0-9]+$", it)) {
      parts <- strsplit(it, "-")[[1]]
      out <- c(out, sprintf(paste0("%0", nchar(parts[1]), "d"),
                            seq(as.integer(parts[1]), as.integer(parts[2]))))
    } else out <- c(out, it)
  }
  out
}

.required_absence_suffixes <- function(recipe) {
  types <- c("value_absence", "assert_absent_values", "none_equal",
             "sentinel_absence", "no_residual_sentinels", "assert_no_values",
             "value_absence_check", "value_restriction")
  collect <- function(check) {
    keys <- c("suffixes", "variables", "scope", "applies_to", "items",
              "stems", "variable", "column")
    targets <- character(0)
    for (key in keys) {
      if (!is.null(check[[key]])) {
        targets <- as.character(unlist(check[[key]]))
        if (key == "items") targets <- .expand_rng(targets)
        break
      }
    }
    for (block in check$targets %||% check$checks %||% list())
      targets <- c(targets, collect(block))
    targets
  }
  checks <- Filter(function(check) check$type %in% types,
                   recipe$validation_checks %||% list())
  targets <- unique(unlist(lapply(checks, collect)))
  # renamed targets need their raw sources, such as cd's rent-period columns
  for (rule in c(recipe$variable_rules, recipe$harmonization_rules)) {
    if (identical(rule$action, "rename")) {
      mapping <- unlist(rule$mapping)
      targets <- c(targets, names(mapping)[mapping %in% targets])
    }
  }
  targets <- sub("^(s|stem_|q|Q)([0-9]{3})$", "\\2", targets)
  unique(targets[grepl("^[0-9]{3}$", targets)])
}

.required_structural_targets <- function(recipe) {
  types <- c("structural_missingness", "structural_absence", "all_na",
             "structural_na_count", "missingness_check")
  checks <- Filter(function(check) check$type %in% types,
                   recipe$validation_checks %||% list())
  targets <- unlist(lapply(checks, function(check) {
    check$suffixes %||% check$variables %||% check$variable %||% check$scope
  }))
  unique(sub("^(s|stem_|q|Q)([0-9]{3})$", "\\2", as.character(targets)))
}

.required_na_rate_suffixes <- function(recipe) {
  types <- c("na_rate", "na_rate_check", "na_rate_above", "na_rate_below",
             "not_missing")
  checks <- Filter(function(check) check$type %in% types,
                   recipe$validation_checks %||% list())
  targets <- unlist(lapply(checks, function(check) {
    for (key in c("suffixes", "variables", "scope", "items")) {
      if (!is.null(check[[key]])) {
        targets <- as.character(unlist(check[[key]]))
        return(if (key == "items") .expand_rng(targets) else targets)
      }
    }
    character(0)
  }))
  targets <- sub("^(s|stem_|q|Q)([0-9]{3})$", "\\2", targets)
  unique(targets[grepl("^[0-9]{3}$", targets)])
}

.absent_specs <- function(recipe) {
  # suffix x wave combinations that checks declare structurally all-NA;
  # the generator must not plant values there
  ab <- list()
  add <- function(sfx, waves) {
    for (s in sfx) ab[[s]] <<- unique(c(ab[[s]], waves))
  }
  for (chk in (recipe$validation_checks %||% list())) {
    ty <- paste0(chk$type %||% "", collapse = "")
    if (ty %in% c("structural_missingness", "structural_absence", "all_na",
                  "structural_na_count", "missingness_check")) {
      sfx <- as.character(unlist(chk$suffixes %||% chk$variables %||%
                                   chk$variable %||% chk$scope %||% list()))
      sfx <- sub("^(s|stem_|q|Q)([0-9]{3})$", "\\2", sfx)
      waves <- as.character(unlist(chk$waves_must_be_all_na %||%
                                     chk$must_be_na_in %||%
                                     chk$expected_na_waves %||%
                                     chk$wave_filter %||% chk$waves %||%
                                     list()))
      present <- as.character(unlist(chk$waves_expected_present %||% list()))
      if (length(present) &&
          identical(chk$expect_elsewhere %||% chk$expect, "all_na"))
        waves <- setdiff(recipe$meta$covered_waves, present)
      if (identical(waves, "all")) waves <- recipe$meta$covered_waves
      if (length(sfx) && length(waves)) add(sfx, waves)
    }
    if (ty %in% c("na_rate", "na_rate_above") &&
        identical(chk$direction %||%
                    (if (identical(ty, "na_rate_above")) "above" else "below"),
                  "above") &&
        (chk$threshold %||% 0) >= 1) {
      sfx <- .expand_rng(chk$items %||% chk$suffixes %||% chk$variables %||%
                           chk$scope)
      waves <- as.character(unlist(chk$waves %||% chk$wave_filter %||% list()))
      if (length(sfx) && length(waves)) add(sfx, waves)
    }
  }
  ab
}

.gen_module_fixture <- function(recipe, data_dir) {
  sfx <- unique(unlist(c(
    lapply(recipe$variable_rules %||% list(), .collect_suffixes),
    lapply(recipe$harmonization_rules %||% list(), .collect_suffixes),
    lapply(recipe$validation_checks %||% list(), .collect_suffixes)
  )))
  sfx <- sfx[grepl("^[0-9]{3}$", sfx)]
  sfx <- utils::head(sort(unique(sfx)), 40)
  plant <- .plant_specs(recipe)
  absent <- .absent_specs(recipe)
  structural_targets <- .required_structural_targets(recipe)
  # planted/constrained suffixes must exist even beyond the cap
  extra <- setdiff(grep("^[0-9]{3}$",
                        c(names(plant$allowed), names(plant$present)),
                        value = TRUE), sfx)
  sfx <- unique(c(sfx, extra, .required_absence_suffixes(recipe),
                 .required_na_rate_suffixes(recipe),
                 structural_targets[grepl("^[0-9]{3}$", structural_targets)]))
  # boundary split_variable sources must exist for era-scoped outputs
  bnd <- unlist(lapply(recipe$boundary_rules %||% list(), function(r) {
    c(r$suffix %||% character(0),
      vapply(r$output_vars %||% list(),
             function(ov) as.character(ov$source_suffix %||% ""),
             character(1)))
  }))
  sfx <- unique(c(sfx, bnd[grepl("^[0-9]{3}$", bnd)]))

  for (w in recipe$wave_index) {
    wid <- w$id
    n <- 3L
    df <- data.frame(nomem_encr = seq_len(n))
    # household ids exist before the recipe's declared structural absence
    if ("nohouse_encr" %in% structural_targets)
      df$nohouse_encr <- if (wid %in% absent$nohouse_encr) rep(NA_real_, n) else
        as.numeric(seq_len(n))
    df[[paste0(wid, "_m")]] <- rep(as.numeric(paste0(w$year, "03")), n)
    for (s in sfx) {
      col <- paste0(wid, s)
      if (wid %in% (absent[[s]] %||% character(0))) {
        df[[col]] <- rep(NA_real_, n)
        next
      }
      pool <- plant$allowed[[s]] %||% c(1, 2, 3)
      restriction <- plant$restricted[[s]]
      if (!is.null(restriction) && !(wid %in% restriction$waves))
        pool <- setdiff(pool, restriction$values)
      vals <- rep_len(pool, n)
      if (!is.null(plant$present[[s]])) vals[1] <- plant$present[[s]]
      df[[col]] <- as.numeric(vals)
    }
    pat <- as.character(w$file_pattern %||% paste0(wid, "_*"))
    fname <- if (grepl("\\*$", pat)) {
      if (grepl("_EN_", pat) || grepl("p\\*$", pat))
        sub("\\*$", ".sav", pat) else sub("\\*$", "EN_1.0p.sav", pat)
    } else paste0(wid, "_EN_1.0p.sav")
    haven::write_sav(df, file.path(data_dir, fname))
  }
  invisible(sfx)
}

.flag_cols_declared <- function(recipe) {
  out <- character(0)
  for (rule in (recipe$boundary_rules %||% list())) {
    act <- rule$action %||% ""
    fc <- if (act %in% c("add_flag", "add_era_flag")) {
      rule$flag_name %||% rule$flag_variable
    } else if (act == "add_period_flag") {
      rule$flag_column %||% paste0(rule$rule_id, "_period")
    } else if (act == "structural_na") {
      rule$flag_column
    } else NULL
    if (!is.null(fc)) out <- c(out, fc)
  }
  unique(out)
}

test_that("bundled uniqueness checks require both keys and detect duplicate rows", {
  types <- c("uniqueness", "assert_unique", "n_duplicates", "unique_key",
             "no_duplicate_ids", "unique_per_wave", "assert_identifier")
  mods <- c("ca", "cd", "cf", "ch", "ci", "cp", "cr", "cs", "cv", "cw")
  checked <- character(0)
  evaluate <- function(data, check) {
    suppressWarnings(suppressMessages(
      lissr:::run_validations(data, list(check), list())))$results[[1]]
  }
  for (mod in mods) {
    recipe <- yaml::yaml.load_file(system.file("recipes",
      paste0(mod, "_merge_recipe.yml"), package = "lissr"))
    checks <- Filter(function(check) check$type %in% types,
                     recipe$validation_checks)
    df <- expand.grid(nomem_encr = 1:2, wave_id = recipe$meta$covered_waves,
                      stringsAsFactors = FALSE)
    for (check in checks) {
      checked <- c(checked, paste(mod, check$check_id, sep = ":"))
      result <- evaluate(df, check)
      expect_true(result$passed)
      expect_identical(result$severity, check$severity)
      expect_identical(result$detail, "duplicates: 0")

      for (column in c("nomem_encr", "wave_id")) {
        missing <- df
        missing[[column]] <- NULL
        result <- evaluate(missing, check)
        expect_identical(result$passed, NA)
        expect_identical(result$severity, check$severity)
        requested <- if (mod == "cv" && column == "wave_id") "wave" else column
        expect_match(result$detail %||% "", requested, fixed = TRUE)
      }

      result <- evaluate(rbind(df, df[1, ]), check)
      expect_false(result$passed)
      expect_identical(result$severity, check$severity)
      expect_identical(result$detail, "duplicates: 2")

      # identifier aliases assert uniqueness, not nonmissing key values
      missing_values <- df
      missing_values$nomem_encr[1] <- NA
      missing_values$wave_id[2] <- ""
      expect_true(evaluate(missing_values, check)$passed)

      if (identical(check$check_id, "CHK11_cd10c_stack_uniqueness")) {
        # scope_wave remains metadata; duplicate rows in other waves still fail
        expect_identical(check$scope_wave, "cd10c")
        outside <- which(df$wave_id != check$scope_wave)[[1]]
        result <- evaluate(rbind(df, df[outside, ]), check)
        expect_false(result$passed)
        expect_identical(result$detail, "duplicates: 2")
      }
    }
  }
  expect_length(checked, 10L)
})

.ci_identifier_results <- function(data, recipe) {
  ids <- c("V-05", "V-05_nonmissing")
  checks <- Filter(function(check) check$check_id %in% ids, recipe$validation_checks)
  results <- suppressWarnings(suppressMessages(
    lissr:::run_validations(data, checks, list())))$results
  stats::setNames(results, vapply(results, function(check) check$check_id, character(1)))
}

test_that("ci identifier checks separate per-wave uniqueness and actual missingness", {
  recipe <- yaml::yaml.load_file(system.file("recipes", "ci_merge_recipe.yml",
                                             package = "lissr"))
  checks <- stats::setNames(recipe$validation_checks,
    vapply(recipe$validation_checks, function(check) check$check_id, character(1)))
  completeness <- checks[["V-05_nonmissing"]]
  expect_identical(checks[["V-05"]]$type, "assert_identifier")
  expect_identical(checks[["V-05"]]$severity, "warning")
  expect_identical(completeness$type, "not_missing")
  expect_identical(completeness$severity, "warning")
  expect_identical(completeness$variables, "nomem_encr")
  expect_equal(completeness$threshold, 0)
  expect_identical(completeness$direction, "below")
  expect_identical(completeness$waves, "all")
  df <- data.frame(nomem_encr = c(1, 2, 1, 2),
                   wave_id = rep(c("ci08a", "ci25r"), each = 2L))
  for (missing in list(NA_real_, NA_integer_, NA_character_, NA, NaN,
                       haven::tagged_na("a"))) {
    data <- df
    data$nomem_encr[[1]] <- missing
    before <- data
    results <- .ci_identifier_results(data, recipe)
    expect_true(results[["V-05"]]$passed)
    expect_identical(results[["V-05_nonmissing"]]$passed, FALSE)
    expect_identical(results[["V-05_nonmissing"]]$severity, "warning")
    expect_match(results[["V-05_nonmissing"]]$detail %||% "", "NA rate 0.25 in nomem_encr",
                 fixed = TRUE)
    expect_identical(data, before)
  }
  cases <- list(
    list(data = df, unique = TRUE, complete = TRUE),
    list(data = transform(df, nomem_encr = c(1, 1, 1, 2)), unique = FALSE, complete = TRUE),
    list(data = transform(df, nomem_encr = c(NA, 2, NA, 2)), unique = TRUE, complete = FALSE),
    list(data = transform(df, nomem_encr = rep(NA_real_, 4)), unique = FALSE, complete = FALSE),
    list(data = data.frame(nomem_encr = c("", " ", "NA", "NaN"), wave_id = "ci08a"),
         unique = TRUE, complete = TRUE))
  for (case in cases) {
    before <- case$data
    results <- .ci_identifier_results(case$data, recipe)
    expect_identical(results[["V-05"]]$passed, case$unique)
    expect_identical(results[["V-05_nonmissing"]]$passed, case$complete)
    expect_identical(case$data, before)
  }
  empty <- .ci_identifier_results(df[FALSE, , drop = FALSE], recipe)
  expect_true(empty[["V-05"]]$passed)
  expect_identical(empty[["V-05_nonmissing"]]$passed, TRUE)
  expect_match(empty[["V-05_nonmissing"]]$detail %||% "", "NA rate not calculated", fixed = TRUE)
  missing_column <- df
  missing_column$nomem_encr <- NULL
  results <- .ci_identifier_results(missing_column, recipe)
  for (id in c("V-05", "V-05_nonmissing")) {
    expect_identical(results[[id]]$passed, NA)
    expect_identical(results[[id]]$severity, "warning")
    expect_match(results[[id]]$detail %||% "", "nomem_encr", fixed = TRUE)
  }
  no_wave <- df
  no_wave$wave_id <- NULL
  results <- .ci_identifier_results(no_wave, recipe)
  expect_identical(results[["V-05"]]$passed, NA)
  expect_identical(results[["V-05_nonmissing"]]$passed, TRUE)
  no_wave$nomem_encr[[1]] <- NA_real_
  expect_identical(.ci_identifier_results(no_wave, recipe)[["V-05_nonmissing"]]$passed, FALSE)
})

test_that("ci missing identifier warnings preserve strict outputs, reports and values", {
  skip_if_not_installed("haven")
  recipe <- yaml::yaml.load_file(system.file("recipes", "ci_merge_recipe.yml",
                                             package = "lissr"))
  for (scenario in c("complete", "single_missing", "missing_wave")) {
    fixture_dir <- withr::local_tempdir("lissr_ci_identifier_")
    data_dir <- file.path(fixture_dir, "data")
    output_dir <- file.path(fixture_dir, "output")
    dir.create(data_dir)
    .gen_module_fixture(recipe, data_dir)
    expected_ids <- rep(as.numeric(1:3), length(recipe$wave_index))
    if (scenario != "complete") {
      source <- file.path(data_dir, "ci08a_EN_1.0p.sav")
      raw <- haven::read_sav(source)
      rows <- if (scenario == "single_missing") 1L else 1:3
      raw$nomem_encr[rows] <- expected_ids[rows] <- NA_real_
      haven::write_sav(raw, source)
    }
    source_hashes <- tools::md5sum(list.files(data_dir, full.names = TRUE))
    for (strict in if (scenario == "single_missing") c(TRUE, FALSE) else TRUE) {
      result <- suppressWarnings(suppressMessages(merge_liss_module(
        recipe, data_dir, output_dir, strict = strict)))
      checks <- stats::setNames(result$validation,
        vapply(result$validation, function(check) check$check_id, character(1)))
      expect_true(result$valid_for_analysis)
      expect_identical(checks[["V-05"]]$passed, scenario != "missing_wave")
      expect_identical(checks[["V-05_nonmissing"]]$passed, scenario == "complete")
      expect_identical(checks[["V-05_nonmissing"]]$severity, "warning")
      report <- readLines(file.path(output_dir, "ci_merge_report.txt"))
      expect_true(any(grepl(paste0("[warning] V-05: ",
        if (scenario == "missing_wave") "FAIL" else "PASS"), report, fixed = TRUE)))
      expect_true(any(grepl(paste0("[warning] V-05_nonmissing: ",
        if (scenario == "complete") "PASS" else "FAIL"), report, fixed = TRUE)))
      expect_true(any(grepl("Valid for analysis: TRUE", report, fixed = TRUE)))
      if (scenario != "complete") {
        detail <- checks[["V-05_nonmissing"]]$detail %||% ""
        rate <- if (scenario == "single_missing") "0.0185" else "0.0556"
        expect_match(detail, paste0("NA rate ", rate, " in nomem_encr"), fixed = TRUE)
        if (nzchar(detail)) expect_true(any(grepl(detail, report, fixed = TRUE)))
      }
      expect_equal(as.numeric(result$data$nomem_encr), expected_ids)
      written <- haven::read_sav(file.path(output_dir, "ci_merged.sav"))
      expect_equal(as.numeric(written$nomem_encr), expected_ids)
      expect_identical(as.character(written$wave_id), as.character(result$data$wave_id))
      expect_identical(tools::md5sum(list.files(data_dir, full.names = TRUE)), source_hashes)
    }
  }
})

test_that("ci absent identifier columns and duplicated source IDs still abort before outputs", {
  skip_if_not_installed("haven")
  recipe <- yaml::yaml.load_file(system.file("recipes", "ci_merge_recipe.yml",
                                             package = "lissr"))
  for (scenario in c("missing_column", "duplicate")) {
    fixture_dir <- withr::local_tempdir("lissr_ci_identifier_guard_")
    data_dir <- file.path(fixture_dir, "data")
    dir.create(data_dir)
    .gen_module_fixture(recipe, data_dir)
    source <- file.path(data_dir, "ci08a_EN_1.0p.sav")
    raw <- haven::read_sav(source)
    if (scenario == "missing_column") raw$nomem_encr <- NULL else
      raw$nomem_encr[[1]] <- raw$nomem_encr[[2]]
    haven::write_sav(raw, source)
    source_hashes <- tools::md5sum(list.files(data_dir, full.names = TRUE))
    pattern <- if (scenario == "missing_column") "expected_presence.*nomem_encr.*ci08a" else
      "ci08a.*duplicated.*nomem_encr"
    for (strict in c(TRUE, FALSE)) {
      output_dir <- file.path(fixture_dir, paste0("output_", strict))
      expect_error(suppressWarnings(suppressMessages(merge_liss_module(
        recipe, data_dir, output_dir, strict = strict))), pattern)
      expect_false(dir.exists(output_dir))
      dir.create(output_dir)
      writeLines("keep this", file.path(output_dir, "existing.txt"))
      expect_error(suppressWarnings(suppressMessages(merge_liss_module(
        recipe, data_dir, output_dir, strict = strict))), pattern)
      expect_identical(list.files(output_dir), "existing.txt")
      expect_identical(readLines(file.path(output_dir, "existing.txt")), "keep this")
      expect_identical(tools::md5sum(list.files(data_dir, full.names = TRUE)), source_hashes)
    }
  }
})

test_that("cv row-count check requires its wave and excludes other observations", {
  recipe <- yaml::yaml.load_file(system.file("recipes", "cv_merge_recipe.yml",
                                             package = "lissr"))
  checks <- Filter(function(check) check$type %in% c("row_count", "assert_row_count_range"),
                   recipe$validation_checks)
  expect_length(checks, 1L)
  check <- checks[[1]]
  expect_identical(check$check_id, "VC08_cv16h_filter_count")
  expect_identical(check$wave, "cv16h")
  expect_equal(check$min_rows, 1)
  df <- data.frame(wave_id = recipe$meta$covered_waves)
  evaluate <- function(data, declaration = check) {
    suppressWarnings(suppressMessages(
      lissr:::run_validations(data, list(declaration), list())))$results[[1]]
  }
  result <- evaluate(df)
  expect_true(result$passed)
  expect_identical(result$severity, "warning")
  expect_identical(result$detail, "rows: 1 in wave cv16h (bounds 1..Inf)")
  expect_identical(evaluate(rbind(df, df))$detail,
                   "rows: 2 in wave cv16h (bounds 1..Inf)")
  for (data in list(df[df$wave_id != check$wave, , drop = FALSE],
                    data.frame(value = 1:3), df[FALSE, , drop = FALSE])) {
    result <- evaluate(data)
    expect_identical(result$passed, NA)
    expect_identical(result$severity, "warning")
    expect_match(result$detail, "cv16h", fixed = TRUE)
  }
  check$min_rows <- 2
  expect_false(evaluate(df, check)$passed)
  outside <- df[df$wave_id != check$wave, , drop = FALSE]
  expect_false(evaluate(rbind(df, outside), check)$passed)
  expect_true(evaluate(rbind(df, df[df$wave_id == check$wave, , drop = FALSE]), check)$passed)
})

test_that("cp wave-count check requires inputs and counts distinct waves per person", {
  recipe <- yaml::yaml.load_file(system.file("recipes", "cp_merge_recipe.yml",
                                             package = "lissr"))
  checks <- Filter(function(check) check$check_id == "V07_wave_count",
                   recipe$validation_checks)
  expect_length(checks, 1L)
  check <- checks[[1]]
  expect_equal(check$max_waves, 17)
  expect_length(recipe$meta$covered_waves, 17L)
  df <- data.frame(wave_id = recipe$meta$covered_waves, nomem_encr = 1)
  evaluate <- function(data) {
    suppressWarnings(suppressMessages(
      lissr:::run_validations(data, list(check), list())))$results[[1]]
  }
  expect_true(evaluate(df)$passed)
  expect_identical(evaluate(df)$detail, "max waves per person: 17")
  expect_true(evaluate(rbind(df, df))$passed)
  extra <- rbind(df, data.frame(wave_id = "extra_wave", nomem_encr = 1))
  result <- evaluate(extra)
  expect_false(result$passed)
  expect_identical(result$severity, "error")
  expect_identical(result$detail, "max waves per person: 18")

  for (column in c("wave_id", "nomem_encr")) {
    missing <- df
    missing[[column]] <- NULL
    result <- evaluate(missing)
    expect_identical(result$passed, NA)
    expect_identical(result$severity, "error")
    expect_match(result$detail %||% "", column, fixed = TRUE)
  }
})

test_that("cs wave-count check compares all covered waves without a person key", {
  recipe <- yaml::yaml.load_file(system.file("recipes", "cs_merge_recipe.yml",
                                             package = "lissr"))
  checks <- Filter(function(check) check$check_id == "V07_wave_count",
                   recipe$validation_checks)
  expect_length(checks, 1L)
  check <- checks[[1]]
  expect_equal(check$expected, 18)
  expect_length(recipe$meta$covered_waves, 18L)
  expect_match(check$description, "exactly 18 distinct wave_ids", fixed = TRUE)
  df <- data.frame(wave_id = recipe$meta$covered_waves)
  evaluate <- function(data) {
    suppressWarnings(suppressMessages(
      lissr:::run_validations(data, list(check), list())))$results[[1]]
  }
  expect_true(evaluate(df)$passed)
  expect_identical(evaluate(df)$detail, "distinct waves: 18 (expected 18)")
  expect_true(evaluate(rbind(df, df))$passed)
  for (data in list(df[-1, , drop = FALSE],
                   rbind(df, data.frame(wave_id = "extra_wave")))) {
    result <- evaluate(data)
    expect_false(result$passed)
    expect_identical(result$severity, "error")
  }
  result <- evaluate(data.frame(nomem_encr = 1))
  expect_identical(result$passed, NA)
  expect_identical(result$severity, "error")
  expect_match(result$detail %||% "", "wave_id", fixed = TRUE)
})

test_that("cp mean-spike check requires every item and tests every observed wave", {
  recipe <- yaml::yaml.load_file(system.file("recipes", "cp_merge_recipe.yml",
                                             package = "lissr"))
  checks <- Filter(function(check) check$check_id == "V03_dk_mean_spike",
                   recipe$validation_checks)
  expect_length(checks, 1L)
  check <- checks[[1]]
  targets <- as.character(unlist(check$items))
  columns <- paste0("s", targets)
  df <- data.frame(wave_id = recipe$meta$covered_waves)
  for (column in columns) df[[column]] <- 1
  evaluate <- function(data) {
    suppressWarnings(suppressMessages(
      lissr:::run_validations(data, list(check), list())))$results[[1]]
  }
  expect_true(evaluate(df)$passed)

  for (i in seq_along(columns)) {
    missing <- df
    missing[[columns[[i]]]] <- NULL
    result <- evaluate(missing)
    expect_identical(result$passed, NA)
    expect_identical(result$severity, "error")
    expect_match(result$detail %||% "", targets[[i]], fixed = TRUE)

    contaminated <- df
    contaminated[[columns[[i]]]][i] <- 999
    result <- evaluate(contaminated)
    expect_false(result$passed)
    expect_match(result$detail, columns[[i]], fixed = TRUE)
    expect_match(result$detail, df$wave_id[[i]], fixed = TRUE)
  }

  # an undefined earlier mean must not conceal a later finite violation
  partial <- df
  partial[[columns[[1]]]] <- NA_real_
  expect_true(evaluate(partial)$passed)
  partial[[utils::tail(columns, 1)]][nrow(df)] <- 999
  result <- evaluate(partial)
  expect_false(result$passed)
  expect_match(result$detail, utils::tail(columns, 1), fixed = TRUE)
  expect_match(result$detail, utils::tail(df$wave_id, 1), fixed = TRUE)

  for (column in columns) df[[column]] <- NA_real_
  result <- evaluate(df)
  expect_true(result$passed)
  expect_match(result$detail %||% "", "finite", fixed = TRUE)
})

test_that("cp all-NA rate checks require every item and honor declared waves", {
  recipe <- yaml::yaml.load_file(system.file("recipes", "cp_merge_recipe.yml",
                                             package = "lissr"))
  ids <- c("V04_structural_na_post_cp19k", "V05_lotr_structural_na_pre_cp12e")
  checks <- Filter(function(check) check$check_id %in% ids,
                   recipe$validation_checks)
  expect_length(checks, 2L)
  evaluate <- function(data, check) {
    suppressWarnings(suppressMessages(
      lissr:::run_validations(data, list(check), list())))$results[[1]]
  }
  for (check in checks) {
    targets <- .expand_rng(check$items)
    columns <- paste0("s", targets)
    df <- data.frame(wave_id = recipe$meta$covered_waves)
    inside <- df$wave_id %in% check$waves
    for (column in columns) df[[column]] <- ifelse(inside, NA_real_, 1)
    expect_true(evaluate(df, check)$passed)

    missing <- df
    missing[[columns[[2]]]] <- NULL
    result <- evaluate(missing, check)
    expect_identical(result$passed, NA)
    expect_match(result$detail, targets[[2]], fixed = TRUE)

    contaminated <- df
    contaminated[[columns[[1]]]][which(inside)[[1]]] <- 1
    result <- evaluate(contaminated, check)
    expect_false(result$passed)
    expect_match(result$detail, columns[[1]], fixed = TRUE)

    missing_wave <- as.character(check$waves[[1]])
    result <- evaluate(df[df$wave_id != missing_wave, , drop = FALSE], check)
    expect_identical(result$passed, NA)
    expect_match(result$detail, missing_wave, fixed = TRUE)
  }
})

test_that("cr sentinel checks match recode scopes and detect residual values", {
  recipe <- yaml::yaml.load_file(system.file("recipes", "cr_merge_recipe.yml",
                                             package = "lissr"))
  rules <- stats::setNames(recipe$harmonization_rules,
    vapply(recipe$harmonization_rules, function(rule) rule$rule_id, character(1)))
  checks <- stats::setNames(recipe$validation_checks,
    vapply(recipe$validation_checks, function(check) check$check_id, character(1)))
  blocks <- c(checks$VC04$targets, list(checks$VC05))
  rule_ids <- c("HR01", "HR02", "HR03")
  for (i in seq_along(rule_ids)) {
    rule <- rules[[rule_ids[[i]]]]
    expect_identical(blocks[[i]]$suffixes, rule$suffixes)
    expect_identical(blocks[[i]]$waves, rule$waves)
    expect_identical(blocks[[i]]$sentinel_values, rule$codes)
  }

  df <- data.frame(wave_id = recipe$meta$covered_waves)
  for (suffix in unique(unlist(lapply(blocks, function(block) block$suffixes))))
    df[[paste0("s", suffix)]] <- 1
  df$s120 <- rep_len(c(99, 999), nrow(df))
  df$fieldwork_ym <- 999
  df$wave_year <- 99
  evaluate <- function(data, check) {
    suppressWarnings(suppressMessages(
      lissr:::run_validations(data, list(check), list())))$results[[1]]
  }
  for (check in checks[c("VC04", "VC05")])
    expect_true(evaluate(df, check)$passed)

  for (i in seq_along(rule_ids)) {
    rule <- rules[[rule_ids[[i]]]]
    check <- checks[[if (i <= 2L) "VC04" else "VC05"]]
    col <- paste0("s", rule$suffixes[[1]])
    inside <- match(rule$waves[[1]], df$wave_id)
    outside <- which(!(df$wave_id %in% rule$waves))[[1]]
    planted <- df
    planted[[col]][outside] <- rule$codes[[1]]
    expect_true(evaluate(planted, check)$passed)
    planted[[col]][inside] <- rule$codes[[1]]
    result <- evaluate(planted, check)
    expect_false(result$passed)
    expect_match(result$detail, paste0("forbidden value(s) in ", col), fixed = TRUE)
  }

  # the positive codes belong to distinct target sets, not their cross-product
  df$s003[df$wave_id == "cr08a"] <- 99
  df$s002[df$wave_id == "cr08a"] <- 999
  expect_true(evaluate(df, checks$VC04)$passed)
})

test_that("ch gender restriction checks the declared column and allowed waves", {
  recipe <- yaml::yaml.load_file(system.file("recipes", "ch_merge_recipe.yml",
                                             package = "lissr"))
  check <- Filter(function(check) check$check_id == "CHK05",
                  recipe$validation_checks)[[1]]
  expect_identical(check[["variable"]], "001")
  df <- data.frame(wave_id = recipe$meta$covered_waves, s001 = 1)
  df$s001[df$wave_id %in% check$waves_allowed] <- 3
  evaluate <- function(data) {
    suppressWarnings(suppressMessages(
      lissr:::run_validations(data, list(check), list())))$results[[1]]
  }
  expect_true(evaluate(df)$passed)
  df$s001[which(!(df$wave_id %in% check$waves_allowed))[[1]]] <- 3
  result <- evaluate(df)
  expect_false(result$passed)
  expect_match(result$detail, "forbidden value(s) in s001", fixed = TRUE)
})

test_that("ch structural checks match split outputs without requiring presence", {
  recipe <- yaml::yaml.load_file(system.file("recipes", "ch_merge_recipe.yml",
                                             package = "lissr"))
  rules <- stats::setNames(recipe$boundary_rules,
    vapply(recipe$boundary_rules, function(rule) rule$rule_id, character(1)))
  checks <- stats::setNames(recipe$validation_checks,
    vapply(recipe$validation_checks, function(check) check$check_id, character(1)))
  for (pair in list(c("CHK04", "B04"), c("CHK07", "B01"))) {
    check <- checks[[pair[[1]]]]
    output <- Filter(function(output) output$name == check[["variable"]],
                     rules[[pair[[2]]]]$output_vars)[[1]]
    expect_identical(check$waves_must_be_all_na,
                     setdiff(recipe$meta$covered_waves, output$waves))
    expect_null(check[["waves_expected_present"]])
    df <- data.frame(wave_id = recipe$meta$covered_waves)
    df[[output$name]] <- NA_real_
    evaluate <- function(data) {
      suppressWarnings(suppressMessages(
        lissr:::run_validations(data, list(check), list())))$results[[1]]
    }
    expect_true(evaluate(df)$passed)
    df[[output$name]][df$wave_id %in% output$waves] <- 1
    expect_true(evaluate(df)$passed)
    df[[output$name]][match(check$waves_must_be_all_na[[1]], df$wave_id)] <- 1
    result <- evaluate(df)
    expect_false(result$passed)
    expect_match(result$detail, output$name, fixed = TRUE)
  }
})

test_that("cs structural scope preserves the zero-padded target", {
  recipe <- yaml::yaml.load_file(system.file("recipes", "cs_merge_recipe.yml",
                                             package = "lissr"))
  check <- Filter(function(check) check$check_id == "V08_stem002_absent_post_cs19l",
                  recipe$validation_checks)[[1]]
  expect_identical(check$scope, "002")
  df <- data.frame(wave_id = recipe$meta$covered_waves, s002 = NA_real_)
  df$s002[!(df$wave_id %in% check$wave_filter)] <- 1
  evaluate <- function(data) {
    suppressWarnings(suppressMessages(
      lissr:::run_validations(data, list(check), list())))$results[[1]]
  }
  expect_true(evaluate(df)$passed)
  df$s002[match(check$wave_filter[[1]], df$wave_id)] <- 1
  result <- evaluate(df)
  expect_false(result$passed)
  expect_match(result$detail, "s002", fixed = TRUE)
})

test_that("cs value presence checks its zero-padded target in every wave", {
  recipe <- yaml::yaml.load_file(system.file("recipes", "cs_merge_recipe.yml",
                                             package = "lissr"))
  check <- Filter(function(check) check$check_id == "V02_dk_code_present_all_waves",
                  recipe$validation_checks)[[1]]
  expect_identical(check$scope, "001")
  df <- data.frame(wave_id = rep(recipe$meta$covered_waves, each = 2L),
                   s001 = rep(c(-9, NA_real_), length(recipe$meta$covered_waves)),
                   s002 = -9, s1 = -9)
  evaluate <- function(data) {
    suppressWarnings(suppressMessages(
      lissr:::run_validations(data, list(check), list())))$results[[1]]
  }
  expect_true(evaluate(df)$passed)
  missing_wave <- recipe$meta$covered_waves[[2]]
  df$s001[df$wave_id == missing_wave] <- 1
  result <- evaluate(df)
  expect_false(result$passed)
  expect_type(result$detail, "character")
  if (is.character(result$detail))
    expect_match(result$detail, missing_wave, fixed = TRUE)
  df$s001 <- NA_real_
  expect_false(evaluate(df)$passed)
})

.cw_range_checks <- function(recipe) {
  checks <- stats::setNames(recipe$validation_checks,
    vapply(recipe$validation_checks, function(check) check$check_id, character(1)))
  checks[c("V02_wage_numeric", "V07_pension_dates_numeric")]
}

test_that("cw range declarations enforce their inclusive bounds on every target", {
  recipe <- yaml::yaml.load_file(system.file("recipes", "cw_merge_recipe.yml",
                                             package = "lissr"))
  checks <- .cw_range_checks(recipe)
  targets <- list("q323", sprintf("q%03d", seq(149, 185, 4)))
  bounds <- list(c(0, 1000000), c(1990, 2030))
  evaluate <- function(data, check) suppressWarnings(suppressMessages(
    lissr:::run_validations(data, list(check), list())))$results[[1]]
  for (i in seq_along(checks)) {
    check <- checks[[i]]
    lo <- bounds[[i]][[1]]; hi <- bounds[[i]][[2]]
    expect_equal(check[["min"]], lo)
    expect_equal(check[["max"]], hi)
    expect_null(check[["valid_range"]])
    expect_identical(check$severity, c("error", "warning")[[i]])
    expect_identical(check$variables %||% check$variable, targets[[i]])
    expect_true(check$allow_na)
    columns <- sub("^q", "s", targets[[i]])
    df <- data.frame(wave_id = rep(c("cw11d", "cw25r"), each = 2L))
    for (column in columns) df[[column]] <- c(lo, hi, NA_real_, NaN)
    expect_true(evaluate(df, check)$passed)
    expect_true(evaluate(df[FALSE, , drop = FALSE], check)$passed)
    missing <- df
    for (column in columns) missing[[column]] <- NA_real_
    expect_true(evaluate(missing, check)$passed)
    before <- df
    for (column in columns) {
      for (value in c(lo - 1, hi + 1, -Inf, Inf)) {
        bad <- df
        bad[[column]][[1]] <- value
        result <- evaluate(bad, check)
        expect_false(result$passed)
        expect_identical(result$severity, check$severity)
        expect_match(result$detail %||% "", paste0("1 out-of-range value(s) in ", column),
                     fixed = TRUE)
      }
    }
    expect_identical(df, before)
  }
  expect_null(checks[[1]][["allow_sentinel"]])
  expect_null(checks[[1]][["sentinel_values"]])
  absence <- Filter(function(check) check$check_id == "V08_no_sentinels",
                     recipe$validation_checks)[[1]]
  for (value in c(-9, -8)) {
    df <- data.frame(s323 = value)
    expect_false(evaluate(df, checks[[1]])$passed)
    expect_false(evaluate(df, absence)$passed)
  }
})

test_that("cw range validation follows existing wage and pension recodes", {
  recipe <- yaml::yaml.load_file(system.file("recipes", "cw_merge_recipe.yml",
                                             package = "lissr"))
  checks <- .cw_range_checks(recipe)
  rules <- stats::setNames(c(recipe$variable_rules, recipe$harmonization_rules),
    vapply(c(recipe$variable_rules, recipe$harmonization_rules),
           function(rule) rule$rule_id, character(1)))
  columns <- sprintf("s%03d", seq(149, 185, 4))
  wage <- c(-9, -8, 0, 1000000, 1000001)
  for (values in list(wage, as.character(wage),
                      haven::labelled(wage, c(dk = -9, refusal = -8)))) {
    for (wave in c("cw24q", "cw25r")) {
      pension <- if (wave == "cw25r") c(1, 2, -9, -8, 5) else
        c(1990, 2030, -9, -8, 1989)
      df <- data.frame(s323 = values, s359 = values, s362 = values)
      for (column in columns) df[[column]] <- pension
      before <- df
      output <- suppressWarnings(suppressMessages(lissr:::exec_variable_rule(
        df, rules$VR03_q323_force_numeric, wave, list(), recipe$meta$covered_waves, list())))
      expect_equal(output$df$s323, wage)
      expect_type(output$df$s323, "double")
      output <- suppressWarnings(suppressMessages(lissr:::exec_harmonization_rule(
        output$df, rules$HR01_sentinel_recode, wave, list(),
        recipe$meta$covered_waves, output$log)))
      output <- suppressWarnings(suppressMessages(lissr:::exec_harmonization_rule(
        output$df, rules$HR03_pension_dates, wave, list(),
        recipe$meta$covered_waves, output$log)))
      expect_equal(output$df$s323, c(NA, NA, 0, 1000000, 1000001))
      expected <- if (wave == "cw25r") c(2023, 2024, NA, NA, 5) else
        c(1990, 2030, NA, NA, 1989)
      for (column in columns) expect_equal(output$df[[column]], expected)
      sentinel_log <- Filter(function(entry) entry$rule_id == "HR01_sentinel_recode",
                              output$log)
      expect_equal(sentinel_log[[1]]$values_changed, 26)
      pension_log <- Filter(function(entry) entry$rule_id == "HR03_pension_dates",
                             output$log)
      expect_length(pension_log, if (wave == "cw25r") 10L else 0L)
      if (length(pension_log))
        expect_true(all(vapply(pension_log, function(entry) entry$values_changed == 2,
                               logical(1))))
      output$df$wave_id <- wave
      for (check in checks) {
        result <- suppressWarnings(suppressMessages(lissr:::run_validations(
          output$df, list(check), list())))$results[[1]]
        expect_false(result$passed)
        result <- suppressWarnings(suppressMessages(lissr:::run_validations(
          output$df[1:4, , drop = FALSE], list(check), list())))$results[[1]]
        expect_true(result$passed)
      }
      expect_identical(df, before)
    }
  }
})

test_that("cw range outcomes preserve strict outputs, reports and harmonized values", {
  skip_if_not_installed("haven")
  recipe <- yaml::yaml.load_file(system.file("recipes", "cw_merge_recipe.yml",
                                             package = "lissr"))
  pension_columns <- sprintf("s%03d", seq(149, 185, 4))
  for (scenario in c("valid", "wage", "pension")) {
    fixture_dir <- withr::local_tempdir("lissr_cw_range_")
    data_dir <- file.path(fixture_dir, "data")
    output_dir <- file.path(fixture_dir, "output")
    dir.create(data_dir)
    .gen_module_fixture(recipe, data_dir)
    source <- file.path(data_dir, "cw25r_EN_1.0p.sav")
    raw <- haven::read_sav(source)
    raw$cw25r323 <- c(-9, -8, if (scenario == "wage") 1000001 else 1000000)
    for (column in pension_columns)
      raw[[sub("^s", "cw25r", column)]] <- c(1, 2, 1990)
    if (scenario == "pension") raw$cw25r185[[3]] <- 2031
    haven::write_sav(raw, source)
    source_hashes <- tools::md5sum(list.files(data_dir, full.names = TRUE))
    if (scenario == "wage") {
      dir.create(output_dir)
      writeLines("keep this", file.path(output_dir, "existing.txt"))
      expect_error(suppressWarnings(suppressMessages(merge_liss_module(
        recipe, data_dir, output_dir, strict = TRUE))), "strict mode: no outputs were written")
      expect_identical(list.files(output_dir), "existing.txt")
      expect_identical(readLines(file.path(output_dir, "existing.txt")), "keep this")
    }
    for (strict in if (scenario == "pension") c(TRUE, FALSE) else scenario == "valid") {
      result <- suppressWarnings(suppressMessages(merge_liss_module(
        recipe, data_dir, output_dir, strict = strict)))
      expect_identical(result$valid_for_analysis, scenario != "wage")
      checks <- stats::setNames(result$validation,
        vapply(result$validation, function(check) check$check_id, character(1)))
      expect_identical(checks$V02_wage_numeric$passed, scenario != "wage")
      expect_identical(checks$V07_pension_dates_numeric$passed, scenario != "pension")
      expect_true(checks$V08_no_sentinels$passed)
      report <- readLines(file.path(output_dir, "cw_merge_report.txt"))
      expect_true(any(grepl(paste0("[error] V02_wage_numeric: ",
        if (scenario == "wage") "FAIL" else "PASS"), report, fixed = TRUE)))
      expect_true(any(grepl(paste0("[warning] V07_pension_dates_numeric: ",
        if (scenario == "pension") "FAIL" else "PASS"), report, fixed = TRUE)))
      if (scenario != "valid") {
        check <- checks[[if (scenario == "wage") "V02_wage_numeric" else
          "V07_pension_dates_numeric"]]
        expect_match(check$detail %||% "", if (scenario == "wage") "s323" else "s185")
        if (!is.null(check$detail))
          expect_true(any(grepl(check$detail, report, fixed = TRUE)))
      }
      written <- haven::read_sav(file.path(output_dir, "cw_merged.sav"))
      for (data in list(result$data, written)) {
        latest <- data$wave_id == "cw25r"
        expect_equal(as.numeric(data$s323[latest]),
                     c(NA, NA, if (scenario == "wage") 1000001 else 1000000))
        for (column in pension_columns) {
          expected <- c(2023, 2024, if (scenario == "pension" && column == "s185") 2031 else 1990)
          expect_equal(as.numeric(data[[column]][latest]), expected)
          if (scenario == "valid")
            expect_true(all(is.na(data[[column]]) |
              (data[[column]] >= 1990 & data[[column]] <= 2030)))
        }
        if (scenario == "valid")
          expect_true(all(is.na(data$s323) | (data$s323 >= 0 & data$s323 <= 1000000)))
      }
      expect_equal(as.data.frame(written[c("s323", pension_columns)]),
                   as.data.frame(result$data[c("s323", pension_columns)]),
                   ignore_attr = TRUE)
      expect_identical(tools::md5sum(list.files(data_dir, full.names = TRUE)), source_hashes)
    }
  }
})

test_that("cv VC01 outcomes preserve strict outputs, reports and serialized carve-outs", {
  skip_if_not_installed("haven")
  recipe_path <- system.file("recipes", "cv_merge_recipe.yml", package = "lissr",
                             mustWork = TRUE)
  recipe <- yaml::yaml.load_file(recipe_path)
  for (scenario in c("carveouts", "retained")) {
    fixture_dir <- withr::local_tempdir("lissr_cv_vc01_")
    data_dir <- file.path(fixture_dir, "data")
    output_dir <- file.path(fixture_dir, "output")
    dir.create(data_dir)
    .gen_module_fixture(recipe, data_dir)
    edit <- function(wave, fun) {
      path <- file.path(data_dir, paste0(wave, "_EN_1.0p.sav"))
      haven::write_sav(fun(haven::read_sav(path)), path)
    }
    edit("cv17i", function(raw) { raw$Total <- c(99, 1, 2); raw })
    edit("cv19k", function(raw) { raw$cv19k243 <- c(999, 1, 2); raw })
    edit("cv20l", function(raw) {
      raw$cv20l243 <- c(-9, -9, 1); raw$cv20l245 <- c(0, 99, 100)
      if (scenario == "retained") raw$cv20l001 <- c(1, 99, 3)
      raw
    })
    source_hashes <- tools::md5sum(list.files(data_dir, full.names = TRUE))
    dir.create(output_dir)
    writeLines("keep this", file.path(output_dir, "existing.txt"))
    # strict mode as an assertion, not an uncaught abort, so the report-mode
    # comparisons below still execute when the declaration is defective
    strict_run <- tryCatch(suppressWarnings(suppressMessages(merge_liss_module(
      recipe_path, data_dir, output_dir, strict = TRUE))), error = function(e) e)
    if (scenario == "carveouts") {
      expect_false(inherits(strict_run, "error"),
                   info = if (inherits(strict_run, "error")) conditionMessage(strict_run))
      if (!inherits(strict_run, "error")) {
        expect_true(strict_run$valid_for_analysis)
        expect_true(file.exists(file.path(output_dir, "cv_merged.sav")))
      }
    } else {
      expect_s3_class(strict_run, "error")
      expect_match(conditionMessage(strict_run), "strict mode: no outputs were written")
      expect_identical(list.files(output_dir), "existing.txt")
    }
    result <- suppressWarnings(suppressMessages(merge_liss_module(
      recipe_path, data_dir, output_dir, strict = FALSE)))
    expect_identical(result$valid_for_analysis, scenario == "carveouts")
    checks <- stats::setNames(result$validation,
      vapply(result$validation, function(check) check$check_id, character(1)))
    expect_identical(checks$VC01_no_raw_dk$passed, scenario == "carveouts")
    report <- readLines(file.path(output_dir, "cv_merge_report.txt"))
    expect_true(any(grepl(paste0("[error] VC01_no_raw_dk: ",
      if (scenario == "carveouts") "PASS" else "FAIL"), report, fixed = TRUE)))
    expect_true(any(grepl(paste0("Valid for analysis: ", scenario == "carveouts"),
                          report, fixed = TRUE)))
    if (scenario == "retained") {
      expect_match(checks$VC01_no_raw_dk$detail %||% "",
                   "1 forbidden value(s) in s001", fixed = TRUE)
      expect_true(any(grepl(checks$VC01_no_raw_dk$detail %||% "", report, fixed = TRUE)))
    }
    drops <- Filter(function(entry) identical(entry$rule_id, "DR02_drop_total"), result$log)
    expect_length(drops, 1L)
    expect_identical(drops[[1]]$variable, "cv17i_Total")
    written <- haven::read_sav(file.path(output_dir, "cv_merged.sav"), user_na = TRUE)
    for (data in list(result$data, written)) {
      expect_false(any(c("Total", "cv17i_Total") %in% names(data)))
      w20 <- data$wave_id == "cv20l"; w19 <- data$wave_id == "cv19k"
      expect_equal(as.numeric(data$s245[w20]), c(0, 99, 100))
      expect_equal(as.numeric(data$s243[w20]), c(-9, -9, 1))
      expect_equal(as.numeric(data$s243[w19]), c(999, 1, 2))
      if (scenario == "retained") expect_equal(as.numeric(data$s001[w20]), c(1, 99, 3))
    }
    expect_equal(as.data.frame(written[c("s243", "s245")]),
                 as.data.frame(result$data[c("s243", "s245")]), ignore_attr = TRUE)
    expect_identical(tools::md5sum(list.files(data_dir, full.names = TRUE)), source_hashes)
  }
})

test_that("every bundled recipe merges a synthetic panel end to end", {
  skip_if_not_installed("haven")
  mods <- c("ca", "cd", "cf", "ch", "ci", "cp", "cr", "cs", "cv", "cw")
  problems <- character(0)

  for (mod in mods) {
    recipe_path <- system.file("recipes", paste0(mod, "_merge_recipe.yml"),
                               package = "lissr")
    recipe <- yaml::yaml.load_file(recipe_path)
    data_dir <- file.path(tempdir(), paste0("lissr_fix_", mod))
    out_dir  <- file.path(tempdir(), paste0("lissr_fixo_", mod))
    unlink(c(data_dir, out_dir), recursive = TRUE)
    dir.create(data_dir, recursive = TRUE)
    dir.create(out_dir, recursive = TRUE)

    .gen_module_fixture(recipe, data_dir)
    res <- tryCatch(
      suppressWarnings(suppressMessages(
        merge_liss_module(recipe_path, data_dir, out_dir))),
      error = function(e) e)

    if (inherits(res, "error")) {
      problems <- c(problems, paste0(mod, ": merge errored: ",
                                     conditionMessage(res)))
      next
    }

    acts <- vapply(res$log, function(e) as.character(e$action), character(1))
    if (any(grepl("^ERROR:", acts)))
      problems <- c(problems, paste0(mod, ": rolled-back rule(s): ",
        paste(unique(vapply(res$log[grepl("^ERROR:", acts)],
                            function(e) as.character(e$rule_id),
                            character(1))), collapse = ", ")))
    if (any(grepl("^SKIPPED:", acts)))
      problems <- c(problems, paste0(mod, ": unimplemented action(s) hit: ",
        paste(unique(acts[grepl("^SKIPPED:", acts)]), collapse = ", ")))

    for (fc in .flag_cols_declared(recipe)) {
      if (!(fc %in% names(res$data)) || all(is.na(res$data[[fc]])))
        problems <- c(problems, paste0(mod, ": degenerate flag in output: ", fc))
    }

    n_skip <- sum(vapply(res$validation, function(r)
      !isTRUE(r$passed) && !isFALSE(r$passed) && !isTRUE(r$documentary),
      logical(1)))
    if (n_skip > 0)
      problems <- c(problems, paste0(mod, ": ", n_skip, " skipped check(s)"))

    err_fails <- vapply(res$validation, function(r)
      identical(r$severity, "error") && isFALSE(r$passed), logical(1))
    if (any(err_fails))
      problems <- c(problems, paste0(mod, ": error-severity FAIL: ",
        paste(vapply(res$validation[err_fails],
                     function(r) r$check_id, character(1)), collapse = ", ")))

    structural_types <- c("structural_missingness", "structural_absence", "all_na",
                          "structural_na_count", "missingness_check")
    structural_ids <- vapply(Filter(function(check) check$type %in% structural_types,
                                    recipe$validation_checks),
                              function(check) check$check_id, character(1))
    structural_fails <- vapply(res$validation, function(check)
      check$check_id %in% structural_ids && !isTRUE(check$passed), logical(1))
    if (any(structural_fails))
      problems <- c(problems, paste0(mod, ": unresolved/failed structural checks: ",
        paste(vapply(res$validation[structural_fails], function(check) check$check_id,
                     character(1)), collapse = ", ")))

    presence_types <- c("value_present", "value_present_per_wave")
    presence_ids <- vapply(Filter(function(check) check$type %in% presence_types,
                                  recipe$validation_checks),
                            function(check) check$check_id, character(1))
    presence_fails <- vapply(res$validation, function(check)
      check$check_id %in% presence_ids && !isTRUE(check$passed), logical(1))
    if (any(presence_fails))
      problems <- c(problems, paste0(mod, ": unresolved/failed value presence checks: ",
        paste(vapply(res$validation[presence_fails], function(check) check$check_id,
                     character(1)), collapse = ", ")))

    unlink(c(data_dir, out_dir), recursive = TRUE)
  }

  expect_identical(problems, character(0))
})

# ---- provenance, valid_for_analysis, overwrite, release pins ----------------

.mini_fixture <- function(pin = NULL, extra_file = FALSE) {
  data_dir <- file.path(tempdir(), "lissr_s4_data")
  out_dir  <- file.path(tempdir(), "lissr_s4_out")
  unlink(c(data_dir, out_dir), recursive = TRUE)
  dir.create(data_dir, recursive = TRUE)
  dir.create(out_dir, recursive = TRUE)
  haven::write_sav(data.frame(nomem_encr = 1:2, yy01a005 = c(1, 2)),
                   file.path(data_dir, "yy01a_EN_1.0p.sav"))
  if (extra_file)
    haven::write_sav(data.frame(nomem_encr = 1:2, yy01a005 = c(1, 2)),
                     file.path(data_dir, "yy01a_EN_1.1p.sav"))
  wave <- list(id = "yy01a", year = 2001, file_pattern = "yy01a_*")
  if (!is.null(pin)) wave$expected_release <- pin
  recipe <- list(
    meta = list(module = "yy", module_label = "F", schema_version = "1.0.0",
                recipe_version = "9.9.9", created = "t", source_spec = "t",
                covered_waves = list("yy01a")),
    global = list(id_variable = "nomem_encr", wave_variable = "wave_id",
                  year_variable = "wave_year", labelled_policy = "to_numeric",
                  missing_variable_policy = "warn_and_create_na",
                  strip_label_whitespace = TRUE),
    wave_index = list(wave),
    logging = list(summary_artifact = FALSE))
  list(recipe = recipe, data_dir = data_dir, out_dir = out_dir)
}

test_that("provenance and valid_for_analysis are attached to the result", {
  skip_if_not_installed("haven")
  fx <- .mini_fixture()
  on.exit(unlink(c(fx$data_dir, fx$out_dir), recursive = TRUE), add = TRUE)
  res <- suppressWarnings(suppressMessages(
    merge_liss_module(fx$recipe, fx$data_dir, fx$out_dir)))
  expect_true(res$valid_for_analysis)
  expect_identical(res$provenance$recipe_version, "9.9.9")
  expect_identical(res$provenance$package_version,
                   as.character(utils::packageVersion("lissr")))
  expect_identical(res$provenance$inputs$file, "yy01a_EN_1.0p.sav")
  expect_match(res$provenance$inputs$md5, "^[a-f0-9]{32}$")
  rpt <- readLines(file.path(fx$out_dir, "yy_merge_report.txt"))
  expect_true(any(grepl("Valid for analysis: TRUE", rpt)))
  expect_true(any(grepl("md5", rpt)))
})

test_that("the overwrite guard refuses to clobber an existing output", {
  skip_if_not_installed("haven")
  fx <- .mini_fixture()
  on.exit(unlink(c(fx$data_dir, fx$out_dir), recursive = TRUE), add = TRUE)
  suppressWarnings(suppressMessages(
    merge_liss_module(fx$recipe, fx$data_dir, fx$out_dir)))
  expect_error(
    suppressWarnings(suppressMessages(
      merge_liss_module(fx$recipe, fx$data_dir, fx$out_dir,
                        overwrite = FALSE))),
    "already exists")
})

test_that("expected_release pins report, invalidate, and abort in mixed folders", {
  skip_if_not_installed("haven")
  # release ranking keeps 1.1p; a 1.0p pin is then violated
  fx <- .mini_fixture(pin = "1.0p", extra_file = TRUE)
  file.create(file.path(fx$data_dir, "yy01a_codebook_9.0_EN.pdf"))
  on.exit(unlink(c(fx$data_dir, fx$out_dir), recursive = TRUE), add = TRUE)
  res <- suppressWarnings(suppressMessages(
    merge_liss_module(fx$recipe, fx$data_dir, fx$out_dir)))
  expect_false(res$valid_for_analysis)
  expect_identical(res$provenance$release_violations, "yy01a")
  # the multi-file ranking decision is recorded
  expect_length(res$provenance$release_decisions, 1)
  expect_identical(res$provenance$release_decisions[[1]]$selected,
                   "yy01a_EN_1.1p.sav")
  expect_error(
    suppressWarnings(suppressMessages(
      merge_liss_module(fx$recipe, fx$data_dir, fx$out_dir, strict = TRUE))),
    "expected_release")

  # a matching pin stays valid
  fx2 <- .mini_fixture(pin = "1.0p")
  file.create(file.path(fx2$data_dir, "yy01a_codebook_9.0_EN.pdf"))
  on.exit(unlink(c(fx2$data_dir, fx2$out_dir), recursive = TRUE), add = TRUE)
  res2 <- suppressWarnings(suppressMessages(
    merge_liss_module(fx2$recipe, fx2$data_dir, fx2$out_dir)))
  expect_true(res2$valid_for_analysis)
})

# ---- stage 5b: output-changing recipe semantics (ca, cv, cr) ----------------

.merge_bundled <- function(mod) {
  recipe_path <- system.file("recipes", paste0(mod, "_merge_recipe.yml"),
                             package = "lissr")
  recipe <- yaml::yaml.load_file(recipe_path)
  data_dir <- file.path(tempdir(), paste0("lissr_5b_", mod))
  out_dir  <- file.path(tempdir(), paste0("lissr_5bo_", mod))
  unlink(c(data_dir, out_dir), recursive = TRUE)
  dir.create(data_dir, recursive = TRUE)
  dir.create(out_dir, recursive = TRUE)
  .gen_module_fixture(recipe, data_dir)
  res <- suppressWarnings(suppressMessages(
    merge_liss_module(recipe_path, data_dir, out_dir)))
  unlink(c(data_dir, out_dir), recursive = TRUE)
  res
}

test_that("ca wave_year is the fieldwork year; DV00 keeps the reference year", {
  skip_if_not_installed("haven")
  d <- .merge_bundled("ca")$data
  yr <- vapply(split(as.numeric(d$wave_year), as.character(d$wave_id)),
               unique, numeric(1))
  expect_equal(yr[["ca08a"]], 2008)  # fieldwork year, no longer 2007
  expect_equal(yr[["ca24i"]], 2024)
  expect_equal(yr[["ca25j"]], 2025)
  expect_true("asset_reference_year" %in% names(d))
  ref <- vapply(split(as.numeric(d$asset_reference_year),
                      as.character(d$wave_id)), unique, numeric(1))
  expect_equal(ref[["ca08a"]], 2007)
  expect_equal(ref[["ca25j"]], 2024)
  # the reference year sits one year before fieldwork in every wave
  expect_true(all(ref == yr[names(ref)] - 1))
})

test_that("cv fieldwork_month executes as fieldwork_ym mod 100", {
  skip_if_not_installed("haven")
  d <- .merge_bundled("cv")$data
  expect_true("fieldwork_month" %in% names(d))
  ok <- !is.na(d$fieldwork_ym)
  expect_gt(sum(ok), 0)
  expect_equal(as.numeric(d$fieldwork_month[ok]),
               as.numeric(d$fieldwork_ym[ok]) %% 100)
})

test_that("cr scale-break splits populate era-scoped output columns", {
  skip_if_not_installed("haven")
  d <- .merge_bundled("cr")$data
  splits <- list(
    c(pre = "attendance_pre2019_8pt", post = "attendance_post2019_6pt"),
    c(pre = "prayer_pre2019_8pt", post = "prayer_post2019_6pt"),
    c(pre = "afterlife_pre2019_4pt", post = "afterlife_post2019_3pt"))
  pre_rows  <- as.character(d$wave_id) <= "cr18k"
  post_rows <- !pre_rows
  for (sp in splits) {
    expect_true(all(c(sp[["pre"]], sp[["post"]]) %in% names(d)))
    # pre column carries the source values on pre-2019 rows, NA after
    expect_true(all(is.na(d[[sp[["pre"]]]][post_rows])))
    expect_true(all(is.na(d[[sp[["post"]]]][pre_rows])))
    expect_gt(sum(!is.na(d[[sp[["pre"]]]][pre_rows])), 0)
    expect_gt(sum(!is.na(d[[sp[["post"]]]][post_rows])), 0)
  }
})
