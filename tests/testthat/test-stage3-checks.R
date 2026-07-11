# ============================================================================
# stage 3a (v1.4 development) regressions: the normalized check grammar.
# alias families execute through canonical executors, the new primitives
# behave, documentary types report DOC, unknown error-severity checks
# escalate, and the action vocabulary loads from the installed registry.
# ============================================================================

.rv <- function(df, checks) {
  suppressWarnings(suppressMessages(lissr:::run_validations(df, checks, list())))
}

.df6 <- function() {
  data.frame(
    wave_id    = c("w1", "w1", "w1", "w2", "w2", "w2"),
    nomem_encr = c(1, 2, 3, 1, 2, 3),
    s005       = c(1, 999, 2, -9, 1, 2),
    s006       = c(0, 1, NA, 2, 0, 1),
    s007       = c(NA, NA, NA, 5, 6, 7)
  )
}

test_that("uniqueness aliases resolve their payload key variants", {
  df <- .df6()
  dup <- rbind(df, df[1, ])   # duplicate (w1, id 1)
  checks <- list(
    list(check_id = "U1", type = "unique_key", severity = "error",
         key_columns = list("nomem_encr", "wave_id")),
    list(check_id = "U2", type = "no_duplicate_ids", severity = "error",
         variables = list("nomem_encr", "wave_id")),
    list(check_id = "U3", type = "unique_per_wave", severity = "error",
         scope = "nomem_encr"),
    list(check_id = "U4", type = "assert_identifier", severity = "warning")
  )
  res_ok  <- .rv(df, checks)
  res_dup <- .rv(dup, checks)
  expect_equal(res_ok$n_pass, 4)
  expect_equal(res_dup$n_fail, 4)
})

test_that("value_absence aliases: scalar value, sentinels, blocks, restriction", {
  df <- .df6()
  checks <- list(
    # none_equal: 999 present in s005 -> FAIL
    list(check_id = "A1", type = "none_equal", severity = "error",
         scope = list("005"), value = 999),
    # sentinel_absence over all_numeric with an exclusion -> -9 in s005 FAILs
    list(check_id = "A2", type = "sentinel_absence", severity = "error",
         sentinel_values = list(-9), applies_to = "all_numeric",
         exclude_variables = list("006", "007")),
    # block form (cs V06 / ci V-01 shape): forbidden only in wave w2 -> pass
    list(check_id = "A3", type = "assert_no_values", severity = "warning",
         targets = list(list(waves = list("w2"), variables = list("005"),
                             forbidden_values = list(999)))),
    # value_restriction: 999 allowed ONLY in w1; it occurs in w1 -> pass
    list(check_id = "A4", type = "value_restriction", severity = "error",
         suffixes = list("005"), value = 999, waves_allowed = list("w1")),
    # value_restriction violated: -9 allowed only in w1 but occurs in w2
    list(check_id = "A5", type = "value_restriction", severity = "error",
         suffixes = list("005"), value = -9, waves_allowed = list("w1"))
  )
  res <- .rv(df, checks)
  st <- vapply(res$results, function(r) isTRUE(r$passed), logical(1))
  expect_identical(unname(st), c(FALSE, FALSE, TRUE, TRUE, FALSE))
})

test_that("value_in_set handles shared and per-variable allowed sets", {
  df <- .df6()
  checks <- list(
    list(check_id = "S1", type = "value_set", severity = "error",
         variables = list("006"), allowed_values = list(0, 1, 2)),
    list(check_id = "S2", type = "assert_values", severity = "warning",
         variables = list(list(name = "006", allowed = list(0, 1)))),
    list(check_id = "S3", type = "value_in_set", severity = "error",
         variables = list("006"), allowed_values = list(0, 1, 2),
         allow_na = FALSE)
  )
  res <- .rv(df, checks)
  st <- vapply(res$results, function(r) isTRUE(r$passed), logical(1))
  # S1 passes (values 0,1,2,NA with NA allowed); S2 fails (2 out of set);
  # S3 fails (NA not allowed)
  expect_identical(unname(st), c(TRUE, FALSE, FALSE))
})

test_that("value_present asserts per-wave occurrence and names the gap", {
  df <- .df6()
  res <- .rv(df, list(
    list(check_id = "P1", type = "value_present_per_wave", severity = "warning",
         scope = "005", value = 999)))
  expect_false(isTRUE(res$results[[1]]$passed))
  expect_match(res$results[[1]]$detail, "w2")
})

test_that("structural_missingness aliases cover absence and presence shapes", {
  df <- .df6()
  checks <- list(
    # s007 is all-NA in w1 -> pass
    list(check_id = "M1", type = "structural_absence", severity = "error",
         variable = "007", must_be_na_in = list("w1")),
    # s005 is not all-NA in w1 -> fail
    list(check_id = "M2", type = "all_na", severity = "error",
         scope = "005", wave_filter = list("w1")),
    # missingness_check: present in w2 and all-NA elsewhere -> pass
    list(check_id = "M3", type = "missingness_check", severity = "warning",
         variable = "007", waves_expected_present = list("w2"),
         expect_elsewhere = "all_na")
  )
  res <- .rv(df, checks)
  st <- vapply(res$results, function(r) isTRUE(r$passed), logical(1))
  expect_identical(unname(st), c(TRUE, FALSE, TRUE))
})

test_that("na_rate aliases: not_missing and direction variants", {
  df <- .df6()
  res <- .rv(df, list(
    list(check_id = "N1", type = "not_missing", severity = "error",
         variables = list("nomem_encr")),
    list(check_id = "N2", type = "not_missing", severity = "error",
         variables = list("006")),          # has one NA -> fail
    list(check_id = "N3", type = "na_rate_above", severity = "warning",
         scope = list("007"), wave_filter = list("w1"), threshold = 0.5)))
  st <- vapply(res$results, function(r) isTRUE(r$passed), logical(1))
  expect_identical(unname(st), c(TRUE, FALSE, TRUE))
})

test_that("wave_count expected, row_count bounds, and per_wave_mean execute", {
  df <- .df6()
  res <- .rv(df, list(
    list(check_id = "W1", type = "n_distinct_wave", severity = "error",
         expected = 2),
    list(check_id = "W2", type = "n_distinct_wave", severity = "error",
         expected = 3),
    list(check_id = "R1", type = "assert_row_count_range", severity = "warning",
         wave = "w1", min_rows = 1),
    list(check_id = "R2", type = "row_count", severity = "warning",
         wave = "w1", min_rows = 10),
    list(check_id = "PM1", type = "per_wave_mean", severity = "error",
         items = list("006"), max_mean = 11),
    list(check_id = "PM2", type = "per_wave_mean", severity = "error",
         items = list("005"), max_mean = 3)))   # w1 mean includes 999 -> fail
  st <- vapply(res$results, function(r) isTRUE(r$passed), logical(1))
  expect_identical(unname(st), c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))
})

test_that("documentary types report DOC; unknown error checks escalate", {
  df <- .df6()
  res <- .rv(df, list(
    list(check_id = "D1", type = "distribution_check", severity = "warning"),
    list(check_id = "D2", type = "panel_overlap", severity = "warning"),
    list(check_id = "X1", type = "made_up_type", severity = "error"),
    list(check_id = "X2", type = "made_up_type", severity = "warning")))
  expect_equal(res$n_doc, 2)
  expect_equal(res$n_skip, 2)
  expect_identical(res$error_skips, "X1")
  expect_true(isTRUE(res$results[[1]]$documentary))
})

test_that("strict mode aborts on unevaluable error-severity checks", {
  skip_if_not_installed("haven")
  data_dir <- file.path(tempdir(), "lissr_s3_data")
  out_dir  <- file.path(tempdir(), "lissr_s3_out")
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(c(data_dir, out_dir), recursive = TRUE), add = TRUE)
  haven::write_sav(data.frame(nomem_encr = 1:2, zz01a005 = c(1, 2)),
                   file.path(data_dir, "zz01a_EN_1.0p.sav"))
  recipe <- list(
    meta = list(module = "zz", module_label = "F", schema_version = "1.0.0",
                recipe_version = "t", created = "t", source_spec = "t",
                covered_waves = list("zz01a")),
    global = list(id_variable = "nomem_encr", wave_variable = "wave_id",
                  year_variable = "wave_year", labelled_policy = "to_numeric",
                  missing_variable_policy = "warn_and_create_na",
                  strip_label_whitespace = TRUE),
    wave_index = list(list(id = "zz01a", year = 2001, file_pattern = "zz01a_*")),
    validation_checks = list(
      list(check_id = "BAD", type = "made_up_type", severity = "error")),
    logging = list(summary_artifact = FALSE))
  expect_error(
    suppressWarnings(suppressMessages(
      merge_liss_module(recipe, data_dir, out_dir, strict = TRUE))),
    "unevaluable")
})

test_that("the loaded action vocabulary matches the installed registry", {
  path <- system.file("extdata", "action_vocabulary.yml", package = "lissr")
  expect_true(nzchar(path) && file.exists(path))
  from_file <- lissr:::.load_action_vocab(path)
  in_ns <- get("VALID_ACTIONS", envir = asNamespace("lissr"))
  for (sec in names(from_file)) {
    expect_setequal(in_ns[[sec]], from_file[[sec]])
  }
})
