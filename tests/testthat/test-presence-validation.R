.presence_check <- function(variable = "s005", waves = c("w1", "w2"),
                            severity = "error") {
  list(check_id = "PRESENCE", type = "expected_presence", severity = severity,
       variable = variable, waves = waves)
}

.presence_data <- function() {
  data.frame(wave_id = c("w1", "w1", "w2"), s005 = c(NA, 2, 3))
}

.presence_validation <- function(check, df = .presence_data()) {
  suppressWarnings(suppressMessages(
    lissr:::run_validations(df, list(check), list())))
}

test_that("expected_presence checks each explicit wave and supports all waves", {
  for (waves in list("w1", c("w1", "w2"), list("w1", "w2"),
                     "all", list("all"), c(scope = "all"))) {
    result <- .presence_validation(.presence_check(waves = waves))
    expect_true(result$results[[1]]$passed)
    expect_identical(result$error_count, 0L)
    expect_length(result$error_skips, 0L)
  }
  df <- .presence_data()
  df$s005[df$wave_id == "w2"] <- NA_real_
  expect_true(.presence_validation(.presence_check(waves = "w1"), df)$results[[1]]$passed)
  for (waves in list(c("w1", "w2"), "all")) {
    result <- .presence_validation(.presence_check(waves = waves), df)
    expect_false(result$results[[1]]$passed)
    expect_match(result$results[[1]]$detail, "w2")
  }
})

test_that("absent columns and waves fail with their original severity", {
  cases <- list(
    list(check = .presence_check(variable = "absent_column"), detail = "absent_column"),
    list(check = .presence_check(waves = c("w1", "absent_wave")), detail = "absent_wave"))
  for (case in cases) {
    for (severity in c("error", "warning", "info")) {
      check <- case$check
      check$severity <- severity
      result <- .presence_validation(check)
      expect_false(result$results[[1]]$passed)
      expect_identical(result$results[[1]]$severity, severity)
      expect_match(result$results[[1]]$detail, case$detail)
      expect_equal(result$error_count, as.integer(severity == "error"))
    }
  }
  # presence uses exact column names, without suffix or partial-name resolution
  expect_false(.presence_validation(.presence_check(variable = "005"))$results[[1]]$passed)
})

test_that("malformed and empty presence requests are unevaluable", {
  invalid <- list(
    .presence_check(variable = NULL),
    .presence_check(variable = " "),
    .presence_check(variable = NA_character_),
    .presence_check(variable = c("s005", "other")),
    .presence_check(variable = 5),
    .presence_check(waves = NULL),
    .presence_check(waves = character()),
    .presence_check(waves = list()),
    .presence_check(waves = c("w1", "")),
    .presence_check(waves = c("w1", NA_character_)),
    .presence_check(waves = list("w1", NULL)),
    .presence_check(waves = list(list("w1"))),
    .presence_check(waves = 1),
    .presence_check(waves = c("all", "w1")))
  partial_name <- .presence_check()
  partial_name$variable <- NULL
  partial_name$variables <- "s005"
  partial_waves <- .presence_check()
  partial_waves$waves <- NULL
  partial_waves$waves_expected_present <- "w1"
  invalid <- c(invalid, list(partial_name, partial_waves))
  for (check in invalid) {
    result <- .presence_validation(check)
    expect_identical(result$results[[1]]$passed, NA)
    expect_identical(result$results[[1]]$severity, "error")
    expect_identical(result$error_skips, "PRESENCE")
    expect_identical(result$n_pass, 0L)
  }
  for (severity in c("warning", "info")) {
    result <- .presence_validation(.presence_check(waves = NULL, severity = severity))
    expect_identical(result$results[[1]]$passed, NA)
    expect_identical(result$results[[1]]$severity, severity)
    expect_length(result$error_skips, 0L)
  }
})

test_that("empty data fails and unavailable wave identification is unevaluable", {
  df <- .presence_data()
  for (waves in list("w1", "all")) {
    result <- .presence_validation(.presence_check(waves = waves), df[FALSE, ])
    expect_false(result$results[[1]]$passed)
    expect_identical(result$error_count, 1L)
  }
  missing_wave <- df
  missing_wave$wave_id <- NULL
  unknown_wave <- df
  unknown_wave$wave_id[1] <- NA_character_
  blank_wave <- df
  blank_wave$wave_id[1] <- " "
  for (input in list(missing_wave, unknown_wave, blank_wave)) {
    result <- .presence_validation(.presence_check(), input)
    expect_identical(result$results[[1]]$passed, NA)
    expect_identical(result$error_skips, "PRESENCE")
    expect_match(result$results[[1]]$detail, "wave_id")
  }
  # unknown row membership remains unevaluable even outside the explicit scope
  result <- .presence_validation(.presence_check(waves = "w2"), blank_wave)
  expect_identical(result$results[[1]]$passed, NA)
})

.presence_merge_fixture <- function(check, .local_envir = parent.frame()) {
  root <- withr::local_tempdir("lissr_presence_", .local_envir = .local_envir)
  data_dir <- file.path(root, "data")
  dir.create(data_dir)
  haven::write_sav(data.frame(nomem_encr = 1:2, yy01a005 = c(1, 2)),
                   file.path(data_dir, "yy01a_EN_1.0p.sav"))
  recipe <- list(
    meta = list(module = "yy", module_label = "presence fixture",
                schema_version = "1.0.0", recipe_version = "test",
                created = "test", source_spec = "test",
                covered_waves = list("yy01a")),
    global = list(id_variable = "nomem_encr", wave_variable = "wave_id",
                  year_variable = "wave_year", labelled_policy = "to_numeric",
                  missing_variable_policy = "warn_and_create_na",
                  strip_label_whitespace = TRUE),
    wave_index = list(list(id = "yy01a", year = 2001L, file_pattern = "yy01a_*")),
    validation_checks = list(check),
    logging = list(summary_artifact = list(enabled = TRUE)))
  list(recipe = recipe, data_dir = data_dir, output_dir = file.path(root, "output"))
}

test_that("strict presence errors block every output artifact", {
  checks <- list(.presence_check("absent_column", "yy01a"),
                 .presence_check(waves = "absent_wave"),
                 .presence_check(waves = character()))
  for (check in checks) {
    fx <- .presence_merge_fixture(check)
    expect_error(suppressWarnings(suppressMessages(
      merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE))),
      "strict mode: no outputs were written")
    expect_false(dir.exists(fx$output_dir))
    # existing output directories must also remain untouched on a strict failure
    dir.create(fx$output_dir, showWarnings = FALSE)
    sentinel <- file.path(fx$output_dir, "existing.txt")
    writeLines("keep this", sentinel)
    expect_error(suppressWarnings(suppressMessages(
      merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE))),
      "strict mode: no outputs were written")
    expect_identical(list.files(fx$output_dir), "existing.txt")
    expect_identical(readLines(sentinel), "keep this")
  }
})

test_that("report mode records failed and unevaluable presence checks as invalid", {
  for (check in list(.presence_check("absent_column", "yy01a"),
                     .presence_check(waves = character()))) {
    fx <- .presence_merge_fixture(check)
    result <- suppressWarnings(suppressMessages(
      merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir)))
    expect_false(result$valid_for_analysis)
    expect_false(isTRUE(result$validation[[1]]$passed))
    report <- readLines(file.path(fx$output_dir, "yy_merge_report.txt"))
    expect_true(any(grepl("Valid for analysis: FALSE", report, fixed = TRUE)))
    status <- if (isFALSE(result$validation[[1]]$passed)) "FAIL" else "SKIP"
    expect_true(any(grepl(paste0("[error] PRESENCE: ", status, " -- "),
                         report, fixed = TRUE)))
    expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
  }
})

test_that("valid strict presence checks retain output and a valid verdict", {
  fx <- .presence_merge_fixture(.presence_check(waves = "yy01a"))
  result <- suppressWarnings(suppressMessages(
    merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE)))
  expect_true(result$valid_for_analysis)
  expect_true(result$validation[[1]]$passed)
  expect_equal(as.numeric(result$data$s005), c(1, 2))
  expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
})
