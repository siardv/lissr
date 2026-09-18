.validation_report_data <- function() {
  data.frame(nomem_encr = 1:2, wave_id = "yy01a", s005 = c(1, NA_real_))
}

.validation_report_lines <- function(data, results) {
  path <- withr::local_tempfile(fileext = ".txt")
  recipe <- list(meta = list(module = "yy", recipe_version = "test"))
  lissr:::write_report(data, results, list(), recipe, path)
  grep("^\\[", readLines(path), value = TRUE)
}

test_that("text reports match console statuses and preserve severity and detail", {
  data <- .validation_report_data()
  statuses <- c("DOC", "PASS", "FAIL", "SKIP", "SKIP")
  for (severity in c("error", "warning", "info")) {
    checks <- list(
      list(check_id = "DOCUMENTARY", type = "distribution_check"),
      list(check_id = "PASSED", type = "row_count", min_rows = 2, max_rows = 2),
      list(check_id = "FAILED", type = "row_count", max_rows = 0),
      list(check_id = "UNKNOWN", type = "unregistered_report_check"),
      list(check_id = "UNEVALUABLE", type = "row_count", wave = "absent_wave"))
    checks <- lapply(checks, function(check) {
      check$severity <- severity
      check
    })
    messages <- character()
    result <- withCallingHandlers(
      suppressWarnings(lissr:::run_validations(data, checks, list())),
      message = function(condition) {
        messages <<- c(messages, conditionMessage(condition))
        invokeRestart("muffleMessage")
      })
    console <- cli::ansi_strip(paste(messages, collapse = "\n"))
    expected <- vapply(seq_along(checks), function(i) {
      prefix <- paste0("[", severity, "] ", checks[[i]]$check_id, ": ", statuses[[i]])
      expect_match(console, prefix, fixed = TRUE)
      paste0(prefix, " -- ", result$results[[i]]$detail)
    }, character(1))
    expect_identical(.validation_report_lines(data, result$results), expected)
    expect_identical(vapply(result$results, `[[`, logical(1), "passed"),
                     c(NA, TRUE, FALSE, NA, NA))
    expect_equal(c(result$n_pass, result$n_fail, result$n_doc, result$n_skip),
                 c(1, 1, 1, 2))
    expect_identical(result$error_count, if (severity == "error") 1L else 0L)
    expect_identical(result$error_skips,
                     if (severity == "error") c("UNKNOWN", "UNEVALUABLE") else character())
  }
})

test_that("all registered documentary checks retain DOC at every severity", {
  data <- .validation_report_data()
  for (severity in c("error", "warning", "info")) {
    types <- lissr:::.DOCUMENTARY_CHECK_TYPES
    checks <- lapply(types, function(type) {
      list(check_id = type, type = type, severity = severity)
    })
    result <- suppressMessages(lissr:::run_validations(data, checks, list()))
    expect_identical(.validation_report_lines(data, result$results),
      paste0("[", severity, "] ", types,
             ": DOC -- documentary diagnostic; no executor by design"))
    expect_equal(result$n_doc, length(types))
    expect_equal(result$n_skip, 0)
    expect_identical(result$error_count, 0L)
    expect_identical(result$error_skips, character())
  }
})

test_that("explicit pass and fail results take precedence over documentary flags", {
  results <- list(
    list(check_id = "PASSED", passed = TRUE, severity = "info", documentary = TRUE),
    list(check_id = "FAILED", passed = FALSE, severity = "error", documentary = TRUE))
  expect_identical(.validation_report_lines(.validation_report_data(), results),
                   c("[info] PASSED: PASS", "[error] FAILED: FAIL"))
})

test_that("unrelated result fields do not supply a documentary marker", {
  result <- list(check_id = "UNEVALUABLE", passed = NA, severity = "error",
                 documentary_note = TRUE)
  expect_identical(.validation_report_lines(.validation_report_data(), list(result)),
                   "[error] UNEVALUABLE: SKIP")
})

.validation_report_fixture <- function(checks, .local_envir = parent.frame()) {
  root <- withr::local_tempdir("lissr_validation_report_", .local_envir = .local_envir)
  data_dir <- file.path(root, "data")
  dir.create(data_dir)
  haven::write_sav(data.frame(nomem_encr = 1:2, yy01a005 = c(1, NA_real_)),
                   file.path(data_dir, "yy01a_EN_1.0p.sav"))
  recipe <- list(
    meta = list(module = "yy", module_label = "report fixture", schema_version = "1.0.0",
                recipe_version = "test", created = "test", source_spec = "test",
                covered_waves = list("yy01a")),
    global = list(id_variable = "nomem_encr", wave_variable = "wave_id",
                  year_variable = "wave_year", labelled_policy = "to_numeric",
                  missing_variable_policy = "warn_and_create_na", strip_label_whitespace = TRUE),
    wave_index = list(list(id = "yy01a", year = 2001L, file_pattern = "yy01a_*")),
    validation_checks = checks, logging = list(summary_artifact = FALSE))
  list(recipe = recipe, data_dir = data_dir, output_dir = file.path(root, "output"))
}

test_that("documentary checks preserve valid output in strict and report modes", {
  checks <- lapply(c("error", "warning", "info"), function(severity) {
    list(check_id = paste0("DOC_", severity), type = "distribution_check", severity = severity)
  })
  checks <- c(checks, list(
    list(check_id = "UNKNOWN", type = "unregistered_report_check", severity = "warning"),
    list(check_id = "UNEVALUABLE", type = "row_count", wave = "absent_wave", severity = "info")))
  for (strict in c(FALSE, TRUE)) {
    fx <- .validation_report_fixture(checks)
    result <- suppressMessages(merge_liss_module(
      fx$recipe, fx$data_dir, fx$output_dir, strict = strict))
    expect_true(result$valid_for_analysis)
    expect_equal(as.numeric(result$data$s005), c(1, NA_real_))
    expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
    report <- readLines(file.path(fx$output_dir, "yy_merge_report.txt"))
    expect_true("Valid for analysis: TRUE" %in% report)
    for (severity in c("error", "warning", "info")) {
      expect_true(paste0("[", severity, "] DOC_", severity,
        ": DOC -- documentary diagnostic; no executor by design") %in% report)
    }
    for (i in 4:5) {
      validation <- result$validation[[i]]
      expect_identical(validation$passed, NA)
      expect_true(paste0("[", validation$severity, "] ", validation$check_id,
                         ": SKIP -- ", validation$detail) %in% report)
    }
  }
})

test_that("documentary checks do not conceal error failures or genuine skips", {
  documentary <- list(check_id = "DOCUMENTARY", type = "distribution_check", severity = "error")
  cases <- list(
    list(check_id = "UNKNOWN", type = "unregistered_report_check", severity = "error"),
    list(check_id = "UNEVALUABLE", type = "row_count", wave = "absent_wave", severity = "error"),
    list(check_id = "FAILED", type = "row_count", max_rows = 0, severity = "error"))
  for (i in seq_along(cases)) {
    fx <- .validation_report_fixture(list(documentary, cases[[i]]))
    expect_error(suppressWarnings(suppressMessages(
      merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE))),
      "strict mode: no outputs were written")
    expect_false(dir.exists(fx$output_dir))
    dir.create(fx$output_dir)
    sentinel <- file.path(fx$output_dir, "existing.txt")
    writeLines("keep this", sentinel)
    expect_error(suppressWarnings(suppressMessages(
      merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE))),
      "strict mode: no outputs were written")
    expect_identical(list.files(fx$output_dir), "existing.txt")
    expect_identical(readLines(sentinel), "keep this")
    result <- suppressWarnings(suppressMessages(
      merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir)))
    expect_false(result$valid_for_analysis)
    expect_equal(as.numeric(result$data$s005), c(1, NA_real_))
    expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
    report <- readLines(file.path(fx$output_dir, "yy_merge_report.txt"))
    expect_true("Valid for analysis: FALSE" %in% report)
    expect_true(paste0("[error] DOCUMENTARY: DOC -- ",
                      "documentary diagnostic; no executor by design") %in% report)
    validation <- result$validation[[2]]
    expect_identical(validation$passed, if (i == 3L) FALSE else NA)
    expect_identical(validation$severity, "error")
    expect_true(paste0("[error] ", validation$check_id, ": ",
      if (i == 3L) "FAIL" else "SKIP", " -- ", validation$detail) %in% report)
  }
})
