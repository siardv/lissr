.wave_count_check <- function(severity = "error") {
  list(check_id = "WAVE_COUNT", type = "wave_count", severity = severity,
       max_waves = 2)
}

.wave_count_data <- function() {
  data.frame(nomem_encr = c(1, 1, 1, 2, 2, 2),
             wave_id = c("w1", "w1", "w2", "w2", "w3", "w3"))
}

.wave_count_validation <- function(check, df = .wave_count_data()) {
  suppressWarnings(suppressMessages(
    lissr:::run_validations(df, list(check), list())))
}

.expect_wave_count_unevaluable <- function(check, df = .wave_count_data(),
                                          pattern = NULL) {
  result <- .wave_count_validation(check, df)
  expect_identical(result$results[[1]]$passed, NA)
  expect_identical(result$results[[1]]$severity, check$severity)
  expect_identical(result$error_count, 0L)
  expect_identical(result$error_skips,
                   if (check$severity == "error") "WAVE_COUNT" else character())
  if (!is.null(pattern)) {
    detail <- result$results[[1]]$detail
    expect_match(if (is.null(detail)) "" else detail, pattern, fixed = TRUE)
  }
  invisible(result)
}

test_that("wave counts require the columns used by the selected branch", {
  for (type in c("wave_count", "n_distinct_wave")) {
    for (severity in c("error", "warning", "info")) {
      check <- .wave_count_check(severity)
      check$type <- type
      for (empty in c(FALSE, TRUE)) {
        df <- .wave_count_data()
        if (empty) df <- df[FALSE, ]
        .expect_wave_count_unevaluable(check, df["wave_id"], "nomem_encr")
        .expect_wave_count_unevaluable(check, df["nomem_encr"], "wave_id")
        custom <- check
        custom$column <- "absent_person"
        .expect_wave_count_unevaluable(custom, df, "absent_person")
        custom$max_waves <- 0
        .expect_wave_count_unevaluable(custom, df, "absent_person")
        check$expected <- if (empty) 0 else 3
        expect_true(.wave_count_validation(check, df["wave_id"])$results[[1]]$passed)
        .expect_wave_count_unevaluable(check, df["nomem_encr"], "wave_id")
        check$expected <- NULL
      }
    }
  }
})

test_that("global wave counts ignore all person-key and maximum declarations", {
  check <- .wave_count_check()
  check$expected <- 3
  for (key in c("column", "key", "max_waves")) {
    for (value in list("absent_person", character(), list(list("nomem_encr")),
                       c("nomem_encr", "absent_person"), NA, matrix("nomem_encr"))) {
      check[key] <- list(value)
      expect_true(.wave_count_validation(check)$results[[1]]$passed)
    }
  }
  for (values in list(as.list(1:6), matrix(1:6), data.frame(id = 1:6))) {
    df <- .wave_count_data()
    df$nomem_encr <- values
    expect_true(.wave_count_validation(check, df)$results[[1]]$passed)
  }
  check$expected <- 2
  expect_false(.wave_count_validation(check)$results[[1]]$passed)
})

test_that("person keys use exact scalar names and first non-null precedence", {
  df <- .wave_count_data()
  df$alternate <- seq_len(nrow(df))
  check <- .wave_count_check()
  check$max_waves <- 1
  check$column <- "alternate"
  check$key <- "absent_person"
  expect_true(.wave_count_validation(check, df)$results[[1]]$passed)
  check["column"] <- list(NULL)
  .expect_wave_count_unevaluable(check, df, "absent_person")
  check$key <- "alternate"
  expect_true(.wave_count_validation(check, df)$results[[1]]$passed)
  check["key"] <- list(NULL)
  expect_false(.wave_count_validation(check, df)$results[[1]]$passed)
  check$column <- c(wave_id = "nomem_encr")
  expect_false(.wave_count_validation(check, df)$results[[1]]$passed)
  check$column <- "wave_id"
  expect_true(.wave_count_validation(check, df["wave_id"])$results[[1]]$passed)

  for (key in c("column", "key")) {
    for (value in list(character(), "", " ", NA_character_, 1, TRUE,
                       c("nomem_encr", "wave_id"), list("nomem_encr"),
                       list(list("nomem_encr")), matrix("nomem_encr"),
                       factor("nomem_encr"))) {
      check <- .wave_count_check()
      check[[key]] <- value
      .expect_wave_count_unevaluable(check, pattern = key)
    }
  }
  check <- .wave_count_check()
  check$column <- "nomem_encr"
  check$key <- list(list("ignored"))
  expect_true(.wave_count_validation(check)$results[[1]]$passed)
})

test_that("wave-count keys do not gain shorthand or suffix lookup", {
  for (name in c("person", "respondent", "wave", "005", "numeric", "001-003")) {
    check <- .wave_count_check()
    check$column <- name
    df <- .wave_count_data()
    df$s005 <- seq_len(nrow(df))
    .expect_wave_count_unevaluable(check, df, name)
    df[[name]] <- seq_len(nrow(df))
    check$max_waves <- 1
    expect_true(.wave_count_validation(check, df)$results[[1]]$passed)
  }
})

test_that("wave counts use exact payload fields without unsupported filters", {
  for (field in c("expected_wave", "column_note", "key_note", "max_waves_note")) {
    check <- list(check_id = "WAVE_COUNT", type = "wave_count", severity = "error")
    check[[field]] <- if (field == "expected_wave") 99 else
      if (field == "max_waves_note") 0 else "absent_person"
    expect_true(.wave_count_validation(check)$results[[1]]$passed)
  }
  check <- .wave_count_check()
  check["expected"] <- list(NULL)
  check$expected_wave <- 99
  expect_true(.wave_count_validation(check)$results[[1]]$passed)
  for (field in c("waves", "wave_filter", "scope_wave", "in_waves")) {
    for (value in list("w1", "absent_wave", "all", list("all"), character(),
                       list(list("w1")), NA_character_)) {
      check <- .wave_count_check()
      check[[field]] <- value
      check$max_waves <- 1
      expect_false(.wave_count_validation(check)$results[[1]]$passed)
      check$expected <- 3
      expect_true(.wave_count_validation(check)$results[[1]]$passed)
    }
  }
})

test_that("wave-count inputs must be atomic undimensioned vectors", {
  for (field in c("wave_id", "nomem_encr")) {
    for (value in list(as.list(1:6), matrix(1:6), matrix(1:12, nrow = 6),
                       data.frame(id = 1:6))) {
      df <- .wave_count_data()
      df[[field]] <- value
      check <- .wave_count_check()
      .expect_wave_count_unevaluable(check, df, field)
      check$expected <- 3
      if (field == "wave_id") {
        .expect_wave_count_unevaluable(check, df, field)
      } else {
        expect_true(.wave_count_validation(check, df)$results[[1]]$passed)
      }
    }
  }
})

test_that("wave counts retain distinctness and ordinary missing-value grouping", {
  for (ids in list(c(1, 1, 1, 2, 2, 2), factor(c(1, 1, 1, 2, 2, 2)),
                   c(NA, NA, NA, 2, 2, 2), c("", "", "", " ", " ", " "),
                   c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))) {
    for (waves in list(c("w1", "w1", "w2", "w2", "w3", "w3"),
                      c(NA, NA, "w2", "w2", "w3", "w3"),
                      c("", "", " ", " ", NA, NA),
                      c(NA_real_, NA_real_, NaN, NaN, 1, 1),
                      factor(c("w1", "w1", "w2", "w2", "w3", "w3")))) {
      df <- data.frame(nomem_encr = ids, wave_id = waves)
      check <- .wave_count_check()
      result <- .wave_count_validation(check, df)$results[[1]]
      expect_true(result$passed)
      expect_identical(result$detail, "max waves per person: 2")
      check$max_waves <- 1
      expect_false(.wave_count_validation(check, df)$results[[1]]$passed)
      check$expected <- 3
      result <- .wave_count_validation(check, df)$results[[1]]
      expect_true(result$passed)
      expect_identical(result$detail, "distinct waves: 3 (expected 3)")
    }
  }
  check <- .wave_count_check()
  df <- .wave_count_data()
  df$wave_id <- NA_character_
  check$max_waves <- 1
  expect_true(.wave_count_validation(check, df)$results[[1]]$passed)
  check$expected <- 1
  expect_true(.wave_count_validation(check, df)$results[[1]]$passed)
})

test_that("wave-count comparisons retain supported coercion and scalar bounds", {
  for (expected in list(3, 3.9, "3", "3.9", list(3), matrix(3), c(count = 3))) {
    check <- .wave_count_check()
    check$expected <- expected
    expect_true(.wave_count_validation(check)$results[[1]]$passed)
  }
  for (maximum in list(2, 2.5, "2", Inf, c(limit = 2))) {
    check <- .wave_count_check()
    check$max_waves <- maximum
    expect_true(.wave_count_validation(check)$results[[1]]$passed)
  }
  for (maximum in list(-Inf, -1, 0, 1, 1.5, "1")) {
    check <- .wave_count_check()
    check$max_waves <- maximum
    expect_false(.wave_count_validation(check)$results[[1]]$passed)
  }
  check <- .wave_count_check()
  check["max_waves"] <- list(NULL)
  expect_true(.wave_count_validation(check)$results[[1]]$passed)
})

test_that("wave-count comparisons report non-scalar and unknown verdicts", {
  for (severity in c("error", "warning", "info")) {
    for (field in c("expected", "max_waves")) {
      invalid <- if (field == "expected")
        list(integer(), c(2, 3), NA, NaN, Inf, "not a count") else
        list(numeric(), c(1, 2), NA, NaN, matrix(2), matrix(c(1, 2)))
      for (value in invalid) {
        check <- .wave_count_check(severity)
        check[field] <- list(value)
        .expect_wave_count_unevaluable(check, pattern = field)
      }
    }
  }
})

test_that("empty wave-count data preserve zero global and uncalculated person counts", {
  df <- .wave_count_data()[FALSE, ]
  for (maximum in c(-Inf, -1, 0, Inf)) {
    check <- .wave_count_check()
    check$max_waves <- maximum
    expect_warning(result <- suppressMessages(
      lissr:::run_validations(df, list(check), list())), NA)
    expect_true(result$results[[1]]$passed)
    detail <- result$results[[1]]$detail
    expect_match(if (is.null(detail)) "" else detail,
      "no observed persons; no per-person wave counts calculated", fixed = TRUE)
  }
  check <- .wave_count_check()
  check$max_waves <- NA
  .expect_wave_count_unevaluable(check, df, "max_waves")
  check$expected <- 0
  result <- .wave_count_validation(check, df["wave_id"])$results[[1]]
  expect_true(result$passed)
  expect_identical(result$detail, "distinct waves: 0 (expected 0)")
  check$expected <- 1
  expect_false(.wave_count_validation(check, df["wave_id"])$results[[1]]$passed)
})

test_that("observed wave-count violations preserve the declared severity", {
  for (severity in c("error", "warning", "info")) {
    for (global in c(FALSE, TRUE)) {
      check <- .wave_count_check(severity)
      if (global) check$expected <- 2 else check$max_waves <- 1
      result <- .wave_count_validation(check)
      expect_false(result$results[[1]]$passed)
      expect_identical(result$results[[1]]$severity, severity)
      expect_identical(result$error_count, if (severity == "error") 1L else 0L)
      expect_identical(result$error_skips, character())
    }
  }
})

.wave_count_merge_fixture <- function(check, drop_wave = FALSE,
                                       .local_envir = parent.frame()) {
  root <- withr::local_tempdir("lissr_wave_count_", .local_envir = .local_envir)
  data_dir <- file.path(root, "data")
  dir.create(data_dir)
  for (wave in c("yy01a", "yy02b")) {
    df <- data.frame(nomem_encr = 1:2)
    df[[paste0(wave, "005")]] <- if (wave == "yy01a") c(2, NA) else c(3, 4)
    haven::write_sav(df, file.path(data_dir, paste0(wave, "_EN_1.0p.sav")))
  }
  recipe <- list(
    meta = list(module = "yy", module_label = "wave count fixture", schema_version = "1.0.0",
                recipe_version = "test", created = "test", source_spec = "test",
                covered_waves = list("yy01a", "yy02b")),
    global = list(id_variable = "nomem_encr", wave_variable = "wave_id",
                  year_variable = "wave_year", labelled_policy = "to_numeric",
                  missing_variable_policy = "warn_and_create_na", strip_label_whitespace = TRUE),
    wave_index = list(list(id = "yy01a", year = 2001L, file_pattern = "yy01a_*"),
                      list(id = "yy02b", year = 2002L, file_pattern = "yy02b_*")),
    validation_checks = list(check), logging = list(summary_artifact = list(enabled = TRUE)))
  if (drop_wave) recipe$drop_retain_rules <- list(
    list(rule_id = "DROP_WAVE", action = "drop", columns = "wave_id"))
  list(recipe = recipe, data_dir = data_dir, output_dir = file.path(root, "output"))
}

test_that("strict and report modes handle unresolved and violated wave counts", {
  missing <- .wave_count_check()
  missing$column <- "absent_person"
  malformed <- .wave_count_check()
  malformed$column <- character()
  person_violation <- .wave_count_check()
  person_violation$max_waves <- 1
  global_violation <- .wave_count_check()
  global_violation$expected <- 1
  global_missing <- .wave_count_check()
  global_missing$expected <- 2
  unknown <- .wave_count_check()
  unknown$expected <- c(1, 2)
  checks <- list(missing, malformed, person_violation, global_violation,
                 unknown, global_missing)
  for (i in seq_along(checks)) {
    fx <- .wave_count_merge_fixture(checks[[i]], drop_wave = i == length(checks))
    expect_error(suppressWarnings(suppressMessages(
      merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE))),
      "strict mode: no outputs were written")
    expect_false(dir.exists(fx$output_dir))
    dir.create(fx$output_dir, showWarnings = FALSE)
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
    expect_false(isTRUE(result$validation[[1]]$passed))
    expect_identical(result$validation[[1]]$severity, "error")
    report <- readLines(file.path(fx$output_dir, "yy_merge_report.txt"))
    expect_true(any(grepl("Valid for analysis: FALSE", report, fixed = TRUE)))
    expect_true(any(grepl("[error] WAVE_COUNT:", report, fixed = TRUE)))
    detail <- result$validation[[1]]$detail
    expect_type(detail, "character")
    expect_length(detail, 1L)
    if (is.character(detail) && length(detail) == 1L)
      expect_true(any(grepl(detail, report, fixed = TRUE)))
    expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
  }
})

test_that("valid wave counts preserve strict output and actual values", {
  for (type in c("wave_count", "n_distinct_wave")) {
    for (global in c(FALSE, TRUE)) {
      check <- .wave_count_check()
      check$type <- type
      if (global) {
        check$expected <- 2
        check$column <- "absent_person"
      }
      fx <- .wave_count_merge_fixture(check)
      result <- suppressWarnings(suppressMessages(
        merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE)))
      expect_true(result$valid_for_analysis)
      expect_true(result$validation[[1]]$passed)
      expect_equal(as.numeric(result$data$nomem_encr), c(1, 2, 1, 2))
      expect_equal(as.numeric(result$data$s005), c(2, NA, 3, 4))
      expect_equal(as.character(result$data$wave_id), c("yy01a", "yy01a", "yy02b", "yy02b"))
      expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
    }
  }
})

test_that("warning and info wave-count outcomes retain strict output eligibility", {
  for (severity in c("warning", "info")) {
    for (unresolved in c(FALSE, TRUE)) {
      check <- .wave_count_check(severity)
      if (unresolved) check$column <- "absent_person" else check$max_waves <- 1
      fx <- .wave_count_merge_fixture(check)
      result <- suppressWarnings(suppressMessages(
        merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE)))
      expect_true(result$valid_for_analysis)
      expect_identical(result$validation[[1]]$passed, if (unresolved) NA else FALSE)
      expect_identical(result$validation[[1]]$severity, severity)
      report <- readLines(file.path(fx$output_dir, "yy_merge_report.txt"))
      expect_true(any(grepl("Valid for analysis: TRUE", report, fixed = TRUE)))
      expect_true(any(grepl(paste0("[", severity, "] WAVE_COUNT: ",
        if (unresolved) "SKIP" else "FAIL"), report, fixed = TRUE)))
      expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
    }
  }
})
