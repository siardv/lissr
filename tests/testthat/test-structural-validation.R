.structural_check <- function(targets = "005", severity = "error") {
  list(check_id = "STRUCTURAL", type = "structural_missingness", severity = severity,
       variables = targets, waves = "w1")
}

.structural_data <- function() {
  data.frame(wave_id = c("w1", "w1", "w2", "w2"),
             s005 = c(NA, NA, 1, NA), s006 = c(NA, NA, 2, NA))
}

.structural_validation <- function(check, df = .structural_data()) {
  suppressWarnings(suppressMessages(
    lissr:::run_validations(df, list(check), list())))
}

.expect_structural_unevaluable <- function(check, df = .structural_data(),
                                         pattern = NULL) {
  result <- .structural_validation(check, df)
  expect_identical(result$results[[1]]$passed, NA)
  expect_identical(result$results[[1]]$severity, check$severity)
  expect_identical(result$error_skips,
                   if (check$severity == "error") "STRUCTURAL" else character())
  expect_identical(result$error_count, 0L)
  if (!is.null(pattern)) {
    detail <- result$results[[1]]$detail
    expect_match(if (is.null(detail)) "" else detail, pattern)
  }
  invisible(result)
}

test_that("structural checks require every target and requested wave", {
  for (targets in list("absent_column", c("005", "absent_column"))) {
    .expect_structural_unevaluable(.structural_check(targets), pattern = "absent_column")
  }
  for (key in c("waves", "waves_expected_present")) {
    for (waves in list("absent_wave", c("w1", "absent_wave"))) {
      check <- .structural_check()
      check[[key]] <- waves
      .expect_structural_unevaluable(check, pattern = "absent_wave")
    }
  }
})

test_that("structural type and target aliases retain exact and suffix resolution", {
  for (type in c("structural_missingness", "structural_absence", "all_na",
                 "structural_na_count", "missingness_check")) {
    check <- .structural_check()
    check$type <- type
    expect_true(.structural_validation(check)$results[[1]]$passed)
    check$waves <- "w2"
    expect_false(.structural_validation(check)$results[[1]]$passed)
  }
  for (key in c("suffixes", "variables", "variable", "scope")) {
    check <- .structural_check()
    check$variables <- NULL
    check[[key]] <- list("005", "006")
    expect_true(.structural_validation(check)$results[[1]]$passed)
    check[[key]] <- c("005", "absent_column")
    .expect_structural_unevaluable(check, pattern = "absent_column")
  }
  check <- .structural_check()
  check$waves <- "all"
  for (column in c("s005", "stem_005", "q005", "Q005", "005")) {
    df <- stats::setNames(data.frame(NA_real_), column)
    expect_true(.structural_validation(check, df)$results[[1]]$passed)
  }
  for (target in c("s005", "q005", "Q005")) {
    check$variables <- target
    expect_false(.structural_validation(check)$results[[1]]$passed)
    check$waves <- "w1"
    expect_true(.structural_validation(check)$results[[1]]$passed)
    check$waves <- "all"
  }
  check$variables <- "005"
  df <- data.frame(s005 = NA_real_, check.names = FALSE)
  df[["005"]] <- 1
  expect_false(.structural_validation(check, df)$results[[1]]$passed)
  check$variables <- "s005"
  expect_true(.structural_validation(check, df)$results[[1]]$passed)
  check$variables <- 5
  expect_true(.structural_validation(check, data.frame(s5 = NA_real_))$results[[1]]$passed)
})

test_that("structural targets retain key precedence and literal names", {
  check <- .structural_check()
  check$suffixes <- "006"
  check$variables <- "absent_column"
  check$variable <- "also_absent"
  expect_true(.structural_validation(check)$results[[1]]$passed)
  check["suffixes"] <- list(NULL)
  .expect_structural_unevaluable(check, pattern = "absent_column")
  check["variables"] <- list(NULL)
  check$variable <- "006"
  check$scope <- "absent_column"
  expect_true(.structural_validation(check)$results[[1]]$passed)
  check$variable <- NULL
  .expect_structural_unevaluable(check, pattern = "absent_column")

  check <- .structural_check()
  check$variables <- NULL
  check$variables_documentation <- "005"
  .expect_structural_unevaluable(check)
  for (target in c("numeric", "all_numeric", "005-006")) {
    check <- .structural_check(target)
    .expect_structural_unevaluable(check, pattern = target)
    df <- .structural_data()
    df[[target]] <- NA_real_
    expect_true(.structural_validation(check, df)$results[[1]]$passed)
  }
})

test_that("empty and malformed structural targets are unevaluable", {
  invalid <- list(NULL, character(), list(), c("005", ""), " ",
                  c("005", NA_character_), list("005", NULL), list(list("005")),
                  matrix("005"), matrix(list("005")), TRUE, Inf)
  for (targets in invalid) {
    check <- .structural_check()
    check["variables"] <- list(targets)
    .expect_structural_unevaluable(check)
  }
})

test_that("structural all-NA wave aliases exclude observations outside scope", {
  keys <- c("waves_must_be_all_na", "must_be_na_in", "expected_na_waves",
            "wave_filter", "waves")
  for (key in keys) {
    for (scope in list("w1", list("w1"), c(selected = "w1"))) {
      check <- .structural_check()
      check$waves <- NULL
      check[[key]] <- scope
      expect_true(.structural_validation(check)$results[[1]]$passed)
      check[[key]] <- "w2"
      result <- .structural_validation(check)$results[[1]]
      expect_false(result$passed)
      expect_match(result$detail, "1 non-NA value\\(s\\) in s005")
    }
  }
  for (i in seq_len(length(keys) - 1L)) {
    check <- .structural_check()
    check[[keys[[i]]]] <- "w1"
    check[[keys[[i + 1L]]]] <- "w2"
    expect_true(.structural_validation(check)$results[[1]]$passed)
    check[keys[[i]]] <- list(NULL)
    .expect_structural_unevaluable(check, pattern = "wave")
  }
  check <- .structural_check()
  check$waves <- NULL
  check$waves_documentation <- "w1"
  .expect_structural_unevaluable(check)
})

test_that("structural scopes must be declared and well formed", {
  check <- .structural_check()
  check$waves <- NULL
  .expect_structural_unevaluable(check)
  invalid <- list(NULL, character(), list(), c("w1", ""), " ",
                  c("w1", NA_character_), list("w1", NULL), list(list("w1")),
                  matrix("w1"), matrix(list("w1")), 1, c("all", "w1"))
  for (key in c("waves", "waves_expected_present")) {
    for (scope in invalid) {
      check <- .structural_check()
      check[key] <- list(scope)
      .expect_structural_unevaluable(check, pattern = "wave")
    }
  }
})

test_that("specific structural scopes require usable wave membership", {
  absent_ids <- .structural_data()
  absent_ids$wave_id <- NULL
  inputs <- list(absent_ids)
  for (wave_ids in list(c("w1", "w1", "w2", NA_character_),
                       c("w1", "w1", "w2", " "),
                       matrix(c("w1", "w1", "w2", "w2")),
                       list("w1", "w1", "w2", c("w1", "w2")))) {
    df <- .structural_data()
    df$wave_id <- wave_ids
    inputs <- c(inputs, list(df))
  }
  for (key in c("waves", "waves_expected_present")) {
    check <- .structural_check()
    check$waves <- NULL
    check[[key]] <- if (key == "waves") "w1" else "w2"
    for (df in inputs) .expect_structural_unevaluable(check, df, "wave_id")
    df <- .structural_data()
    df$wave_id <- factor(df$wave_id)
    expect_true(.structural_validation(check, df)$results[[1]]$passed)
  }
  check <- .structural_check()
  check$waves <- NULL
  check$waves_expected_present <- "all"
  for (df in inputs) .expect_structural_unevaluable(check, df, "wave_id")
})

test_that("all-NA all scopes check every row and allow legitimate empty data", {
  for (scope in list("all", list("all"))) {
    check <- .structural_check()
    check$waves <- scope
    expect_false(.structural_validation(check)$results[[1]]$passed)
    for (df in list(data.frame(s005 = c(NA_real_, NA_real_)),
                    data.frame(s005 = c(NA_character_, NA_character_)),
                    data.frame(s005 = numeric()))) {
      expect_true(.structural_validation(check, df)$results[[1]]$passed)
    }
    expect_false(.structural_validation(check, data.frame(s005 = c(NA, 1)))$results[[1]]$passed)
    expect_false(.structural_validation(check, data.frame(s005 = c(NA, "yes")))$results[[1]]$passed)
  }
  check <- .structural_check()
  .expect_structural_unevaluable(check,
    data.frame(wave_id = character(), s005 = numeric()), "w1")
})

test_that("expected-present scopes require some data in each requested wave", {
  check <- .structural_check(c("005", "006"))
  check$waves <- NULL
  check$waves_expected_present <- list("w2")
  expect_true(.structural_validation(check)$results[[1]]$passed)
  df <- .structural_data()
  df$s006 <- NA_real_
  result <- .structural_validation(check, df)$results[[1]]
  expect_false(result$passed)
  expect_match(result$detail, "s006 all-NA in expected-present wave w2", fixed = TRUE)
  check$waves_expected_present <- c("w2", "w1")
  result <- .structural_validation(check)$results[[1]]
  expect_false(result$passed)
  expect_match(result$detail, "expected-present wave w1", fixed = TRUE)
  for (scope in list("all", list("all"))) {
    check$waves_expected_present <- scope
    expect_false(.structural_validation(check)$results[[1]]$passed)
    df <- .structural_data()
    df$s005[1] <- 1
    df$s006[1] <- 2
    expect_true(.structural_validation(check, df)$results[[1]]$passed)
    df$wave_id <- NULL
    .expect_structural_unevaluable(check, df, "wave_id")
    .expect_structural_unevaluable(check,
      data.frame(wave_id = character(), s005 = numeric(), s006 = numeric()))
  }
})

test_that("all-NA and expected-present predicates both apply after preflight", {
  check <- .structural_check()
  check$waves_expected_present <- "w2"
  expect_true(.structural_validation(check)$results[[1]]$passed)
  check$waves_expected_present <- "w1"
  expect_false(.structural_validation(check)$results[[1]]$passed)
  df <- .structural_data()
  df$s005[1] <- 1
  expect_false(.structural_validation(check, df)$results[[1]]$passed)
  check$variables <- c("005", "absent_column")
  .expect_structural_unevaluable(check, df, "absent_column")
  check$variables <- "005"
  check$waves_expected_present <- "absent_wave"
  .expect_structural_unevaluable(check, df, "absent_wave")
  check$waves <- c("w1", "absent_na_wave")
  check$waves_expected_present <- "w1"
  .expect_structural_unevaluable(check, pattern = "absent_na_wave")
})

test_that("structural complements validate present waves and exclude them from all-NA", {
  for (key in c("expect_elsewhere", "expect")) {
    check <- .structural_check()
    check$waves_expected_present <- "w2"
    check[[key]] <- "all_na"
    check$waves <- "w2"
    expect_true(.structural_validation(check)$results[[1]]$passed)
    check["waves"] <- list(NULL)
    expect_true(.structural_validation(check)$results[[1]]$passed)
    df <- .structural_data()
    df$s005[1] <- 3
    expect_false(.structural_validation(check, df)$results[[1]]$passed)
    check$waves_expected_present <- c("w1", "w2")
    expect_true(.structural_validation(check, df)$results[[1]]$passed)
    for (scope in list("all", list("all"))) {
      check$waves_expected_present <- scope
      expect_true(.structural_validation(check, df)$results[[1]]$passed)
    }
    check$waves_expected_present <- c("w2", "absent_wave")
    .expect_structural_unevaluable(check, pattern = "absent_wave")
  }
  check <- .structural_check()
  check$waves_expected_present <- "w2"
  check$waves <- "w2"
  check$expect_elsewhere <- "other"
  check$expect <- "all_na"
  expect_false(.structural_validation(check)$results[[1]]$passed)
  check["expect_elsewhere"] <- list(NULL)
  expect_true(.structural_validation(check)$results[[1]]$passed)
})

test_that("structural failures and unavailable inputs preserve severity", {
  for (severity in c("error", "warning", "info")) {
    check <- .structural_check(severity = severity)
    check$waves <- "w2"
    result <- .structural_validation(check)
    expect_false(result$results[[1]]$passed)
    expect_identical(result$results[[1]]$severity, severity)
    expect_identical(result$error_count, as.integer(severity == "error"))
    expect_identical(result$error_skips, character())
    check$variables <- "absent_column"
    .expect_structural_unevaluable(check, pattern = "absent_column")
  }
})

.structural_merge_fixture <- function(check, .local_envir = parent.frame()) {
  root <- withr::local_tempdir("lissr_structural_", .local_envir = .local_envir)
  data_dir <- file.path(root, "data")
  dir.create(data_dir)
  for (wave in c("yy01a", "yy02b")) {
    df <- data.frame(nomem_encr = 1:2)
    df[[paste0(wave, "005")]] <- if (wave == "yy01a") c(NA_real_, NA_real_) else c(1, NA)
    haven::write_sav(df, file.path(data_dir, paste0(wave, "_EN_1.0p.sav")))
  }
  recipe <- list(
    meta = list(module = "yy", module_label = "structural fixture", schema_version = "1.0.0",
                recipe_version = "test", created = "test", source_spec = "test",
                covered_waves = list("yy01a", "yy02b")),
    global = list(id_variable = "nomem_encr", wave_variable = "wave_id",
                  year_variable = "wave_year", labelled_policy = "to_numeric",
                  missing_variable_policy = "warn_and_create_na", strip_label_whitespace = TRUE),
    wave_index = list(list(id = "yy01a", year = 2001L, file_pattern = "yy01a_*"),
                      list(id = "yy02b", year = 2002L, file_pattern = "yy02b_*")),
    validation_checks = list(check), logging = list(summary_artifact = list(enabled = TRUE)))
  list(recipe = recipe, data_dir = data_dir, output_dir = file.path(root, "output"))
}

test_that("strict and report modes retain structural diagnostics and protect output", {
  missing_target <- .structural_check(c("005", "absent_column"))
  missing_target$waves <- "yy01a"
  missing_wave <- .structural_check()
  missing_wave$waves <- c("yy01a", "absent_wave")
  malformed_scope <- .structural_check()
  malformed_scope$waves <- "yy01a"
  malformed_scope$waves_expected_present <- character()
  violated <- .structural_check()
  violated$waves <- "yy02b"
  for (check in list(missing_target, missing_wave, malformed_scope, violated)) {
    fx <- .structural_merge_fixture(check)
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
    report <- readLines(file.path(fx$output_dir, "yy_merge_report.txt"))
    expect_true(any(grepl("Valid for analysis: FALSE", report, fixed = TRUE)))
    expect_true(any(grepl("[error] STRUCTURAL:", report, fixed = TRUE)))
    detail <- result$validation[[1]]$detail
    expect_type(detail, "character")
    if (is.character(detail)) expect_true(any(grepl(detail, report, fixed = TRUE)))
    expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
  }
})

test_that("valid structural scopes preserve strict output and actual values", {
  for (complement in c(FALSE, TRUE)) {
    check <- .structural_check()
    check$waves <- "yy01a"
    check$waves_expected_present <- "yy02b"
    if (complement) check$expect_elsewhere <- "all_na"
    fx <- .structural_merge_fixture(check)
    result <- suppressWarnings(suppressMessages(
      merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE)))
    expect_true(result$valid_for_analysis)
    expect_true(result$validation[[1]]$passed)
    expect_equal(as.numeric(result$data$s005), c(NA, NA, 1, NA))
    expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
  }
})
