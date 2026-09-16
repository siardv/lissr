.absence_check <- function(targets = "005", severity = "error") {
  list(check_id = "ABSENCE", type = "value_absence", severity = severity,
       variables = targets, forbidden_values = 99)
}

.absence_data <- function() {
  data.frame(wave_id = c("w1", "w1", "w2"), s005 = c(1, NA, 99),
             s006 = c(2, NA, 98))
}

.absence_validation <- function(check, df = .absence_data()) {
  suppressWarnings(suppressMessages(
    lissr:::run_validations(df, list(check), list())))
}

.expect_absence_unevaluable <- function(check, df = .absence_data(), pattern = NULL) {
  result <- .absence_validation(check, df)
  expect_identical(result$results[[1]]$passed, NA)
  expect_identical(result$results[[1]]$severity, check$severity)
  expect_identical(result$error_skips,
                   if (check$severity == "error") "ABSENCE" else character())
  if (!is.null(pattern)) {
    detail <- result$results[[1]]$detail
    expect_match(if (is.null(detail)) "" else detail, pattern)
  }
  invisible(result)
}

test_that("absence checks require every named target and requested wave", {
  for (targets in list("absent_column", c("005", "absent_column"))) {
    .expect_absence_unevaluable(.absence_check(targets), pattern = "absent_column")
  }
  for (waves in list("absent_wave", c("w1", "absent_wave"))) {
    check <- .absence_check()
    check$waves <- waves
    .expect_absence_unevaluable(check, pattern = "absent_wave")
  }
  check <- .absence_check("absent_column")
  check$waves <- "absent_wave"
  result <- .expect_absence_unevaluable(check)
  detail <- result$results[[1]]$detail
  expect_match(if (is.null(detail)) "" else detail, "absent_column")
  expect_match(if (is.null(detail)) "" else detail, "absent_wave")
})

test_that("absence target aliases preserve suffix and exact-name resolution", {
  keys <- c("suffixes", "variables", "scope", "applies_to", "items", "stems",
            "variable", "column")
  for (key in keys) {
    check <- .absence_check()
    check$variables <- NULL
    check[[key]] <- list("005")
    expect_false(.absence_validation(check)$results[[1]]$passed)
    check[[key]] <- c("005", "absent_column")
    .expect_absence_unevaluable(check, pattern = "absent_column")
  }
  for (column in c("s005", "stem_005", "q005", "Q005", "005")) {
    df <- stats::setNames(data.frame(99), column)
    expect_false(.absence_validation(.absence_check(), df)$results[[1]]$passed)
  }
  for (target in c("s005", "q005", "Q005")) {
    expect_false(.absence_validation(.absence_check(target))$results[[1]]$passed)
  }
  df <- data.frame(s005 = 99, check.names = FALSE)
  df[["005"]] <- 1
  expect_true(.absence_validation(.absence_check(), df)$results[[1]]$passed)
  expect_false(.absence_validation(.absence_check("s005"), df)$results[[1]]$passed)
  expect_false(.absence_validation(.absence_check(5), data.frame(s5 = 99))$results[[1]]$passed)
})

test_that("absence type and forbidden-value aliases preserve numeric and text matching", {
  types <- c("value_absence", "assert_absent_values", "none_equal", "sentinel_absence",
             "no_residual_sentinels", "assert_no_values", "value_absence_check",
             "value_restriction")
  for (type in types) {
    check <- .absence_check()
    check$type <- type
    expect_false(.absence_validation(check)$results[[1]]$passed)
  }
  for (key in c("forbidden_values", "forbidden_value", "sentinel_values", "codes", "value")) {
    for (value in list(99, "099", "refused")) {
      check <- .absence_check()
      check$forbidden_values <- NULL
      check[[key]] <- value
      df <- data.frame(s005 = c(NA, if (identical(value, "refused")) "refused" else "99"))
      expect_false(.absence_validation(check, df)$results[[1]]$passed)
    }
  }
  for (key in c("forbidden_values", "sentinel_values", "value")) {
    check <- .absence_check()
    check$forbidden_values <- NULL
    check[[key]] <- 99
    check$targets <- list(list(variables = "005"))
    expect_false(.absence_validation(check)$results[[1]]$passed)
    check$targets[[1]]$value <- 98
    expect_true(.absence_validation(check)$results[[1]]$passed)
  }
})

test_that("absence items expand at either parent or block scope", {
  for (block_scope in c(FALSE, TRUE)) {
    check <- .absence_check()
    check$variables <- NULL
    check$items <- list("005-007")
    if (block_scope) {
      check$targets <- list(list(items = check$items))
      check$items <- NULL
    }
    df <- data.frame(s005 = 1, s006 = 2, s007 = 3)
    expect_true(.absence_validation(check, df)$results[[1]]$passed)
    df$s006 <- 99
    expect_false(.absence_validation(check, df)$results[[1]]$passed)
    df$s006 <- NULL
    .expect_absence_unevaluable(check, df, "006")
  }
})

test_that("absence items preserve exact and suffix lookup before range expansion", {
  for (column in c("005-006", "s005-006")) {
    for (block_scope in c(FALSE, TRUE)) {
      check <- .absence_check()
      check$variables <- NULL
      check$items <- "005-006"
      if (block_scope) {
        check$targets <- list(list(items = check$items))
        check$items <- NULL
      }
      df <- stats::setNames(data.frame(99), column)
      expect_false(.absence_validation(check, df)$results[[1]]$passed)
      df[[column]] <- 1
      expect_true(.absence_validation(check, df)$results[[1]]$passed)
      if (block_scope) check$targets[[1]]$items <- c("005-006", "007-008")
      else check$items <- c("005-006", "007-008")
      df$s007 <- 2
      .expect_absence_unevaluable(check, df, "008")
      df$s008 <- 99
      expect_false(.absence_validation(check, df)$results[[1]]$passed)
    }
  }
})

test_that("absence block targets fall back only when the target keys are omitted", {
  for (key in c("targets", "checks")) {
    check <- .absence_check("005")
    check[[key]] <- list(list(value = 99))
    expect_false(.absence_validation(check)$results[[1]]$passed)
    check[[key]] <- list(list(variables = "006", value = 99))
    expect_true(.absence_validation(check)$results[[1]]$passed)
    for (targets in list("absent_column", c("006", "absent_column"), NULL,
                         character(), list())) {
      check[[key]][[1]]["variables"] <- list(targets)
      .expect_absence_unevaluable(check)
    }
  }
  check <- .absence_check("absent_parent")
  check$targets <- list(list(variables = "006", value = 99))
  expect_true(.absence_validation(check)$results[[1]]$passed)
  check$targets <- list(list(variables_documentation = "006", value = 99))
  .expect_absence_unevaluable(check, pattern = "absent_parent")
})

test_that("absence scopes are preflighted across all blocks before value matching", {
  check <- .absence_check()
  check$targets <- list(list(variables = "005"), list(variables = "absent_column"))
  .expect_absence_unevaluable(check, pattern = "absent_column")
  check$targets[[2]] <- list(variables = "006", waves = "absent_wave")
  .expect_absence_unevaluable(check, pattern = "absent_wave")
  check$targets[[2]] <- list(variables = "006", exclude_variables = list(list("006")))
  .expect_absence_unevaluable(check, pattern = "exclu")
})

test_that("malformed absence target declarations and block containers cannot pass", {
  invalid_targets <- list(NULL, character(), list(), " ", NA_character_, TRUE,
                          Inf, c("005", ""), list("005", NULL), list(list("005")),
                          matrix("005"), matrix(list("005")))
  for (targets in invalid_targets) {
    .expect_absence_unevaluable(.absence_check(targets))
  }
  invalid_blocks <- list(NULL, list(), character(), "005", list("005"),
                         list(NULL), list(list()), list(list("005")),
                         list(list(variables = "005"), "006"),
                         matrix(list(list(variables = "005"))))
  for (key in c("targets", "checks")) {
    for (blocks in invalid_blocks) {
      check <- .absence_check()
      check[key] <- list(blocks)
      .expect_absence_unevaluable(check)
    }
  }
  check <- .absence_check()
  check$variables <- NULL
  check$variables_documentation <- "005"
  .expect_absence_unevaluable(check)
  check <- .absence_check()
  check$targets_documentation <- list(list(variables = "absent_column"))
  expect_false(.absence_validation(check)$results[[1]]$passed)
})

test_that("absence exclusions preserve precedence and optional unknown names", {
  check <- .absence_check(c("005", "006"))
  check$targets <- list(list(value = c(98, 99), exclude_variables = "005"))
  expect_false(.absence_validation(check)$results[[1]]$passed)
  check$exclude_suffixes <- c("005", "006", "optional_missing")
  expect_true(.absence_validation(check)$results[[1]]$passed)
  check$exclude_variables <- "005"
  result <- .absence_validation(check)
  expect_false(result$results[[1]]$passed)
  expect_match(result$results[[1]]$detail, "s006")
  check$exclude_variables <- list()
  result <- .absence_validation(check)
  expect_false(result$results[[1]]$passed)
  expect_match(result$results[[1]]$detail, "s005")
  check <- .absence_check("006")
  check$exclude_variables <- "optional_missing"
  expect_true(.absence_validation(check)$results[[1]]$passed)
  check <- .absence_check(c("005", "absent_column"))
  check$exclude_variables <- c("005", "absent_column")
  .expect_absence_unevaluable(check, pattern = "absent_column")
  for (exclusion in list("", NA_character_, list("005", NULL), list(list("005")),
                         matrix("005"), TRUE)) {
    check <- .absence_check()
    check$exclude_variables <- exclusion
    .expect_absence_unevaluable(check, pattern = "exclu")
  }
})

test_that("absence wave aliases combine valid parent and block scopes", {
  for (key in c("in_waves", "waves", "wave_filter", "must_be_na_in")) {
    for (scope in list("w1", list("w1"), c(selected = "w1"))) {
      check <- .absence_check()
      check[[key]] <- scope
      expect_true(.absence_validation(check)$results[[1]]$passed)
      check[[key]] <- "w2"
      expect_false(.absence_validation(check)$results[[1]]$passed)
      check$targets <- list(list(variables = "005", waves = "w1"))
      expect_true(.absence_validation(check)$results[[1]]$passed)
    }
    check <- .absence_check()
    check$waves <- "w1"
    check$targets <- list(list(variables = "005"))
    check$targets[[1]][[key]] <- "absent_wave"
    .expect_absence_unevaluable(check, pattern = "absent_wave")
  }
  check <- .absence_check()
  check$in_waves <- "w1"
  check$waves <- "w2"
  expect_true(.absence_validation(check)$results[[1]]$passed)
  for (scope in list("all", list("all"))) {
    check$in_waves <- scope
    expect_false(.absence_validation(check)$results[[1]]$passed)
  }
  check <- .absence_check()
  check$waves_documentation <- "w1"
  expect_false(.absence_validation(check)$results[[1]]$passed)
})

test_that("absence allowed waves use the validated complement and override ordinary scopes", {
  check <- .absence_check()
  check$waves_allowed <- "w2"
  expect_true(.absence_validation(check)$results[[1]]$passed)
  check$waves_allowed <- list("w1")
  expect_false(.absence_validation(check)$results[[1]]$passed)
  check$waves_allowed <- c("w1", "absent_wave")
  .expect_absence_unevaluable(check, pattern = "absent_wave")
  check$waves_allowed <- "w2"
  check$waves <- character()
  check$targets <- list(list(variables = "005", waves = "absent_wave"))
  expect_true(.absence_validation(check)$results[[1]]$passed)
  df <- .absence_data()
  df$wave_id <- NULL
  for (scope in list("all", list("all"))) {
    check$waves_allowed <- scope
    expect_true(.absence_validation(check, df)$results[[1]]$passed)
  }
})

test_that("malformed absence scopes and unknown row membership are unevaluable", {
  invalid <- list(NULL, character(), list(), c("w1", ""), c("w1", " "),
                  c("w1", NA_character_), list("w1", NULL), list(list("w1")),
                  matrix("w1"), matrix(list("w1")), 1, c("all", "w1"))
  for (key in c("waves", "waves_allowed")) {
    for (scope in invalid) {
      check <- .absence_check()
      check[key] <- list(scope)
      .expect_absence_unevaluable(check, pattern = "wave")
    }
    check <- .absence_check()
    check[[key]] <- "w1"
    for (wave_ids in list(NULL, c("w1", "w1", NA_character_), c("w1", "w1", " "),
                          list("w1", "w1", c("w1", "w2")))) {
      df <- .absence_data()
      df$wave_id <- wave_ids
      .expect_absence_unevaluable(check, df, "wave_id")
    }
  }
})

test_that("absence numeric selectors and empty predicates retain their meaning", {
  for (selector in c("numeric", "all_numeric")) {
    check <- .absence_check(selector)
    expect_true(.absence_validation(check, data.frame(s005 = 1, text = "99"))$results[[1]]$passed)
    expect_false(.absence_validation(check, data.frame(s005 = 99, text = "ok"))$results[[1]]$passed)
    .expect_absence_unevaluable(check, data.frame(text = "99"), selector)
  }
  for (df in list(data.frame(s005 = c(NA_real_, NA_real_)), data.frame(s005 = numeric()))) {
    check <- .absence_check()
    expect_true(.absence_validation(check, df)$results[[1]]$passed)
    for (scope in list("all", list("all"))) {
      check$waves <- scope
      expect_true(.absence_validation(check, df)$results[[1]]$passed)
    }
  }
  check <- .absence_check()
  check$waves <- "w1"
  .expect_absence_unevaluable(check, data.frame(wave_id = character(), s005 = numeric()), "w1")
  check <- .absence_check()
  check$forbidden_values <- numeric()
  expect_true(.absence_validation(check)$results[[1]]$passed)
  nested <- data.frame(wave_id = c("w1", "w2"))
  nested$s005 <- I(list(c(1, 2), c(3, 4)))
  expect_true(.absence_validation(check, nested)$results[[1]]$passed)
  check$variables <- "absent_column"
  .expect_absence_unevaluable(check, pattern = "absent_column")
  .expect_absence_unevaluable(check, nested, "absent_column")
  check$variables <- "005"
  check$waves <- "absent_wave"
  .expect_absence_unevaluable(check, pattern = "absent_wave")
  .expect_absence_unevaluable(check, nested, "absent_wave")
})

test_that("absence failures and unevaluable outcomes preserve the declared severity", {
  for (severity in c("error", "warning", "info")) {
    result <- .absence_validation(.absence_check(severity = severity))
    expect_false(result$results[[1]]$passed)
    expect_identical(result$results[[1]]$severity, severity)
    expect_identical(result$error_count, as.integer(severity == "error"))
    expect_identical(result$error_skips, character())
    result <- .expect_absence_unevaluable(.absence_check("absent_column", severity))
    expect_identical(result$error_count, 0L)
  }
})

.absence_merge_fixture <- function(check, .local_envir = parent.frame()) {
  root <- withr::local_tempdir("lissr_absence_", .local_envir = .local_envir)
  data_dir <- file.path(root, "data")
  dir.create(data_dir)
  for (wave in c("yy01a", "yy02b")) {
    df <- data.frame(nomem_encr = 1:2)
    df[[paste0(wave, "005")]] <- if (wave == "yy01a") c(1, NA) else c(99, 99)
    haven::write_sav(df, file.path(data_dir, paste0(wave, "_EN_1.0p.sav")))
  }
  recipe <- list(
    meta = list(module = "yy", module_label = "absence fixture", schema_version = "1.0.0",
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

test_that("strict and report modes preserve absence diagnostics and protect output", {
  missing_target <- .absence_check(c("005", "absent_column"))
  missing_target$forbidden_values <- 98
  missing_wave <- .absence_check()
  missing_wave$waves_allowed <- "absent_wave"
  missing_wave$forbidden_values <- 98
  malformed_block <- .absence_check()
  malformed_block$targets <- list(list(variables = character()))
  malformed_block$forbidden_values <- 98
  for (check in list(missing_target, missing_wave,
                     malformed_block, .absence_check())) {
    fx <- .absence_merge_fixture(check)
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
    expect_true(any(grepl("[error] ABSENCE:", report, fixed = TRUE)))
    detail <- result$validation[[1]]$detail
    expect_type(detail, "character")
    if (is.character(detail)) expect_true(any(grepl(detail, report, fixed = TRUE)))
    expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
  }
})

test_that("valid absence scopes preserve strict output and merged values", {
  for (key in c("waves", "waves_allowed")) {
    check <- .absence_check()
    check[[key]] <- if (key == "waves") "yy01a" else "yy02b"
    fx <- .absence_merge_fixture(check)
    result <- suppressWarnings(suppressMessages(
      merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE)))
    expect_true(result$valid_for_analysis)
    expect_true(result$validation[[1]]$passed)
    expect_equal(as.numeric(result$data$s005), c(1, NA, 99, 99))
    expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
  }
})
