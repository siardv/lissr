.value_present_check <- function(targets = "005", severity = "error") {
  list(check_id = "VALUE_PRESENT", type = "value_present", severity = severity,
       variables = targets, value = 1, waves = "w1")
}

.value_present_data <- function() {
  data.frame(wave_id = c("w1", "w1", "w2", "w2"),
             s005 = c(1, NA, 2, NA), s006 = c(NA, 2, 1, NA))
}

.value_present_validation <- function(check, df = .value_present_data()) {
  suppressWarnings(suppressMessages(
    lissr:::run_validations(df, list(check), list())))
}

.expect_value_present_unevaluable <- function(check, df = .value_present_data(),
                                            pattern = NULL) {
  result <- .value_present_validation(check, df)
  expect_identical(result$results[[1]]$passed, NA)
  expect_identical(result$results[[1]]$severity, check$severity)
  expect_identical(result$error_skips,
                   if (check$severity == "error") "VALUE_PRESENT" else character())
  expect_identical(result$error_count, 0L)
  if (!is.null(pattern)) {
    detail <- result$results[[1]]$detail
    expect_match(if (is.null(detail)) "" else detail, pattern)
  }
  invisible(result)
}

test_that("value presence resolves all required targets and waves before matching", {
  for (targets in list("absent_column", c("005", "absent_column"))) {
    .expect_value_present_unevaluable(.value_present_check(targets),
                                      pattern = "absent_column")
  }
  for (waves in list("absent_wave", c("w1", "absent_wave"),
                    c("w2", "absent_wave"))) {
    check <- .value_present_check()
    check$waves <- waves
    .expect_value_present_unevaluable(check, pattern = "absent_wave")
  }
  check <- .value_present_check(c("005", "absent_column"))
  check$waves <- "w2"
  .expect_value_present_unevaluable(check, pattern = "absent_column")
})

test_that("value presence keeps any-target matching within every selected wave", {
  for (type in c("value_present", "value_present_per_wave")) {
    check <- .value_present_check(c("005", "006"))
    check$type <- type
    check$waves <- c("w1", "w2")
    expect_true(.value_present_validation(check)$results[[1]]$passed)
    check$variables <- "005"
    result <- .value_present_validation(check)$results[[1]]
    expect_false(result$passed)
    expect_match(result$detail, "value 1 absent in wave w2", fixed = TRUE)
    check$waves <- "w1"
    expect_true(.value_present_validation(check)$results[[1]]$passed)
  }
  check <- .value_present_check()
  df <- .value_present_data()
  df$s005 <- NA_real_
  expect_false(.value_present_validation(check, df)$results[[1]]$passed)
})

test_that("value-presence target aliases retain exact and suffix lookup", {
  for (key in c("suffixes", "variables", "scope", "applies_to", "items", "stems",
                "variable", "column")) {
    check <- .value_present_check()
    check$variables <- NULL
    check[[key]] <- list("005")
    expect_true(.value_present_validation(check)$results[[1]]$passed)
    check[[key]] <- c("005", "absent_column")
    .expect_value_present_unevaluable(check, pattern = "absent_column")
  }
  for (column in c("s005", "stem_005", "q005", "Q005", "005")) {
    df <- data.frame(wave_id = "w1")
    df[[column]] <- 1
    expect_true(.value_present_validation(.value_present_check(), df)$results[[1]]$passed)
  }
  for (target in c("s005", "q005", "Q005")) {
    expect_true(.value_present_validation(.value_present_check(target))$results[[1]]$passed)
  }
  df <- .value_present_data()
  df[["005"]] <- 99
  expect_false(.value_present_validation(.value_present_check(), df)$results[[1]]$passed)
  expect_true(.value_present_validation(.value_present_check("s005"), df)$results[[1]]$passed)
  expect_true(.value_present_validation(.value_present_check(5),
    data.frame(wave_id = "w1", s5 = 1))$results[[1]]$passed)
})

test_that("value-presence targets retain first non-null key precedence", {
  check <- .value_present_check("absent_column")
  check$suffixes <- "005"
  expect_true(.value_present_validation(check)$results[[1]]$passed)
  check["suffixes"] <- list(NULL)
  .expect_value_present_unevaluable(check, pattern = "absent_column")
  check["variables"] <- list(NULL)
  check$variable <- "005"
  check$column <- "absent_column"
  expect_true(.value_present_validation(check)$results[[1]]$passed)
  check$variables <- character()
  .expect_value_present_unevaluable(check)
  check <- .value_present_check()
  check$variables <- NULL
  check$variables_documentation <- "005"
  .expect_value_present_unevaluable(check)
})

test_that("value-presence targets reject empty and malformed declarations", {
  invalid <- list(NULL, character(), list(), " ", c("005", ""),
                  c("005", NA_character_), list("005", NULL), list(list("005")),
                  matrix("005"), matrix(list("005")), TRUE, Inf)
  for (targets in invalid) {
    check <- .value_present_check()
    check["variables"] <- list(targets)
    .expect_value_present_unevaluable(check)
  }
})

test_that("value-presence numeric selectors normalize scalar and list forms", {
  for (selector in c("numeric", "all_numeric")) {
    for (targets in list(selector, list(selector))) {
      check <- .value_present_check(targets)
      check$waves <- "all"
      expect_true(.value_present_validation(check)$results[[1]]$passed)
      df <- data.frame(wave_id = "w1", text = "1")
      .expect_value_present_unevaluable(check, df, selector)
    }
  }
})

test_that("value-presence items preserve literal range names without expanding", {
  check <- .value_present_check()
  check$variables <- NULL
  check$items <- list("005-006")
  .expect_value_present_unevaluable(check, pattern = "005-006")
  for (column in c("005-006", "s005-006")) {
    df <- .value_present_data()
    df[[column]] <- 1
    expect_true(.value_present_validation(check, df)$results[[1]]$passed)
  }
})

test_that("value-presence wave filters exclude out-of-scope observations", {
  for (key in c("wave_filter", "waves")) {
    for (scope in list("w1", list("w1"), c(selected = "w1"))) {
      check <- .value_present_check()
      check$waves <- NULL
      check[[key]] <- scope
      expect_true(.value_present_validation(check)$results[[1]]$passed)
      check[[key]] <- "w2"
      expect_false(.value_present_validation(check)$results[[1]]$passed)
    }
  }
  for (scope in list("all", list("all"), c(selected = "all"))) {
    check <- .value_present_check()
    check$waves <- scope
    expect_false(.value_present_validation(check)$results[[1]]$passed)
    check$variables <- c("005", "006")
    expect_true(.value_present_validation(check)$results[[1]]$passed)
  }
  check <- .value_present_check()
  check$waves <- NULL
  expect_false(.value_present_validation(check)$results[[1]]$passed)
})

test_that("value presence keeps wave-filter precedence and supported wave keys", {
  check <- .value_present_check()
  check$wave_filter <- "w1"
  check$waves <- "w2"
  expect_true(.value_present_validation(check)$results[[1]]$passed)
  check$wave_filter <- "all"
  expect_false(.value_present_validation(check)$results[[1]]$passed)
  check["wave_filter"] <- list(NULL)
  .expect_value_present_unevaluable(check, pattern = "wave")
  check <- .value_present_check()
  check$waves <- NULL
  check$waves_documentation <- "w1"
  check$wave_filter_documentation <- "w1"
  check$in_waves <- "w1"
  check$must_be_na_in <- "w1"
  expect_false(.value_present_validation(check)$results[[1]]$passed)
})

test_that("malformed value-presence wave scopes cannot fall through or pass", {
  invalid <- list(NULL, character(), list(), " ", c("w1", ""),
                  c("w1", NA_character_), list("w1", NULL), list(list("w1")),
                  matrix("w1"), matrix(list("w1")), 1, c("all", "w1"))
  for (key in c("wave_filter", "waves")) {
    for (scope in invalid) {
      check <- .value_present_check()
      check[key] <- list(scope)
      .expect_value_present_unevaluable(check, pattern = "wave")
    }
  }
})

test_that("every value-presence scope requires observed and usable wave membership", {
  absent_ids <- .value_present_data()
  absent_ids$wave_id <- NULL
  inputs <- list(absent_ids, .value_present_data()[FALSE, ])
  for (wave_ids in list(c("w1", "w1", "w2", NA_character_),
                       c("w1", "w1", "w2", " "),
                       matrix(c("w1", "w1", "w2", "w2")),
                       list("w1", "w1", "w2", c("w1", "w2")))) {
    df <- .value_present_data()
    df$wave_id <- wave_ids
    inputs <- c(inputs, list(df))
  }
  for (scope in list(NULL, "all", list("all"), "w1")) {
    check <- .value_present_check()
    check$waves <- scope
    for (df in inputs) .expect_value_present_unevaluable(check, df, "wave")
  }
  df <- .value_present_data()
  df$wave_id <- factor(df$wave_id)
  expect_true(.value_present_validation(.value_present_check(), df)$results[[1]]$passed)
  df$wave_id <- c(1, 1, 2, 2)
  check <- .value_present_check()
  check$waves <- "1"
  expect_true(.value_present_validation(check, df)$results[[1]]$passed)
})

test_that("value presence preserves numeric coercion and multiple-value matching", {
  for (values in list(1, list(1), "1", list("1"))) {
    check <- .value_present_check()
    check$value <- values
    expect_true(.value_present_validation(check)$results[[1]]$passed)
  }
  for (values in list(c(1, 2), list(1, 2), c("1", "2"))) {
    check <- .value_present_check()
    check$value <- NULL
    check$values <- values
    check$waves <- "all"
    expect_true(.value_present_validation(check)$results[[1]]$passed)
  }
  check <- .value_present_check()
  check$value <- 99
  check$values <- 1
  expect_false(.value_present_validation(check)$results[[1]]$passed)
  check["value"] <- list(NULL)
  expect_true(.value_present_validation(check)$results[[1]]$passed)
  check$values <- NULL
  expect_false(.value_present_validation(check)$results[[1]]$passed)
  check$value <- numeric()
  check$values <- 1
  expect_false(.value_present_validation(check)$results[[1]]$passed)
  check$value <- NA_real_
  expect_true(.value_present_validation(check)$results[[1]]$passed)
  check$value <- "not numeric"
  expect_true(.value_present_validation(check)$results[[1]]$passed)
  check <- .value_present_check()
  df <- .value_present_data()
  df$s005 <- as.character(df$s005)
  expect_true(.value_present_validation(check, df)$results[[1]]$passed)
})

test_that("value-presence failures and unavailable inputs retain severity", {
  for (severity in c("error", "warning", "info")) {
    check <- .value_present_check(severity = severity)
    check$waves <- "w2"
    result <- .value_present_validation(check)
    expect_false(result$results[[1]]$passed)
    expect_identical(result$results[[1]]$severity, severity)
    expect_identical(result$error_count, as.integer(severity == "error"))
    expect_identical(result$error_skips, character())
    check$variables <- "absent_column"
    .expect_value_present_unevaluable(check, pattern = "absent_column")
  }
})

.value_present_merge_fixture <- function(check, .local_envir = parent.frame()) {
  root <- withr::local_tempdir("lissr_value_present_", .local_envir = .local_envir)
  data_dir <- file.path(root, "data")
  dir.create(data_dir)
  for (wave in c("yy01a", "yy02b")) {
    df <- data.frame(nomem_encr = 1:2)
    df[[paste0(wave, "005")]] <- if (wave == "yy01a") c(1, NA) else c(2, NA)
    haven::write_sav(df, file.path(data_dir, paste0(wave, "_EN_1.0p.sav")))
  }
  recipe <- list(
    meta = list(module = "yy", module_label = "value presence fixture", schema_version = "1.0.0",
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

test_that("strict and report modes retain value-presence diagnostics and protect output", {
  missing_target <- .value_present_check(c("005", "absent_column"))
  missing_target$waves <- "yy01a"
  missing_wave <- .value_present_check()
  missing_wave$waves <- c("yy01a", "absent_wave")
  malformed_scope <- .value_present_check()
  malformed_scope$waves <- character()
  violated <- .value_present_check()
  violated$waves <- "yy02b"
  for (check in list(missing_target, missing_wave, malformed_scope, violated)) {
    fx <- .value_present_merge_fixture(check)
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
    expect_true(any(grepl("[error] VALUE_PRESENT:", report, fixed = TRUE)))
    detail <- result$validation[[1]]$detail
    expect_type(detail, "character")
    if (is.character(detail)) expect_true(any(grepl(detail, report, fixed = TRUE)))
    expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
  }
})

test_that("valid value-presence scopes preserve strict output and actual values", {
  for (type in c("value_present", "value_present_per_wave")) {
    check <- .value_present_check()
    check$type <- type
    check$waves <- "yy01a"
    fx <- .value_present_merge_fixture(check)
    result <- suppressWarnings(suppressMessages(
      merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE)))
    expect_true(result$valid_for_analysis)
    expect_true(result$validation[[1]]$passed)
    expect_equal(as.numeric(result$data$s005), c(1, NA, 2, NA))
    expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
  }
})
