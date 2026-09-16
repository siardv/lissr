.na_rate_check <- function(targets = "005", severity = "error") {
  list(check_id = "NA_RATE", type = "na_rate", severity = severity,
       variables = targets, threshold = 0.5, waves = "w1")
}

.na_rate_data <- function() {
  data.frame(wave_id = c("w1", "w1", "w2", "w2"),
             s005 = c(1, NA, NA, NA), s006 = c(1, 2, NA, NA))
}

.na_rate_validation <- function(check, df = .na_rate_data()) {
  suppressWarnings(suppressMessages(
    lissr:::run_validations(df, list(check), list())))
}

.expect_na_rate_unevaluable <- function(check, df = .na_rate_data(), pattern = NULL) {
  result <- .na_rate_validation(check, df)
  expect_identical(result$results[[1]]$passed, NA)
  expect_identical(result$results[[1]]$severity, check$severity)
  expect_identical(result$error_skips,
                   if (check$severity == "error") "NA_RATE" else character())
  expect_identical(result$error_count, 0L)
  if (!is.null(pattern)) {
    detail <- result$results[[1]]$detail
    expect_match(if (is.null(detail)) "" else detail, pattern)
  }
  invisible(result)
}

test_that("NA rates require complete scopes before values or conditions are tested", {
  for (targets in list("absent_column", c("005", "absent_column"))) {
    check <- .na_rate_check(targets)
    check$waves <- "w2"
    .expect_na_rate_unevaluable(check, pattern = "absent_column")
    check$condition <- "s006 < 0"
    .expect_na_rate_unevaluable(check, pattern = "absent_column")
  }
  for (waves in list("absent_wave", c("w1", "absent_wave"),
                    c("w2", "absent_wave"))) {
    check <- .na_rate_check()
    check$waves <- waves
    .expect_na_rate_unevaluable(check, pattern = "absent_wave")
    check$condition <- "s006 < 0"
    .expect_na_rate_unevaluable(check, pattern = "absent_wave")
  }
})

test_that("NA-rate target aliases preserve lookup and first non-null precedence", {
  for (key in c("suffixes", "variables", "scope", "items")) {
    check <- .na_rate_check()
    check$variables <- NULL
    check[[key]] <- list("005")
    expect_true(.na_rate_validation(check)$results[[1]]$passed)
    check[[key]] <- c("005", "absent_column")
    .expect_na_rate_unevaluable(check, pattern = "absent_column")
  }
  check <- .na_rate_check("absent_column")
  check$suffixes <- "005"
  check$scope <- "absent_column"
  check$items <- "absent_column"
  expect_true(.na_rate_validation(check)$results[[1]]$passed)
  check["suffixes"] <- list(NULL)
  .expect_na_rate_unevaluable(check, pattern = "absent_column")
  check["variables"] <- list(NULL)
  check$scope <- "005"
  expect_true(.na_rate_validation(check)$results[[1]]$passed)
  check$scope <- character()
  .expect_na_rate_unevaluable(check)
  check <- .na_rate_check()
  check$variables <- NULL
  check$variables_documentation <- "005"
  check$variable <- "005"
  .expect_na_rate_unevaluable(check)

  for (column in c("s005", "stem_005", "q005", "Q005", "005")) {
    df <- data.frame(wave_id = "w1")
    df[[column]] <- 1
    expect_true(.na_rate_validation(.na_rate_check(), df)$results[[1]]$passed)
  }
  for (target in c("s005", "q005", "Q005")) {
    expect_true(.na_rate_validation(.na_rate_check(target))$results[[1]]$passed)
  }
  df <- .na_rate_data()
  df[["005"]] <- NA_real_
  expect_false(.na_rate_validation(.na_rate_check(), df)$results[[1]]$passed)
  expect_true(.na_rate_validation(.na_rate_check("s005"), df)$results[[1]]$passed)
  expect_true(.na_rate_validation(.na_rate_check(list(5)),
    data.frame(wave_id = "w1", s5 = 1))$results[[1]]$passed)
})

test_that("NA-rate targets reject empty and malformed declarations", {
  for (targets in list(NULL, character(), list(), " ", c("005", ""),
                       c("005", NA_character_), list("005", NULL),
                       list(list("005")), matrix("005"), matrix(list("005")),
                       TRUE, Inf)) {
    check <- .na_rate_check()
    check["variables"] <- list(targets)
    .expect_na_rate_unevaluable(check)
  }
})

test_that("NA-rate items expand ranges before lookup while other targets stay literal", {
  df <- .na_rate_data()
  df[["005-006"]] <- NA_real_
  check <- .na_rate_check()
  check$variables <- NULL
  check$items <- list("005-006")
  expect_true(.na_rate_validation(check, df)$results[[1]]$passed)
  df$s006 <- NULL
  .expect_na_rate_unevaluable(check, df, "006")
  for (key in c("suffixes", "variables", "scope")) {
    check <- .na_rate_check()
    check$variables <- NULL
    check[[key]] <- "005-006"
    expect_false(.na_rate_validation(check, df)$results[[1]]$passed)
    .expect_na_rate_unevaluable(check, pattern = "005-006")
  }
  for (target in c("numeric", "all_numeric")) {
    check <- .na_rate_check(target)
    .expect_na_rate_unevaluable(check, pattern = target)
    df[[target]] <- 1
    expect_true(.na_rate_validation(check, df)$results[[1]]$passed)
  }
})

test_that("NA-rate wave filters exclude observations and retain waves precedence", {
  for (key in c("waves", "wave_filter")) {
    for (scope in list("w1", list("w1"), c(selected = "w1"))) {
      check <- .na_rate_check()
      check$waves <- NULL
      check[[key]] <- scope
      expect_true(.na_rate_validation(check)$results[[1]]$passed)
      check[[key]] <- "w2"
      expect_false(.na_rate_validation(check)$results[[1]]$passed)
    }
  }
  check <- .na_rate_check()
  check$wave_filter <- "w2"
  expect_true(.na_rate_validation(check)$results[[1]]$passed)
  check$waves <- "all"
  expect_false(.na_rate_validation(check)$results[[1]]$passed)
  check["waves"] <- list(NULL)
  .expect_na_rate_unevaluable(check, pattern = "wave")
  check <- .na_rate_check()
  check$waves <- NULL
  check$waves_documentation <- "w1"
  check$wave_filter_documentation <- "w1"
  check$in_waves <- "w1"
  expect_false(.na_rate_validation(check)$results[[1]]$passed)
})

test_that("NA-rate explicit wave scopes reject malformed inputs without fallback", {
  for (key in c("waves", "wave_filter")) {
    for (scope in list(NULL, character(), list(), " ", c("w1", ""),
                       c("w1", NA_character_), list("w1", NULL), list(list("w1")),
                       matrix("w1"), matrix(list("w1")), 1, c("all", "w1"))) {
      check <- .na_rate_check()
      check$waves <- NULL
      check[key] <- list(scope)
      .expect_na_rate_unevaluable(check, pattern = "wave")
    }
  }
})

test_that("NA-rate named waves require complete usable membership", {
  df <- .na_rate_data()
  df$wave_id <- NULL
  .expect_na_rate_unevaluable(.na_rate_check(), df, "wave_id")
  for (ids in list(c("w1", "w1", "w2", NA_character_),
                   c("w1", "w1", "w2", " "),
                   matrix(c("w1", "w1", "w2", "w2")),
                   list("w1", "w1", "w2", "w2"))) {
    df <- .na_rate_data()
    df$wave_id <- ids
    .expect_na_rate_unevaluable(.na_rate_check(), df, "wave_id")
  }
  .expect_na_rate_unevaluable(.na_rate_check(), .na_rate_data()[FALSE, ], "w1")
  df <- .na_rate_data()
  df$wave_id <- factor(df$wave_id)
  expect_true(.na_rate_validation(.na_rate_check(), df)$results[[1]]$passed)
  df$wave_id <- c(1, 1, 2, 2)
  check <- .na_rate_check()
  check$waves <- "1"
  expect_true(.na_rate_validation(check, df)$results[[1]]$passed)
})

test_that("NA-rate all-row scopes need no wave identifiers and allow empty data", {
  df <- .na_rate_data()
  df$wave_id <- NULL
  for (scope in list(NULL, "all", list("all"), c(selected = "all"))) {
    check <- .na_rate_check()
    check$waves <- scope
    expect_false(.na_rate_validation(check, df)$results[[1]]$passed)
    result <- .na_rate_validation(check, df[FALSE, , drop = FALSE])$results[[1]]
    expect_true(result$passed)
    expect_match(if (is.null(result$detail)) "" else result$detail,
                 "no eligible rows", fixed = TRUE)
    check$variables <- "absent_column"
    .expect_na_rate_unevaluable(check, df[FALSE, , drop = FALSE], "absent_column")
  }
})

test_that("NA-rate conditions intersect valid waves and report empty selections", {
  check <- .na_rate_check()
  check$threshold <- 0
  check$condition <- "s006 == 1"
  expect_true(.na_rate_validation(check)$results[[1]]$passed)
  check$condition <- "s006 == 2"
  expect_false(.na_rate_validation(check)$results[[1]]$passed)
  for (condition in c("s006 < 0", "s006 == NA", "wave_id == 'w2'")) {
    check$condition <- condition
    result <- .na_rate_validation(check)$results[[1]]
    expect_true(result$passed)
    expect_match(if (is.null(result$detail)) "" else result$detail,
                 "no eligible rows", fixed = TRUE)
  }
  check$condition <- ""
  expect_false(.na_rate_validation(check)$results[[1]]$passed)
})

test_that("NA-rate conditions reject unavailable inputs and cannot execute calls", {
  for (condition in list("absent_condition > 0", "s005 >", "sum(s005) > 0",
                         "s005 > 0; s006 > 0", "s005", "TRUE", character())) {
    check <- .na_rate_check()
    check$condition <- condition
    .expect_na_rate_unevaluable(check)
  }
  check <- .na_rate_check()
  check$waves <- "all"
  check$condition <- "absent_condition > 0"
  .expect_na_rate_unevaluable(check, .na_rate_data()[FALSE, ], "condition")
  check$condition <- "s005 >"
  .expect_na_rate_unevaluable(check, .na_rate_data()[FALSE, ], "condition")
  sentinel <- tempfile("lissr_na_rate_condition_")
  check$condition <- paste0("file.create('", sentinel, "')")
  .expect_na_rate_unevaluable(check, pattern = "condition")
  expect_false(file.exists(sentinel))
})

test_that("NA rates remain pooled per target and preserve inclusive alias defaults", {
  for (type in c("na_rate", "na_rate_check", "na_rate_above", "na_rate_below",
                 "not_missing")) {
    check <- .na_rate_check()
    check$type <- type
    expect_true(.na_rate_validation(check)$results[[1]]$passed)
    check$waves <- "all"
    check$threshold <- 0.75
    expect_true(.na_rate_validation(check)$results[[1]]$passed)
    check$direction <- "below"
    check$threshold <- 0.5
    expect_false(.na_rate_validation(check)$results[[1]]$passed)
    check$direction <- "above"
    expect_true(.na_rate_validation(check)$results[[1]]$passed)
  }
  check <- .na_rate_check(c("006", "005"))
  check$threshold <- 0
  expect_false(.na_rate_validation(check)$results[[1]]$passed)
  check <- .na_rate_check()
  check$threshold <- NULL
  check$max_rate <- 0.5
  expect_true(.na_rate_validation(check)$results[[1]]$passed)
  check$threshold <- 0
  expect_false(.na_rate_validation(check)$results[[1]]$passed)
  check$threshold <- NULL
  check$max_rate <- NULL
  expect_true(.na_rate_validation(check)$results[[1]]$passed)
  check$type <- "not_missing"
  expect_false(.na_rate_validation(check)$results[[1]]$passed)
  check$variables <- "006"
  expect_true(.na_rate_validation(check)$results[[1]]$passed)
  check$type <- "na_rate_above"
  expect_false(.na_rate_validation(check)$results[[1]]$passed)
  check$waves <- "w2"
  expect_true(.na_rate_validation(check)$results[[1]]$passed)
  check$direction <- "unrecognized"
  check$threshold <- 0
  expect_false(.na_rate_validation(check)$results[[1]]$passed)
  check <- .na_rate_check()
  check$waves <- "w2"
  check$threshold <- 1
  df <- .na_rate_data()
  df$s005 <- as.character(df$s005)
  expect_true(.na_rate_validation(check, df)$results[[1]]$passed)
})

test_that("NA-rate failures and unavailable scopes preserve severity", {
  for (severity in c("error", "warning", "info")) {
    check <- .na_rate_check(severity = severity)
    check$waves <- "w2"
    result <- .na_rate_validation(check)
    expect_false(result$results[[1]]$passed)
    expect_identical(result$results[[1]]$severity, severity)
    expect_identical(result$error_count, as.integer(severity == "error"))
    expect_identical(result$error_skips, character())
    check$variables <- "absent_column"
    .expect_na_rate_unevaluable(check, pattern = "absent_column")
  }
})

.na_rate_merge_fixture <- function(check, .local_envir = parent.frame()) {
  root <- withr::local_tempdir("lissr_na_rate_", .local_envir = .local_envir)
  data_dir <- file.path(root, "data")
  dir.create(data_dir)
  for (wave in c("yy01a", "yy02b")) {
    df <- data.frame(nomem_encr = 1:2)
    df[[paste0(wave, "005")]] <- if (wave == "yy01a") c(1, NA) else c(NA_real_, NA)
    haven::write_sav(df, file.path(data_dir, paste0(wave, "_EN_1.0p.sav")))
  }
  recipe <- list(
    meta = list(module = "yy", module_label = "NA rate fixture", schema_version = "1.0.0",
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

test_that("strict and report modes retain NA-rate diagnostics and protect output", {
  missing_target <- .na_rate_check(c("005", "absent_column"))
  missing_target$waves <- "yy01a"
  missing_wave <- .na_rate_check()
  missing_wave$waves <- c("yy01a", "absent_wave")
  malformed_scope <- .na_rate_check()
  malformed_scope$waves <- character()
  invalid_condition <- .na_rate_check()
  invalid_condition$waves <- "yy01a"
  invalid_condition$condition <- "absent_condition > 0"
  violated <- .na_rate_check()
  violated$waves <- "yy02b"
  for (check in list(missing_target, missing_wave, malformed_scope,
                    invalid_condition, violated)) {
    fx <- .na_rate_merge_fixture(check)
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
    expect_true(any(grepl("[error] NA_RATE:", report, fixed = TRUE)))
    detail <- result$validation[[1]]$detail
    expect_type(detail, "character")
    if (is.character(detail)) expect_true(any(grepl(detail, report, fixed = TRUE)))
    expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
  }
})

test_that("valid and empty NA-rate selections preserve strict output and values", {
  for (empty in c(FALSE, TRUE)) {
    check <- .na_rate_check()
    check$waves <- "yy01a"
    if (empty) check$condition <- "s005 < 0"
    fx <- .na_rate_merge_fixture(check)
    result <- suppressWarnings(suppressMessages(
      merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE)))
    expect_true(result$valid_for_analysis)
    expect_true(result$validation[[1]]$passed)
    if (empty) {
      detail <- result$validation[[1]]$detail
      expect_match(if (is.null(detail)) "" else detail, "no eligible rows", fixed = TRUE)
    }
    expect_equal(as.numeric(result$data$s005), c(1, NA, NA, NA))
    expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
  }
})
