.per_wave_mean_check <- function(targets = "005", severity = "error") {
  list(check_id = "PER_WAVE_MEAN", type = "per_wave_mean", severity = severity,
       variables = targets, min_mean = 2, max_mean = 3)
}

.per_wave_mean_data <- function() {
  data.frame(wave_id = c("w1", "w1", "w2", "w2"),
             s005 = c(1, 3, 2, 4), s006 = c(2, 2, 3, 3))
}

.per_wave_mean_validation <- function(check, df = .per_wave_mean_data()) {
  suppressWarnings(suppressMessages(
    lissr:::run_validations(df, list(check), list())))
}

.expect_per_wave_mean_unevaluable <- function(check, df = .per_wave_mean_data(),
                                             pattern = NULL) {
  result <- .per_wave_mean_validation(check, df)
  expect_identical(result$results[[1]]$passed, NA)
  expect_identical(result$results[[1]]$severity, check$severity)
  expect_identical(result$error_skips,
                   if (check$severity == "error") "PER_WAVE_MEAN" else character())
  expect_identical(result$error_count, 0L)
  if (!is.null(pattern)) {
    detail <- result$results[[1]]$detail
    expect_match(if (is.null(detail)) "" else detail, pattern)
  }
  invisible(result)
}

test_that("per-wave means resolve every required target before comparing values", {
  for (targets in list("absent_column", c("005", "absent_column"))) {
    check <- .per_wave_mean_check(targets)
    .expect_per_wave_mean_unevaluable(check, pattern = "absent_column")
    df <- .per_wave_mean_data()
    df$s005 <- 99
    .expect_per_wave_mean_unevaluable(check, df, "absent_column")
    df$s005 <- NA_real_
    .expect_per_wave_mean_unevaluable(check, df, "absent_column")
    .expect_per_wave_mean_unevaluable(check, df[FALSE, ], "absent_column")
  }
})

test_that("per-wave-mean target aliases retain exact and suffix lookup", {
  for (key in c("suffixes", "variables", "scope", "applies_to", "items", "stems",
                "variable", "column")) {
    check <- .per_wave_mean_check()
    check$variables <- NULL
    check[[key]] <- list("005")
    expect_true(.per_wave_mean_validation(check)$results[[1]]$passed)
    check[[key]] <- c("005", "absent_column")
    .expect_per_wave_mean_unevaluable(check, pattern = "absent_column")
  }
  for (column in c("s005", "stem_005", "q005", "Q005", "005")) {
    df <- data.frame(wave_id = "w1")
    df[[column]] <- 2
    expect_true(.per_wave_mean_validation(.per_wave_mean_check(), df)$results[[1]]$passed)
  }
  for (target in c("s005", "q005", "Q005")) {
    expect_true(.per_wave_mean_validation(.per_wave_mean_check(target))$results[[1]]$passed)
  }
  df <- .per_wave_mean_data()
  df[["005"]] <- 99
  expect_false(.per_wave_mean_validation(.per_wave_mean_check(), df)$results[[1]]$passed)
  expect_true(.per_wave_mean_validation(.per_wave_mean_check("s005"), df)$results[[1]]$passed)
  expect_true(.per_wave_mean_validation(.per_wave_mean_check(list(5)),
    data.frame(wave_id = "w1", s5 = 2))$results[[1]]$passed)
})

test_that("per-wave means retain first non-null target precedence and literal items", {
  check <- .per_wave_mean_check("absent_column")
  check$suffixes <- "005"
  expect_true(.per_wave_mean_validation(check)$results[[1]]$passed)
  check["suffixes"] <- list(NULL)
  .expect_per_wave_mean_unevaluable(check, pattern = "absent_column")
  check["variables"] <- list(NULL)
  check$variable <- "005"
  check$column <- "absent_column"
  expect_true(.per_wave_mean_validation(check)$results[[1]]$passed)
  check$variables <- character()
  .expect_per_wave_mean_unevaluable(check)
  check <- .per_wave_mean_check()
  check$variables <- NULL
  check$variables_documentation <- "005"
  .expect_per_wave_mean_unevaluable(check)

  check <- .per_wave_mean_check()
  check$variables <- NULL
  check$items <- list("005-006")
  .expect_per_wave_mean_unevaluable(check, pattern = "005-006")
  for (column in c("005-006", "s005-006")) {
    df <- .per_wave_mean_data()
    df[[column]] <- 2
    expect_true(.per_wave_mean_validation(check, df)$results[[1]]$passed)
  }
})

test_that("per-wave-mean target declarations reject malformed and empty scopes", {
  for (targets in list(NULL, character(), list(), " ", c("005", ""),
                       c("005", NA_character_), list("005", NULL),
                       list(list("005")), matrix("005"), matrix(list("005")),
                       TRUE, Inf)) {
    check <- .per_wave_mean_check()
    check["variables"] <- list(targets)
    .expect_per_wave_mean_unevaluable(check)
  }
})

test_that("per-wave-mean scalar numeric selectors require a nonempty selection", {
  for (selector in c("numeric", "all_numeric")) {
    for (targets in list(selector, c(selected = selector))) {
      check <- .per_wave_mean_check(targets)
      expect_true(.per_wave_mean_validation(check)$results[[1]]$passed)
      df <- .per_wave_mean_data()
      df$s006 <- 99
      expect_false(.per_wave_mean_validation(check, df)$results[[1]]$passed)
      .expect_per_wave_mean_unevaluable(check,
        data.frame(wave_id = "w1", text = "2"), selector)
    }
  }
})

test_that("per-wave-mean list numeric names retain literal column lookup", {
  for (target in c("numeric", "all_numeric")) {
    check <- .per_wave_mean_check(list(target))
    .expect_per_wave_mean_unevaluable(check, pattern = target)
    df <- .per_wave_mean_data()
    df$s005 <- 99
    df[[target]] <- 2
    expect_true(.per_wave_mean_validation(check, df)$results[[1]]$passed)
    df[[target]] <- 99
    expect_false(.per_wave_mean_validation(check, df)$results[[1]]$passed)
  }
})

test_that("per-wave means require usable wave membership before comparisons", {
  df <- .per_wave_mean_data()
  df$wave_id <- NULL
  .expect_per_wave_mean_unevaluable(.per_wave_mean_check(), df, "wave_id")
  .expect_per_wave_mean_unevaluable(.per_wave_mean_check(), df[FALSE, ], "wave_id")
  for (ids in list(c("w1", "w1", "w2", NA_character_),
                   c("w1", "w1", "w2", " "),
                   matrix(c("w1", "w1", "w2", "w2")),
                   list("w1", "w1", "w2", "w2"))) {
    df <- .per_wave_mean_data()
    df$wave_id <- ids
    .expect_per_wave_mean_unevaluable(.per_wave_mean_check(), df, "wave_id")
    df$s005 <- 99
    .expect_per_wave_mean_unevaluable(.per_wave_mean_check(), df, "wave_id")
  }
  for (ids in list(factor(c("w1", "w1", "w2", "w2")), c(1, 1, 2, 2))) {
    df <- .per_wave_mean_data()
    df$wave_id <- ids
    expect_true(.per_wave_mean_validation(.per_wave_mean_check(), df)$results[[1]]$passed)
  }
})

test_that("per-wave means retain all-wave behavior despite unsupported filter fields", {
  for (key in c("waves", "wave_filter", "in_waves", "must_be_na_in")) {
    for (scope in list("w1", "absent_wave", "all", list("all"), NULL,
                       character(), list(list("w1")), c("all", "w1"))) {
      check <- .per_wave_mean_check()
      check[key] <- list(scope)
      expect_true(.per_wave_mean_validation(check)$results[[1]]$passed)
      df <- .per_wave_mean_data()
      df$s005[df$wave_id == "w2"] <- 99
      expect_false(.per_wave_mean_validation(check, df)$results[[1]]$passed)
    }
  }
})

test_that("per-wave means use inclusive bounds for every column and every wave", {
  check <- .per_wave_mean_check(c("005", "006"))
  expect_true(.per_wave_mean_validation(check)$results[[1]]$passed)
  df <- .per_wave_mean_data()
  df$s005 <- c(0, 0, 6, 6)
  check$min_mean <- 0
  result <- .per_wave_mean_validation(check, df)$results[[1]]
  expect_false(result$passed)
  expect_match(result$detail, "mean(s005) = 6 in wave w2", fixed = TRUE)
  df <- .per_wave_mean_data()
  df$s006[1:2] <- -1
  result <- .per_wave_mean_validation(check, df)$results[[1]]
  expect_false(result$passed)
  expect_match(result$detail, "mean(s006) = -1 in wave w1", fixed = TRUE)

  check <- .per_wave_mean_check()
  check$min_mean <- NULL
  check$max_mean <- NULL
  df$s005 <- c(-100, -100, 100, 100)
  expect_true(.per_wave_mean_validation(check, df)$results[[1]]$passed)
  check$max_mean <- 99
  expect_false(.per_wave_mean_validation(check, df)$results[[1]]$passed)
  check$max_mean <- NULL
  check$min_mean <- -99
  expect_false(.per_wave_mean_validation(check, df)$results[[1]]$passed)
})

test_that("per-wave means preserve numeric coercion and removal of missing values", {
  check <- .per_wave_mean_check()
  check$min_mean <- 2
  check$max_mean <- 2
  df <- .per_wave_mean_data()
  df$s005 <- c(2, NA, 2, NaN)
  expect_true(.per_wave_mean_validation(check, df)$results[[1]]$passed)
  df$s005 <- c("2", "not numeric", "2", NA_character_)
  expect_true(.per_wave_mean_validation(check, df)$results[[1]]$passed)
  df$s005 <- factor(c("100", "200", "100", "200"))
  check$min_mean <- 1.5
  check$max_mean <- 1.5
  expect_true(.per_wave_mean_validation(check, df)$results[[1]]$passed)
})

test_that("per-wave means report unchecked non-finite means while preserving passes", {
  for (values in list(rep(NA_real_, 4), rep(NaN, 4), rep("not numeric", 4),
                      c(Inf, Inf, -Inf, -Inf), c(Inf, -Inf, NA, NA))) {
    df <- .per_wave_mean_data()
    df$s005 <- values
    result <- .per_wave_mean_validation(.per_wave_mean_check(), df)$results[[1]]
    expect_true(result$passed)
    detail <- if (is.null(result$detail)) "" else result$detail
    expect_match(detail, "2 non-finite mean(s) not compared", fixed = TRUE)
    expect_match(detail, "s005")
    expect_match(detail, "w1")
  }
  df <- .per_wave_mean_data()
  df$s005 <- NA_real_
  df$s006 <- NA_real_
  result <- .per_wave_mean_validation(.per_wave_mean_check(c("005", "006")), df)$results[[1]]
  expect_true(result$passed)
  expect_match(if (is.null(result$detail)) "" else result$detail,
               "4 non-finite mean(s) not compared", fixed = TRUE)
  df$s006[df$wave_id == "w2"] <- 99
  result <- .per_wave_mean_validation(.per_wave_mean_check(c("005", "006")), df)$results[[1]]
  expect_false(result$passed)
  expect_match(result$detail, "mean(s006) = 99 in wave w2", fixed = TRUE)
})

test_that("per-wave means allow valid empty data and explain that no means were calculated", {
  result <- .per_wave_mean_validation(.per_wave_mean_check(),
                                      .per_wave_mean_data()[FALSE, ])$results[[1]]
  expect_true(result$passed)
  expect_match(if (is.null(result$detail)) "" else result$detail,
               "no observed waves; no means calculated", fixed = TRUE)
})

test_that("per-wave-mean failures and unavailable inputs retain declared severity", {
  for (severity in c("error", "warning", "info")) {
    check <- .per_wave_mean_check(severity = severity)
    check$max_mean <- 1
    result <- .per_wave_mean_validation(check)
    expect_false(result$results[[1]]$passed)
    expect_identical(result$results[[1]]$severity, severity)
    expect_identical(result$error_count, as.integer(severity == "error"))
    expect_identical(result$error_skips, character())
    check$variables <- "absent_column"
    .expect_per_wave_mean_unevaluable(check, pattern = "absent_column")
  }
})

.per_wave_mean_merge_fixture <- function(check, all_na = FALSE,
                                         .local_envir = parent.frame()) {
  root <- withr::local_tempdir("lissr_per_wave_mean_", .local_envir = .local_envir)
  data_dir <- file.path(root, "data")
  dir.create(data_dir)
  for (wave in c("yy01a", "yy02b")) {
    df <- data.frame(nomem_encr = 1:2)
    df[[paste0(wave, "005")]] <- if (all_na) rep(NA_real_, 2) else
      if (wave == "yy01a") c(2, NA) else c(3, NA)
    haven::write_sav(df, file.path(data_dir, paste0(wave, "_EN_1.0p.sav")))
  }
  recipe <- list(
    meta = list(module = "yy", module_label = "per-wave mean fixture", schema_version = "1.0.0",
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

test_that("strict and report modes handle unresolved and violated per-wave means", {
  missing_target <- .per_wave_mean_check(c("005", "absent_column"))
  malformed_target <- .per_wave_mean_check(character())
  violated <- .per_wave_mean_check()
  violated$max_mean <- 2
  for (check in list(missing_target, malformed_target, violated)) {
    fx <- .per_wave_mean_merge_fixture(check)
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
    expect_true(any(grepl("[error] PER_WAVE_MEAN:", report, fixed = TRUE)))
    detail <- result$validation[[1]]$detail
    expect_type(detail, "character")
    if (is.character(detail)) expect_true(any(grepl(detail, report, fixed = TRUE)))
    expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
  }
})

test_that("passing and all-NA per-wave means retain strict output and actual values", {
  for (all_na in c(FALSE, TRUE)) {
    fx <- .per_wave_mean_merge_fixture(.per_wave_mean_check(), all_na = all_na)
    result <- suppressWarnings(suppressMessages(
      merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE)))
    expect_true(result$valid_for_analysis)
    expect_true(result$validation[[1]]$passed)
    expected <- if (all_na) rep(NA_real_, 4) else c(2, NA, 3, NA)
    expect_equal(as.numeric(result$data$s005), expected)
    expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
    if (all_na) {
      detail <- result$validation[[1]]$detail
      expect_match(if (is.null(detail)) "" else detail,
                   "2 non-finite mean(s) not compared", fixed = TRUE)
      report <- readLines(file.path(fx$output_dir, "yy_merge_report.txt"))
      expect_true(any(grepl("2 non-finite mean(s) not compared", report, fixed = TRUE)))
    }
  }
})
