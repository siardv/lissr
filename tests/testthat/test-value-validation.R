.value_check <- function(type = "value_range", targets = "005", severity = "error") {
  list(check_id = "VALUE", type = type, severity = severity,
       variables = targets, min = 0, max = 2, allowed_values = c(0, 1, 2))
}

.value_data <- function() {
  data.frame(wave_id = c("w1", "w1", "w2"), s005 = c(1, NA, 99))
}

.value_validation <- function(check, df = .value_data()) {
  suppressWarnings(suppressMessages(
    lissr:::run_validations(df, list(check), list())))
}

.expect_range_unevaluable <- function(check, df = .value_data(), pattern = "min|max") {
  result <- .value_validation(check, df)
  expect_identical(result$results[[1]]$passed, NA)
  expect_identical(result$results[[1]]$severity, check$severity)
  expect_identical(result$error_skips,
                   if (check$severity == "error") "VALUE" else character())
  expect_identical(result$error_count, 0L)
  detail <- result$results[[1]]$detail
  expect_match(if (is.null(detail)) "" else detail, pattern)
  invisible(result)
}

test_that("missing value-check targets and waves are unevaluable", {
  for (type in c("value_range", "value_in_set")) {
    checks <- list(.value_check(type, "absent_column"),
                   .value_check(type, c("005", "absent_column")))
    for (waves in list("absent_wave", c("w1", "absent_wave"))) {
      check <- .value_check(type)
      check$waves <- waves
      checks <- c(checks, list(check))
    }
    for (check in checks) {
      result <- .value_validation(check)
      expect_identical(result$results[[1]]$passed, NA)
      expect_match(result$results[[1]]$detail, "absent_column|absent_wave")
      expect_identical(result$error_skips, "VALUE")
      expect_identical(result$error_count, 0L)
    }
  }
})

test_that("value-check wave aliases exclude out-of-scope violations", {
  for (type in c("value_range", "value_in_set")) {
    for (key in c("in_waves", "waves", "wave_filter", "must_be_na_in")) {
      for (waves in list("w1", c(selected = "w1"), list("w1"))) {
        check <- .value_check(type)
        check[[key]] <- waves
        expect_true(.value_validation(check)$results[[1]]$passed)
        check[[key]] <- list("w2")
        result <- .value_validation(check)
        expect_false(result$results[[1]]$passed)
        expect_match(result$results[[1]]$detail, "s005")
      }
    }
    for (waves in list(c("w1", "w2"), "all", list("all"), c(scope = "all"))) {
      check <- .value_check(type)
      check$waves <- waves
      expect_false(.value_validation(check)$results[[1]]$passed)
    }
    expect_false(.value_validation(.value_check(type))$results[[1]]$passed)
  }
})

test_that("the first declared value-check wave alias determines scope", {
  for (type in c("value_range", "value_in_set")) {
    check <- .value_check(type)
    check$in_waves <- "w1"
    check$waves <- "w2"
    expect_true(.value_validation(check)$results[[1]]$passed)
    check$in_waves <- "all"
    expect_false(.value_validation(check)$results[[1]]$passed)
    check$in_waves <- list("all")
    expect_false(.value_validation(check)$results[[1]]$passed)
  }
})

test_that("empty and malformed value-check wave scopes are unevaluable", {
  invalid <- list(NULL, character(), list(), c("w1", ""), c("w1", " "),
                  c("w1", NA_character_), list("w1", NULL),
                  list(list("w1")), matrix("w1"), matrix(list("w1")),
                  1, c("all", "w1"))
  for (type in c("value_range", "value_in_set")) {
    for (waves in invalid) {
      check <- .value_check(type)
      check["waves"] <- list(waves)
      result <- .value_validation(check)
      expect_identical(result$results[[1]]$passed, NA)
      expect_identical(result$error_skips, "VALUE")
      expect_match(result$results[[1]]$detail, "wave")
    }
  }
})

test_that("specific scopes require usable wave membership but all rows do not", {
  df <- .value_data()
  missing_wave <- df
  missing_wave$wave_id <- NULL
  unknown_wave <- df
  unknown_wave$wave_id[3] <- NA_character_
  blank_wave <- df
  blank_wave$wave_id[3] <- " "
  nested_wave <- df
  nested_wave$wave_id <- list("w1", "w1", c("w1", "w2"))
  for (type in c("value_range", "value_in_set")) {
    check <- .value_check(type)
    check$waves <- "w1"
    for (input in list(missing_wave, unknown_wave, blank_wave, nested_wave)) {
      result <- .value_validation(check, input)
      expect_identical(result$results[[1]]$passed, NA)
      expect_match(result$results[[1]]$detail, "wave_id")
    }
    for (waves in list("all", list("all"))) {
      check$waves <- waves
      expect_false(.value_validation(check, missing_wave)$results[[1]]$passed)
    }
    check$waves <- NULL
    expect_false(.value_validation(check, missing_wave)$results[[1]]$passed)
  }
})

test_that("value-check type and target aliases preserve exact-name priority", {
  types <- c("value_range", "range_check", "value_in_range", "assert_range",
             "value_in_set", "value_set", "assert_values")
  for (type in types) {
    for (column in c("s005", "stem_005", "q005", "Q005", "005")) {
      df <- stats::setNames(data.frame(c(1, 2)), column)
      expect_true(.value_validation(.value_check(type, "005"), df)$results[[1]]$passed)
    }
    for (target in c("s005", "q005", "Q005")) {
      expect_true(.value_validation(.value_check(type, target),
                                    data.frame(s005 = c(1, 2)))$results[[1]]$passed)
    }
    df <- data.frame(s005 = 99, check.names = FALSE)
    df[["005"]] <- 1
    expect_true(.value_validation(.value_check(type, "005"), df)$results[[1]]$passed)
    expect_false(.value_validation(.value_check(type, "s005"), df)$results[[1]]$passed)
    expect_true(.value_validation(.value_check(type, 5),
                                  data.frame(s5 = 1))$results[[1]]$passed)
  }
  for (type in c("value_range", "value_in_set")) {
    keys <- if (type == "value_range") c("suffixes", "variables", "variable", "items")
            else c("suffixes", "variables", "scope", "applies_to", "items", "stems",
                   "variable", "column")
    for (key in keys) {
      check <- .value_check(type)
      check$variables <- NULL
      check[[key]] <- list("005")
      expect_true(.value_validation(check, data.frame(s005 = 1))$results[[1]]$passed)
    }
  }
})

test_that("range items retain zero-padded expansion and missing members", {
  check <- .value_check()
  check$variables <- NULL
  check$items <- list("005-007")
  df <- data.frame(s005 = 0, s006 = 1, s007 = 2)
  expect_true(.value_validation(check, df)$results[[1]]$passed)
  df$s006 <- NULL
  result <- .value_validation(check, df)
  expect_identical(result$results[[1]]$passed, NA)
  expect_match(result$results[[1]]$detail, "006")
})

test_that("empty and malformed value-check target declarations cannot pass", {
  invalid <- list(NULL, character(), list(), " ", NA_character_,
                  c("005", ""), list("005", NULL), list(list("005")),
                  matrix("005"), matrix(list("005")))
  for (type in c("value_range", "value_in_set")) {
    for (targets in invalid) {
      check <- .value_check(type, targets)
      result <- .value_validation(check, data.frame(s005 = 1))
      expect_identical(result$results[[1]]$passed, NA)
      expect_identical(result$error_skips, "VALUE")
    }
    check <- .value_check(type)
    check$variables <- NULL
    check$variables_documentation <- "005"
    expect_identical(.value_validation(check)$results[[1]]$passed, NA)
  }
})

test_that("set checks preserve shared and per-variable allowed aliases", {
  df <- data.frame(wave_id = c("w1", "w2"), s005 = c(1, 99), s006 = c(2, 99))
  for (allowed_key in c("allowed_values", "allowed", "values")) {
    check <- .value_check("value_in_set", c("005", "006"))
    check$allowed_values <- NULL
    check[[allowed_key]] <- list(1, 2)
    check$wave_filter <- "w1"
    expect_true(.value_validation(check, df)$results[[1]]$passed)
  }
  for (allowed_key in c("allowed", "allowed_values")) {
    variables <- list(list(name = "005"), list(name = "006"))
    variables[[1]][[allowed_key]] <- 1
    variables[[2]][[allowed_key]] <- 2
    check <- .value_check("value_in_set", variables)
    check$waves <- "w1"
    expect_true(.value_validation(check, df)$results[[1]]$passed)
    check$waves <- "w2"
    expect_false(.value_validation(check, df)$results[[1]]$passed)
    check$variables[[2]]$name <- "absent_column"
    result <- .value_validation(check, df)
    expect_identical(result$results[[1]]$passed, NA)
    expect_match(result$results[[1]]$detail, "absent_column")
    check$variables <- check$variables[2]
    expect_identical(.value_validation(check, df)$results[[1]]$passed, NA)
  }
})

test_that("malformed per-variable set targets are unevaluable", {
  invalid <- list(list(name = ""), list(allowed = 1), list(names = "005", allowed = 1),
                  list(name = c("005", "006")), NULL, "006")
  for (entry in invalid) {
    check <- .value_check("value_in_set", list(list(name = "005", allowed = 1), entry))
    expect_identical(.value_validation(check, data.frame(s005 = 1))$results[[1]]$passed, NA)
  }
  check <- .value_check("value_in_set", matrix(list(list(name = "005", allowed = 1))))
  expect_identical(.value_validation(check, data.frame(s005 = 1))$results[[1]]$passed, NA)
})

test_that("numeric selectors, all-NA values and zero rows keep their meaning", {
  for (selector in c("all_numeric", "numeric")) {
    check <- .value_check("value_in_set", selector)
    expect_true(.value_validation(check, data.frame(s005 = c(1, NA), text = "x"))$results[[1]]$passed)
    expect_false(.value_validation(check, data.frame(s005 = 99, text = "x"))$results[[1]]$passed)
    expect_identical(.value_validation(check, data.frame(text = "x"))$results[[1]]$passed, NA)
    # range names are literal targets, including names reserved by set selectors
    df <- data.frame(s005 = 99)
    df[[selector]] <- 1
    expect_true(.value_validation(.value_check("value_range", selector), df)$results[[1]]$passed)
  }
  df <- data.frame(s005 = c(NA_real_, NA_real_))
  for (type in c("value_range", "value_in_set")) {
    check <- .value_check(type)
    expect_true(.value_validation(check, df)$results[[1]]$passed)
    expect_true(.value_validation(check, df[FALSE, , drop = FALSE])$results[[1]]$passed)
    check$waves <- "all"
    expect_true(.value_validation(check, df[FALSE, , drop = FALSE])$results[[1]]$passed)
    check$waves <- "w1"
    empty <- data.frame(wave_id = character(), s005 = numeric())
    expect_identical(.value_validation(check, empty)$results[[1]]$passed, NA)
  }
  check <- .value_check("value_in_set")
  check$allow_na <- FALSE
  expect_false(.value_validation(check, df)$results[[1]]$passed)
  check$variables <- list(list(name = "005", allowed = 1))
  expect_false(.value_validation(check, df)$results[[1]]$passed)
  check$allow_na <- TRUE
  expect_true(.value_validation(check, df)$results[[1]]$passed)
})

test_that("value-check failures and unevaluable inputs preserve severity", {
  for (type in c("value_range", "value_in_set")) {
    for (severity in c("error", "warning", "info")) {
      for (target in c("005", "absent_column")) {
        result <- .value_validation(.value_check(type, target, severity))
        expect_identical(result$results[[1]]$severity, severity)
        expect_equal(result$error_count, as.integer(severity == "error" && target == "005"))
        expect_identical(result$error_skips,
                         if (severity == "error" && target == "absent_column") "VALUE"
                         else character())
      }
    }
  }
})

test_that("other validation consumers retain existing resolver behavior", {
  df <- data.frame(wave_id = "w1", s005 = NA_real_)
  checks <- list(
    list(type = "structural_missingness", variable = "005", waves = "w1"),
    list(type = "row_count", min_rows = 0, max_rows = 0))
  for (check in checks) {
    check$check_id <- "LEGACY"
    input <- if (check$type == "row_count") df[FALSE, ] else df
    expect_true(.value_validation(check, input)$results[[1]]$passed)
  }
})

test_that("range bounds parse text and scalar wrappers for multiple observations", {
  df <- data.frame(s005 = c(2, 12, NA, NaN))
  wrappers <- list(identity, function(value) c(limit = value),
                   function(value) list(value), function(value) list(limit = value),
                   matrix, function(value) array(value, c(1, 1, 1)))
  for (type in c("value_range", "range_check", "value_in_range", "assert_range")) {
    for (bounds in list(c("0", "2"), c("2", "10"), c(" 10 ", "2e1"),
                        c("-1.5", ".5"), c("-Inf", "Inf"))) {
      lo <- as.numeric(bounds[[1]])
      hi <- as.numeric(bounds[[2]])
      bad <- sum(c(2, 12) < lo | c(2, 12) > hi)
      check <- .value_check(type)
      check$min <- lo
      check$max <- hi
      expected <- .value_validation(check, df)$results[[1]]
      expect_identical(expected$passed, bad == 0L)
      for (wrap in wrappers) {
        check$min <- wrap(bounds[[1]])
        check$max <- wrap(bounds[[2]])
        result <- .value_validation(check, df)$results[[1]]
        expect_identical(result, expected)
        if (bad) expect_identical(result$detail,
          paste0(bad, " out-of-range value(s) in s005 [", lo, "..", hi,
                 "]; wave scope: all"))
      }
    }
  }
  for (wrap in wrappers) {
    check <- .value_check()
    check$min <- wrap(-3L)
    check$max <- wrap(3L)
    expect_true(.value_validation(check, data.frame(s005 = c(-3L, 3L)))$results[[1]]$passed)
  }
})

test_that("range bounds retain exact fields and omitted or null unbounded defaults", {
  df <- data.frame(s005 = c(-Inf, -2.5, 0, 2.5, Inf, NA))
  for (type in c("value_range", "range_check", "value_in_range", "assert_range")) {
    for (field in c("min", "max")) {
      check <- .value_check(type)
      check$min <- check$max <- NULL
      check[[paste0(field, "_note")]] <- if (field == "min") 10 else -10
      expect_true(.value_validation(check, df)$results[[1]]$passed)
      check[field] <- list(NULL)
      expect_true(.value_validation(check, df)$results[[1]]$passed)
    }
  }
  check <- .value_check()
  check$min <- check$max <- NULL
  check$valid_range <- list(min = 0, max = 2)
  expect_true(.value_validation(check, df)$results[[1]]$passed)
  check$min <- 0
  check$max <- 2
  check$allow_sentinel <- TRUE
  check$sentinel_values <- c(-Inf, -2.5, 2.5, Inf)
  check$condition <- "s005 == 0"
  result <- .value_validation(check, df)$results[[1]]
  expect_false(result$passed)
  expect_match(result$detail, "4 out-of-range", fixed = TRUE)
})

test_that("range checks retain numeric-only evaluation and infinite observations", {
  check <- .value_check()
  check$min <- "-2.5"
  check$max <- "2.5"
  for (values in list(c("99", "junk"), c(TRUE, FALSE), factor(c("99", "100")),
                      as.Date(c("1970-01-01", "1970-01-02")),
                      c(NA_real_, NaN), numeric())) {
    df <- data.frame(s005 = values)
    before <- df
    result <- .value_validation(check, df)$results[[1]]
    expect_true(result$passed)
    expect_null(result$detail)
    expect_identical(df, before)
  }
  df <- data.frame(s005 = c(-Inf, -2.5, 0, 2.5, Inf, NA, NaN))
  df$s006 <- haven::labelled(df$s005, labels = c(zero = 0))
  before <- df
  for (targets in list("005", "006", c("006", "005"))) {
    check$variables <- targets
    for (bounds in list(c(-2.5, 2.5), c(-Inf, Inf), c(-Inf, -Inf), c(Inf, Inf))) {
      check$min <- as.character(bounds[[1]])
      check$max <- as.character(bounds[[2]])
      bad <- sum(df$s005 < bounds[[1]] | df$s005 > bounds[[2]], na.rm = TRUE)
      result <- .value_validation(check, df)$results[[1]]
      expect_identical(result$passed, bad == 0L)
      if (bad) expect_identical(result$detail,
        paste0(bad, " out-of-range value(s) in s", targets[[1]], " [",
               bounds[[1]], "..", bounds[[2]], "]; wave scope: all"))
    }
  }
  expect_identical(df, before)
})

test_that("malformed range bounds cannot hide behind skipped observations", {
  invalid <- list("garbage", "", " \t", "2 units", "NaN", NA_real_, NaN,
                  numeric(), c(0, 2), TRUE, as.raw(2), 2 + 1i, factor("2"),
                  as.Date("1970-01-03"), structure(2, class = "bound"), list(),
                  list(NULL), list(list(2)), list(c(0, 2)), list(TRUE),
                  list(factor("2")), list(as.Date("1970-01-03")),
                  list(structure(2, class = "bound")), list(as.raw(2)),
                  list(2 + 1i), list(NA_real_), list("garbage"),
                  list(matrix(2)), matrix(list(2)), matrix(TRUE), data.frame(x = 2))
  for (values in list(c(1, 2), c(NA_real_, NaN), numeric(), c("1", "2"))) {
    df <- data.frame(s005 = values)
    for (field in c("min", "max")) {
      for (value in invalid) {
        check <- .value_check()
        check[field] <- list(value)
        .expect_range_unevaluable(check, df, field)
      }
    }
  }
  check <- .value_check()
  check$min <- check$max <- numeric()
  .expect_range_unevaluable(check)
})

test_that("range bounds reject reversed intervals and masked malformed endpoints", {
  for (severity in c("error", "warning", "info")) {
    for (bounds in list(list(3, 0), list(Inf, -Inf), list("10", "2"),
                        list(matrix("10"), list("2")), list(NA_real_, 0),
                        list(3, NA_real_))) {
      check <- .value_check(severity = severity)
      check$min <- bounds[[1]]
      check$max <- bounds[[2]]
      for (values in list(c(1, 2), c(NA_real_, NaN), numeric(), c("1", "2"))) {
        .expect_range_unevaluable(check, data.frame(s005 = values))
      }
    }
  }
  for (field in c("min", "max")) {
    check <- .value_check(severity = "info")
    check[[field]] <- "garbage"
    expect_warning(suppressMessages(
      lissr:::run_validations(.value_data(), list(check), list())), NA)
    check$variables <- "absent_column"
    .expect_range_unevaluable(check, pattern = "absent_column")
    check$variables <- "005"
    check$waves <- "absent_wave"
    .expect_range_unevaluable(check, pattern = "absent_wave")
    check$waves <- "w1"
    .expect_range_unevaluable(check, .value_data()["s005"], "wave_id")
  }
})

.value_merge_fixture <- function(check, all_na = FALSE, .local_envir = parent.frame()) {
  root <- withr::local_tempdir("lissr_value_", .local_envir = .local_envir)
  data_dir <- file.path(root, "data")
  dir.create(data_dir)
  for (wave in c("yy01a", "yy02b")) {
    df <- data.frame(nomem_encr = 1:2)
    df[[paste0(wave, "005")]] <- if (wave == "yy01a") c(1, 2) else c(99, 99)
    if (all_na) df[[paste0(wave, "005")]] <- NA_real_
    haven::write_sav(df, file.path(data_dir, paste0(wave, "_EN_1.0p.sav")))
  }
  recipe <- list(
    meta = list(module = "yy", module_label = "value fixture", schema_version = "1.0.0",
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

test_that("strict and report modes handle unresolved and violated value checks", {
  for (type in c("value_range", "value_in_set")) {
    missing_wave <- .value_check(type)
    missing_wave$waves <- "absent_wave"
    malformed_wave <- .value_check(type)
    malformed_wave$waves <- character()
    checks <- list(.value_check(type, c("005", "absent_column")), missing_wave,
                   malformed_wave, .value_check(type))
    for (check in checks) {
      fx <- .value_merge_fixture(check)
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
      expect_true(any(grepl("[error] VALUE:", report, fixed = TRUE)))
      expect_true(any(grepl(result$validation[[1]]$detail, report, fixed = TRUE)))
      expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
    }
  }
})

test_that("valid scoped value checks preserve strict output and actual values", {
  for (type in c("value_range", "value_in_set")) {
    check <- .value_check(type)
    check$waves <- "yy01a"
    fx <- .value_merge_fixture(check)
    result <- suppressWarnings(suppressMessages(
      merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE)))
    expect_true(result$valid_for_analysis)
    expect_true(result$validation[[1]]$passed)
    expect_equal(as.numeric(result$data$s005), c(1, 2, 99, 99))
    expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
  }
})

test_that("malformed range bounds protect strict output and remain visible in reports", {
  for (bounds in list(list(NA_real_, 0), list(100, "garbage"), list(3, 0))) {
    for (all_na in c(FALSE, TRUE)) {
      check <- .value_check()
      check$min <- bounds[[1]]
      check$max <- bounds[[2]]
      fx <- .value_merge_fixture(check, all_na = all_na)
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
      expect_identical(result$validation[[1]]$passed, NA)
      expect_identical(result$validation[[1]]$severity, "error")
      report <- readLines(file.path(fx$output_dir, "yy_merge_report.txt"))
      expect_true(any(grepl("Valid for analysis: FALSE", report, fixed = TRUE)))
      expect_true(any(grepl("[error] VALUE: SKIP", report, fixed = TRUE)))
      detail <- result$validation[[1]]$detail
      expect_type(detail, "character")
      if (is.character(detail)) expect_true(any(grepl(detail, report, fixed = TRUE)))
      expected <- if (all_na) rep(NA_real_, 4) else c(1, 2, 99, 99)
      expect_equal(as.numeric(result$data$s005), expected)
      written <- haven::read_sav(file.path(fx$output_dir, "yy_merged.sav"))
      expect_equal(as.numeric(written$s005), expected)
    }
  }
})

test_that("valid wrapped range bounds preserve strict output and actual values", {
  for (all_na in c(FALSE, TRUE)) {
    check <- .value_check()
    check$min <- matrix("0")
    check$max <- list("1e2")
    fx <- .value_merge_fixture(check, all_na = all_na)
    result <- suppressWarnings(suppressMessages(
      merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE)))
    expect_true(result$valid_for_analysis)
    expect_true(result$validation[[1]]$passed)
    expected <- if (all_na) rep(NA_real_, 4) else c(1, 2, 99, 99)
    expect_equal(as.numeric(result$data$s005), expected)
    written <- haven::read_sav(file.path(fx$output_dir, "yy_merged.sav"))
    expect_equal(as.numeric(written$s005), expected)
  }
})

test_that("non-error malformed range bounds retain strict output eligibility", {
  for (severity in c("warning", "info")) {
    for (field in c("min", "max")) {
      for (all_na in c(FALSE, TRUE)) {
        check <- .value_check(severity = severity)
        check[[field]] <- "garbage"
        fx <- .value_merge_fixture(check, all_na = all_na)
        result <- suppressWarnings(suppressMessages(
          merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE)))
        expect_true(result$valid_for_analysis)
        expect_identical(result$validation[[1]]$passed, NA)
        expect_identical(result$validation[[1]]$severity, severity)
        report <- readLines(file.path(fx$output_dir, "yy_merge_report.txt"))
        expect_true(any(grepl(paste0("[", severity, "] VALUE: SKIP"),
                              report, fixed = TRUE)))
        expected <- if (all_na) rep(NA_real_, 4) else c(1, 2, 99, 99)
        expect_equal(as.numeric(result$data$s005), expected)
        written <- haven::read_sav(file.path(fx$output_dir, "yy_merged.sav"))
        expect_equal(as.numeric(written$s005), expected)
      }
    }
  }
})
