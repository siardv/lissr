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
  .expect_value_present_unevaluable(check, pattern = "value")
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

test_that("presence requests preserve numeric vectors, arrays and scalar lists", {
  values <- c(-Inf, -1.5, 0, 2, Inf, NA, NaN)
  text <- c("-Inf", " -1.5 ", "0", "2e0", "Inf", NA_character_, "NaN")
  forms <- list(values, stats::setNames(values, letters[seq_along(values)]), text,
                array(values, c(1, 7, 1)), matrix(text), as.list(values),
                list(-Inf, "-1.5", 0L, "2e0", Inf, NA, "NaN"))
  for (type in c("value_present", "value_present_per_wave")) {
    for (field in c("value", "values")) {
      for (requested in forms) {
        check <- .value_present_check()
        check$type <- type
        check$value <- NULL
        check[[field]] <- requested
        check$waves <- "all"
        df <- data.frame(wave_id = paste0("w", seq_along(values)), s005 = values)
        expect_true(.value_present_validation(check, df)$results[[1]]$passed)
        df$s005[[1]] <- 99
        result <- .value_present_validation(check, df)$results[[1]]
        expect_false(result$passed)
        expect_identical(result$detail, paste0("value ", paste(values, collapse = "/"),
                                               " absent in wave w1; columns: s005"))
      }
    }
  }
  for (requested in list(c(-1L, 2L), list(-1L, 2L), matrix(c(-1L, 2L)))) {
    check <- .value_present_check()
    check$value <- requested
    check$waves <- "all"
    expect_true(.value_present_validation(check,
      data.frame(wave_id = c("w1", "w2"), s005 = c(-1L, 2L)))$results[[1]]$passed)
  }
  check <- .value_present_check()
  check$value <- list(1 / 3, "0.5")
  df <- data.frame(wave_id = "w1", s005 = 1 / 3)
  expect_true(.value_present_validation(check, df)$results[[1]]$passed)
  df$s005 <- as.numeric(as.character(1 / 3))
  expect_false(.value_present_validation(check, df)$results[[1]]$passed)
})

test_that("presence requests retain exact aliases, null fallback and empty predicates", {
  for (type in c("value_present", "value_present_per_wave")) {
    check <- .value_present_check()
    check$type <- type
    check$values <- "garbage"
    expect_true(.value_present_validation(check)$results[[1]]$passed)
    check$value <- "garbage"
    check$values <- 1
    .expect_value_present_unevaluable(check, pattern = "value")
    check["value"] <- list(NULL)
    expect_true(.value_present_validation(check)$results[[1]]$passed)
    for (field in c("value", "values")) {
      check <- .value_present_check()
      check$type <- type
      check$value <- NULL
      check[[paste0(field, "_note")]] <- 1
      expect_false(.value_present_validation(check)$results[[1]]$passed)
      check[field] <- list(NULL)
      expect_false(.value_present_validation(check)$results[[1]]$passed)
    }
    for (requested in list(NULL, numeric(), character(), logical(), list(),
                           array(numeric(), c(0, 1)))) {
      check <- .value_present_check()
      check$type <- type
      check["value"] <- list(requested)
      result <- .value_present_validation(check)
      expect_false(result$results[[1]]$passed)
      expect_identical(result$error_count, 1L)
      expect_identical(result$error_skips, character())
      check$values <- 1
      expect_identical(.value_present_validation(check)$results[[1]]$passed,
                       is.null(requested))
    }
  }
})

test_that("presence requests distinguish explicit NA, NaN and YAML missing values", {
  forms <- list(NA_real_, NA_integer_, NA_character_, NA, c(NA, NA), list(NA),
                matrix(NA), NaN, "NaN", list(NaN), c(NA_real_, NaN),
                yaml::yaml.load("value: [.na]")$value,
                yaml::yaml.load("value: [.nan]")$value,
                yaml::yaml.load("value: [.na, .nan]")$value)
  for (field in c("value", "values")) {
    for (requested in forms) {
      check <- .value_present_check()
      check$value <- NULL
      check[[field]] <- requested
      for (value in c(NA_real_, NaN)) {
        df <- data.frame(wave_id = "w1", s005 = value)
        expect_identical(.value_present_validation(check, df)$results[[1]]$passed,
                         value %in% as.numeric(requested))
      }
    }
  }
})

test_that("entire presence requests validate before any matching observation", {
  invalid <- list("garbage", "", " \t", "NA", c(1, "junk"), list(1, "junk"),
                  TRUE, c(NA, FALSE), as.raw(1), 1 + 1i, factor("1"),
                  as.Date("1970-01-02"), structure(1, class = "request"),
                  list(NULL), list(1, NULL, 2), list(numeric()), list(character()),
                  list(list()), list(list(1)), list(c(1, 2)), list(TRUE),
                  list(as.raw(1)), list(1 + 1i), list(factor("1")),
                  list(structure(1, class = "request")), list(matrix(1)),
                  matrix(list(1)), matrix(TRUE), data.frame(x = 1),
                  yaml::yaml.load("value: [null]")$value,
                  yaml::yaml.load("value: [NA]")$value)
  for (field in c("value", "values")) {
    for (requested in invalid) {
      check <- .value_present_check()
      check$value <- NULL
      check[[field]] <- requested
      for (values in list(c(1, NA), c(NA_real_, NaN))) {
        .expect_value_present_unevaluable(check,
          data.frame(wave_id = "w1", s005 = values), "value")
      }
    }
  }
})

test_that("presence payloads preserve severity and follow complete scope preflight", {
  for (severity in c("error", "warning", "info")) {
    check <- .value_present_check(severity = severity)
    check$value <- "garbage"
    .expect_value_present_unevaluable(check, pattern = "value")
    check$variables <- c("005", "absent_column")
    .expect_value_present_unevaluable(check, pattern = "absent_column")
    check$variables <- "005"
    check$waves <- "absent_wave"
    .expect_value_present_unevaluable(check, pattern = "absent_wave")
    for (scope in list(NULL, "all", "w1")) {
      check$waves <- scope
      .expect_value_present_unevaluable(check, .value_present_data()[FALSE, ], "wave")
      .expect_value_present_unevaluable(check, .value_present_data()["s005"], "wave_id")
    }
  }
})

test_that("presence keeps any-value target matching and unchanged data coercion", {
  check <- .value_present_check(c("005", "006"))
  check$value <- c(1, 2, 999)
  check$waves <- "all"
  df <- data.frame(wave_id = c("w1", "w2"), s005 = c(1, 0), s006 = c(0, 2))
  expect_true(.value_present_validation(check, df)$results[[1]]$passed)
  check$variables <- "005"
  expect_false(.value_present_validation(check, df)$results[[1]]$passed)
  inputs <- list(c("1", "junk", NA, "NaN"), factor(c("100", "200")),
                 as.Date(c("1970-01-01", "1970-01-02")), c(FALSE, TRUE),
                 haven::labelled(c(1, 2, NA), labels = c(one = 1)))
  for (values in inputs) {
    df <- data.frame(wave_id = paste0("w", seq_along(values)), s005 = values)
    before <- df
    check$value <- c(0, 1, 2, NA, NaN)
    check$condition <- "stop('ignored condition')"
    check$allow_na <- "ignored malformed flag"
    expect_true(.value_present_validation(check, df)$results[[1]]$passed)
    expect_identical(df, before)
  }
})

test_that("the bundled social-integration presence declaration retains its predicate", {
  recipe <- suppressWarnings(suppressMessages(liss_recipe("cs")))
  checks <- Filter(function(check) check$type %in% c("value_present", "value_present_per_wave"),
                   recipe$validation_checks)
  expect_length(checks, 1L)
  expect_identical(checks[[1]]$check_id, "V02_dk_code_present_all_waves")
  df <- data.frame(wave_id = c("cs08a", "cs09b"), s001 = c(-9, -9))
  expect_true(.value_present_validation(checks[[1]], df)$results[[1]]$passed)
  df$s001[[2]] <- NA_real_
  result <- .value_present_validation(checks[[1]], df)$results[[1]]
  expect_false(result$passed)
  expect_match(result$detail, "value -9 absent in wave cs09b", fixed = TRUE)
})

.value_present_merge_fixture <- function(check, all_na = FALSE, .local_envir = parent.frame()) {
  root <- withr::local_tempdir("lissr_value_present_", .local_envir = .local_envir)
  data_dir <- file.path(root, "data")
  dir.create(data_dir)
  for (wave in c("yy01a", "yy02b")) {
    df <- data.frame(nomem_encr = 1:2)
    df[[paste0(wave, "005")]] <- if (wave == "yy01a") c(1, NA) else c(2, NA)
    if (all_na) df[[paste0(wave, "005")]] <- NA_real_
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

test_that("presence payload errors preserve strict protection, reports and data values", {
  for (field in c("value", "values")) {
    for (severity in c("error", "warning", "info")) {
      for (all_na in c(FALSE, TRUE)) {
        check <- .value_present_check(severity = severity)
        check$value <- NULL
        check[[field]] <- "garbage"
        check$waves <- "all"
        fx <- .value_present_merge_fixture(check, all_na = all_na)
        if (severity == "error") {
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
        }
        result <- suppressWarnings(suppressMessages(merge_liss_module(
          fx$recipe, fx$data_dir, fx$output_dir, strict = severity != "error")))
        expect_identical(result$valid_for_analysis, severity != "error")
        expect_identical(result$validation[[1]]$passed, NA)
        expect_identical(result$validation[[1]]$severity, severity)
        report <- readLines(file.path(fx$output_dir, "yy_merge_report.txt"))
        expect_true(any(grepl(paste0("[", severity, "] VALUE_PRESENT: SKIP"),
                              report, fixed = TRUE)))
        detail <- result$validation[[1]]$detail
        expect_match(if (is.null(detail)) "" else detail, "value")
        if (is.character(detail)) expect_true(any(grepl(detail, report, fixed = TRUE)))
        expected <- if (all_na) rep(NA_real_, 4) else c(1, NA, 2, NA)
        expect_equal(as.numeric(result$data$s005), expected)
        written <- haven::read_sav(file.path(fx$output_dir, "yy_merged.sav"))
        expect_equal(as.numeric(written$s005), expected)
      }
    }
  }
})

test_that("valid presence payloads preserve strict outputs including explicit missing requests", {
  for (type in c("value_present", "value_present_per_wave")) {
    for (all_na in c(FALSE, TRUE)) {
      check <- .value_present_check()
      check$type <- type
      check$value <- list("1", 2L, NA)
      check$waves <- "all"
      fx <- .value_present_merge_fixture(check, all_na = all_na)
      result <- suppressWarnings(suppressMessages(
        merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE)))
      expect_true(result$valid_for_analysis)
      expect_true(result$validation[[1]]$passed)
      expected <- if (all_na) rep(NA_real_, 4) else c(1, NA, 2, NA)
      expect_equal(as.numeric(result$data$s005), expected)
      written <- haven::read_sav(file.path(fx$output_dir, "yy_merged.sav"))
      expect_equal(as.numeric(written$s005), expected)
    }
  }
})
