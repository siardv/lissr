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
  expect_identical(result$error_count, 0L)
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

test_that("absence payloads preserve primitive vectors, arrays and flat lists", {
  cases <- list(list(payload = c(98L, 99L), values = c(98, 99, 1, NA)),
                list(payload = c(-1.5, Inf), values = c(-1.5, Inf, 1, NA)),
                list(payload = c("refused", ""), values = c("refused", "", "ok", NA)),
                list(payload = c(FALSE, TRUE), values = c(FALSE, TRUE, NA)))
  wrappers <- list(identity, function(value) stats::setNames(value, seq_along(value)),
                   matrix, function(value) array(value, c(1, length(value), 1)), as.list)
  for (case in cases) {
    for (wrap in wrappers) {
      check <- .absence_check()
      check$forbidden_values <- wrap(case$payload)
      result <- .absence_validation(check, data.frame(s005 = case$values))$results[[1]]
      expect_false(result$passed)
      expect_identical(result$detail,
        "2 forbidden value(s) in s005; block 1; parent waves: all; block waves: all")
    }
  }
  for (value in list("refused", "", " ", "NA", "NaN", "nan", -Inf)) {
    check <- .absence_check()
    check$forbidden_values <- value
    expect_false(.absence_validation(check, data.frame(s005 = value))$results[[1]]$passed)
  }
  check <- .absence_check()
  check$forbidden_values <- "099"
  result <- .absence_validation(check, data.frame(s005 = c("099", "99", "ok", NA)))$results[[1]]
  expect_match(result$detail, "2 forbidden value(s)", fixed = TRUE)
})

test_that("absence preserves logical promotion and both numeric precision matches", {
  cases <- list(list(TRUE, 1, FALSE), list(TRUE, "TRUE", FALSE),
                list(list(TRUE, 99), 1, FALSE), list(list(TRUE, 99), "TRUE", TRUE),
                list(list(TRUE, "refused"), 1, TRUE),
                list(list(TRUE, "refused"), "TRUE", FALSE),
                list(list(FALSE, 99), 0, FALSE), list(list(FALSE, 99), "FALSE", TRUE),
                list(list(FALSE, "refused"), 0, TRUE),
                list(list(FALSE, "refused"), "FALSE", FALSE))
  for (case in cases) {
    check <- .absence_check()
    check$forbidden_values <- case[[1]]
    expect_identical(.absence_validation(check, data.frame(s005 = case[[2]]))$results[[1]]$passed,
                     case[[3]])
  }
  for (payload in list(1 / 3, list(1 / 3, "refused"),
                        list(NULL, 1 / 3, "refused", NULL),
                        list(text = "refused", code = 1 / 3, empty = NULL))) {
    check <- .absence_check()
    check$forbidden_values <- payload
    for (value in list(1 / 3, as.numeric(as.character(1 / 3)), as.character(1 / 3),
                       sprintf("%.17g", 1 / 3))) {
      result <- .absence_validation(check, data.frame(s005 = value))$results[[1]]
      expect_false(result$passed)
      expect_match(if (is.null(result$detail)) "" else result$detail,
                   "1 forbidden value(s)", fixed = TRUE)
    }
  }
})

test_that("absence keeps actual missing observations separate from literal missing codes", {
  for (payload in list(NA_real_, NA_integer_, NA_character_, NA, list(NA), NaN,
                        "NA", "NaN", "nan", yaml::yaml.load("codes: [.na, .nan]")$codes)) {
    check <- .absence_check()
    check$forbidden_values <- payload
    expect_true(.absence_validation(check, data.frame(s005 = c(NA_real_, NaN)))$results[[1]]$passed)
  }
  for (payload in list(NA_real_, NA_character_, NA, list(NA))) {
    check <- .absence_check()
    check$forbidden_values <- payload
    expect_true(.absence_validation(check, data.frame(s005 = "NA"))$results[[1]]$passed)
  }
  check <- .absence_check()
  check$forbidden_values <- NaN
  expect_false(.absence_validation(check, data.frame(s005 = "NaN"))$results[[1]]$passed)
  expect_true(.absence_validation(check, data.frame(s005 = "nan"))$results[[1]]$passed)
  check$forbidden_values <- "nan"
  expect_true(.absence_validation(check, data.frame(s005 = "NaN"))$results[[1]]$passed)
})

test_that("absence payload aliases preserve active-empty and inherited-parent precedence", {
  keys <- c("forbidden_values", "forbidden_value", "sentinel_values", "codes", "value")
  for (container in c("direct", "targets", "checks")) {
    for (active in seq_along(keys)) {
      payload <- stats::setNames(rep(list(NULL), length(keys)), keys)
      payload[[keys[[active]]]] <- 99
      if (active < length(keys)) payload[[keys[[active + 1L]]]] <- list(list(99))
      check <- .absence_check()
      if (container == "direct") check[keys] <- payload
      else check[[container]] <- list(c(list(variables = "005"), payload))
      expect_false(.absence_validation(check)$results[[1]]$passed)
      payload[[keys[[active]]]] <- list(NULL)
      if (container == "direct") check[keys] <- payload
      else check[[container]] <- list(c(list(variables = "005"), payload))
      expect_true(.absence_validation(check)$results[[1]]$passed)
      payload[[keys[[active]]]] <- list(list(99))
      if (active < length(keys)) payload[[keys[[active + 1L]]]] <- 99
      if (container == "direct") check[keys] <- payload
      else check[[container]] <- list(c(list(variables = "005"), payload))
      .expect_absence_unevaluable(check, pattern = "forbidden")
    }
  }
  for (key in keys) {
    check <- .absence_check()
    check$forbidden_values <- NULL
    check[[key]] <- 99
    check$targets <- list(list(variables = "005", value = NULL))
    expect_identical(.absence_validation(check)$results[[1]]$passed,
                     key %in% c("forbidden_value", "codes"))
    check[[key]] <- list(list(99))
    check$targets[[1]]$value <- 98
    expect_true(.absence_validation(check)$results[[1]]$passed)
    check <- .absence_check()
    check$forbidden_values <- NULL
    check[[paste0(key, "_note")]] <- 99
    expect_true(.absence_validation(check)$results[[1]]$passed)
  }
})

test_that("absence empties bypass target conversion while missing-only payloads do not", {
  df <- data.frame(wave_id = c("w1", "w2"))
  df$s005 <- I(list(c(1, 2), c(3, 4)))
  for (payload in list(NULL, numeric(), character(), logical(), list(), list(NULL),
                       array(numeric(), c(0, 1)), yaml::yaml.load("codes: [null]")$codes)) {
    check <- .absence_check()
    check["forbidden_values"] <- list(payload)
    expect_true(.absence_validation(check, df)$results[[1]]$passed)
  }
  check <- .absence_check()
  check$forbidden_values <- NA_real_
  .expect_absence_unevaluable(check, df, "list")
  check$forbidden_values <- list(1, NULL, 2)
  expect_false(.absence_validation(check, data.frame(s005 = 1))$results[[1]]$passed)
})

test_that("absence malformed payload shapes are rejected before flattening", {
  invalid <- list(list(list(99)), list(c(98, 99)), list(numeric()), list(character()),
                  list(logical()), list(list()), matrix(list(99)), list(matrix(99)),
                  data.frame(x = 99), factor("99"), as.Date("1970-04-10"),
                  structure(99, class = "code"), as.raw(99), 99 + 1i,
                  list(factor("99")), list(structure(99, class = "code")),
                  list(as.raw(99)), list(99 + 1i), quote(code), function() 99)
  for (payload in invalid) {
    check <- .absence_check()
    check$forbidden_values <- payload
    .expect_absence_unevaluable(check, pattern = "forbidden")
    .expect_absence_unevaluable(check, .absence_data()[FALSE, ], "forbidden")
  }
})

test_that("absence payload validation survives every legitimate empty selection", {
  for (shape in c("zero_rows", "all_missing", "intersection", "complement", "excluded")) {
    check <- .absence_check()
    df <- .absence_data()
    if (shape == "zero_rows") df <- df[FALSE, ]
    if (shape == "all_missing") df$s005 <- NA_real_
    if (shape == "intersection") {
      check$waves <- "w1"
      check$targets <- list(list(waves = "w2"))
    }
    if (shape == "complement") check$waves_allowed <- "all"
    if (shape == "excluded") check$exclude_variables <- "005"
    expect_true(.absence_validation(check, df)$results[[1]]$passed)
    check$forbidden_values <- list(list(99))
    .expect_absence_unevaluable(check, df, "forbidden")
  }
})

test_that("all absence scopes precede all payloads and all payloads precede comparisons", {
  for (severity in c("error", "warning", "info")) {
    check <- .absence_check(severity = severity)
    check$targets <- list(list(variables = "005", value = 99),
                          list(variables = "006", value = list(list(98))))
    .expect_absence_unevaluable(check, pattern = "block 2.*forbidden")
    check$targets[[1]]$value <- list(list(99))
    check$targets[[2]]$variables <- "absent_column"
    .expect_absence_unevaluable(check, pattern = "block 2.*absent_column")
    check$targets[[2]]$variables <- "006"
    check$targets[[2]]$waves <- "absent_wave"
    .expect_absence_unevaluable(check, pattern = "block 2.*absent_wave")
    check$targets[[2]]$waves <- NULL
    check$targets[[2]]$exclude_variables <- list(list("006"))
    .expect_absence_unevaluable(check, pattern = "block 2.*exclu")
  }
})

test_that("absence retains target conversion, ignored fields and unmodified values", {
  inputs <- list(factor(c("refused", "safe")), as.Date(c("1970-01-02", "1970-01-03")),
                 c(TRUE, FALSE), haven::labelled(c(1, 2, NA), labels = c(one = 1)))
  for (values in inputs) {
    df <- data.frame(s005 = values)
    before <- df
    check <- .absence_check()
    check$forbidden_values <- 1
    check$condition <- "stop('ignored condition')"
    check$allow_na <- "ignored malformed flag"
    result <- .absence_validation(check, df)$results[[1]]
    expect_false(result$passed)
    expect_match(result$detail, "1 forbidden value(s)", fixed = TRUE)
    expect_identical(df, before)
  }
})

test_that("bundled absence declarations retain integer, numeric-text and literal-text codes", {
  cd <- suppressWarnings(suppressMessages(liss_recipe("cd")))
  cd_check <- Filter(function(check) identical(check$check_id, "CHK03_rent_period_val5"),
                     cd$validation_checks)[[1]]
  df <- data.frame(wave_id = cd_check$in_waves, h_rent_period = 1)
  expect_true(.absence_validation(cd_check, df)$results[[1]]$passed)
  df$h_rent_period[[1]] <- 5
  expect_false(.absence_validation(cd_check, df)$results[[1]]$passed)
  ci <- suppressWarnings(suppressMessages(liss_recipe("ci")))
  ci_check <- Filter(function(check) identical(check$check_id, "V-01"), ci$validation_checks)[[1]]
  ci_check$targets <- ci_check$targets[1]
  df <- data.frame(wave_id = ci_check$targets[[1]]$waves, s001 = 1)
  expect_true(.absence_validation(ci_check, df)$results[[1]]$passed)
  df$s001[[1]] <- 9999999998
  expect_false(.absence_validation(ci_check, df)$results[[1]]$passed)
  ch <- suppressWarnings(suppressMessages(liss_recipe("ch")))
  ch_check <- Filter(function(check) identical(check$check_id, "CHK10"), ch$validation_checks)[[1]]
  expect_true(.absence_validation(ch_check, data.frame(wave_id = "ch13f"))$results[[1]]$passed)
  expect_false(.absence_validation(ch_check, data.frame(wave_id = "ch14"))$results[[1]]$passed)
})

# ---- cv VC01_no_raw_dk exclusions -----------------------------------------
.cv_vc01_recipe_path <- function() {
  system.file("recipes", "cv_merge_recipe.yml", package = "lissr", mustWork = TRUE)
}
.cv_vc01_check <- function() {
  recipe <- yaml::yaml.load_file(.cv_vc01_recipe_path())
  Filter(function(check) identical(check$check_id, "VC01_no_raw_dk"),
         recipe$validation_checks)[[1]]
}
# the 27 declared suffix carve-outs plus the renamed cv17i Total column
.cv_vc01_exclusions <- c("243", sprintf("%03d", 245:263), "304",
                         sprintf("%03d", 321:324), "340", "341", "cv17i_Total")
.cv_vc01_result <- function(data, check = .cv_vc01_check()) {
  suppressWarnings(suppressMessages(
    lissr:::run_validations(data, list(check), list())))$results[[1]]
}

test_that("cv VC01 declares one exclusion list that resolves every carve-out", {
  check <- .cv_vc01_check()
  declared <- intersect(c("exclude_variables", "exclude_suffixes"), names(check))
  # the executor applies the first non-null key and never unions the two
  expect_length(declared, 1L)
  expect_setequal(as.character(unlist(check[declared])), .cv_vc01_exclusions)
  expect_identical(check$severity, "error")
  expect_identical(check$scope, "all_numeric")
  expect_setequal(as.numeric(unlist(check$forbidden_values)), c(99, 999, -9))
  columns <- ifelse(grepl("^[0-9]{3}$", .cv_vc01_exclusions),
                    paste0("s", .cv_vc01_exclusions), .cv_vc01_exclusions)
  df <- as.data.frame(stats::setNames(rep(list(1), length(columns)), columns))
  df$s001 <- 1
  blocks <- lissr:::.resolve_absence_blocks(df, check)
  expect_length(blocks, 1L)
  expect_identical(blocks[[1]]$columns, "s001")
})

test_that("no bundled absence check declares competing parent exclusion keys", {
  types <- c("value_absence", "assert_absent_values", "none_equal", "sentinel_absence",
             "no_residual_sentinels", "assert_no_values", "value_absence_check",
             "value_restriction")
  for (mod in c("ca", "cd", "cf", "ch", "ci", "cp", "cr", "cs", "cv", "cw")) {
    recipe <- yaml::yaml.load_file(system.file("recipes", paste0(mod, "_merge_recipe.yml"),
                                               package = "lissr", mustWork = TRUE))
    for (check in Filter(function(check) check$type %in% types, recipe$validation_checks)) {
      both <- all(c("exclude_variables", "exclude_suffixes") %in% names(check))
      expect_false(both, label = paste(mod, check$check_id, "declares both exclusion keys"))
    }
  }
})

test_that("cv VC01 spares documented carve-outs and still fails retained targets", {
  base <- data.frame(wave_id = c("cv19k", "cv20l", "cv20l"), nomem_encr = 1:3,
                     s001 = c(1, 2, 3))
  # legitimate 99 on a percent-chance item
  df <- base; df$s245 <- c(0, 99, 100)
  expect_true(.cv_vc01_result(df)$passed)
  # structural 999 (cv17i-cv19k) and -9 (cv20l onward) on suffix 243
  df <- base; df$s243 <- c(999, -9, 1)
  expect_true(.cv_vc01_result(df)$passed)
  # the renamed cv17i validation artefact, when it is still present
  df <- base; df$cv17i_Total <- c(99, 999, -9)
  expect_true(.cv_vc01_result(df)$passed)
  # every declared suffix at once
  df <- base
  for (suffix in setdiff(.cv_vc01_exclusions, c("243", "cv17i_Total")))
    df[[paste0("s", suffix)]] <- c(99, 0, 100)
  expect_true(.cv_vc01_result(df)$passed)
  # retained targets keep failing on each forbidden value
  for (value in c(99, 999, -9)) {
    df <- base; df$s245 <- c(0, 99, 100); df$s001[[2]] <- value
    result <- .cv_vc01_result(df)
    expect_false(result$passed)
    expect_match(result$detail, "1 forbidden value(s) in s001", fixed = TRUE)
  }
  # exclusion names absent from the data are optional; typed empty data passes
  expect_true(.cv_vc01_result(base)$passed)
  expect_true(.cv_vc01_result(base[FALSE, , drop = FALSE])$passed)
})

.absence_merge_fixture <- function(check, all_na = FALSE, .local_envir = parent.frame()) {
  root <- withr::local_tempdir("lissr_absence_", .local_envir = .local_envir)
  data_dir <- file.path(root, "data")
  dir.create(data_dir)
  for (wave in c("yy01a", "yy02b")) {
    df <- data.frame(nomem_encr = 1:2)
    df[[paste0(wave, "005")]] <- if (wave == "yy01a") c(1, NA) else c(99, 99)
    if (all_na) df[[paste0(wave, "005")]] <- NA_real_
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

test_that("absence payload errors preserve strict protection, reports and data values", {
  for (blocks in c(FALSE, TRUE)) {
    for (severity in c("error", "warning", "info")) {
      for (all_na in c(FALSE, TRUE)) {
        check <- .absence_check(severity = severity)
        if (blocks) {
          check$targets <- list(list(value = 99), list(value = list(list(99))))
        } else check$forbidden_values <- list(list(99))
        fx <- .absence_merge_fixture(check, all_na = all_na)
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
        expect_true(any(grepl(paste0("[", severity, "] ABSENCE: SKIP"), report, fixed = TRUE)))
        detail <- result$validation[[1]]$detail
        expect_match(if (is.null(detail)) "" else detail, "forbidden")
        if (is.character(detail)) expect_true(any(grepl(detail, report, fixed = TRUE)))
        expected <- if (all_na) rep(NA_real_, 4) else c(1, NA, 99, 99)
        expect_equal(as.numeric(result$data$s005), expected)
        written <- haven::read_sav(file.path(fx$output_dir, "yy_merged.sav"))
        expect_equal(as.numeric(written$s005), expected)
      }
    }
  }
})

test_that("valid mixed absence payloads preserve strict outputs and actual values", {
  for (all_na in c(FALSE, TRUE)) {
    check <- .absence_check()
    check$forbidden_values <- list(TRUE, "refused", NULL)
    fx <- .absence_merge_fixture(check, all_na = all_na)
    result <- suppressWarnings(suppressMessages(
      merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE)))
    expect_true(result$valid_for_analysis)
    expect_true(result$validation[[1]]$passed)
    expected <- if (all_na) rep(NA_real_, 4) else c(1, NA, 99, 99)
    expect_equal(as.numeric(result$data$s005), expected)
    written <- haven::read_sav(file.path(fx$output_dir, "yy_merged.sav"))
    expect_equal(as.numeric(written$s005), expected)
  }
})
