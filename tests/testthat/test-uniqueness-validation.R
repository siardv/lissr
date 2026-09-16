.uniqueness_check <- function(severity = "error") {
  list(check_id = "UNIQUENESS", type = "uniqueness", severity = severity)
}

.uniqueness_data <- function() {
  data.frame(nomem_encr = c(1, 2, 1, 2), wave_id = c("w1", "w1", "w2", "w2"),
             wave_year = c(2001, 2001, 2002, 2002), nohouse_encr = c(10, 20, 10, 20),
             s005 = c(1, 1, 1, 1), observation = 1:4)
}

.uniqueness_validation <- function(check, df = .uniqueness_data()) {
  suppressWarnings(suppressMessages(
    lissr:::run_validations(df, list(check), list())))
}

.expect_uniqueness_unevaluable <- function(check, df = .uniqueness_data(),
                                          pattern = NULL) {
  result <- .uniqueness_validation(check, df)
  expect_identical(result$results[[1]]$passed, NA)
  expect_identical(result$results[[1]]$severity, check$severity)
  expect_identical(result$error_skips,
                   if (check$severity == "error") "UNIQUENESS" else character())
  expect_identical(result$error_count, 0L)
  if (!is.null(pattern)) {
    detail <- result$results[[1]]$detail
    expect_match(if (is.null(detail)) "" else detail, pattern)
  }
  invisible(result)
}

test_that("uniqueness requires every primary and grouping key before counting", {
  checks <- list(
    list(column = "absent_person"), list(within = "absent_wave"),
    list(column = c("nomem_encr", "absent_person")),
    list(within = c("wave_id", "absent_wave")),
    list(column = "absent_person", within = "absent_wave"))
  df <- .uniqueness_data()
  for (fields in checks) {
    check <- c(.uniqueness_check(), fields)
    for (data in list(df, df[1:2, ], rbind(df, df[1, ]), df[FALSE, ])) {
      result <- .expect_uniqueness_unevaluable(check, data, "absent_")
      detail <- result$results[[1]]$detail
      expect_match(if (is.null(detail)) "" else detail, "key scope")
    }
  }
  check <- .uniqueness_check()
  for (missing in list("nomem_encr", "wave_id", c("nomem_encr", "wave_id"))) {
    data <- df[, setdiff(names(df), missing), drop = FALSE]
    .expect_uniqueness_unevaluable(check, data, missing[[1]])
    .expect_uniqueness_unevaluable(check, data[FALSE, ], missing[[1]])
  }
})

test_that("all uniqueness type aliases preserve required keys and duplicate counts", {
  for (type in c("uniqueness", "assert_unique", "n_duplicates", "unique_key",
                 "no_duplicate_ids", "unique_per_wave", "assert_identifier")) {
    check <- .uniqueness_check()
    check$type <- type
    expect_true(.uniqueness_validation(check)$results[[1]]$passed)
    df <- .uniqueness_data()
    result <- .uniqueness_validation(check, rbind(df, df[1, ]))$results[[1]]
    expect_false(result$passed)
    expect_identical(result$detail, "duplicates: 2")
    check$column <- "absent_person"
    .expect_uniqueness_unevaluable(check, pattern = "absent_person")
  }
})

test_that("uniqueness direct key fields accept vectors and flat character lists", {
  for (key in c("column", "key", "variable", "scope")) {
    for (targets in list("nomem_encr", c("nomem_encr", "observation"))) {
      check <- .uniqueness_check()
      check[[key]] <- targets
      expect_true(.uniqueness_validation(check)$results[[1]]$passed)
      check[[key]] <- c(targets, "absent_person")
      .expect_uniqueness_unevaluable(check, pattern = "absent_person")
    }
    if (key != "scope") {
      check <- .uniqueness_check()
      check[[key]] <- list(first = "nomem_encr", second = "observation")
      expect_true(.uniqueness_validation(check)$results[[1]]$passed)
    }
  }
  for (key in c("within", "group_by")) {
    for (targets in list("wave_id", c("wave_id", "wave_year"),
                         list(first = "wave_id", second = "wave_year"))) {
      check <- .uniqueness_check()
      check[[key]] <- targets
      expect_true(.uniqueness_validation(check)$results[[1]]$passed)
      check[[key]] <- c("wave_id", "absent_wave")
      .expect_uniqueness_unevaluable(check, pattern = "absent_wave")
    }
  }
})

test_that("uniqueness key shorthands resolve exact names before aliases", {
  aliases <- c(person = "nomem_encr", respondent = "nomem_encr",
               household = "nohouse_encr", wave = "wave_id", year = "wave_year")
  for (alias in names(aliases)) {
    check <- .uniqueness_check()
    check$column <- alias
    check$within <- if (alias %in% c("wave", "year")) "nomem_encr" else "wave_id"
    expect_true(.uniqueness_validation(check)$results[[1]]$passed)
    df <- .uniqueness_data()
    df[[alias]] <- 1
    expect_false(.uniqueness_validation(check, df)$results[[1]]$passed)
    df[[alias]] <- NULL
    df[[aliases[[alias]]]] <- NULL
    .expect_uniqueness_unevaluable(check, df, alias)
  }
  check <- .uniqueness_check()
  check$column <- c("person", "respondent", "nomem_encr")
  check$within <- list("wave", "wave_id", "person")
  expect_true(.uniqueness_validation(check)$results[[1]]$passed)
  df <- .uniqueness_data()
  result <- .uniqueness_validation(check, rbind(df, df[1, ], df[1, ]))$results[[1]]
  expect_false(result$passed)
  expect_identical(result$detail, "duplicates: 3")
})

test_that("uniqueness uses literal key names without suffix or selector expansion", {
  for (target in c("005", "q005", "Q005", "numeric", "all_numeric", "005-006")) {
    check <- .uniqueness_check()
    check$column <- target
    .expect_uniqueness_unevaluable(check, pattern = target)
    df <- .uniqueness_data()
    df[[target]] <- 1:4
    expect_true(.uniqueness_validation(check, df)$results[[1]]$passed)
  }
})

test_that("uniqueness preserves non-null primary and grouping key precedence", {
  primary_keys <- c("column", "key", "variable", "key_columns", "variables", "scope")
  for (i in seq_along(primary_keys)) {
    check <- .uniqueness_check()
    check$within <- "wave_id"
    for (j in seq.int(i, length(primary_keys))) {
      check[[primary_keys[[j]]]] <- if (i == j) "nomem_encr" else "absent_person"
    }
    expect_true(.uniqueness_validation(check)$results[[1]]$passed)
    check[[primary_keys[[i]]]] <- "absent_person"
    .expect_uniqueness_unevaluable(check, pattern = "absent_person")
  }
  group_keys <- c("within", "group_by", "key_columns", "variables")
  for (i in seq_along(group_keys)) {
    check <- .uniqueness_check()
    check$column <- "nomem_encr"
    for (j in seq.int(i, length(group_keys))) {
      target <- if (i == j) "wave_id" else "absent_wave"
      check[[group_keys[[j]]]] <- if (j <= 2L) target else c("unused_person", target)
    }
    expect_true(.uniqueness_validation(check)$results[[1]]$passed)
  }
  check <- .uniqueness_check()
  for (key in c(primary_keys, "within", "group_by")) check[key] <- list(NULL)
  expect_true(.uniqueness_validation(check)$results[[1]]$passed)
  check$scope_wave <- "absent_wave"
  check$column_documentation <- "absent_person"
  check$within_documentation <- "absent_wave"
  expect_true(.uniqueness_validation(check)$results[[1]]$passed)
})

test_that("uniqueness positional key declarations support one or two names lazily", {
  for (key in c("key_columns", "variables")) {
    for (targets in list("nomem_encr", list("nomem_encr"),
                         c("nomem_encr", "wave_id"), list("nomem_encr", "wave_id"))) {
      check <- .uniqueness_check()
      check[[key]] <- targets
      expect_true(.uniqueness_validation(check)$results[[1]]$passed)
    }
    check <- .uniqueness_check()
    check[[key]] <- c("nomem_encr", "absent_wave")
    .expect_uniqueness_unevaluable(check, pattern = "absent_wave")
    check$within <- "wave_id"
    expect_true(.uniqueness_validation(check)$results[[1]]$passed)
    check <- .uniqueness_check()
    check$column <- "nomem_encr"
    check[[key]] <- c("unused_person", "wave_id")
    expect_true(.uniqueness_validation(check)$results[[1]]$passed)
    check[[key]] <- c("nomem_encr", "wave_id", "observation")
    .expect_uniqueness_unevaluable(check, pattern = "one or two")
  }
  check <- .uniqueness_check()
  check$key_columns <- "nomem_encr"
  check$variables <- c("unused_person", "wave_id")
  expect_true(.uniqueness_validation(check)$results[[1]]$passed)
  check$variables[[2]] <- "absent_wave"
  .expect_uniqueness_unevaluable(check, pattern = "absent_wave")
  check$within <- "wave_id"
  check$variables <- list(list("malformed_but_shadowed"))
  expect_true(.uniqueness_validation(check)$results[[1]]$passed)
})

test_that("uniqueness rejects malformed selected keys without falling back", {
  malformed <- list(character(), list(), " ", c("nomem_encr", ""),
                    c("nomem_encr", NA_character_), list("nomem_encr", NULL),
                    list(list("nomem_encr")), matrix("nomem_encr"),
                    matrix(list("nomem_encr")), TRUE, 1, NA_real_)
  for (key in c("column", "key", "variable", "scope", "within", "group_by",
                "key_columns", "variables")) {
    for (targets in malformed) {
      check <- .uniqueness_check()
      check[key] <- list(targets)
      .expect_uniqueness_unevaluable(check)
    }
  }
  check <- .uniqueness_check()
  check$scope <- list("nomem_encr")
  .expect_uniqueness_unevaluable(check, pattern = "scope")
  for (key in c("key_columns", "variables")) {
    check <- .uniqueness_check()
    check$column <- "nomem_encr"
    check[[key]] <- c(NA_character_, "wave_id")
    .expect_uniqueness_unevaluable(check)
    check <- .uniqueness_check()
    check$within <- "wave_id"
    check[[key]] <- c("nomem_encr", NA_character_)
    .expect_uniqueness_unevaluable(check)
  }
})

test_that("uniqueness ignores malformed lower-priority fields after both roles resolve", {
  check <- .uniqueness_check()
  check$column <- "nomem_encr"
  check$within <- "wave_id"
  for (key in c("key", "variable", "key_columns", "variables", "scope", "group_by")) {
    check[key] <- list(list(list("malformed_but_shadowed")))
  }
  expect_true(.uniqueness_validation(check)$results[[1]]$passed)
  check <- .uniqueness_check()
  check$key_columns <- c("nomem_encr", "wave_id")
  check$variables <- matrix("malformed_but_shadowed")
  check$scope <- TRUE
  expect_true(.uniqueness_validation(check)$results[[1]]$passed)
})

test_that("uniqueness compares complete compound tuples and counts every duplicate row", {
  check <- .uniqueness_check()
  check$column <- c("nomem_encr", "observation")
  df <- .uniqueness_data()
  df$nomem_encr <- 1
  expect_true(.uniqueness_validation(check, df)$results[[1]]$passed)
  check$column <- "nomem_encr"
  check$within <- c("wave_id", "observation")
  expect_true(.uniqueness_validation(check, df)$results[[1]]$passed)
  check <- .uniqueness_check()
  df <- .uniqueness_data()
  df <- rbind(df, df[1, ], df[1, ], df[4, ])
  result <- .uniqueness_validation(check, df)$results[[1]]
  expect_false(result$passed)
  expect_identical(result$detail, "duplicates: 5")
})

test_that("uniqueness retains empty-data and ordinary missing-key-value semantics", {
  check <- .uniqueness_check()
  for (rows in list(integer(), 1L)) {
    result <- .uniqueness_validation(check, .uniqueness_data()[rows, ])$results[[1]]
    expect_true(result$passed)
    expect_identical(result$detail, "duplicates: 0")
  }
  for (key in c("nomem_encr", "wave_id")) {
    for (value in list(NA_character_, "", " ")) {
      df <- .uniqueness_data()
      df[[key]][1:2] <- value
      if (key == "nomem_encr") df$wave_id[[2]] <- "different_wave"
      expect_true(.uniqueness_validation(check, df)$results[[1]]$passed)
      df <- rbind(df, df[1, ])
      result <- .uniqueness_validation(check, df)$results[[1]]
      expect_false(result$passed)
      expect_identical(result$detail, "duplicates: 2")
    }
  }
  df <- .uniqueness_data()
  df$nomem_encr <- factor(df$nomem_encr)
  df$wave_id <- factor(df$wave_id)
  expect_true(.uniqueness_validation(check, df)$results[[1]]$passed)
  df$wave_id <- as.numeric(df$wave_id)
  expect_true(.uniqueness_validation(check, df)$results[[1]]$passed)
  df$wave_id <- NULL
  check$within <- "observation"
  expect_true(.uniqueness_validation(check, df)$results[[1]]$passed)
})

test_that("uniqueness retains all-row behavior despite unsupported wave filters", {
  df <- .uniqueness_data()
  df <- rbind(df, df[4, ])
  for (key in c("waves", "wave_filter", "in_waves", "must_be_na_in", "scope_wave")) {
    for (scope in list("w1", "absent_wave", "all", list("all"), NULL,
                       character(), list(list("w1")))) {
      check <- .uniqueness_check()
      check[key] <- list(scope)
      result <- .uniqueness_validation(check, df)$results[[1]]
      expect_false(result$passed)
      expect_identical(result$detail, "duplicates: 2")
    }
  }
})

test_that("uniqueness failures and unavailable keys retain declared severity", {
  df <- .uniqueness_data()
  for (severity in c("error", "warning", "info")) {
    check <- .uniqueness_check(severity)
    result <- .uniqueness_validation(check, rbind(df, df[1, ]))
    expect_false(result$results[[1]]$passed)
    expect_identical(result$results[[1]]$severity, severity)
    expect_identical(result$error_count, as.integer(severity == "error"))
    expect_identical(result$error_skips, character())
    check$column <- "absent_person"
    check$within <- "absent_wave"
    .expect_uniqueness_unevaluable(check, pattern = "absent_person")
    check$column <- character()
    .expect_uniqueness_unevaluable(check)
  }
})

.uniqueness_merge_fixture <- function(check, duplicate = FALSE,
                                      .local_envir = parent.frame()) {
  root <- withr::local_tempdir("lissr_uniqueness_", .local_envir = .local_envir)
  data_dir <- file.path(root, "data")
  dir.create(data_dir)
  for (wave in c("yy01a", "yy02b")) {
    df <- data.frame(nomem_encr = 1:2)
    df[[paste0(wave, "005")]] <- if (wave == "yy01a") c(2, NA) else
      if (duplicate) c(3, 3) else c(3, 4)
    haven::write_sav(df, file.path(data_dir, paste0(wave, "_EN_1.0p.sav")))
  }
  recipe <- list(
    meta = list(module = "yy", module_label = "uniqueness fixture", schema_version = "1.0.0",
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

test_that("strict and report modes handle unresolved and duplicate uniqueness keys", {
  missing_primary <- .uniqueness_check()
  missing_primary$column <- c("nomem_encr", "absent_person")
  missing_group <- .uniqueness_check()
  missing_group$within <- "absent_wave"
  malformed <- .uniqueness_check()
  malformed$column <- character()
  duplicate <- .uniqueness_check()
  duplicate$column <- "s005"
  checks <- list(missing_primary, missing_group, malformed, duplicate)
  for (i in seq_along(checks)) {
    fx <- .uniqueness_merge_fixture(checks[[i]], duplicate = i == length(checks))
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
    expect_true(any(grepl("[error] UNIQUENESS:", report, fixed = TRUE)))
    detail <- result$validation[[1]]$detail
    expect_type(detail, "character")
    if (is.character(detail)) expect_true(any(grepl(detail, report, fixed = TRUE)))
    expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
  }
})

test_that("valid uniqueness keys preserve strict output and actual merged values", {
  check <- .uniqueness_check()
  check$key_columns <- list("person", "wave")
  fx <- .uniqueness_merge_fixture(check)
  result <- suppressWarnings(suppressMessages(
    merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE)))
  expect_true(result$valid_for_analysis)
  expect_true(result$validation[[1]]$passed)
  expect_identical(result$validation[[1]]$detail, "duplicates: 0")
  expect_equal(as.numeric(result$data$nomem_encr), c(1, 2, 1, 2))
  expect_equal(as.numeric(result$data$s005), c(2, NA, 3, 4))
  expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
})
