.row_count_check <- function(severity = "error") {
  list(check_id = "ROW_COUNT", type = "row_count", severity = severity,
       wave = "w1", min_rows = 0)
}

.row_count_data <- function() {
  data.frame(value = 1:5, wave_id = c("w1", "w2", "w1", "w2", "w2"))
}

.row_count_validation <- function(check, df = .row_count_data()) {
  suppressWarnings(suppressMessages(
    lissr:::run_validations(df, list(check), list())))
}

.expect_row_count_unevaluable <- function(check, df = .row_count_data(),
                                         pattern = "wave") {
  result <- .row_count_validation(check, df)
  expect_identical(result$results[[1]]$passed, NA)
  expect_identical(result$results[[1]]$severity, check$severity)
  expect_identical(result$error_count, 0L)
  expect_identical(result$error_skips,
                   if (check$severity == "error") "ROW_COUNT" else character())
  detail <- result$results[[1]]$detail
  expect_type(detail, "character")
  expect_length(detail, 1L)
  if (is.character(detail) && length(detail) == 1L)
    expect_match(detail, pattern, fixed = TRUE)
  invisible(result)
}

test_that("row counts require the requested wave even when zero would satisfy bounds", {
  for (type in c("row_count", "assert_row_count_range")) {
    for (severity in c("error", "warning", "info")) {
      for (minimum in c(0, 1)) {
        check <- .row_count_check(severity)
        check$type <- type
        check$min_rows <- minimum
        for (empty in c(FALSE, TRUE)) {
          df <- .row_count_data()
          if (empty) df <- df[FALSE, ]
          .expect_row_count_unevaluable(check, df["value"], "wave_id")
          df$wave_id_note <- df$wave_id
          df$wave_id <- NULL
          .expect_row_count_unevaluable(check, df, "wave_id")
          df <- .row_count_data()
          if (empty) df <- df[FALSE, ]
          check$wave <- "absent_wave"
          .expect_row_count_unevaluable(check, df, "absent_wave")
          check$wave <- "w1"
        }
      }
    }
  }
})

test_that("row-count scopes require one nonmissing undimensioned atomic value", {
  invalid <- list(character(), c("w1", "w2"), c("w1", "absent_wave"),
                  rep("w1", 5), NA_character_, NA_real_, NaN, "", " \t",
                  list("w1"), list(NULL), list(list("w1")), matrix("w1"),
                  array("w1", c(1, 1, 1)), data.frame(wave = "w1"))
  for (severity in c("error", "warning", "info")) {
    for (value in invalid) {
      check <- .row_count_check(severity)
      check["wave"] <- list(value)
      .expect_row_count_unevaluable(check)
    }
  }
})

test_that("scoped row counts require known membership for every row", {
  for (bad in list(NA_character_, "", " \t")) {
    for (index in c(1L, 2L)) {
      df <- .row_count_data()
      df$wave_id[index] <- bad
      .expect_row_count_unevaluable(.row_count_check(), df, "wave_id")
    }
  }
  for (value in list(as.list(rep("w1", 5)), matrix(rep("w1", 5)),
                     matrix(rep("w1", 10), nrow = 5), data.frame(id = rep("w1", 5)))) {
    df <- .row_count_data()
    df$wave_id <- value
    .expect_row_count_unevaluable(.row_count_check(), df, "wave_id")
  }
  check <- .row_count_check()
  check$wave <- 1
  for (missing in c(NA_real_, NaN)) {
    df <- data.frame(wave_id = c(1, missing, 2))
    .expect_row_count_unevaluable(check, df, "wave_id")
  }
})

test_that("global row counts need no columns and preserve legitimate zero rows", {
  for (type in c("row_count", "assert_row_count_range")) {
    for (explicit_null in c(FALSE, TRUE)) {
      check <- .row_count_check()
      check$type <- type
      check$wave <- NULL
      if (explicit_null) check["wave"] <- list(NULL)
      for (df in list(data.frame(), data.frame(value = numeric()),
                       .row_count_data()[FALSE, ])) {
        check$min_rows <- 0
        check$max_rows <- 0
        result <- .row_count_validation(check, df)$results[[1]]
        expect_true(result$passed)
        expect_identical(result$detail, "rows: 0 (bounds 0..0)")
        check$min_rows <- 1
        expect_false(.row_count_validation(check, df)$results[[1]]$passed)
      }
      check$min_rows <- check$max_rows <- 5
      for (value in list(rep(NA_character_, 5), rep("", 5), as.list(1:5), matrix(1:5))) {
        df <- .row_count_data()
        df$wave_id <- value
        expect_true(.row_count_validation(check, df)$results[[1]]$passed)
      }
      df <- .row_count_data()["value"]
      expect_true(.row_count_validation(check, df)$results[[1]]$passed)
      df <- df[, FALSE, drop = FALSE]
      expect_true(.row_count_validation(check, df)$results[[1]]$passed)
    }
  }
})

test_that("row counts preserve scalar conversion and exact literal wave names", {
  for (wave in list("w1", c(name = "w1"), factor("w1"), 1, TRUE,
                    as.Date("2001-01-01"), as.raw(1), Inf, "all", " w1 ", "w1-w2")) {
    df <- data.frame(wave_id = rep(wave, 3))
    check <- .row_count_check()
    check$wave <- wave
    check$min_rows <- check$max_rows <- 3
    expect_true(.row_count_validation(check, df)$results[[1]]$passed)
    df$wave_id <- c(as.character(wave), "other_wave", as.character(wave))
    check$min_rows <- check$max_rows <- 2
    result <- .row_count_validation(check, df)$results[[1]]
    expect_true(result$passed)
    expect_identical(result$detail,
                     paste0("rows: 2 in wave ", as.character(wave), " (bounds 2..2)"))
  }
  check <- .row_count_check()
  check$wave <- "all"
  .expect_row_count_unevaluable(check, pattern = "all")
  check$wave <- "W1"
  .expect_row_count_unevaluable(check, pattern = "W1")
})

test_that("row counts use exact fields and do not add plural wave filters", {
  for (field in c("waves", "wave_filter", "wave_note", "in_waves", "scope_wave")) {
    for (value in list("w1", "absent_wave", "all", list("w1", "w2"), character())) {
      check <- .row_count_check()
      check$wave <- NULL
      check$min_rows <- check$max_rows <- 5
      check[field] <- list(value)
      expect_true(.row_count_validation(check)$results[[1]]$passed)
      check["wave"] <- list(NULL)
      expect_true(.row_count_validation(check)$results[[1]]$passed)
      check$wave <- "w1"
      check$min_rows <- check$max_rows <- 2
      expect_true(.row_count_validation(check)$results[[1]]$passed)
    }
  }
  for (field in c("min_rows_note", "max_rows_note")) {
    check <- .row_count_check()
    check$min_rows <- NULL
    check[[field]] <- if (field == "min_rows_note") 99 else 0
    expect_true(.row_count_validation(check)$results[[1]]$passed)
  }
})

test_that("row counts retain inclusive bounds defaults and scalar comparisons", {
  for (type in c("row_count", "assert_row_count_range")) {
    for (severity in c("error", "warning", "info")) {
      for (scoped in c(FALSE, TRUE)) {
        check <- .row_count_check(severity)
        check$type <- type
        if (!scoped) check$wave <- NULL
        count <- if (scoped) 2 else 5
        for (bounds in list(c(count, count), c(count - 0.5, count + 0.5),
                            c(-Inf, Inf), c(count + 1, Inf), c(0, count - 1))) {
          check$min_rows <- bounds[[1]]
          check$max_rows <- bounds[[2]]
          result <- .row_count_validation(check)
          pass <- bounds[[1]] <= count && count <= bounds[[2]]
          expect_identical(result$results[[1]]$passed, pass)
          expect_identical(result$results[[1]]$severity, severity)
          expect_identical(result$error_count,
                           if (!pass && severity == "error") 1L else 0L)
          expect_identical(result$error_skips, character())
        }
        for (minimum in list(count, as.character(count), c(limit = count))) {
          check$min_rows <- minimum
          check$max_rows <- count
          expect_true(.row_count_validation(check)$results[[1]]$passed)
        }
        check[c("min_rows", "max_rows")] <- list(NULL, NULL)
        expect_true(.row_count_validation(check)$results[[1]]$passed)
      }
    }
  }
})

.row_count_merge_fixture <- function(check, drop_wave = FALSE,
                                      .local_envir = parent.frame()) {
  root <- withr::local_tempdir("lissr_row_count_", .local_envir = .local_envir)
  data_dir <- file.path(root, "data")
  dir.create(data_dir)
  for (wave in c("yy01a", "yy02b")) {
    df <- data.frame(nomem_encr = 1:2)
    df[[paste0(wave, "005")]] <- if (wave == "yy01a") c(2, NA) else c(3, 4)
    haven::write_sav(df, file.path(data_dir, paste0(wave, "_EN_1.0p.sav")))
  }
  recipe <- list(
    meta = list(module = "yy", module_label = "row count fixture", schema_version = "1.0.0",
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

test_that("strict and report modes handle unresolved and violated row counts", {
  missing <- .row_count_check()
  missing$wave <- "absent_wave"
  malformed <- .row_count_check()
  malformed$wave <- character()
  violated <- .row_count_check()
  violated$wave <- "yy01a"
  violated$max_rows <- 1
  missing_column <- .row_count_check()
  missing_column$wave <- "yy01a"
  checks <- list(missing, malformed, violated, missing_column)
  for (i in seq_along(checks)) {
    fx <- .row_count_merge_fixture(checks[[i]], drop_wave = i == length(checks))
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
    expect_identical(result$validation[[1]]$passed, if (i == 3L) FALSE else NA)
    expect_identical(result$validation[[1]]$severity, "error")
    report <- readLines(file.path(fx$output_dir, "yy_merge_report.txt"))
    expect_true(any(grepl("Valid for analysis: FALSE", report, fixed = TRUE)))
    expect_true(any(grepl("[error] ROW_COUNT:", report, fixed = TRUE)))
    detail <- result$validation[[1]]$detail
    expect_type(detail, "character")
    expect_length(detail, 1L)
    if (is.character(detail) && length(detail) == 1L)
      expect_true(any(grepl(detail, report, fixed = TRUE)))
    expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
  }
})

test_that("valid row counts preserve strict output and actual values", {
  for (type in c("row_count", "assert_row_count_range")) {
    for (scoped in c(FALSE, TRUE)) {
      check <- .row_count_check()
      check$type <- type
      check$wave <- if (scoped) "yy01a" else NULL
      check$min_rows <- check$max_rows <- if (scoped) 2 else 4
      fx <- .row_count_merge_fixture(check, drop_wave = !scoped)
      result <- suppressWarnings(suppressMessages(
        merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE)))
      expect_true(result$valid_for_analysis)
      expect_true(result$validation[[1]]$passed)
      expect_equal(as.numeric(result$data$nomem_encr), c(1, 2, 1, 2))
      expect_equal(as.numeric(result$data$s005), c(2, NA, 3, 4))
      expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
    }
  }
})

test_that("warning and info row-count outcomes retain strict output eligibility", {
  for (severity in c("warning", "info")) {
    for (unresolved in c(FALSE, TRUE)) {
      check <- .row_count_check(severity)
      check$wave <- if (unresolved) "absent_wave" else "yy01a"
      if (!unresolved) check$max_rows <- 1
      fx <- .row_count_merge_fixture(check)
      result <- suppressWarnings(suppressMessages(
        merge_liss_module(fx$recipe, fx$data_dir, fx$output_dir, strict = TRUE)))
      expect_true(result$valid_for_analysis)
      expect_identical(result$validation[[1]]$passed, if (unresolved) NA else FALSE)
      expect_identical(result$validation[[1]]$severity, severity)
      report <- readLines(file.path(fx$output_dir, "yy_merge_report.txt"))
      expect_true(any(grepl("Valid for analysis: TRUE", report, fixed = TRUE)))
      expect_true(any(grepl(paste0("[", severity, "] ROW_COUNT: ",
        if (unresolved) "SKIP" else "FAIL"), report, fixed = TRUE)))
      expect_true(file.exists(file.path(fx$output_dir, "yy_merged.sav")))
    }
  }
})
