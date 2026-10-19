# all rows and native carriers used here are frozen invented fixtures
.nr_export_saved <- function(version = 1L) {
  readRDS(testthat::test_path("fixtures",
    paste0("source_nonresponse_v", version, "_synthetic.rds")))
}

.nr_export_ordinary <- function() {
  suppressWarnings(lissr::liss_clean_income(.nr_export_saved()$inputs$unguarded,
    mode = "flag", variables = list(target = "s339"), verbose = FALSE))
}

.nr_export_panel <- function(input, ...) {
  suppressMessages(lissr::merge_liss_panel(list(ci = input), ...))
}

.nr_export_forms <- function(frame, key, value) {
  marked <- frame
  attr(marked, key) <- value
  outer <- list(data = frame)
  attr(outer, key) <- value
  list(bare = marked, exact = list(data = marked),
    partial = list(data_partial = marked), outer = outer)
}

.nr_export_report_forms <- function(result, key, value) {
  marked <- result
  attr(marked$data, key) <- value
  partial <- marked
  names(partial)[names(partial) == "data"] <- "data_partial"
  outer <- result
  attr(outer, key) <- value
  list(exact = marked, partial = partial, outer = outer)
}

.nr_export_expect_panel <- function(input, output) {
  before <- serialize(input, NULL, version = 2L)
  expect_error(.nr_export_panel(input, write_to = output), "source_nonresponse.*lineage")
  expect_false(file.exists(dirname(output)))
  expect_identical(serialize(input, NULL, version = 2L), before)
}

.nr_export_expect_report <- function(input, output) {
  before <- serialize(input, NULL, version = 2L)
  expect_error(lissr::liss_cleaning_report(input, output, verbose = FALSE), "guarded.*report export")
  expect_false(file.exists(output))
  expect_identical(serialize(input, NULL, version = 2L), before)
}

test_that("complete scalar frames cannot lose native lineage through partial panel wrappers", {
  saved <- .nr_export_saved(2L)
  old <- .nr_export_saved()
  inputs <- list(v1 = old$inputs$normalized,
    v2_337 = saved$fields[["337"]]$inputs$normalized,
    v2_338 = saved$fields[["338"]]$inputs$normalized)
  expected <- list(v1 = old$validated$normalized,
    v2_337 = saved$fields[["337"]]$validated$normalized,
    v2_338 = saved$fields[["338"]]$validated$normalized)
  output <- file.path(withr::local_tempdir(), "must_not_create", "panel.sav")
  for (name in names(inputs)) {
    input <- inputs[[name]]
    state <- input$source_nonresponse
    checked <- lissr:::source_nonresponse_validate(input$data, state, state$target, state$wave_col)
    expect_identical(checked, expected[[name]], info = name)
    for (form in list(input, input$data, list(data = input$data), list(data_partial = input$data)))
      .nr_export_expect_panel(form, output)
  }
})

test_that("scalar readiness attributes block panel and report exports at every selected surface", {
  ordinary <- .nr_export_ordinary()
  output <- file.path(withr::local_tempdir(), "must_not_create")
  for (key in c("source_nonresponse_readiness", "liss_source_nonresponse_readiness")) {
    for (schema in c("1.0.0", "2.0.0")) for (status in c("passed", "blocked", "not_evaluated")) {
      value <- list(schema_version = schema, status = status)
      for (input in .nr_export_forms(ordinary$data, key, value))
        .nr_export_expect_panel(input, file.path(output, "panel.sav"))
      for (input in .nr_export_report_forms(ordinary, key, value))
        .nr_export_expect_report(input, output)
    }
  }
})

test_that("known source classes survive bare exact partial and outer export claim checks", {
  ordinary <- .nr_export_ordinary()
  output <- file.path(withr::local_tempdir(), "must_not_create")
  for (claim in c("liss_source_nonresponse_result", "liss_source_nonresponse_v2_result",
    "liss_source_nonresponse_collection_result")) {
    frame <- ordinary$data
    class(frame) <- c(claim, class(frame))
    outer <- structure(list(data = ordinary$data), class = claim)
    for (input in list(frame, list(data = frame), list(data_partial = frame), outer))
      .nr_export_expect_panel(input, file.path(output, "panel.sav"))
    result <- ordinary
    result$data <- frame
    partial <- result
    names(partial)[names(partial) == "data"] <- "data_partial"
    outer <- ordinary
    class(outer) <- c(claim, class(outer))
    for (input in list(result, partial, outer)) .nr_export_expect_report(input, output)
  }
})

test_that("malformed source and readiness attributes remain positive export claims", {
  ordinary <- .nr_export_ordinary()
  values <- list(FALSE, NA, 1, "claimed", character(), list(),
    list(schema_version = "1.0.0", status = "unknown"))
  output <- file.path(withr::local_tempdir(), "must_not_create")
  for (key in c("liss_source_nonresponse", "source_nonresponse_readiness",
    "liss_source_nonresponse_readiness")) for (value in values) {
    for (input in .nr_export_forms(ordinary$data, key, value))
      .nr_export_expect_panel(input, file.path(output, "panel.sav"))
    for (input in .nr_export_report_forms(ordinary, key, value))
      .nr_export_expect_report(input, output)
  }
})

test_that("claim denial precedes report I O panel joins and panel writes", {
  ordinary <- .nr_export_ordinary()
  calls <- character()
  forbidden <- function(name) function(...) {
    calls <<- c(calls, name)
    stop("downstream I/O or joining was reached")
  }
  testthat::local_mocked_bindings(write_clean_jsonl = forbidden("jsonl"),
    build_cleaning_report_md = forbidden("markdown"),
    sanitize_spss_names = forbidden("sanitize"), .package = "lissr")
  testthat::local_mocked_bindings(write.csv = forbidden("csv"), .package = "utils")
  testthat::local_mocked_bindings(write_sav = forbidden("sav"), .package = "haven")
  testthat::local_mocked_bindings(full_join = forbidden("full_join"),
    inner_join = forbidden("inner_join"), left_join = forbidden("left_join"), .package = "dplyr")
  output <- file.path(withr::local_tempdir(), "must_not_create")
  marked <- ordinary
  attr(marked$data, "source_nonresponse_readiness") <- FALSE
  names(marked)[names(marked) == "data"] <- "data_partial"
  for (join in c("full", "inner", "left")) expect_error(
    suppressMessages(lissr::merge_liss_panel(list(ordinary = ordinary$data, claimed = marked),
      join_type = join, write_to = file.path(output, "panel.sav"))),
    "source_nonresponse.*lineage", class = "liss_source_nonresponse_blocked")
  expect_error(lissr::liss_cleaning_report(marked, output, verbose = FALSE),
    "guarded.*report export", class = "liss_source_nonresponse_blocked")
  expect_identical(calls, character())
  expect_false(file.exists(output))
})

test_that("ordinary classes neutral slots and earlier scalar denial classes are preserved", {
  ordinary <- .nr_export_ordinary()
  frame <- ordinary$data
  reference <- .nr_export_panel(frame)
  directory <- withr::local_tempdir()
  for (claim in c("liss_unguarded_result", "unrelated_result", "source_nonresponse_hint")) {
    wrapper <- structure(list(data = frame), class = claim)
    current <- .nr_export_panel(wrapper)
    expect_identical(current, reference)
    result <- ordinary
    class(result) <- c(claim, class(result))
    output <- file.path(directory, claim)
    paths <- lissr::liss_cleaning_report(result, output, verbose = FALSE)
    expect_length(paths, 3L)
    expect_true(all(file.exists(unlist(paths))))
  }
  neutral <- list(status = "not_evaluated")
  for (ready in list(neutral, c(neutral, list(reason = "reviewed source wave is absent")))) {
    expect_identical(.nr_export_panel(list(data = frame, source_nonresponse_readiness = ready)), reference)
    result <- ordinary
    result$source_nonresponse_readiness <- ready
    paths <- lissr::liss_cleaning_report(result, file.path(directory, basename(tempfile("neutral_"))), verbose = FALSE)
    expect_true(all(file.exists(unlist(paths))))
  }
  passed <- list(data = frame, source_nonresponse_readiness = list(status = "passed"))
  old_panel <- tryCatch(.nr_export_panel(passed), error = identity)
  expect_s3_class(old_panel, "rlang_error")
  expect_false(inherits(old_panel, "liss_source_nonresponse_blocked"))
  result <- ordinary
  result$source_nonresponse_readiness <- list(status = "passed")
  old_report <- tryCatch(lissr::liss_cleaning_report(result, file.path(directory, "passed")), error = identity)
  expect_s3_class(old_report, "rlang_error")
  expect_false(inherits(old_report, "liss_source_nonresponse_blocked"))
  expect_false(file.exists(file.path(directory, "passed")))
  scalar <- .nr_export_saved()$inputs$normalized
  old_panel <- tryCatch(.nr_export_panel(scalar), error = identity)
  expect_s3_class(old_panel, "rlang_error")
  expect_false(inherits(old_panel, "liss_source_nonresponse_blocked"))
  result <- ordinary
  result$source_nonresponse <- scalar$source_nonresponse
  old_report <- tryCatch(lissr::liss_cleaning_report(result, file.path(directory, "scalar")), error = identity)
  expect_s3_class(old_report, "rlang_error")
  expect_false(inherits(old_report, "liss_source_nonresponse_blocked"))
  expect_error(lissr::liss_cleaning_report(structure(list(data = frame), class = "liss_unguarded_result"),
    file.path(directory, "wrong_type")), "must be a liss_clean_result")
  expect_false(file.exists(file.path(directory, "wrong_type")))
})

test_that("actual absent scalar source merges retain neutral export readiness", {
  saved <- .nr_export_saved(2L)
  directory <- withr::local_tempdir("export_absent_source_")
  source <- saved$raw
  names(source) <- sub("^ci23p", "ci22o", names(source))
  haven::write_sav(source, file.path(directory, "ci22o_EN_1.0p.sav"))
  testthat::local_mocked_bindings(read_wave_file = function(path, ...) source,
    .liss_nonresponse_source_state = function(...) stop("absent source identity read"),
    .liss_nonresponse_read_source = function(...) stop("absent source read"), .package = "lissr")
  ordinary <- .nr_export_ordinary()
  for (schema in c("1.0.0", "2.0.0")) {
    recipe <- saved$fields[["337"]]$recipe
    if (schema == "1.0.0") recipe$source_nonresponse <- lissr::liss_recipe("ci")$source_nonresponse
    recipe$wave_index <- Filter(function(wave) wave$id == "ci22o", lissr::liss_recipe("ci")$wave_index)
    recipe$meta$covered_waves <- list("ci22o")
    merged <- suppressWarnings(suppressMessages(lissr::merge_liss_module(recipe,
      directory, output_dir = NULL, source_nonresponse = TRUE)))
    ready <- merged$source_nonresponse_readiness
    expect_identical(ready$status, "not_evaluated")
    expect_identical(ready$reason, "reviewed source wave is absent")
    expect_null(merged[["source_nonresponse"]])
    if (schema == "1.0.0") expect_identical(names(ready), c("status", "reason"))
    else expect_identical(as.numeric(unlist(ready[c("evaluated_cells", "classified_cells", "unresolved_cells")])), c(0, 0, 0))
    expect_identical(.nr_export_panel(merged), .nr_export_panel(merged$data))
    report <- ordinary
    report$source_nonresponse_readiness <- ready
    paths <- lissr::liss_cleaning_report(report, file.path(directory, paste0("report_", schema)), verbose = FALSE)
    expect_true(all(file.exists(unlist(paths))))
  }
})

test_that("ordinary partial matching warning vectors exact precedence and ambiguity stay unchanged", {
  ordinary <- .nr_export_ordinary()
  frame <- ordinary$data
  marked <- .nr_export_saved()$inputs$normalized$data
  directory <- withr::local_tempdir()
  withr::local_options(warnPartialMatchDollar = TRUE)
  capture <- function(expr) {
    warnings <- character()
    value <- withCallingHandlers(force(expr), warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    })
    list(value = value, warnings = warnings)
  }
  partial <- capture(.nr_export_panel(list(data_partial = frame)))
  expect_identical(partial$value, .nr_export_panel(list(data = frame)))
  expect_identical(partial$warnings, rep("partial match of 'data' to 'data_partial'", 5L))
  result <- ordinary
  names(result)[names(result) == "data"] <- "data_partial"
  report <- capture(lissr::liss_cleaning_report(result, file.path(directory, "partial"), verbose = FALSE))
  expect_identical(report$warnings, rep("partial match of 'data' to 'data_partial'", 3L))
  expect_true(all(file.exists(unlist(report$value))))
  precedence <- capture(.nr_export_panel(list(data = frame, data_partial = marked)))
  expect_identical(precedence$value, partial$value)
  expect_identical(precedence$warnings, character())
  result <- ordinary
  result$data_partial <- marked
  report <- capture(lissr::liss_cleaning_report(result, file.path(directory, "precedence"), verbose = FALSE))
  expect_identical(report$warnings, character())
  expect_true(all(file.exists(unlist(report$value))))
  expect_error(.nr_export_panel(list(data_first = frame, data_second = marked)), "no valid data frames")
  result <- ordinary
  names(result)[names(result) == "data"] <- "data_first"
  result$data_second <- marked
  report <- capture(lissr::liss_cleaning_report(result, file.path(directory, "ambiguous"), verbose = FALSE))
  expect_identical(report$warnings, character())
  expect_true(all(file.exists(unlist(report$value))))
})
