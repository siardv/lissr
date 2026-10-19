# these tests use invented rows and frozen synthetic carriers only
.nr_claim_saved <- function(version = 1L) {
  readRDS(testthat::test_path("fixtures",
    paste0("source_nonresponse_v", version, "_synthetic.rds")))
}

.nr_claim_frame <- function() {
  frame <- .nr_claim_saved(4L)$orders[[1L]]$input$data
  attr(frame, "liss_source_nonresponse") <- NULL
  frame
}

.nr_claim_clean <- function(input, mode = "flag", ...) {
  lissr::liss_clean_income(input, mode = mode, variables = list(target = "s339"),
    verbose = FALSE, ...)
}

.nr_claim_forms <- function(frame, key, value) {
  marked_frame <- frame
  attr(marked_frame, key) <- value
  marked_wrapper <- list(data = frame)
  attr(marked_wrapper, key) <- value
  list(frame = marked_frame, nested_frame = list(data = marked_frame),
    outer_wrapper = marked_wrapper)
}

.nr_claim_stable <- function(result) {
  result$summary$duration_s <- NULL
  result$ruleset$meta$source_path <- NULL
  result$log <- lapply(result$log, function(entry) {
    entry$timestamp <- NULL
    entry
  })
  result
}

test_that("the seven inherited class and readiness claims cannot become ordinary input", {
  frame <- .nr_claim_frame()
  class_only <- frame
  class(class_only) <- c("liss_source_nonresponse_result", "data.frame")
  inputs <- list(class_only = class_only)
  for (schema in c("1.0.0", "2.0.0")) for (status in c("passed", "blocked", "not_evaluated")) {
    value <- frame
    attr(value, "source_nonresponse_readiness") <- list(schema_version = schema, status = status)
    inputs[[paste(schema, status)]] <- value
  }
  expect_length(inputs, 7L)
  for (name in names(inputs)) expect_error(.nr_claim_clean(inputs[[name]]),
    class = "liss_source_nonresponse_blocked", info = name)
})

test_that("known source classes block on bare frames nested frames and wrappers", {
  frame <- .nr_claim_frame()
  classes <- c("liss_source_nonresponse_result", "liss_source_nonresponse_v2_result",
    "liss_source_nonresponse_collection_result")
  for (claim in classes) {
    marked_frame <- frame
    class(marked_frame) <- c(claim, class(marked_frame))
    marked_wrapper <- structure(list(data = frame), class = claim)
    for (input in list(marked_frame, list(data = marked_frame), marked_wrapper)) {
      before <- serialize(input, NULL, version = 2L)
      for (mode in c("flag", "correct", "na_only")) for (override in list(
        list(), list(disable = c("P01", "D01", "C01")), list(enable_only = character()))) {
        expect_error(do.call(.nr_claim_clean, c(list(input = input, mode = mode), override)),
          class = "liss_source_nonresponse_blocked", info = paste(claim, mode))
      }
      expect_identical(serialize(input, NULL, version = 2L), before)
    }
  }
})

test_that("readiness attributes require carriers for every schema status and placement", {
  frame <- .nr_claim_frame()
  for (key in c("source_nonresponse_readiness", "liss_source_nonresponse_readiness")) {
    for (schema in c("1.0.0", "2.0.0", "3.0.0", "4.0.0")) {
      for (status in c("passed", "blocked", "not_evaluated")) {
        forms <- .nr_claim_forms(frame, key, list(schema_version = schema, status = status))
        for (name in names(forms)) for (mode in c("flag", "correct", "na_only")) {
          expect_error(.nr_claim_clean(forms[[name]], mode = mode),
            class = "liss_source_nonresponse_blocked",
            info = paste(key, schema, status, name, mode))
        }
      }
    }
  }
})

test_that("claims on a frame cannot escape the existing partial data unwrap", {
  frame <- .nr_claim_frame()
  inputs <- list()
  for (claim in c("liss_source_nonresponse_result", "liss_source_nonresponse_v2_result",
    "liss_source_nonresponse_collection_result")) {
    marked <- frame
    class(marked) <- c(claim, class(marked))
    inputs[[claim]] <- list(data_partial = marked)
  }
  for (key in c("source_nonresponse_readiness", "liss_source_nonresponse_readiness")) {
    marked <- frame
    attr(marked, key) <- list(schema_version = "1.0.0", status = "not_evaluated")
    inputs[[key]] <- list(data_partial = marked)
  }
  for (name in names(inputs)) for (mode in c("flag", "correct", "na_only"))
    expect_error(.nr_claim_clean(inputs[[name]], mode = mode),
      class = "liss_source_nonresponse_blocked", info = paste(name, mode))
  saved <- .nr_claim_saved()
  for (mode in c("flag", "correct", "na_only")) {
    current <- suppressWarnings(.nr_claim_clean(list(data_partial = saved$inputs$unguarded), mode = mode))
    .nr_expect_frozen_unguarded(.nr_claim_stable(current), saved$expected$unguarded[[mode]])
  }
  # the frozen T2c baseline emits five partial-$data warnings for this wrapper
  withr::local_options(warnPartialMatchDollar = TRUE)
  warnings <- character()
  current <- withCallingHandlers(.nr_claim_clean(list(data_partial = saved$inputs$unguarded)),
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    })
  expect_identical(warnings, rep("partial match of 'data' to 'data_partial'", 5L))
  .nr_expect_frozen_unguarded(.nr_claim_stable(current), saved$expected$unguarded$flag)
})

test_that("malformed readiness attributes cannot evade missing carrier rejection", {
  values <- list(FALSE, NA, 1, "not_evaluated", character(), list(),
    list(status = NA_character_), list(status = "unknown"), list(status = character()),
    list(schema_version = "99.0.0", status = "not_evaluated"))
  for (key in c("source_nonresponse_readiness", "liss_source_nonresponse_readiness")) {
    for (index in seq_along(values)) {
      forms <- .nr_claim_forms(.nr_claim_frame(), key, values[[index]])
      for (name in names(forms)) expect_error(.nr_claim_clean(forms[[name]]),
        class = "liss_source_nonresponse_blocked", info = paste(key, index, name))
    }
  }
})

test_that("partial wrappers retain consumer claims at every lineage discarding gate", {
  saved <- .nr_claim_saved(4L)$orders[[1L]]$input
  bare <- .nr_claim_frame()
  inputs <- list()
  for (intact in c(FALSE, TRUE)) {
    frame <- if (intact) saved$data else bare
    marked <- frame
    class(marked) <- c("liss_clean_collection_flag_result", class(marked))
    inputs[[paste("consumer_class", intact)]] <- list(data_partial = marked)
    for (key in c("source_nonresponse_consumer_readiness", "liss_source_nonresponse_consumer_readiness")) {
      marked <- frame
      attr(marked, key) <- list()
      inputs[[paste(key, intact)]] <- list(data_partial = marked)
    }
    residual <- frame
    class(residual) <- c("liss_clean_result", "liss_source_nonresponse_collection_result", class(residual))
    inputs[[paste("residual_classes", intact)]] <- list(data_partial = residual)
  }
  output <- file.path(withr::local_tempdir(), "must_not_export")
  for (name in names(inputs)) {
    input <- inputs[[name]]
    before <- serialize(input, NULL, version = 2L)
    expect_true(lissr:::.liss_nonresponse_claims_consumer(input), info = name)
    for (mode in c("flag", "correct", "na_only")) expect_error(
      .nr_claim_clean(input, mode = mode, ruleset = stop("ruleset promise was forced")),
      "previous collection consumer", class = "liss_source_nonresponse_blocked", info = name)
    expect_error(lissr::liss_classify_source_nonresponse(input,
      file.path(dirname(output), "missing_ci23p_EN_1.0p.sav"), "s339", "wave_id"),
      "existing collection lineage", class = "liss_source_nonresponse_blocked", info = name)
    expect_error(lissr::merge_liss_panel(list(ci = input)), "collection lineage",
      class = "liss_source_nonresponse_blocked", info = name)
    expect_error(lissr::liss_cleaning_report(input, output, verbose = FALSE),
      "guarded collection report", class = "liss_source_nonresponse_blocked", info = name)
    expect_false(file.exists(output))
    expect_identical(serialize(input, NULL, version = 2L), before)
  }
})

test_that("outer source attributes cannot be discarded by wrapper unwrapping", {
  frame <- .nr_claim_frame()
  states <- list(FALSE, NA, 1, "claimed", character(), list(),
    list(schema_version = "1.0.0"), list(schema_version = "2.0.0"),
    list(schema_version = "3.0.0"), list(schema_version = "4.0.0"),
    .nr_claim_saved()$inputs$canonical$source_nonresponse)
  for (index in seq_along(states)) {
    input <- list(data = frame)
    attr(input, "liss_source_nonresponse") <- states[[index]]
    for (mode in c("flag", "correct", "na_only")) expect_error(
      .nr_claim_clean(input, mode = mode), class = "liss_source_nonresponse_blocked",
      info = paste(index, mode))
  }
  for (value in list(FALSE, NA, 1, "claimed", character(), list(),
    list(schema_version = "1.0.0"), list(schema_version = "2.0.0"),
    list(schema_version = "3.0.0"), list(schema_version = "4.0.0"))) {
    input <- frame
    attr(input, "liss_source_nonresponse") <- value
    expect_error(.nr_claim_clean(input), class = "liss_source_nonresponse_blocked")
    for (mode in c("flag", "correct", "na_only")) expect_error(
      .nr_claim_clean(list(data = frame, source_nonresponse = value), mode = mode),
      class = "liss_source_nonresponse_blocked")
  }
})

test_that("missing carrier rejection precedes target rules background and output work", {
  calls <- character()
  forbidden <- function(name) function(...) {
    calls <<- c(calls, name)
    stop("downstream work was reached")
  }
  testthat::local_mocked_bindings(
    liss_cleaning_ruleset = forbidden("ruleset"),
    apply_ruleset_overrides = forbidden("overrides"),
    resolve_cleaning_variables = forbidden("variables"),
    attach_background_frame = forbidden("background"),
    .liss_nonresponse_source_state = forbidden("source_identity"),
    .liss_nonresponse_read_source = forbidden("source_read"), .package = "lissr")
  output <- file.path(withr::local_tempdir(), "must_not_exist")
  inputs <- list(
    structure(data.frame(unrelated = 1), class = c("liss_source_nonresponse_result", "data.frame")),
    structure(list(data = data.frame(unrelated = 1)), liss_source_nonresponse = FALSE),
    list(data = structure(data.frame(unrelated = 1), source_nonresponse_readiness = FALSE)))
  for (input in inputs) for (mode in c("flag", "correct", "na_only")) {
    before <- serialize(input, NULL, version = 2L)
    expect_error(lissr::liss_clean_income(input, mode = mode,
      background = stop("background promise was forced"),
      ruleset = stop("ruleset promise was forced"),
      variables = list(target = "absent"),
      params = stop("params promise was forced"), output_dir = output, verbose = FALSE),
      class = "liss_source_nonresponse_blocked")
    expect_identical(serialize(input, NULL, version = 2L), before)
  }
  expect_identical(calls, character())
  expect_false(file.exists(output))
})

test_that("neutral unclassified wrapper readiness slots preserve ordinary cleaning", {
  saved <- .nr_claim_saved()
  frame <- saved$inputs$unguarded
  for (input in list(frame, list(data = frame), structure(list(data = frame), class = "liss_unguarded_result"),
    list(data = frame, source_nonresponse_readiness = list(status = "not_evaluated")),
    list(data = frame, source_nonresponse_readiness = list(schema_version = "1.0.0", status = "not_evaluated")),
    list(data = frame, source_nonresponse_readiness = list(schema_version = "2.0.0", status = "not_evaluated")))) {
    for (mode in c("flag", "correct", "na_only")) {
      current <- suppressWarnings(.nr_claim_clean(input, mode = mode))
      .nr_expect_frozen_unguarded(.nr_claim_stable(current), saved$expected$unguarded[[mode]])
      expect_null(current[["source_nonresponse"]])
      expect_identical(current$source_nonresponse_readiness$status, "not_evaluated")
    }
  }
  expect_error(.nr_claim_clean(list(data = frame,
    source_nonresponse_readiness = list(status = "passed"))),
    class = "liss_source_nonresponse_blocked")
})

test_that("real absent source waves keep native scalar readiness and no false carrier", {
  saved <- .nr_claim_saved(2L)
  directory <- withr::local_tempdir("claim_absent_wave_")
  source <- saved$raw
  names(source) <- sub("^ci23p", "ci22o", names(source))
  haven::write_sav(source, file.path(directory, "ci22o_EN_1.0p.sav"))
  testthat::local_mocked_bindings(read_wave_file = function(path, ...) source,
    .liss_nonresponse_source_state = function(...) stop("absent source identity read"),
    .liss_nonresponse_read_source = function(...) stop("absent source read"), .package = "lissr")
  for (schema in c("1.0.0", "2.0.0")) {
    recipe <- saved$fields[["337"]]$recipe
    if (schema == "1.0.0") recipe$source_nonresponse <- lissr::liss_recipe("ci")$source_nonresponse
    full <- lissr::liss_recipe("ci")
    recipe$wave_index <- Filter(function(wave) wave$id == "ci22o", full$wave_index)
    recipe$meta$covered_waves <- list("ci22o")
    result <- suppressWarnings(suppressMessages(lissr::merge_liss_module(recipe,
      directory, output_dir = NULL, source_nonresponse = TRUE)))
    expect_null(result[["source_nonresponse"]])
    expect_null(attr(result$data, "liss_source_nonresponse", exact = TRUE))
    expect_identical(result$source_nonresponse_readiness$status, "not_evaluated")
    expect_identical(result$source_nonresponse_readiness$reason, "reviewed source wave is absent")
    if (schema == "1.0.0") {
      expect_identical(result$source_nonresponse_readiness,
        list(status = "not_evaluated", reason = "reviewed source wave is absent"))
    } else {
      expect_equal(result$source_nonresponse_readiness$evaluated_cells, 0)
      expect_equal(result$source_nonresponse_readiness$classified_cells, 0)
      expect_equal(result$source_nonresponse_readiness$unresolved_cells, 0)
      expect_identical(result$source_nonresponse_readiness$field_id, "CI23P337_FIELD")
      expect_identical(result$source_nonresponse_readiness$case_counts,
        c(CI23P337_N01 = 0L, CI23P337_N02 = 0L))
    }
    cleaned <- suppressWarnings(.nr_claim_clean(result, disable = "P01"))
    expect_null(cleaned[["source_nonresponse"]])
    expect_identical(cleaned$source_nonresponse_readiness$status, "not_evaluated")
    expect_identical(cleaned$source_nonresponse_readiness$reason,
      "no source-bound classification supplied")
  }
})

test_that("complete frozen scalar and collection inputs retain their contracts", {
  saved <- .nr_claim_saved()
  for (name in names(saved$validated)) {
    input <- saved$inputs[[name]]
    state <- input$source_nonresponse
    expect_identical(lissr:::source_nonresponse_validate(input$data, state,
      state$target, state$wave_col), saved$validated[[name]], info = name)
    for (mode in names(saved$expected[[name]])) {
      current <- suppressWarnings(lissr::liss_clean_income(input, mode = mode,
        variables = list(target = state$target), verbose = FALSE))
      expect_identical(.nr_claim_stable(current), saved$expected[[name]][[mode]],
        info = paste(name, mode))
    }
  }
  for (suffix in c("337", "338")) {
    saved <- .nr_claim_saved(2L)$fields[[suffix]]
    for (name in names(saved$inputs)) {
      input <- saved$inputs[[name]]
      state <- input$source_nonresponse
      expect_identical(lissr:::source_nonresponse_validate(input$data, state,
        state$target, state$wave_col), saved$validated[[name]], info = paste(suffix, name))
      expect_error(.nr_claim_clean(input), class = "liss_source_nonresponse_blocked")
    }
  }
  for (version in c(3L, 4L)) for (saved in .nr_claim_saved(version)$orders) {
    input <- saved$input
    expect_identical(lissr:::source_nonresponse_validate(input, input$source_nonresponse),
      saved$validated)
    if (version == 3L) {
      expect_error(.nr_claim_clean(input), class = "liss_source_nonresponse_blocked")
    } else {
      current <- suppressWarnings(.nr_claim_clean(input))
      checked <- lissr:::source_nonresponse_validate(current, current$source_nonresponse)
      expect_identical(checked$state, current$source_nonresponse)
      expect_identical(current$source_nonresponse, input$source_nonresponse)
      expect_s3_class(current, "liss_clean_collection_flag_result")
      for (remove_class in c(FALSE, TRUE)) for (remove_record in c(FALSE, TRUE)) {
        if (!remove_class && !remove_record) next
        bad <- current
        if (remove_class) class(bad) <- setdiff(class(bad), "liss_clean_collection_flag_result")
        if (remove_record) bad$source_nonresponse_consumer_readiness <- NULL
        expect_error(.nr_claim_clean(bad), class = "liss_source_nonresponse_blocked")
        expect_error(lissr:::source_nonresponse_validate(bad, bad$source_nonresponse),
          class = "liss_source_nonresponse_blocked")
      }
    }
  }
})
