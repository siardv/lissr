# all rows and source receipts below are invented; no original SAV is read
.nra_saved <- function(version = 4L) {
  readRDS(testthat::test_path("fixtures",
    paste0("source_nonresponse_v", version, "_synthetic.rds")))
}

.nra_fixture <- function(kind = "sign_cap", .local_envir = parent.frame()) {
  saved <- .nra_saved()
  raw <- saved$raw
  if (kind == "sign_cap") {
    raw$ci23p339[2L] <- -17000
    raw$ci23p339[3L] <- 25000
  } else if (kind == "household") {
    raw$ci23p339[2:4] <- c(17000, 170000, 17000)
    raw$nohouse_encr[2:4] <- 2
  }
  directory <- withr::local_tempdir("applied_invented_", .local_envir = .local_envir)
  source <- file.path(directory, saved$mocked_receipt$filename)
  file.create(source)
  testthat::local_mocked_bindings(
    .liss_nonresponse_source_state = function(path) saved$mocked_receipt,
    .liss_nonresponse_read_source = function(path) raw,
    read_wave_file = function(path, ...) raw,
    .package = "lissr", .env = .local_envir)
  input <- suppressWarnings(suppressMessages(lissr::merge_liss_module(
    saved$orders[[1L]]$recipe, directory, output_dir = NULL,
    source_nonresponse = TRUE)))
  list(input = input, saved = saved, source = source, directory = directory)
}

.nra_clean <- function(input, mode = "correct", ...) {
  suppressWarnings(lissr::liss_clean_income(input, mode = mode,
    variables = list(target = "s339"), verbose = FALSE, ...))
}

.nra_scalar <- function(input) {
  # this invented-only comparator is not a public collection projection
  if (is.data.frame(input)) {
    frame <- input
    state <- attr(input, "liss_source_nonresponse", exact = TRUE)
  } else {
    frame <- input$data
    state <- input$source_nonresponse
  }
  checked <- lissr:::source_nonresponse_validate(input, state)
  leaf <- checked$state$fields$CI23P339_FIELD
  attr(frame, "liss_source_nonresponse") <- leaf
  structure(list(data = frame, source_nonresponse = leaf,
    source_nonresponse_readiness = leaf$readiness),
    class = "liss_source_nonresponse_result")
}

.nra_stable <- function(result) {
  result$summary$duration_s <- NULL
  result$ruleset$meta$source_path <- NULL
  result$log <- lapply(result$log, function(entry) {
    entry$timestamp <- NULL
    entry
  })
  result
}

.nra_sign_overrides <- function() {
  rules <- lissr::liss_cleaning_ruleset()
  list(income_cap = 20000,
    disable = vapply(rules$detection_rules, function(rule) rule$rule_id, character(1)),
    params = list(F01 = list(disposition = "winsorise")))
}

.nra_expect_preserved <- function(input, result) {
  before <- if (is.data.frame(input)) input else input$data
  expect_identical(result$source_input, input)
  expect_identical(names(result$data)[seq_along(before)], names(before))
  expect_identical(nrow(result$data), nrow(before))
  expect_identical(row.names(result$data), row.names(before))
  for (column in setdiff(names(before), "s339"))
    expect_identical(result$data[[column]], before[[column]], info = column)
  expect_null(attr(result$data, "liss_source_nonresponse", exact = TRUE))
  expect_false(inherits(result$data, "liss_source_nonresponse_collection_result"))
  expect_s3_class(result$data, "liss_applied_income_frame")
  expect_true(is.list(attr(result$data, "liss_source_nonresponse_applied", exact = TRUE)))
}

test_that("whole applied results keep native source separate from derived analysis", {
  saved <- .nra_saved()
  for (input in list(saved$orders[[1L]]$complete_merge_result,
                     saved$orders[[1L]]$input$data)) {
    for (mode in c("correct", "na_only")) {
      result <- .nra_clean(input, mode)
      expect_identical(class(result),
        c("liss_clean_collection_applied_result", "liss_clean_result"))
      expect_identical(names(result), c("data", "decisions", "log", "summary",
        "ruleset", "variables", "mode", "source_input", "invocation", "applied_lineage"))
      expect_identical(result$mode, mode)
      .nra_expect_preserved(input, result)
      expect_no_error(lissr::liss_validate_applied_income(result))
      expect_no_error(lissr::liss_validate_applied_income(result, replay = TRUE))
      expect_equal(result$data$s339_observed[1L], -9999999999)
      expect_true(is.na(result$data$s339[1L]))
      expect_identical(result$data$s339_clean_status[1L],
        "source_nonresponse:missing_amount")
      state <- if (is.data.frame(input)) attr(input, "liss_source_nonresponse", exact = TRUE)
        else input$source_nonresponse
      expect_error(lissr:::source_nonresponse_validate(result$data, state),
        class = "liss_source_nonresponse_blocked")
    }
  }
})

test_that("correct and na_only each reproduce their own invented sign and cap control", {
  fixture <- .nra_fixture()
  for (mode in c("correct", "na_only")) {
    arguments <- .nra_sign_overrides()
    result <- do.call(.nra_clean, c(list(input = fixture$input, mode = mode), arguments))
    scalar <- do.call(.nra_clean,
      c(list(input = .nra_scalar(fixture$input), mode = mode), arguments))
    .nra_expect_preserved(fixture$input, result)
    expect_identical(result$data$s339_observed, scalar$data$s339_observed)
    expect_identical(result$data$s339, scalar$data$s339)
    expect_identical(result$data$s339_clean_status, scalar$data$s339_clean_status)
    expect_identical(result$decisions, scalar$decisions)
    expect_identical(.nra_stable(result)$summary, .nra_stable(scalar)$summary)
    expect_equal(as.numeric(result$data$s339[2:3]), c(17000, 20000))
    expect_equal(result$data$s339_observed[2:3], c(-17000, 25000))
    expect_equal(result$summary$n_rectified, 1L)
    expect_equal(result$summary$n_winsorised, 1L)
    expect_no_error(lissr::liss_validate_applied_income(result, replay = TRUE))
  }
})

test_that("invented household correction and na_only have distinct D06 outcomes", {
  fixture <- .nra_fixture("household")
  for (mode in c("correct", "na_only")) {
    result <- .nra_clean(fixture$input, mode)
    scalar <- .nra_clean(.nra_scalar(fixture$input), mode)
    expect_identical(result$data$s339, scalar$data$s339)
    expect_identical(result$decisions, scalar$decisions)
    expect_equal(result$data$s339_observed[3L], 170000)
    expect_identical(result$decisions$rule_id, "D06")
    expect_identical(result$decisions$row, 3L)
    if (mode == "correct") {
      expect_equal(as.numeric(result$data$s339[3L]), 17000)
      expect_identical(result$decisions$action, "correct")
    } else {
      expect_true(is.na(result$data$s339[3L]))
      expect_identical(result$decisions$action, "set_na")
    }
    expect_no_error(lissr::liss_validate_applied_income(result, replay = TRUE))
  }
})

test_that("complete accepted native domains include no-hit mixed empty and keyed views", {
  saved <- .nra_saved()
  inputs <- list(saved$orders[[1L]]$subsets$ordinary$input,
    saved$orders[[1L]]$subsets$reverse$input,
    saved$orders[[1L]]$subsets$empty$input,
    saved$mixed_wave$input,
    saved$mixed_wave$subsets$no_ci23p$input)
  for (input in inputs) for (mode in c("correct", "na_only")) {
    result <- .nra_clean(input, mode)
    .nra_expect_preserved(input, result)
    scalar <- .nra_clean(.nra_scalar(input), mode)
    expect_identical(result$data$s339, scalar$data$s339)
    expect_identical(result$data$s339_observed, scalar$data$s339_observed)
    expect_no_error(lissr::liss_validate_applied_income(result))
    expect_no_error(lissr::liss_validate_applied_income(result, replay = TRUE))
  }
})

test_that("applied entry requires explicit exact household mode target and bounded context", {
  input <- .nra_saved()$orders[[1L]]$input
  invalid <- list(list(), list(variables = list(target = "s339")),
    list(mode = "correct"), list(mode = "na_only"),
    list(mode = "correct", variables = list(targ = "s339")),
    list(mode = "correct", variables = list(target = "s339", target = "s339")),
    list(mode = "correct", variables = list(target = c("s339", "s339"))),
    list(mode = "correct", variables = list(target = structure("s339", label = "claim"))),
    list(mode = "correct", variables = list(target = factor("s339"))),
    list(mode = "correct", variables = list(target = "s339"),
      background = data.frame(nomem_encr = 1)),
    list(mode = "na_only", variables = list(target = "s339"),
      output_dir = tempfile("must_not_write_applied_")))
  for (target in c("s337", "s338", "nethh", "ci23p339", "ci_s339"))
    invalid[[length(invalid) + 1L]] <- list(mode = "correct",
      variables = list(target = target))
  reached <- character()
  testthat::local_mocked_bindings(liss_cleaning_ruleset = function(...) {
    reached <<- c(reached, "rules")
    stop("invalid entry reached rules")
  }, attach_background_frame = function(...) {
    reached <<- c(reached, "background")
    stop("invalid entry reached background")
  }, .package = "lissr")
  for (arguments in invalid) {
    expect_error(do.call(lissr::liss_clean_income,
      c(list(data = input, verbose = FALSE), arguments)),
      class = "liss_source_nonresponse_blocked")
    expect_length(reached, 0L)
    if (!is.null(arguments$output_dir)) expect_false(dir.exists(arguments$output_dir))
  }
})

test_that("incomplete native parents reject before household rules or numeric work", {
  input <- .nra_saved()$orders[[1L]]$input
  no_frame_receipt <- input
  attr(no_frame_receipt$data, "liss_source_nonresponse") <- NULL
  no_wrapper_receipt <- input
  no_wrapper_receipt$source_nonresponse <- NULL
  class_only <- no_frame_receipt
  class_only$source_nonresponse <- NULL
  class_only$source_nonresponse_readiness <- NULL
  reached <- FALSE
  testthat::local_mocked_bindings(liss_cleaning_ruleset = function(...) {
    reached <<- TRUE
    stop("partial parent reached rules")
  }, numeric_view = function(...) {
    reached <<- TRUE
    stop("partial parent reached numerical work")
  }, .package = "lissr")
  for (parent in list(no_frame_receipt, no_wrapper_receipt, class_only))
    for (mode in c("correct", "na_only")) {
      expect_error(.nra_clean(parent, mode), class = "liss_source_nonresponse_blocked")
      expect_false(reached)
    }
})

test_that("the separate applied policy requires its exact package-owned authority bytes", {
  policy <- lissr:::.liss_applied_policy()
  expect_identical(policy$policy_id, "CI23P339_V4_APPLIED_CONSUMER")
  expect_identical(policy$policy_version, "1.0.0")
  expect_identical(policy$capability$capability_id, "CI23P339_V4_APPLIED")
  expect_identical(policy$capability$modes, c("correct", "na_only"))
  original <- lissr:::.liss_applied_policy_path()
  changed <- withr::local_tempfile(fileext = ".yml")
  writeLines(c(readLines(original, warn = FALSE), "unreviewed: true"), changed)
  testthat::local_mocked_bindings(.liss_applied_policy_path = function() changed,
    .package = "lissr")
  expect_error(lissr:::.liss_applied_policy(), class = "liss_source_nonresponse_blocked")
})

test_that("all sealed secondary roles reject before numeric preparation", {
  input <- .nra_saved()$orders[[1L]]$input
  roles <- c("person_id", "household_id", "wave", "personal_net", "personal_gross",
    "household_size", "category_code", "category_min", "category_max", "donor_keys",
    "birth_year", "age")
  calls <- 0L
  testthat::local_mocked_bindings(numeric_view = function(...) {
    calls <<- calls + 1L
    stop("sealed role reached numeric preparation")
  }, .package = "lissr")
  for (mode in c("correct", "na_only")) for (column in c("s337", "s338", "s339"))
    for (role in roles) {
      variables <- list(target = "s339")
      variables[[role]] <- column
      expect_error(lissr::liss_clean_income(input, mode = mode,
        variables = variables, verbose = FALSE), class = "liss_source_nonresponse_blocked")
      expect_equal(calls, 0L)
    }
})

test_that("repeat applied claims reject before rules background numeric and report work", {
  result <- .nra_clean(.nra_saved()$orders[[1L]]$input)
  reached <- character()
  testthat::local_mocked_bindings(liss_cleaning_ruleset = function(...) {
    reached <<- c(reached, "rules")
    stop("repeat reached rules")
  }, attach_background_frame = function(...) {
    reached <<- c(reached, "background")
    stop("repeat reached background")
  }, numeric_view = function(...) {
    reached <<- c(reached, "numeric")
    stop("repeat reached numeric")
  }, liss_cleaning_report = function(...) {
    reached <<- c(reached, "report")
    stop("repeat reached report")
  }, .package = "lissr")
  for (input in list(result, result$data, result$data[1:3, , drop = FALSE],
                     list(data = result$data))) for (mode in c("correct", "flag", "na_only")) {
    output <- tempfile("repeat_must_not_write_")
    expect_error(lissr::liss_clean_income(input, mode = mode,
      variables = list(target = "s339"), ruleset = "missing_ruleset",
      background = data.frame(nomem_encr = 1), output_dir = output, verbose = FALSE),
      class = "liss_source_nonresponse_blocked")
    expect_length(reached, 0L)
    expect_false(dir.exists(output))
  }
})

test_that("scoped source readiness remains distinct from analysis execution and parent claims", {
  saved <- .nra_saved()
  input <- saved$mixed_wave$subsets$no_ci23p$input
  parent <- input$source_nonresponse_readiness
  expect_identical(parent$status, "passed")
  expect_true(parent$evaluated_cells > 0)
  result <- .nra_clean(input, "na_only")
  checked <- lissr:::source_nonresponse_validate(input, input$source_nonresponse)
  validation <- lissr::liss_validate_applied_income(result)
  expect_identical(result$source_input, input)
  expect_identical(result$source_input$source_nonresponse_readiness, parent)
  expect_identical(result$applied_lineage$source_readiness, checked$readiness)
  expect_identical(validation$source_readiness$status, "not_evaluated")
  expect_equal(validation$source_readiness$evaluated_cells, 0)
  expect_identical(validation$analysis_execution_status, "completed")
  expect_equal(nrow(result$data), 12L)
  empty <- .nra_clean(saved$orders[[1L]]$subsets$empty$input)
  validated_empty <- lissr::liss_validate_applied_income(empty, replay = TRUE)
  expect_identical(validated_empty$analysis_execution_status, "not_evaluated")
  expect_identical(validated_empty$source_readiness$status, "not_evaluated")
  expect_equal(nrow(empty$data), 0L)
})

test_that("raw bound and applied target metadata are recorded on separate surfaces", {
  input <- .nra_saved()$orders[[1L]]$complete_merge_result
  result <- .nra_clean(input)
  metadata <- result$applied_lineage$target_metadata
  expect_identical(metadata$raw,
    input$source_nonresponse$fields$CI23P339_FIELD$metadata$attributes)
  expect_identical(metadata$input, attributes(input$data$s339))
  expect_identical(metadata$output, attributes(result$data$s339))
  expect_identical(result$source_input$source_nonresponse$fields$CI23P337_FIELD,
    input$source_nonresponse$fields$CI23P337_FIELD)
  expect_identical(result$source_input$source_nonresponse$fields$CI23P338_FIELD,
    input$source_nonresponse$fields$CI23P338_FIELD)
  expect_no_error(lissr::liss_validate_applied_income(result, replay = TRUE))
})

test_that("structural and replay validation report their distinct assurance", {
  result <- .nra_clean(.nra_saved()$orders[[1L]]$input)
  structural <- lissr::liss_validate_applied_income(result)
  replay <- lissr::liss_validate_applied_income(result, replay = TRUE)
  expect_identical(structural$valid, TRUE)
  expect_identical(structural$validation, "structural")
  expect_identical(structural$replay_status, "not_requested")
  expect_identical(replay$valid, TRUE)
  expect_identical(replay$validation, "replay")
  expect_identical(replay$replay_status, "matched")
  for (value in list(NA, 1L, c(TRUE, FALSE), structure(TRUE, label = "claim")))
    expect_error(lissr::liss_validate_applied_income(result, replay = value))
})

test_that("local digest resealing alone cannot establish numeric execution", {
  fixture <- .nra_fixture("household")
  result <- .nra_clean(fixture$input)
  for (mutation in c("amount", "decision")) {
    bad <- result
    if (mutation == "amount") bad$data$s339[4L] <- 12345 else
      bad$decisions$evidence[1L] <- "fabricated numerical evidence"
    bad <- lissr:::.liss_applied_reseal(bad)
    expect_no_error(lissr::liss_validate_applied_income(bad))
    expect_error(lissr::liss_validate_applied_income(bad, replay = TRUE),
      class = "liss_source_nonresponse_blocked")
  }
})

test_that("recorded structure remains readable when current engine or runtime cannot replay", {
  result <- .nra_clean(.nra_saved()$orders[[1L]]$input)
  c_engine <- withr::with_locale(c(LC_COLLATE = "C"), lissr:::.liss_applied_engine())
  default_engine <- withr::with_locale(c(LC_COLLATE = ""), lissr:::.liss_applied_engine())
  expect_identical(c_engine, default_engine)
  for (section in c("functions", "runtime")) local({
    changed <- result$applied_lineage$engine
    changed[[section]] <- list(unavailable_identity = "different_local_engine")
    testthat::local_mocked_bindings(.liss_applied_engine = function() changed,
      .package = "lissr", .env = environment())
    expect_no_error(lissr::liss_validate_applied_income(result))
    expect_error(lissr::liss_validate_applied_income(result, replay = TRUE),
      class = "liss_source_nonresponse_blocked")
  })
})

test_that("structural validation rejects protected source and identity mutations after resealing", {
  result <- .nra_clean(.nra_saved()$orders[[1L]]$input)
  mutations <- list(
    function(x) { x$data$s339[1L] <- 1; x },
    function(x) { x$data$s339_observed[1L] <- 1; x },
    function(x) { x$data$s339_clean_status[1L] <- "ordinary"; x },
    function(x) { x$data$s337[4L] <- 1; x },
    function(x) { x$data$nomem_encr[4L] <- 1; x },
    function(x) { x$data$wave_id[4L] <- "ci22o"; x },
    function(x) { attr(x$data$s338, "label") <- "different construct"; x },
    function(x) { x$source_input$data$s339[4L] <- 1; x },
    function(x) { x$source_input$source_nonresponse$fields$CI23P337_FIELD$coverage$original_value[1L] <- 1; x },
    function(x) { x$data <- x$data[rev(seq_len(nrow(x$data))), , drop = FALSE]; x },
    function(x) { x$data <- x$data[1:3, , drop = FALSE]; x },
    function(x) { x$data <- x$data[c(1L, 1L), , drop = FALSE]; x })
  for (mutate in mutations) {
    bad <- mutate(result)
    expect_error({
      resealed <- lissr:::.liss_applied_reseal(bad)
      lissr::liss_validate_applied_income(resealed)
    }, class = "liss_source_nonresponse_blocked")
  }
})

test_that("unsupported result and invocation shapes cannot be repaired by hashing", {
  result <- .nra_clean(.nra_saved()$orders[[1L]]$input)
  partial_target <- result$invocation
  partial_target$variables <- list(target_aliases = "s339")
  expect_error(lissr:::.liss_applied_invocation_validate(partial_target),
    class = "liss_source_nonresponse_blocked")
  mutations <- list(
    function(x) { x$unreviewed <- TRUE; x },
    function(x) { attr(x, "unreviewed") <- TRUE; x },
    function(x) { x$applied_lineage$unreviewed <- TRUE; x },
    function(x) { attr(x$invocation, "unreviewed") <- TRUE; x },
    function(x) { attr(x$mode, "label") <- "claimed"; x },
    function(x) { x$invocation$variables$personal_net <- "s337"; x },
    function(x) { x$invocation$variables$target <- c("s339", "s339"); x },
    function(x) { x$invocation$variables <- list(target_aliases = "s339"); x },
    function(x) { x$invocation$params <- list(F01 = list(disposition = c("void", "flag"))); x },
    function(x) { attr(x$applied_lineage$row_keys, "unreviewed") <- TRUE; x },
    function(x) { x$applied_lineage$source_readiness$status <- "passed_globally"; x })
  for (mutate in mutations) {
    expect_error({
      bad <- lissr:::.liss_applied_reseal(mutate(result))
      lissr::liss_validate_applied_income(bad)
    }, class = "liss_source_nonresponse_blocked")
  }
})

test_that("partial applied markers never become accepted native or ordinary inputs", {
  result <- .nra_clean(.nra_saved()$orders[[1L]]$input)
  bare <- result$data
  class(bare) <- setdiff(class(bare), "liss_applied_income_frame")
  class_only <- bare
  attr(class_only, "liss_source_nonresponse_applied") <- NULL
  class(class_only) <- c("liss_applied_income_frame", class(class_only))
  line_only <- list(data = bare, applied_lineage = result$applied_lineage)
  no_receipt <- result
  no_receipt$applied_lineage <- NULL
  no_class <- result
  class(no_class) <- "list"
  marker_only <- data.frame(nomem_encr = 1)
  attr(marker_only, "liss_source_nonresponse_applied") <- list(schema_version = "1.0.0")
  forms <- list(result, result$data, bare, class_only, line_only,
    no_receipt, no_class, marker_only)
  for (input in forms) {
    if (!identical(input, result)) expect_error(lissr::liss_validate_applied_income(input))
    expect_error(lissr::merge_liss_panel(list(ci = input)),
      class = "liss_source_nonresponse_blocked")
    directory <- tempfile("applied_report_denied_")
    expect_error(lissr::liss_cleaning_report(input, directory),
      class = "liss_source_nonresponse_blocked")
    expect_false(dir.exists(directory))
  }
})

test_that("applied lineage blocks every reverse source binding before source I/O", {
  result <- .nra_clean(.nra_saved()$orders[[1L]]$input)
  recipes <- list(NULL, .nra_saved(2)$fields[["337"]]$recipe,
    .nra_saved(2)$fields[["338"]]$recipe)
  reached <- FALSE
  testthat::local_mocked_bindings(.liss_nonresponse_source_state = function(...) {
    reached <<- TRUE
    stop("reverse rebind reached source I/O")
  }, .package = "lissr")
  for (input in list(result, result$data, list(data = result$data),
                     result$data[1:3, , drop = FALSE])) for (specification in recipes) {
    expect_error(lissr::liss_classify_source_nonresponse(input,
      source = "missing_ci23p_EN_1.0p.sav", target = "s339",
      wave_col = "wave_id", recipe = specification),
      class = "liss_source_nonresponse_blocked")
    expect_false(reached)
  }
})

test_that("whole RDS preserves structural and replay validation in a fresh R process", {
  result <- .nra_clean(.nra_saved()$orders[[1L]]$complete_merge_result, "na_only")
  path <- withr::local_tempfile(pattern = "applied_whole_", fileext = ".rds")
  saveRDS(result, path, version = 2L)
  restored <- readRDS(path)
  expect_identical(restored, result)
  expect_no_error(lissr::liss_validate_applied_income(restored, replay = TRUE))
  package_root <- getNamespaceInfo(asNamespace("lissr"), "path")
  child <- withr::local_tempfile(pattern = "applied_child_", fileext = ".R")
  writeLines(c(
    "arguments <- commandArgs(trailingOnly = TRUE)",
    "root <- arguments[2L]",
    "if (file.exists(file.path(root, 'R', 'liss_clean_income.R'))) {",
    "  pkgload::load_all(root, quiet = TRUE)",
    "} else {",
    "  loadNamespace('lissr', lib.loc = dirname(root))",
    "}",
    "result <- readRDS(arguments[1L])",
    "recorded <- result$applied_lineage$engine",
    "current <- lissr:::.liss_applied_engine()",
    "if (!identical(recorded, current)) {",
    "  differences <- names(recorded$functions)[recorded$functions != current$functions[names(recorded$functions)]]",
    "  cat('different_engine_functions:', paste(differences, collapse = ', '), '\\n')",
    "  cat('function_order_equal:', identical(names(recorded$functions), names(current$functions)), '\\n')",
    "  cat('constants_identity_equal:', identical(recorded$constants, current$constants), '\\n')",
    "  cat('runtime_identity_equal:', identical(recorded$runtime, current$runtime), '\\n')",
    "}",
    "stopifnot(lissr::liss_validate_applied_income(result)$valid)",
    "stopifnot(lissr::liss_validate_applied_income(result, replay = TRUE)$replay_status == 'matched')",
    "cat('whole_applied_rds_validated\\n')"), child)
  output <- system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(child), shQuote(path), shQuote(package_root)),
    stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_true(any(grepl("whole_applied_rds_validated", output, fixed = TRUE)))
})

test_that("frozen native scalar collection and FLAG compatibility remains intact", {
  old <- .nra_saved(1L)
  for (name in names(old$inputs)) {
    input <- old$inputs[[name]]
    target <- if (name == "unguarded") "s339" else input$source_nonresponse$target
    for (mode in names(old$expected[[name]])) {
      result <- suppressWarnings(lissr::liss_clean_income(input, mode = mode,
        variables = list(target = target), verbose = FALSE))
      expect_identical(.nra_stable(result), old$expected[[name]][[mode]])
    }
  }
  for (suffix in c("337", "338")) {
    saved <- .nra_saved(2L)$fields[[suffix]]
    for (name in names(saved$inputs)) {
      input <- saved$inputs[[name]]
      expect_identical(lissr:::source_nonresponse_validate(input$data,
        input$source_nonresponse, input$source_nonresponse$target,
        input$source_nonresponse$wave_col), saved$validated[[name]])
      for (mode in c("correct", "flag", "na_only"))
        expect_error(lissr::liss_clean_income(input, mode = mode,
          variables = list(target = input$source_nonresponse$target), verbose = FALSE),
          class = "liss_source_nonresponse_blocked")
    }
  }
  for (version in c(3L, 4L)) for (captured in .nra_saved(version)$orders) {
    input <- captured$input
    expect_identical(lissr:::source_nonresponse_validate(input,
      input$source_nonresponse), captured$validated)
    if (version == 4L) {
      result <- .nra_clean(input, "flag")
      scalar <- .nra_clean(.nra_scalar(input), "flag")
      expect_identical(result$data$s339_proposed, scalar$data$s339_proposed)
      expect_identical(result$data$s339_observed, scalar$data$s339_observed)
      expect_identical(result$decisions, scalar$decisions)
      expect_identical(result$source_nonresponse, input$source_nonresponse)
      expect_false(inherits(result, "liss_clean_collection_applied_result"))
      expect_no_error(lissr:::source_nonresponse_validate(result,
        result$source_nonresponse))
    }
  }
})

test_that("a new applied grant does not admit schema-3 tax collections or FLAG as applied", {
  tax <- .nra_saved(3L)$orders[[1L]]$input
  for (mode in c("correct", "na_only")) {
    expect_error(.nra_clean(tax, mode), class = "liss_source_nonresponse_blocked")
    expect_error(.nra_clean(tax, mode, income_cap = 1,
      disable = c("P03", "F01")), class = "liss_source_nonresponse_blocked")
  }
  flag <- .nra_clean(.nra_saved()$orders[[1L]]$input, "flag")
  expect_error(lissr::liss_validate_applied_income(flag),
    class = "liss_source_nonresponse_blocked")
})

# malformed shape probes retain invented sources and recompute ordinary digests
.nra_expect_shape_rejected <- function(result, mutations, replay = FALSE) {
  for (name in names(mutations)) {
    expect_error({
      changed <- lissr:::.liss_applied_reseal(mutations[[name]](result))
      lissr::liss_validate_applied_income(changed, replay = replay)
    }, class = "liss_source_nonresponse_blocked", info = name)
  }
}

test_that("typed applied ledgers reject malformed empty and populated columns", {
  empty <- .nra_clean(.nra_saved()$orders[[1L]]$input)
  populated <- .nra_clean(.nra_fixture("household")$input)
  expect_equal(nrow(empty$decisions), 0L)
  expect_gt(nrow(populated$decisions), 0L)
  for (result in list(empty, populated)) {
    count <- nrow(result$decisions)
    mutations <- list(
      action_type = function(x) { x$decisions$action <- rep(42, count); x },
      applied_type = function(x) { x$decisions$applied <- rep("TRUE", count); x },
      row_type = function(x) { x$decisions$row <- as.double(x$decisions$row); x },
      corrected_type = function(x) { x$decisions$corrected <- rep(FALSE, count); x },
      evidence_list = function(x) { x$decisions$evidence <- rep(list("claimed"), count); x },
      column_metadata = function(x) { attr(x$decisions$action, "label") <- "claimed"; x },
      table_metadata = function(x) { attr(x$decisions, "unsupported") <- TRUE; x },
      table_class = function(x) { class(x$decisions) <- c("claimed", "data.frame"); x })
    .nra_expect_shape_rejected(result, mutations)
  }
})

test_that("applied summary counts require whole nonnegative numeric scalars", {
  result <- .nra_clean(.nra_saved()$orders[[1L]]$input)
  mutations <- list(
    fractional_rows = function(x) { x$summary$n_rows <- nrow(x$data) + 0.9; x },
    character_rows = function(x) { x$summary$n_rows <- as.character(nrow(x$data)); x },
    fractional_sources = function(x) { x$summary$n_source_nonresponse <- 1.9; x },
    fractional_raw = function(x) { x$summary$n_raw_finite_before <- 12.9; x },
    negative_decisions = function(x) { x$summary$n_decisions <- -1L; x },
    inconsistent_decisions = function(x) { x$summary$n_decisions <- 1L; x },
    missing_count = function(x) { x$summary$n_corrected <- NA_integer_; x },
    logical_count = function(x) { x$summary$n_corrected <- FALSE; x },
    vector_count = function(x) { x$summary$n_corrected <- c(0L, 0L); x },
    labelled_count = function(x) { attr(x$summary$n_corrected, "label") <- "claimed"; x },
    nonfinite_count = function(x) { x$summary$remaining_over_cap <- Inf; x })
  .nra_expect_shape_rejected(result, mutations)
  numeric_counts <- result
  for (key in grep("^n_|^remaining_", names(numeric_counts$summary), value = TRUE))
    numeric_counts$summary[[key]] <- as.double(numeric_counts$summary[[key]])
  numeric_counts <- lissr:::.liss_applied_reseal(numeric_counts)
  expect_no_error(lissr::liss_validate_applied_income(numeric_counts))
})

test_that("applied summaries retain closed plain distribution and aggregation shapes", {
  result <- .nra_clean(.nra_saved()$orders[[1L]]$input)
  mutations <- list(
    missing_slot = function(x) { x$summary$n_corrected <- NULL; x },
    extra_slot = function(x) { x$summary$unsupported <- TRUE; x },
    list_class = function(x) { class(x$summary) <- "claimed"; x },
    list_metadata = function(x) { attr(x$summary, "unsupported") <- TRUE; x },
    string_distribution = function(x) { x$summary$distribution_after <- "claimed"; x },
    distribution_slot = function(x) { x$summary$distribution_before$unsupported <- TRUE; x },
    distribution_type = function(x) { x$summary$distribution_before$mean <- "17000"; x },
    distribution_count = function(x) { x$summary$distribution_before$n <- 11.5; x },
    distribution_vector = function(x) { x$summary$distribution_after$sd <- c(0, 0); x },
    aggregation_type = function(x) { x$summary$by_rule <- "claimed"; x },
    aggregation_column = function(x) { x$summary$by_rule$n <- logical(0); x },
    aggregation_metadata = function(x) { attr(x$summary$by_rule, "unsupported") <- TRUE; x },
    notes_type = function(x) { x$summary$notes <- list("claimed"); x },
    overrides_type = function(x) { x$summary$overrides <- TRUE; x })
  .nra_expect_shape_rejected(result, mutations)
  populated <- .nra_clean(.nra_fixture("household")$input)
  .nra_expect_shape_rejected(populated, list(
    aggregation_fraction = function(x) { x$summary$by_rule$n[1L] <- 1.5; x },
    aggregation_negative = function(x) { x$summary$by_rule$n[1L] <- -1L; x },
    aggregation_action = function(x) { x$summary$by_rule$action <- TRUE; x }))
})

test_that("applied audit entries require closed scalar field shapes", {
  result <- .nra_clean(.nra_saved()$orders[[1L]]$input)
  mutations <- list(
    empty_entry = function(x) { x$log <- list(list()); x },
    list_metadata = function(x) { attr(x$log, "unsupported") <- TRUE; x },
    entry_metadata = function(x) { attr(x$log[[1L]], "unsupported") <- TRUE; x },
    missing_key = function(x) { x$log[[1L]]$rows_affected <- NULL; x },
    extra_key = function(x) { x$log[[1L]]$unsupported <- TRUE; x },
    string_count = function(x) { x$log[[1L]]$rows_affected <- "twelve"; x },
    fractional_count = function(x) { x$log[[1L]]$rows_affected <- 12.5; x },
    negative_count = function(x) { x$log[[1L]]$rows_affected <- -1L; x },
    numeric_action = function(x) { x$log[[1L]]$action <- 42; x },
    multiple_action = function(x) { x$log[[1L]]$action <- c("start:correct", "claimed"); x },
    invalid_na_sentinel = function(x) { x$log[[1L]]$distinct_before <- "NA"; x })
  .nra_expect_shape_rejected(result, mutations)
})

test_that("ignored applied timing values retain their typed shapes before replay", {
  result <- .nra_clean(.nra_saved()$orders[[1L]]$input)
  mutations <- list(
    duration_string = function(x) { x$summary$duration_s <- "impossible"; x },
    duration_nonfinite = function(x) { x$summary$duration_s <- Inf; x },
    duration_missing = function(x) { x$summary$duration_s <- NULL; x },
    timestamp_list = function(x) { x$log[[1L]]$timestamp <- list(impossible = TRUE); x },
    timestamp_missing = function(x) { x$log[[1L]]$timestamp <- NULL; x },
    timestamp_vector = function(x) { x$log[[1L]]$timestamp <- c("one", "two"); x },
    timestamp_metadata = function(x) { attr(x$log[[1L]]$timestamp, "label") <- "claimed"; x })
  .nra_expect_shape_rejected(result, mutations)
  .nra_expect_shape_rejected(result, mutations, replay = TRUE)
  changed <- result
  changed$summary$duration_s <- -0.5
  for (index in seq_along(changed$log))
    changed$log[[index]]$timestamp <- "2000-01-01T00:00:00.000"
  expect_no_error(lissr::liss_validate_applied_income(changed, replay = TRUE))
})

test_that("optional applied dataset annotations remain plain character vectors", {
  result <- .nra_clean(.nra_saved()$orders[[1L]]$input)
  expect_true("s339_dataset_flag" %in% names(result$data))
  mutations <- list(
    numeric_flag = function(x) { x$data$s339_dataset_flag <- rep(42, nrow(x$data)); x },
    logical_flag = function(x) { x$data$s339_dataset_flag <- rep(NA, nrow(x$data)); x },
    list_flag = function(x) { x$data$s339_dataset_flag <- rep(list("iqr"), nrow(x$data)); x },
    labelled_flag = function(x) { attr(x$data$s339_dataset_flag, "label") <- "claimed"; x },
    factor_flag = function(x) { x$data$s339_dataset_flag <- factor(x$data$s339_dataset_flag); x })
  .nra_expect_shape_rejected(result, mutations)
  no_flag <- .nra_clean(.nra_saved()$orders[[1L]]$input, disable = "D11")
  expect_false("s339_dataset_flag" %in% names(no_flag$data))
  expect_no_error(lissr::liss_validate_applied_income(no_flag, replay = TRUE))
})

test_that("typed applied shape validation retains empty one-value and keyed domains", {
  saved <- .nra_saved()
  one <- saved$orders[[1L]]$subsets$ordinary$input
  original <- one$data
  one$data <- original[1L, , drop = FALSE]
  for (column in names(original)) attributes(one$data[[column]]) <- attributes(original[[column]])
  locked <- saved$orders[[1L]]$input
  original_locked <- locked$data
  locked$data <- original_locked[1L, , drop = FALSE]
  for (column in names(original_locked))
    attributes(locked$data[[column]]) <- attributes(original_locked[[column]])
  inputs <- list(empty = saved$orders[[1L]]$subsets$empty$input,
    one = one, all_missing = locked, reversed = saved$orders[[1L]]$subsets$reverse$input,
    no_scoped_source = saved$mixed_wave$subsets$no_ci23p$input)
  for (name in names(inputs)) for (mode in c("correct", "na_only")) {
    result <- .nra_clean(inputs[[name]], mode)
    expect_no_error(lissr::liss_validate_applied_income(result, replay = TRUE))
    if (name %in% c("empty", "all_missing")) {
      expect_null(result$summary$distribution_before)
      expect_null(result$summary$distribution_after)
    }
    if (name == "one") {
      expect_equal(result$summary$distribution_after$n, 1L)
      expect_identical(result$summary$distribution_after$sd, NA_real_)
    }
  }
})

test_that("typed applied audit validation retains configured scalar rule identifiers", {
  input <- .nra_saved()$orders[[1L]]$input
  identifiers <- list(7L, 7.5, TRUE, NA_integer_, NA,
    c(named = "P02_custom"), c(named = 7L), Inf, NaN, NA_character_, FALSE,
    list("custom"), list(named = "custom"))
  for (identifier in identifiers) {
    ruleset <- lissr::liss_cleaning_ruleset()
    position <- which(vapply(ruleset$preparation_rules,
      function(rule) identical(rule$action, "resolve_target_variable"), logical(1)))
    ruleset$preparation_rules[[position]]$rule_id <- identifier
    result <- .nra_clean(input, ruleset = ruleset)
    expect_true(any(vapply(result$log,
      function(entry) identical(entry$rule_id, identifier), logical(1))))
    expect_no_error(lissr::liss_validate_applied_income(result, replay = TRUE))
  }
})
