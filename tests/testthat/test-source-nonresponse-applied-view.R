# every parent, row and source receipt below is invented
.nrv_saved <- function(version = 4L) {
  readRDS(testthat::test_path("fixtures",
    paste0("source_nonresponse_v", version, "_synthetic.rds")))
}

.nrv_clean <- function(input, mode = "correct", ...) {
  suppressWarnings(suppressMessages(lissr::liss_clean_income(input, mode = mode,
    variables = list(target = "s339"), verbose = FALSE, ...)))
}

.nrv_populated <- function(mode = "correct", .local_envir = parent.frame()) {
  saved <- .nrv_saved()
  raw <- saved$raw
  raw$ci23p339[2:4] <- c(-25000, 40000, 0)
  directory <- withr::local_tempdir("view_invented_", .local_envir = .local_envir)
  file.create(file.path(directory, saved$mocked_receipt$filename))
  testthat::local_mocked_bindings(
    .liss_nonresponse_source_state = function(path) saved$mocked_receipt,
    .liss_nonresponse_read_source = function(path) raw,
    read_wave_file = function(path, ...) raw,
    .package = "lissr", .env = .local_envir)
  input <- suppressWarnings(suppressMessages(lissr::merge_liss_module(
    saved$orders[[1L]]$recipe, directory, output_dir = NULL,
    source_nonresponse = TRUE)))
  rules <- lissr::liss_cleaning_ruleset()
  for (index in seq_along(rules$preparation_rules)) {
    if (identical(rules$preparation_rules[[index]]$action, "rectify_sign"))
      rules$preparation_rules[[index]]$rule_id <- "PREP_SIGN_CUSTOM"
  }
  for (index in seq_along(rules$finalization_rules)) {
    rules$finalization_rules[[index]]$rule_id <- "CAP_CUSTOM"
    rules$finalization_rules[[index]]$disposition <- "winsorise"
  }
  disabled <- vapply(rules$detection_rules, `[[`, character(1), "rule_id")
  .nrv_clean(input, mode, ruleset = rules, income_cap = 20000, disable = disabled)
}

.nrv_hash <- function(value) {
  .nr_identity_hash(value)
}

.nrv_unmark <- function(frame) {
  attr(frame, "liss_source_nonresponse_applied") <- NULL
  frame
}

.nrv_reseal <- function(view) {
  # refresh ordinary hashes without repairing projection shape or values
  updates <- list(parent_digest = .nrv_hash(view$parent_result),
    selection_digest = .nrv_hash(view$selection),
    output_digest = .nrv_hash(.nrv_unmark(view$data)),
    decisions_digest = .nrv_hash(view$decisions), summary_digest = .nrv_hash(view$summary))
  for (name in names(updates)) view$applied_lineage[[name]] <- updates[[name]]
  record <- view$applied_lineage
  record$digest <- NULL
  view$applied_lineage$digest <- .nrv_hash(record)
  marker <- attr(view$data, "liss_source_nonresponse_applied", exact = TRUE)
  for (name in c("parent_digest", "selection_digest", "output_digest"))
    marker[[name]] <- updates[[name]]
  attr(view$data, "liss_source_nonresponse_applied") <- marker
  view
}

.nrv_column <- function(column, rows) {
  value <- if (is.matrix(column)) column[rows, , drop = FALSE] else column[rows]
  metadata <- attributes(column)
  scoped <- intersect(c("names", "dim", "dimnames"), names(metadata))
  metadata[scoped] <- NULL
  selected <- attributes(value)
  metadata <- c(metadata, selected[intersect(scoped, names(selected))])
  attributes(value) <- if (length(metadata)) metadata else NULL
  value
}

.nrv_scope <- function(parent, rows) {
  source <- parent$source_input
  frame <- if (is.data.frame(source)) source else source$data
  state <- attr(frame, "liss_source_nonresponse", exact = TRUE)
  subset <- frame[rows, , drop = FALSE]
  metadata <- attributes(subset)
  columns <- unclass(subset)
  for (name in names(columns)) columns[[name]] <- .nrv_column(frame[[name]], rows)
  attributes(columns) <- metadata
  lissr:::.liss_nonresponse_validate_mixed(columns, state)
}

.nrv_expect_projection <- function(view, parent, rows) {
  expect_identical(view$parent_result, parent)
  expect_identical(view$selection,
    list(row_keys = parent$applied_lineage$row_keys[rows], root_rows = as.integer(rows)))
  expect_identical(names(view$data), names(parent$data))
  expect_identical(row.names(view$data), row.names(parent$data[rows, , drop = FALSE]))
  for (name in names(parent$data))
    expect_identical(view$data[[name]], .nrv_column(parent$data[[name]], rows), info = name)
  ordinals <- which(parent$decisions$row %in% rows)
  local_rows <- match(parent$decisions$row[ordinals], rows)
  order <- order(local_rows, ordinals, method = "radix")
  expected <- parent$decisions[ordinals[order], , drop = FALSE]
  expected$root_decision_row <- as.integer(ordinals[order])
  expected$view_row <- as.integer(local_rows[order])
  expect_identical(view$decisions, expected)
  scoped <- .nrv_scope(parent, rows)
  expect_identical(view$summary, list(n_rows = as.integer(length(rows)),
    n_decisions = as.integer(nrow(expected)),
    n_finite = as.integer(sum(is.finite(parent$data$s339[rows]))),
    n_source_locked = as.integer(sum(scoped$fields$CI23P339_FIELD$lock)),
    view_status = if (length(rows)) "projected" else "not_evaluated",
    source_readiness = scoped$readiness,
    parent_source_readiness = parent$applied_lineage$source_readiness,
    parent_analysis_execution_status = parent$applied_lineage$analysis_execution_status))
}

test_that("production views retain exact complete parents and ordered projections in both modes", {
  for (mode in c("correct", "na_only")) {
    parent <- .nrv_clean(.nrv_saved()$mixed_wave$input, mode)
    before <- serialize(parent, NULL, version = 2L)
    keys <- parent$applied_lineage$row_keys
    selections <- list(full = seq_along(keys), reverse = rev(seq_along(keys)),
      partial = c(3L, 14L, 1L, 24L), other_wave = which(grepl(":ci22o$", keys)),
      mixed = c(1L, 13L, 14L, 2L), empty = integer())
    for (name in names(selections)) {
      rows <- selections[[name]]
      view <- lissr::liss_create_applied_income_view(parent, keys[rows])
      expect_identical(class(view), "liss_clean_collection_applied_result")
      expect_false(inherits(view, "liss_clean_result"))
      expect_identical(names(view),
        c("parent_result", "selection", "data", "decisions", "summary", "applied_lineage"))
      expect_identical(class(view$data), class(parent$data))
      expect_identical(view$applied_lineage$schema_version, "1.0.0")
      expect_identical(view$applied_lineage$kind, "applied_income_view_lineage")
      expect_identical(view$applied_lineage$parent_digest, .nrv_hash(parent))
      marker <- attr(view$data, "liss_source_nonresponse_applied", exact = TRUE)
      expect_identical(marker$kind, "applied_income_view_frame")
      expect_identical(marker$schema_version, "1.0.0")
      expect_identical(marker$parent_digest, .nrv_hash(parent))
      .nrv_expect_projection(view, parent, rows)
      validation <- lissr::liss_validate_applied_income_view(view)
      expect_true(validation$valid)
      expect_identical(validation$validation, "structural")
      expect_identical(validation$replay_status, "not_requested")
      expect_error(lissr::liss_validate_applied_income(view),
        class = "liss_source_nonresponse_blocked")
      if (name %in% c("other_wave", "empty")) {
        expect_identical(view$summary$source_readiness$status, "not_evaluated")
        expect_equal(view$summary$source_readiness$evaluated_cells, 0)
        expect_identical(view$summary$parent_analysis_execution_status, "completed")
      }
    }
    expect_identical(serialize(parent, NULL, version = 2L), before)
  }
})

test_that("production views project populated and empty ledgers without reinterpreting IDs", {
  for (mode in c("correct", "na_only")) {
    parent <- .nrv_populated(mode)
    expect_gte(nrow(parent$decisions), 4L)
    expect_gt(anyDuplicated(parent$decisions$row), 0L)
    expect_true(all(c("PREP_SIGN_CUSTOM", "CAP_CUSTOM") %in% parent$decisions$rule_id))
    for (rows in list(c(4L, 2L, 3L, 1L), 2L, 1L, integer())) {
      view <- lissr::liss_create_applied_income_view(parent, parent$applied_lineage$row_keys[rows])
      .nrv_expect_projection(view, parent, rows)
      expect_no_error(lissr::liss_validate_applied_income_view(view, replay = TRUE))
      if (identical(rows, 2L)) expect_gt(view$summary$n_decisions, view$summary$n_rows)
      if (!length(rows) || identical(rows, 1L)) {
        expect_equal(nrow(view$decisions), 0L)
        expect_identical(view$decisions$root_decision_row, integer())
        expect_identical(view$decisions$view_row, integer())
      }
    }
    opaque <- parent
    opaque$decisions$decision_id <- rep(7L, nrow(opaque$decisions))
    opaque <- lissr:::.liss_applied_reseal(opaque)
    expect_no_error(lissr::liss_validate_applied_income(opaque))
    view <- lissr::liss_create_applied_income_view(opaque, opaque$applied_lineage$row_keys[2L])
    expect_true(all(view$decisions$decision_id == 7L))
    expect_false(anyDuplicated(view$decisions$root_decision_row) > 0L)
    expect_gt(nrow(view$decisions), 1L)
    expect_no_error(lissr::liss_validate_applied_income_view(view))
    expect_error(lissr::liss_validate_applied_income_view(view, replay = TRUE),
      class = "liss_source_nonresponse_blocked")
  }
})

test_that("view row keys and replay require explicit plain bounded arguments", {
  parent <- .nrv_clean(.nrv_saved()$orders[[1L]]$input)
  key <- parent$applied_lineage$row_keys[1L]
  expect_error(lissr::liss_create_applied_income_view(parent),
    class = "liss_applied_income_view_blocked")
  for (keys in list(NULL, NA_character_, "", rep(key, 2L), "unknown:ci23p", 1,
    1L, TRUE, list(key), factor(key), c(named = key), structure(key, label = "claim")))
    expect_error(lissr::liss_create_applied_income_view(parent, keys),
      class = "liss_applied_income_view_blocked")
  view <- lissr::liss_create_applied_income_view(parent, key)
  expect_error(lissr::liss_create_applied_income_view(view, key),
    class = "liss_source_nonresponse_blocked")
  for (replay in list(NULL, NA, 1L, c(TRUE, FALSE), c(named = TRUE),
    structure(TRUE, label = "claim")))
    expect_error(lissr::liss_validate_applied_income_view(view, replay = replay),
      class = "liss_applied_income_view_blocked")
  for (input in list(parent$data, parent$source_input,
    .nrv_clean(.nrv_saved()$orders[[1L]]$input, "flag")))
    expect_error(lissr::liss_create_applied_income_view(input, key),
      class = "liss_source_nonresponse_blocked")
})

test_that("view metadata scopes names lists matrices labels and factors naturally", {
  input <- .nrv_saved()$mixed_wave$input
  frame <- input$data
  count <- nrow(frame)
  metadata <- attributes(frame)
  columns <- unclass(frame)
  columns$context_names <- structure(seq_len(count),
    names = paste0("case_", seq_len(count)), label = "invented named context")
  columns$context_list <- structure(lapply(seq_len(count), function(index)
    list(index = index, text = paste0("value_", index))),
    names = paste0("list_", seq_len(count)), label = "invented list context")
  columns$context_matrix <- structure(matrix(seq_len(count * 2L), nrow = count,
    dimnames = list(paste0("matrix_", seq_len(count)), c("left", "right"))),
    label = "invented matrix context")
  columns$context_factor <- structure(factor(rep(c("first", "second"), length.out = count)),
    label = "invented factor context")
  columns$context_labelled <- haven::labelled_spss(as.double(seq_len(count)),
    labels = c(first = 1, second = 2), na_values = count, label = "invented labelled context")
  metadata$names <- names(columns)
  attributes(columns) <- metadata
  input$data <- columns
  for (mode in c("correct", "na_only")) {
    parent <- .nrv_clean(input, mode)
    for (rows in list(c(14L, 3L, 1L), rev(seq_len(count)), integer())) {
      view <- lissr::liss_create_applied_income_view(parent, parent$applied_lineage$row_keys[rows])
      .nrv_expect_projection(view, parent, rows)
      expect_identical(names(view$data$context_names), names(parent$data$context_names)[rows])
      expect_identical(names(view$data$context_list), names(parent$data$context_list)[rows])
      expect_identical(dim(view$data$context_matrix), c(length(rows), 2L))
      expect_identical(dimnames(view$data$context_matrix),
        dimnames(parent$data$context_matrix[rows, , drop = FALSE]))
      expect_identical(levels(view$data$context_factor), levels(parent$data$context_factor))
      expect_identical(attr(view$data$context_labelled, "labels"),
        attr(parent$data$context_labelled, "labels"))
      expect_no_error(lissr::liss_validate_applied_income_view(view, replay = TRUE))
    }
  }
})

test_that("source readiness uses selected original source and keeps native leaves inert", {
  for (mode in c("correct", "na_only")) {
    parent <- .nrv_populated(mode)
    for (rows in list(c(3L, 1L, 2L), 1L, integer())) {
      view <- lissr::liss_create_applied_income_view(parent, parent$applied_lineage$row_keys[rows])
      native <- .nrv_scope(parent, rows)
      expect_identical(view$summary$source_readiness, native$readiness)
      expect_identical(view$summary$n_source_locked,
        as.integer(sum(native$fields$CI23P339_FIELD$lock)))
      for (name in setdiff(names(parent$data), "s339"))
        expect_identical(view$data[[name]], .nrv_column(parent$data[[name]], rows), info = name)
      expect_true(all(is.na(view$data$s339[native$fields$CI23P339_FIELD$lock])))
      expect_identical(view$parent_result$source_input$source_nonresponse,
        parent$source_input$source_nonresponse)
      expect_null(attr(view$data, "liss_source_nonresponse", exact = TRUE))
      expect_error(lissr:::source_nonresponse_validate(view$data,
        parent$source_input$source_nonresponse), class = "liss_source_nonresponse_blocked")
    }
  }
})

test_that("the separate view policy requires exact package-owned authority bytes", {
  policy <- lissr:::.liss_view_policy()
  expect_identical(policy$policy_id, "CI23P339_V4_APPLIED_VIEW")
  expect_identical(policy$policy_version, "1.0.0")
  original <- lissr:::.liss_view_policy_path()
  expect_identical(basename(original), "source_nonresponse_collection_applied_views.yml")
  changed <- withr::local_tempfile(fileext = ".yml")
  writeLines(c(readLines(original, warn = FALSE), "unreviewed: true"), changed)
  testthat::local_mocked_bindings(.liss_view_policy_path = function() changed,
    .package = "lissr")
  expect_error(lissr:::.liss_view_policy(), class = "liss_applied_income_view_blocked")
})

test_that("view construction and structure never clean and replay executes the complete parent once", {
  calls <- new.env(parent = emptyenv())
  calls$rows <- integer()
  assign(".nrv_clean_trace", calls, envir = .GlobalEnv)
  suppressMessages(capture.output(trace("liss_clean_income", where = asNamespace("lissr"),
    print = FALSE, tracer = quote({ .GlobalEnv$.nrv_clean_trace$rows <-
      c(.GlobalEnv$.nrv_clean_trace$rows,
        if (is.data.frame(data)) nrow(data) else nrow(data$data)) }))))
  withr::defer({
    suppressMessages(capture.output(untrace("liss_clean_income", where = asNamespace("lissr"))))
    rm(".nrv_clean_trace", envir = .GlobalEnv)
  })
  parent <- .nrv_clean(.nrv_saved()$mixed_wave$input)
  calls$rows <- integer()
  view <- lissr::liss_create_applied_income_view(parent, parent$applied_lineage$row_keys[c(14L, 1L)])
  expect_no_error(lissr::liss_validate_applied_income_view(view))
  expect_identical(calls$rows, integer())
  validation <- lissr::liss_validate_applied_income_view(view, replay = TRUE)
  expect_identical(validation$replay_status, "matched")
  expect_identical(calls$rows, as.integer(nrow(parent$data)))
})

test_that("closed view shape and exact projections reject mutations after ordinary resealing", {
  parent <- .nrv_populated()
  view <- lissr::liss_create_applied_income_view(parent, parent$applied_lineage$row_keys[c(4L, 2L, 3L, 1L)])
  mutations <- list(
    extra_slot = function(x) { x$unsupported <- TRUE; x },
    missing_slot = function(x) { x$summary <- NULL; x },
    wrapper_class = function(x) { class(x) <- "claimed"; x },
    wrapper_metadata = function(x) { attr(x, "unsupported") <- TRUE; x },
    selection_extra = function(x) { x$selection$unsupported <- TRUE; x },
    selection_metadata = function(x) { attr(x$selection, "unsupported") <- TRUE; x },
    selected_rows = function(x) { x$selection$root_rows <- rev(x$selection$root_rows); x },
    selected_key = function(x) { x$selection$row_keys[1L] <- "unknown:ci23p"; x },
    selection_row_type = function(x) { x$selection$root_rows <- as.double(x$selection$root_rows); x },
    frame_metadata = function(x) { attr(x$data, "unsupported") <- TRUE; x },
    frame_class = function(x) { class(x$data) <- "data.frame"; x },
    marker_kind = function(x) { attr(x$data, "liss_source_nonresponse_applied")$kind <- "claimed"; x },
    marker_version = function(x) { attr(x$data, "liss_source_nonresponse_applied")$schema_version <- "0.1.0"; x },
    summary_metadata = function(x) { attr(x$summary, "unsupported") <- TRUE; x },
    summary_extra = function(x) { x$summary$unsupported <- TRUE; x },
    record_kind = function(x) { x$applied_lineage$kind <- "claimed"; x },
    record_extra = function(x) { x$applied_lineage$unsupported <- TRUE; x },
    record_metadata = function(x) { attr(x$applied_lineage, "unsupported") <- TRUE; x },
    tax_value = function(x) { x$data$s337[1L] <- 123; x },
    tax_metadata = function(x) { attr(x$data$s338, "label") <- "claimed"; x },
    observed = function(x) { x$data$s339_observed[1L] <- 123; x },
    status = function(x) { x$data$s339_clean_status[1L] <- "claimed"; x },
    target = function(x) { x$data$s339[1L] <- 123; x },
    row_names = function(x) { row.names(x$data) <- seq_len(nrow(x$data)); x },
    ledger_id = function(x) { x$decisions$decision_id[1L] <- 999L; x },
    ledger_row = function(x) { x$decisions$row[1L] <- 1L; x },
    ledger_ordinal = function(x) { x$decisions$root_decision_row[1L] <- 999L; x },
    ledger_local = function(x) { x$decisions$view_row[1L] <- 999L; x },
    ledger_type = function(x) { x$decisions$root_decision_row <- as.double(x$decisions$root_decision_row); x },
    summary_count = function(x) { x$summary$n_rows <- 1.5; x },
    summary_count_type = function(x) { x$summary$n_rows <- as.double(x$summary$n_rows); x },
    summary_state = function(x) { x$summary$view_status <- "refitted"; x },
    summary_parent = function(x) { x$summary$parent_analysis_execution_status <- "not_evaluated"; x },
    parent_tax = function(x) { x$parent_result$data$s337[1L] <- 123; x })
  for (name in names(mutations)) {
    changed <- mutations[[name]](view)
    blocked_class <- if (name == "parent_tax") "liss_source_nonresponse_blocked" else
      "liss_applied_income_view_blocked"
    expect_error(lissr::liss_validate_applied_income_view(changed),
      class = blocked_class, info = name)
    expect_error(lissr::liss_validate_applied_income_view(.nrv_reseal(changed)),
      class = blocked_class, info = paste(name, "resealed"))
  }
})

test_that("complete-parent hashes expose nonsemantic changes without authenticating their origin", {
  parent <- .nrv_populated()
  view <- lissr::liss_create_applied_income_view(parent, parent$applied_lineage$row_keys[c(2L, 3L)])
  timing <- view
  timing$parent_result$summary$duration_s <- -0.5
  expect_error(lissr::liss_validate_applied_income_view(timing),
    class = "liss_applied_income_view_blocked")
  timing <- .nrv_reseal(timing)
  expect_false(identical(timing$applied_lineage$parent_digest, view$applied_lineage$parent_digest))
  expect_no_error(lissr::liss_validate_applied_income_view(timing))
  expect_no_error(lissr::liss_validate_applied_income_view(timing, replay = TRUE))
  forged <- parent
  row <- which(is.finite(forged$data$s339) &
    forged$data$s339_clean_status != "source_nonresponse:missing_amount")[1L]
  forged$data$s339[row] <- forged$data$s339[row] + 1
  forged <- lissr:::.liss_applied_reseal(forged)
  expect_no_error(lissr::liss_validate_applied_income(forged))
  forged_view <- lissr::liss_create_applied_income_view(forged,
    forged$applied_lineage$row_keys[c(2L, 3L)])
  expect_no_error(lissr::liss_validate_applied_income_view(forged_view))
  expect_error(lissr::liss_validate_applied_income_view(forged_view, replay = TRUE),
    class = "liss_source_nonresponse_blocked")
})

test_that("recognized whole frame partial and residual view claims deny downstream consumers", {
  parent <- .nrv_clean(.nrv_saved()$orders[[1L]]$input)
  view <- lissr::liss_create_applied_income_view(parent, parent$applied_lineage$row_keys[c(3L, 1L)])
  marker_only <- view$data
  class(marker_only) <- "data.frame"
  class_only <- view$data
  attr(class_only, "liss_source_nonresponse_applied") <- NULL
  no_record <- view
  no_record$applied_lineage <- NULL
  forms <- list(view, view$data, list(data = view$data), list(data_partial = view$data),
    marker_only, class_only, no_record,
    list(data = data.frame(nomem_encr = 1), applied_lineage = view$applied_lineage))
  calls <- character()
  forbidden <- function(name) function(...) {
    calls <<- c(calls, name)
    stop("downstream work unexpectedly reached")
  }
  testthat::local_mocked_bindings(.liss_nonresponse_source_state = forbidden("source_identity"),
    .liss_nonresponse_read_source = forbidden("source_read"),
    numeric_view = forbidden("numeric"), attach_background_frame = forbidden("background"),
    write_clean_jsonl = forbidden("jsonl"), build_cleaning_report_md = forbidden("report"),
    sanitize_spss_names = forbidden("sanitize"), .package = "lissr")
  recipes <- list(NULL, .nrv_saved(2L)$fields[["337"]]$recipe,
    .nrv_saved(2L)$fields[["338"]]$recipe)
  directory <- withr::local_tempdir("view_must_not_write_")
  output <- file.path(directory, "denied")
  for (input in forms) {
    before <- serialize(input, NULL, version = 2L)
    for (mode in c("correct", "flag", "na_only"))
      expect_error(lissr::liss_clean_income(input, mode = mode,
        variables = list(target = "s339"), ruleset = stop("ruleset promise was forced"),
        background = stop("background promise was forced"), output_dir = output,
        verbose = FALSE), class = "liss_source_nonresponse_blocked")
    expect_error(lissr::merge_liss_panel(list(ci = input),
      write_to = file.path(output, "panel.sav")), class = "liss_source_nonresponse_blocked")
    expect_error(lissr::liss_cleaning_report(input, output, verbose = FALSE),
      class = "liss_source_nonresponse_blocked")
    for (recipe in recipes)
      expect_error(lissr::liss_classify_source_nonresponse(input,
        source = "missing_ci23p_EN_1.0p.sav", target = "s339", wave_col = "wave_id",
        recipe = recipe), class = "liss_source_nonresponse_blocked")
    expect_identical(serialize(input, NULL, version = 2L), before)
    expect_false(file.exists(output))
    expect_identical(calls, character())
  }
})

test_that("complete deliberate claim stripping leaves unknown ordinary ownership", {
  parent <- .nrv_clean(.nrv_saved()$orders[[1L]]$input)
  view <- lissr::liss_create_applied_income_view(parent, parent$applied_lineage$row_keys[c(3L, 2L)])
  frame <- view$data
  attr(frame, "liss_source_nonresponse_applied") <- NULL
  class(frame) <- setdiff(class(frame), "liss_applied_income_frame")
  expect_false(lissr:::.liss_nonresponse_claims_applied(frame))
  repeated <- tryCatch(.nrv_clean(frame, "flag"), error = identity)
  expect_s3_class(repeated, "rlang_error")
  expect_false(inherits(repeated, "liss_source_nonresponse_blocked"))
  frame <- frame[setdiff(names(frame),
    c("s339_observed", "s339_clean_status", "s339_dataset_flag"))]
  result <- .nrv_clean(frame, "flag")
  expect_identical(result$source_nonresponse_readiness$status, "not_evaluated")
  expect_null(result[["source_nonresponse"]])
})

test_that("whole production view RDS validates and replays in a fresh process", {
  views <- list()
  for (mode in c("correct", "na_only")) {
    parent <- .nrv_clean(.nrv_saved()$mixed_wave$input, mode)
    for (rows in list(c(14L, 3L, 1L), integer()))
      views[[length(views) + 1L]] <- lissr::liss_create_applied_income_view(parent,
        parent$applied_lineage$row_keys[rows])
    populated <- .nrv_populated(mode)
    views[[length(views) + 1L]] <- lissr::liss_create_applied_income_view(populated,
      populated$applied_lineage$row_keys[c(4L, 2L, 3L, 1L)])
  }
  path <- withr::local_tempfile(pattern = "invented_production_views_", fileext = ".rds")
  saveRDS(views, path, version = 2L)
  expect_identical(readRDS(path), views)
  root <- getNamespaceInfo(asNamespace("lissr"), "path")
  child <- withr::local_tempfile(pattern = "production_view_child_", fileext = ".R")
  writeLines(c(
    "arguments <- commandArgs(trailingOnly = TRUE)",
    "root <- arguments[2L]",
    "if (file.exists(file.path(root, 'R', 'liss_clean_income.R'))) {",
    "  pkgload::load_all(root, quiet = TRUE)",
    "} else {",
    "  loadNamespace('lissr', lib.loc = dirname(root))",
    "}",
    "views <- readRDS(arguments[1L])",
    "for (view in views) {",
    "  stopifnot(lissr::liss_validate_applied_income_view(view)$valid)",
    "  stopifnot(lissr::liss_validate_applied_income_view(view, replay = TRUE)$replay_status == 'matched')",
    "}",
    "cat('whole_production_views_validated', length(views), '\\n')"), child)
  output <- system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(child), shQuote(path), shQuote(root)), stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_true(any(grepl("whole_production_views_validated 6", output, fixed = TRUE)))
})
