# all rows and frozen receipts in this file are invented synthetic data
.nrb_saved <- function(version = 3L) {
  readRDS(testthat::test_path("fixtures",
    paste0("source_nonresponse_v", version, "_synthetic.rds")))
}

.nrb_fixture <- function(.local_envir = parent.frame()) {
  saved <- .nrb_saved()
  raw <- saved$raw
  receipt <- saved$mocked_receipt
  directory <- withr::local_tempdir("joint_three_", .local_envir = .local_envir)
  source <- file.path(directory, receipt$filename)
  haven::write_sav(raw, source)
  testthat::local_mocked_bindings(
    .liss_nonresponse_source_state = function(path) receipt,
    .liss_nonresponse_read_source = function(path) raw,
    read_wave_file = function(path, ...) raw,
    .package = "lissr", .env = .local_envir)
  list(saved = saved, raw = raw, receipt = receipt, directory = directory,
    source = source)
}

.nrb_orders <- function() list(c("337", "338", "339"), c("337", "339", "338"),
  c("338", "337", "339"), c("338", "339", "337"), c("339", "337", "338"),
  c("339", "338", "337"))

.nrb_recipe <- function(order = c("337", "338", "339")) {
  recipe <- .nrb_saved(2)$fields[["337"]]$recipe
  registry <- lissr:::.liss_nonresponse_registry()
  recipe$source_nonresponse <- list(schema_version = "4.0.0",
    fields = lapply(order, function(suffix) {
      scalar <- if (suffix == "339") list(schema_version = "1.0.0",
        contracts = list(lissr:::.liss_nonresponse_rule())) else {
          index <- which(vapply(registry$fields, function(field)
            identical(field$field_id, paste0("CI23P", suffix, "_FIELD")), logical(1)))
          list(schema_version = "2.0.0", field = registry$fields[[index]])
        }
      list(field_id = paste0("CI23P", suffix, "_FIELD"), scalar = scalar)
    }))
  recipe
}

.nrb_merge <- function(fixture, recipe = .nrb_recipe(), ...) {
  suppressWarnings(suppressMessages(lissr::merge_liss_module(recipe,
    fixture$directory, output_dir = NULL, source_nonresponse = TRUE, ...)))
}

.nrb_validate <- function(result) {
  lissr:::source_nonresponse_validate(result$data, result$source_nonresponse)
}

.nrb_stable <- function(result) {
  selected <- result[c("data", "source_nonresponse", "source_nonresponse_readiness")]
  class(selected) <- class(result)
  selected
}

.nrb_deep_error <- function(error) {
  while (inherits(error$parent, "error")) error <- error$parent
  error
}

.nrb_reseal <- function(result, leaves = TRUE) {
  if (leaves) result$source_nonresponse$fields <- lapply(
    result$source_nonresponse$fields, lissr:::.liss_nonresponse_seal)
  result$source_nonresponse <- lissr:::.liss_nonresponse_seal(result$source_nonresponse)
  result$source_nonresponse_readiness <- result$source_nonresponse$readiness
  attr(result$data, "liss_source_nonresponse") <- result$source_nonresponse
  result
}

.nrb_rule <- function(leaf) {
  if (identical(leaf$schema_version, "1.0.0")) leaf$rule else leaf$field
}

.nrb_forms <- function(result) {
  clean <- result$data
  attr(clean, "liss_source_nonresponse") <- NULL
  class_only <- list(data = clean)
  class(class_only) <- class(result)
  class_frame <- clean
  class(class_frame) <- c("liss_source_nonresponse_collection_result", class(class_frame))
  forms <- list(intact = result, frame = result$data,
    carrier_only = list(data = clean, source_nonresponse = result$source_nonresponse),
    class_only = class_only, class_frame = class_frame,
    schema_only = list(data = clean, source_nonresponse = list(schema_version = "4.0.0")))
  schema_frame <- clean
  attr(schema_frame, "liss_source_nonresponse") <- list(schema_version = "4.0.0")
  forms$schema_frame <- schema_frame
  for (key in c("source_nonresponse_readiness", "liss_source_nonresponse_readiness")) {
    frame <- clean
    attr(frame, key) <- result$source_nonresponse_readiness
    forms[[paste0(key, "_frame")]] <- frame
  }
  forms$readiness_only <- list(data = clean,
    source_nonresponse_readiness = result$source_nonresponse_readiness)
  missing <- result
  missing$source_nonresponse <- NULL
  forms$missing <- missing
  contradictory <- result
  attr(contradictory$data, "liss_source_nonresponse") <-
    .nrb_saved()$orders[[1L]]$input$source_nonresponse
  forms$contradictory <- contradictory
  forms
}

test_that("complete frozen v1 v2 and v3 receipts cross-read without implicit migration", {
  fixture <- .nrb_fixture()
  expect_identical(fixture$saved$format, "invented_row_t2a_v3_fixture")
  expect_true(fixture$saved$provenance$synthetic_only)
  for (captured in fixture$saved$orders) {
    current <- .nrb_stable(.nrb_merge(fixture, captured$recipe))
    expect_identical(current, captured$input)
    expect_identical(.nrb_validate(current), captured$validated)
    for (subset in captured$subsets) {
      expect_identical(.nrb_validate(subset$input), subset$validated)
      expect_identical(.nrb_validate(subset$resealed_input), subset$resealed_validated)
    }
  }
  old <- .nrb_saved(1)
  for (name in names(old$validated)) {
    bound <- old$inputs[[name]]
    state <- bound$source_nonresponse
    expect_identical(lissr:::source_nonresponse_validate(bound$data, state,
      state$target, state$wave_col), old$validated[[name]], info = name)
  }
  old <- .nrb_saved(2)
  for (suffix in c("337", "338")) {
    field <- old$fields[[suffix]]
    for (name in names(field$inputs)) {
      bound <- field$inputs[[name]]
      state <- bound$source_nonresponse
      expect_identical(lissr:::source_nonresponse_validate(bound$data, state,
        state$target, state$wave_col), field$validated[[name]], info = paste(suffix, name))
    }
    for (subsets in field$subsets) for (subset in subsets) {
      state <- subset$input$source_nonresponse
      expect_identical(lissr:::source_nonresponse_validate(subset$input$data,
        state, state$target, state$wave_col), subset$validated)
    }
  }
})

test_that("all six explicit mixed selections preserve native leaves and mapping orders", {
  fixture <- .nrb_fixture()
  for (order in .nrb_orders()) {
    recipe <- .nrb_recipe(order)
    recipe$source_nonresponse <- recipe$source_nonresponse[c("fields", "schema_version")]
    recipe$source_nonresponse$fields <- lapply(recipe$source_nonresponse$fields,
      function(selector) {
        selector <- selector[c("scalar", "field_id")]
        selector$scalar <- selector$scalar[rev(seq_along(selector$scalar))]
        if (identical(selector$scalar$schema_version, "1.0.0"))
          selector$scalar$contracts[[1L]] <-
            selector$scalar$contracts[[1L]][rev(seq_along(selector$scalar$contracts[[1L]]))]
        else {
          field <- selector$scalar$field
          field <- field[rev(seq_along(field))]
          field$readiness <- field$readiness[rev(seq_along(field$readiness))]
          field$cases <- lapply(field$cases, function(case) case[rev(seq_along(case))])
          selector$scalar$field <- field
        }
        selector
      })
    expect_identical(lissr:::validate_source_nonresponse_contracts(recipe), character())
    result <- .nrb_merge(fixture, recipe)
    state <- result$source_nonresponse
    ids <- paste0("CI23P", order, "_FIELD")
    expect_s3_class(result, "liss_source_nonresponse_collection_result")
    expect_s3_class(result, "liss_source_nonresponse_result")
    expect_identical(names(state), c("schema_version", "kind", "source", "fields",
      "normalized", "readiness", "digest"))
    expect_identical(state$schema_version, "4.0.0")
    expect_identical(state$kind, "source_nonresponse_collection")
    expect_identical(names(state$fields), ids)
    expect_identical(names(state$readiness$fields), ids)
    expect_identical(state, attr(result$data, "liss_source_nonresponse", exact = TRUE))
    expect_identical(state$readiness, result$source_nonresponse_readiness)
    expect_identical(state$digest, lissr:::.liss_nonresponse_digest(state))
    expect_true(state$normalized)
    for (index in seq_along(ids)) {
      leaf <- state$fields[[index]]
      scalar <- recipe$source_nonresponse$fields[[index]]$scalar
      rule <- .nrb_rule(leaf)
      expect_identical(leaf$schema_version, scalar$schema_version)
      if (order[[index]] == "339") {
        expect_identical(leaf$rule, scalar$contracts[[1L]])
        expect_null(leaf$field)
        expect_null(leaf$cases)
        expect_identical(leaf$readiness$rule_id, "CI23P339_N01")
        expect_false("construct" %in% names(leaf$ledger))
      } else {
        expect_identical(leaf$field, scalar$field)
        expect_identical(leaf$readiness$field_id, ids[[index]])
        expect_identical(leaf$readiness$case_counts,
          stats::setNames(c(2L, 2L), paste0("CI23P", order[[index]], c("_N01", "_N02"))))
      }
      expect_identical(leaf$source, state$source)
      expect_identical(leaf$target, paste0("s", order[[index]]))
      expect_identical(leaf$wave_col, "wave_id")
      expect_identical(leaf$id_col, "nomem_encr")
      expect_true(leaf$normalized)
      expect_identical(leaf$digest, lissr:::.liss_nonresponse_digest(leaf))
      expect_identical(leaf$readiness, state$readiness$fields[[ids[[index]]]])
      expect_equal(nrow(leaf$coverage), nrow(fixture$raw))
      expect_equal(nrow(leaf$input_rows), nrow(fixture$raw))
      expect_identical(rule$raw_column, paste0("ci23p", order[[index]]))
    }
    expect_identical(.nrb_validate(result)$state, state)
    expect_identical(state$readiness$status, "passed")
    expect_identical(state$readiness$schema_version, "4.0.0")
    expect_equal(state$readiness$evaluated_cells, 36)
    expect_equal(state$readiness$classified_cells, 9)
    expect_equal(state$readiness$unresolved_cells, 0)
    expect_match(state$readiness$scope, "field")
  }
})

test_that("schema four requires complete exact selectors and scalar authorities", {
  fixture <- .nrb_fixture()
  recipe <- .nrb_recipe()
  changes <- list(
    function(x) { x$source_nonresponse$fields <- list(); x },
    function(x) { x$source_nonresponse$fields <- x$source_nonresponse$fields[1:2]; x },
    function(x) { x$source_nonresponse$fields[[4L]] <- x$source_nonresponse$fields[[1L]]; x },
    function(x) { x$source_nonresponse$fields[[3L]] <- x$source_nonresponse$fields[[2L]]; x },
    function(x) { names(x$source_nonresponse$fields) <- c("first", "second", "third"); x },
    function(x) { x$source_nonresponse$extra <- TRUE; x },
    function(x) { x$source_nonresponse$schema_version <- 4; x },
    function(x) { x$source_nonresponse$fields[[1L]] <- x$source_nonresponse$fields[[1L]]$scalar$field; x },
    function(x) { x$source_nonresponse$fields[[3L]] <- x$source_nonresponse$fields[[3L]]$scalar$contracts[[1L]]; x },
    function(x) { x$source_nonresponse$fields[[3L]]$scalar <- list(schema_version = "1.0.0"); x },
    function(x) { x$source_nonresponse$fields[[3L]]$field_id <- "CI23P339_N01"; x },
    function(x) { x$source_nonresponse$fields[[1L]]$field_id <- "CI23P339_FIELD"; x },
    function(x) { x$source_nonresponse$fields[[1L]]$extra <- TRUE; x },
    function(x) { x$source_nonresponse$fields[[1L]]$scalar$extra <- TRUE; x },
    function(x) { x$source_nonresponse$fields[[1L]]$scalar$schema_version <- "1.0.0"; x },
    function(x) { x$source_nonresponse$fields[[3L]]$scalar$schema_version <- "2.0.0"; x },
    function(x) { x$source_nonresponse$fields[[1L]]$scalar$field$field_id <- "CI23P338_FIELD"; x },
    function(x) { x$source_nonresponse$fields[[1L]]$scalar$field$raw_column <- "ci23p338"; x },
    function(x) { x$source_nonresponse$fields[[1L]]$scalar$field$cases <- rev(x$source_nonresponse$fields[[1L]]$scalar$field$cases); x },
    function(x) { x$source_nonresponse$fields[[2L]]$scalar$field$cases[[2L]]$code <- -9999999999; x },
    function(x) { x$source_nonresponse$fields[[2L]]$scalar$field$cases[[1L]]$code <- "-9999999999"; x },
    function(x) { x$source_nonresponse$fields[[3L]]$scalar$contracts[[2L]] <- x$source_nonresponse$fields[[3L]]$scalar$contracts[[1L]]; x },
    function(x) { x$source_nonresponse$fields[[3L]]$scalar$contracts[[1L]]$code <- -9999999998; x },
    function(x) { x$source_nonresponse$fields[[3L]]$scalar$contracts[[1L]]$source_release <- "1.1p"; x },
    function(x) { x$source_nonresponse$fields[[3L]]$scalar$contracts[[1L]]$language <- "NL"; x },
    function(x) { x$source_nonresponse$fields[[3L]]$scalar$contracts[[1L]]$target_aliases <- list("nethh"); x },
    function(x) { x$source_nonresponse$fields[[3L]]$scalar$contracts[[1L]]$rule_version <- "2.0.0"; x },
    function(x) { x$source_nonresponse$fields[[3L]]$scalar$contracts[[1L]]$source_sha256 <- strrep("0", 64); x },
    function(x) { x$source_nonresponse$fields[[2L]]$scalar$field$consumers <- list(list(name = "liss_clean_income")); x })
  for (index in seq_along(changes)) {
    changed <- changes[[index]](recipe)
    expect_true(length(lissr:::validate_source_nonresponse_contracts(changed)) > 0,
      info = paste("recipe", index))
    expect_error(.nrb_merge(fixture, changed), info = paste("recipe", index))
  }
  pair <- fixture$saved$orders[[1L]]$recipe
  pair$source_nonresponse$fields[[3L]] <- recipe$source_nonresponse$fields[[3L]]
  expect_true(length(lissr:::validate_source_nonresponse_contracts(pair)) > 0)
  reads <- FALSE
  testthat::local_mocked_bindings(.liss_nonresponse_read_source = function(path) {
    reads <<- TRUE
    stop("scalar rejection must precede source reads")
  }, .package = "lissr")
  for (selected in list(recipe, fixture$saved$orders[[1L]]$recipe))
    expect_error(lissr::liss_classify_source_nonresponse(fixture$raw, fixture$source,
      "ci23p339", recipe = selected), class = "liss_source_nonresponse_blocked")
  expect_false(reads)
})

test_that("the pinned collection adapter is closed and loaded only for schema four", {
  fixture <- .nrb_fixture()
  adapter <- lissr:::.liss_nonresponse_collection_adapter()
  registry <- lissr:::.liss_nonresponse_registry()
  expect_identical(adapter$adapter_id, "CI23P_337_338_339_COLLECTION")
  expect_identical(adapter$carrier_schema_version, "4.0.0")
  expect_identical(adapter$consumers, list())
  expect_identical(vapply(adapter$members, `[[`, character(1), "scalar_schema_version"),
    c("2.0.0", "2.0.0", "1.0.0"))
  expect_identical(adapter$members[[3L]]$authority_id, "CI23P339_N01")
  expect_identical(adapter$authority_registry$sha256,
    "a2e6694acc5855d2df389df46d66e0c94b705637626309cc309ceee8d840ddc3")
  path <- lissr:::.liss_nonresponse_collection_adapter_path()
  missing <- file.path(fixture$directory, "missing_adapter.yml")
  corrupt <- file.path(fixture$directory, "corrupt_adapter.yml")
  writeLines(c(readLines(path, warn = FALSE), "# changed bytes"), corrupt)
  legacy_baseline <- lissr::liss_classify_source_nonresponse(fixture$raw,
    fixture$source, "ci23p339")
  for (selected in c(missing, corrupt)) local({
    selected_path <- selected
    testthat::local_mocked_bindings(.liss_nonresponse_collection_adapter_path =
      function() selected_path, .package = "lissr")
    expect_error(.nrb_merge(fixture), class = "liss_source_nonresponse_blocked")
    expect_identical(.nrb_stable(.nrb_merge(fixture,
      fixture$saved$orders[[1L]]$recipe)), fixture$saved$orders[[1L]]$input)
    for (suffix in c("337", "338")) {
      saved <- .nrb_saved(2)$fields[[suffix]]
      expect_identical(.nrb_stable(.nrb_merge(fixture, saved$recipe)), saved$inputs$normalized)
    }
    expect_identical(lissr::liss_classify_source_nonresponse(fixture$raw,
      fixture$source, "ci23p339"), legacy_baseline)
  })
  bad_adapters <- list(
    function(x) { x$extra <- TRUE; x },
    function(x) { x$schema_version <- "2.0.0"; x },
    function(x) { x$carrier_schema_version <- "3.0.0"; x },
    function(x) { x$authority_registry$sha256 <- strrep("0", 64); x },
    function(x) { x$authority_registry$registry_version <- "3.0.0"; x },
    function(x) { x$members <- x$members[1:2]; x },
    function(x) { x$members <- rev(x$members); x },
    function(x) { names(x$members) <- c("first", "second", "third"); x },
    function(x) { x$members[[3L]]$scalar_schema_version <- "2.0.0"; x },
    function(x) { x$members[[3L]]$authority_kind <- "field"; x },
    function(x) { x$members[[3L]]$authority_id <- "CI23P339_FIELD"; x },
    function(x) { x$members[[1L]]$authority_version <- "9.0.0"; x },
    function(x) { x$members[[1L]]$extra <- TRUE; x },
    function(x) { x$consumers <- list("liss_clean_income"); x })
  for (index in seq_along(bad_adapters))
    expect_error(lissr:::.liss_nonresponse_collection_adapter_validate(
      bad_adapters[[index]](adapter), registry), class = "liss_source_nonresponse_blocked",
      info = paste("adapter", index))
  registry_path <- lissr:::.liss_nonresponse_registry_path()
  corrupt_registry <- file.path(fixture$directory, "corrupt_registry.yml")
  writeLines(c(readLines(registry_path, warn = FALSE), "# changed bytes"), corrupt_registry)
  for (selected in c(file.path(fixture$directory, "missing_registry.yml"), corrupt_registry)) local({
    selected_path <- selected
    testthat::local_mocked_bindings(.liss_nonresponse_registry_path = function() selected_path,
      .package = "lissr")
    expect_error(.nrb_merge(fixture))
  })
})

test_that("three-field normalization retains native routing and changes only approved codes", {
  fixture <- .nrb_fixture()
  raw <- fixture$raw
  raw$ci23p339[2] <- -9999999999
  raw$ci23p339[3] <- -9999999998
  raw$ci23p229[2] <- NA_real_
  testthat::local_mocked_bindings(.liss_nonresponse_read_source = function(path) raw,
    read_wave_file = function(path, ...) raw, .package = "lissr")
  recipe <- .nrb_recipe()
  recipe$harmonization_rules <- list(list(rule_id = "THREE_ORDERING_PROBE",
    action = "value_recode", description = "synthetic ordering probe",
    suffixes = list("337", "338"),
    mapping = list(`-9999999999` = 111, `-9999999998` = 222)),
    list(rule_id = "HOUSEHOLD_ORDERING_PROBE", action = "value_recode",
      description = "synthetic household ordering probe", suffixes = list("339"),
      mapping = list(`-9999999999` = 111)))
  result <- .nrb_merge(fixture, recipe)
  baseline <- suppressWarnings(suppressMessages(lissr::merge_liss_module(recipe,
    fixture$directory, output_dir = NULL, source_nonresponse = FALSE)))
  expect_null(baseline[["source_nonresponse"]])
  for (suffix in c("337", "338", "339")) {
    leaf <- result$source_nonresponse$fields[[paste0("CI23P", suffix, "_FIELD")]]
    values <- as.numeric(raw[[paste0("ci23p", suffix)]])
    approved <- if (suffix == "339") -9999999999 else c(-9999999999, -9999999998)
    matched <- is.finite(values) & values %in% approved
    expect_true(all(is.na(as.numeric(result$data[[paste0("s", suffix)]])[matched])))
    unchanged <- !matched & !(is.finite(values) & values %in% c(-9999999999, -9999999998))
    expect_identical(as.numeric(result$data[[paste0("s", suffix)]])[unchanged], values[unchanged])
    expect_equal(nrow(leaf$ledger), sum(matched))
    expect_identical(leaf$ledger$original_value, values[matched])
    expect_identical(leaf$ledger$source_key, paste(fixture$receipt$sha256,
      which(matched), paste0("ci23p", suffix), sep = ":"))
    expect_true(all(leaf$ledger$no_numeric_proposal))
    expect_true(all(is.na(leaf$ledger$analysis_value_after)))
    expect_true(all(leaf$ledger$answer_status == "nonresponse_unknown"))
  }
  household <- result$source_nonresponse$fields$CI23P339_FIELD
  expect_identical(as.numeric(result$data$s339)[3], -9999999998)
  expect_identical(household$ledger$rule_id, rep("CI23P339_N01", 2L))
  expect_identical(household$ledger$candidate_answer_status,
    c("dont_know", NA_character_))
  expect_identical(household$ledger$subtype_evidence, c("routing_supported", "unresolved"))
  expect_identical(household$readiness$rule_id, "CI23P339_N01")
  expect_equal(household$readiness$classified_cells, 2)
  for (column in c("s226", "s229"))
    expect_identical(result$data[[column]], baseline$data[[column]])
  expect_identical(.nrb_validate(result)$state, result$source_nonresponse)
  logs <- Filter(function(entry) identical(entry$action, "source_nonresponse:missing_amount"),
    result$log)
  expect_identical(vapply(logs, `[[`, character(1), "rule_id"),
    c("CI23P337_N01", "CI23P337_N02", "CI23P338_N01", "CI23P338_N02", "CI23P339_N01"))
  expect_equal(vapply(logs, `[[`, numeric(1), "rows_affected"), rep(2, 5))
  expect_length(list.files(fixture$directory), 1)
})

test_that("every preflight observes unchanged raw fields before second or third failure", {
  fixture <- .nrb_fixture()
  field_classifier <- lissr:::.liss_nonresponse_classify_field
  household_classifier <- lissr::liss_classify_source_nonresponse
  for (failure in c(2L, 3L)) local({
    calls <- character()
    observations <- list()
    observe <- function(data, id) {
      calls <<- c(calls, id)
      observations[[length(observations) + 1L]] <<- data
      if (length(calls) == failure)
        lissr:::.liss_nonresponse_abort("synthetic mixed preflight failure")
    }
    testthat::local_mocked_bindings(
      .liss_nonresponse_classify_field = function(data, source, target, wave_col, id_col, field) {
        observe(data, field$field_id)
        field_classifier(data, source, target, wave_col, id_col, field)
      },
      liss_classify_source_nonresponse = function(data, source, target,
        wave_col = NULL, id_col = "nomem_encr", recipe = NULL) {
        observe(data, "CI23P339_FIELD")
        household_classifier(data, source, target, wave_col, id_col, recipe)
      }, .package = "lissr")
    result <- tryCatch(.nrb_merge(fixture), error = identity)
    expect_s3_class(.nrb_deep_error(result), "liss_source_nonresponse_blocked")
    expect_identical(calls, paste0("CI23P", c("337", "338", "339")[seq_len(failure)], "_FIELD"))
    expect_length(observations, failure)
    for (data in observations) {
      expect_identical(data, fixture$raw)
      expect_null(attr(data, "liss_source_nonresponse", exact = TRUE))
    }
    expect_identical(fixture$raw, fixture$saved$raw)
    expect_null(result[["source_nonresponse"]])
    expect_false(identical(result$source_nonresponse_readiness$status, "passed"))
    expect_length(list.files(fixture$directory), 1)
  })
})

test_that("second and third finalizer failures expose no partial carrier or artifact", {
  fixture <- .nrb_fixture()
  original <- lissr:::source_nonresponse_finalize
  for (failure in c(2L, 3L)) local({
    completed <- character()
    testthat::local_mocked_bindings(source_nonresponse_finalize =
      function(data, state, target, wave_col = NULL, normalized = FALSE,
        expected_target_metadata = state$target_metadata) {
        if (length(completed) == failure - 1L)
          lissr:::.liss_nonresponse_abort("synthetic mixed finalizer failure")
        expect_null(attr(data, "liss_source_nonresponse", exact = TRUE))
        answer <- original(data, state, target, wave_col, normalized, expected_target_metadata)
        completed <<- c(completed, target)
        answer
      }, .package = "lissr")
    result <- tryCatch(.nrb_merge(fixture), error = identity)
    expect_s3_class(.nrb_deep_error(result), "liss_source_nonresponse_blocked")
    expect_identical(completed, paste0("s", c("337", "338", "339")[seq_len(failure - 1L)]))
    expect_null(result[["source_nonresponse"]])
    expect_false(identical(result$source_nonresponse_readiness$status, "passed"))
    expect_length(list.files(fixture$directory), 1)
  })
})

test_that("any missing malformed or inconsistent source member blocks the whole call", {
  fixture <- .nrb_fixture()
  for (suffix in c("337", "338", "339")) for (kind in c("absent", "malformed")) local({
    raw <- fixture$raw
    raw[[paste0("ci23p", suffix)]] <- if (kind == "absent") NULL else
      as.character(raw[[paste0("ci23p", suffix)]])
    testthat::local_mocked_bindings(read_wave_file = function(path, ...) raw,
      .liss_nonresponse_read_source = function(path) raw, .package = "lissr")
    expect_error(.nrb_merge(fixture), class = "liss_source_nonresponse_blocked",
      info = paste(suffix, kind))
    expect_identical(fixture$raw, fixture$saved$raw)
  })
  for (key in c("filename", "sha256")) local({
    receipt <- fixture$receipt
    receipt[[key]] <- if (key == "filename") "ci23p_NL_1.1p.sav" else strrep("0", 64)
    testthat::local_mocked_bindings(.liss_nonresponse_source_state = function(path) receipt,
      .package = "lissr")
    expect_error(.nrb_merge(fixture), class = "liss_source_nonresponse_blocked")
  })
  for (suffix in c("337", "338", "339")) for (kind in c("metadata", "value")) local({
    raw <- fixture$raw
    if (kind == "metadata") attr(raw[[paste0("ci23p", suffix)]], "label") <- "changed source construct"
    else raw[[paste0("ci23p", suffix)]][1L] <- 123
    testthat::local_mocked_bindings(.liss_nonresponse_read_source = function(path) raw,
      .package = "lissr")
    expect_error(.nrb_merge(fixture), class = "liss_source_nonresponse_blocked",
      info = paste(suffix, kind))
  })
  local({
    calls <- 0L
    testthat::local_mocked_bindings(.liss_nonresponse_source_state = function(path) {
      calls <<- calls + 1L
      receipt <- fixture$receipt
      if (calls > 4L) receipt$size_bytes <- receipt$size_bytes + 1
      receipt
    }, .package = "lissr")
    expect_error(.nrb_merge(fixture), class = "liss_source_nonresponse_blocked")
    expect_gte(calls, 6L)
  })
})

test_that("late target metadata rows waves aliases and protected codes cannot change", {
  fixture <- .nrb_fixture()
  sanitize <- lissr:::sanitize_spss_names
  for (suffix in c("337", "338", "339")) {
    kinds <- c("metadata", "ordinary", "id", "wave", "missing", "alias", "N01")
    if (suffix != "339") kinds <- c(kinds, "N02")
    for (kind in kinds) local({
      target <- paste0("s", suffix)
      testthat::local_mocked_bindings(sanitize_spss_names = function(data) {
        data <- sanitize(data)
        original <- as.numeric(fixture$raw[[paste0("ci23p", suffix)]])
        approved <- if (suffix == "339") -9999999999 else c(-9999999999, -9999999998)
        protected <- which(is.finite(original) & original %in% approved)[1L]
        if (kind == "metadata") attr(data[[target]], "label") <- "changed canonical metadata"
        if (kind == "ordinary") data[[target]][which(is.finite(original) & original > 0)[1L]] <- 10001
        if (kind == "id") data$nomem_encr[1L] <- 99
        if (kind == "wave") data$wave_id[1L] <- "ci22o"
        if (kind == "missing") data[[target]] <- NULL
        if (kind == "alias") names(data)[names(data) == target] <- paste0("ci_", target)
        if (kind == "N01") data[[target]][protected] <- -9999999999
        if (kind == "N02") data[[target]][protected] <- -9999999998
        data
      }, .package = "lissr")
      expect_error(.nrb_merge(fixture), class = "liss_source_nonresponse_blocked",
        info = paste(suffix, kind))
      expect_length(list.files(fixture$directory), 1)
    })
  }
  original <- lissr:::source_nonresponse_finalize
  testthat::local_mocked_bindings(source_nonresponse_finalize =
    function(data, state, target, wave_col = NULL, normalized = FALSE,
      expected_target_metadata = state$target_metadata) {
      if (identical(state$schema_version, "1.0.0")) {
        attr(data[[target]], "label") <- "household finalizer attempted metadata repin"
        state$target_metadata <- attributes(data[[target]])
        state <- lissr:::.liss_nonresponse_seal(state)
      }
      original(data, state, target, wave_col, normalized, expected_target_metadata)
    }, .package = "lissr")
  expect_error(.nrb_merge(fixture), class = "liss_source_nonresponse_blocked")
})

test_that("zero matches pass but an absent selected wave has no bound collection", {
  fixture <- .nrb_fixture()
  for (zero in c(list("337", "338", "339"), list(c("337", "338", "339")))) local({
    raw <- fixture$raw
    removed <- 0L
    for (suffix in zero) {
      column <- paste0("ci23p", suffix)
      approved <- if (suffix == "339") -9999999999 else c(-9999999999, -9999999998)
      values <- as.numeric(raw[[column]])
      matched <- is.finite(values) & values %in% approved
      removed <- removed + sum(matched)
      raw[[column]][matched] <- 19
    }
    testthat::local_mocked_bindings(read_wave_file = function(path, ...) raw,
      .liss_nonresponse_read_source = function(path) raw, .package = "lissr")
    result <- .nrb_merge(fixture)
    expect_identical(result$source_nonresponse_readiness$status, "passed")
    expect_equal(result$source_nonresponse_readiness$evaluated_cells, 36)
    expect_equal(result$source_nonresponse_readiness$classified_cells, 9 - removed)
    for (suffix in zero) {
      leaf <- result$source_nonresponse$fields[[paste0("CI23P", suffix, "_FIELD")]]
      expect_equal(nrow(leaf$ledger), 0)
      expect_equal(leaf$readiness$classified_cells, 0)
      expect_identical(leaf$readiness$status, "passed")
    }
  })
  recipe <- .nrb_recipe(c("339", "338", "337"))
  full <- lissr::liss_recipe("ci")
  recipe$wave_index <- Filter(function(wave) wave$id == "ci22o", full$wave_index)
  recipe$meta$covered_waves <- list("ci22o")
  raw <- fixture$raw
  names(raw) <- sub("^ci23p", "ci22o", names(raw))
  haven::write_sav(raw, file.path(fixture$directory, "ci22o_EN_1.0p.sav"))
  testthat::local_mocked_bindings(read_wave_file = function(path, ...) raw, .package = "lissr")
  result <- .nrb_merge(fixture, recipe)
  expect_s3_class(result, "liss_source_nonresponse_collection_result")
  expect_null(result[["source_nonresponse"]])
  expect_null(attr(result$data, "liss_source_nonresponse", exact = TRUE))
  ready <- result$source_nonresponse_readiness
  expect_identical(ready$schema_version, "4.0.0")
  expect_identical(ready$status, "not_evaluated")
  expect_identical(names(ready$fields), paste0("CI23P", c("339", "338", "337"), "_FIELD"))
  expect_equal(ready$evaluated_cells, 0)
  expect_equal(ready$classified_cells, 0)
  expect_equal(ready$unresolved_cells, 0)
  for (field in ready$fields) {
    expect_identical(field$status, "not_evaluated")
    expect_equal(field$evaluated_cells, 0)
    expect_equal(field$classified_cells, 0)
  }
})

test_that("RDS subsets and reordered frames validate and reseal all three native leaves", {
  fixture <- .nrb_fixture()
  result <- .nrb_merge(fixture)
  path <- file.path(fixture$directory, "invented_three.rds")
  saveRDS(result, path, version = 2L)
  restored <- readRDS(path)
  expect_identical(restored, result)
  for (rows in list(rev(seq_len(nrow(result$data))), c(12L, 1L, 11L, 2L), c(8L, 9L), integer())) {
    subset <- restored
    subset$data <- dplyr::slice(subset$data, rows)
    checked <- .nrb_validate(subset)
    expected_count <- 0L
    for (suffix in c("337", "338", "339")) {
      leaf <- checked$state$fields[[paste0("CI23P", suffix, "_FIELD")]]
      values <- as.numeric(fixture$raw[[paste0("ci23p", suffix)]])[rows]
      approved <- if (suffix == "339") -9999999999 else c(-9999999999, -9999999998)
      matched <- is.finite(values) & values %in% approved
      expected_count <- expected_count + sum(matched)
      expect_equal(nrow(leaf$coverage), length(rows))
      expect_equal(nrow(leaf$input_rows), length(rows))
      expect_equal(nrow(leaf$ledger), sum(matched))
      expect_equal(leaf$readiness$classified_cells, sum(matched))
      expect_identical(leaf$digest, lissr:::.liss_nonresponse_digest(leaf))
      expect_identical(leaf$source, checked$state$source)
    }
    expect_equal(checked$state$readiness$evaluated_cells, 3 * length(rows))
    expect_equal(checked$state$readiness$classified_cells, expected_count)
    expect_identical(checked$state$digest, lissr:::.liss_nonresponse_digest(checked$state))
    rebound <- subset
    rebound$source_nonresponse <- checked$state
    rebound$source_nonresponse_readiness <- checked$readiness
    attr(rebound$data, "liss_source_nonresponse") <- checked$state
    expect_identical(.nrb_validate(rebound), checked)
    expect_identical(restored, result)
  }
  for (suffix in c("337", "338", "339")) {
    subset <- result
    subset$data <- dplyr::slice(subset$data, c(1L, 2L, 9L))
    attr(subset$data[[paste0("s", suffix)]], "label") <- NULL
    expect_error(.nrb_validate(subset), "target metadata changed",
      class = "liss_source_nonresponse_blocked")
  }
})

test_that("mixed receipt validation reconstructs native ledger and readiness invariants", {
  fixture <- .nrb_fixture()
  result <- .nrb_merge(fixture)
  for (suffix in c("337", "338", "339")) {
    id <- paste0("CI23P", suffix, "_FIELD")
    mutations <- list(
      function(x) { x$source$size_bytes <- x$source$size_bytes + 1; x },
      function(x) { x$normalized <- FALSE; x },
      function(x) { x$target <- "nethh"; x },
      function(x) { x$wave_col <- "other_wave"; x },
      function(x) { x$id_col <- "nohouse_encr"; x },
      function(x) { x$coverage$source_row_index[1L] <- 999L; x },
      function(x) { x$coverage$original_value[1L] <- 123; x },
      function(x) { x$input_rows$value[1L] <- 123; x },
      function(x) { x$input_rows$row_key[1L] <- "changed"; x },
      function(x) { x$metadata$id_attributes <- list(label = "changed key metadata"); x },
      function(x) { x$metadata$raw_column <- "ci23p226"; x },
      function(x) { x$metadata$codebook_filename <- "other_codebook.pdf"; x },
      function(x) { x$metadata$codebook_sha256 <- strrep("0", 64); x },
      function(x) { x$metadata$attributes$na_values <- c(-9999999999, -9999999998); x },
      function(x) { x$metadata$attributes$na_range <- c(-9999999999, -9999999998); x },
      function(x) { x$target_metadata$label <- "changed target metadata"; x },
      function(x) { x$readiness$classified_cells <- 0L; x })
    for (index in seq_along(mutations)) {
      bad <- result
      bad$source_nonresponse$fields[[id]] <- mutations[[index]](bad$source_nonresponse$fields[[id]])
      expect_error(.nrb_validate(.nrb_reseal(bad)), class = "liss_source_nonresponse_blocked",
        info = paste(suffix, "receipt", index))
    }
    bad <- result
    bad$source_nonresponse$fields[[id]]$metadata$attributes$label <- "changed raw metadata"
    expect_error(.nrb_validate(.nrb_reseal(bad, leaves = FALSE)),
      class = "liss_source_nonresponse_blocked")
    leaf <- result$source_nonresponse$fields[[id]]
    for (key in names(leaf$ledger)) {
      bad <- result
      value <- bad$source_nonresponse$fields[[id]]$ledger[[key]]
      value[1L] <- if (is.character(value)) "invalid ledger value" else if (is.logical(value))
        !isTRUE(value[1L]) else if (is.na(value[1L])) 1 else value[1L] + 1
      bad$source_nonresponse$fields[[id]]$ledger[[key]] <- value
      expect_error(.nrb_validate(.nrb_reseal(bad)), class = "liss_source_nonresponse_blocked",
        info = paste(suffix, "ledger", key))
    }
    for (key in c("source", "coverage", "ledger", "input_rows", "metadata", "target_metadata", "readiness")) {
      bad <- result
      bad$source_nonresponse$fields[[id]][[key]] <- NULL
      expect_error(.nrb_validate(.nrb_reseal(bad)), class = "liss_source_nonresponse_blocked",
        info = paste(suffix, "missing", key))
    }
  }
  for (key in c("candidate_answer_status", "subtype_evidence")) {
    bad <- result
    bad$source_nonresponse$fields$CI23P339_FIELD$ledger[[key]][1L] <- NA_character_
    expect_error(.nrb_validate(.nrb_reseal(bad)), class = "liss_source_nonresponse_blocked")
  }
  expect_identical(.nrb_validate(result)$state, result$source_nonresponse)
})

test_that("outer contradictions unsupported versions and protected reintroduction block", {
  fixture <- .nrb_fixture()
  result <- .nrb_merge(fixture)
  mutations <- list(
    function(x) { x$schema_version <- "3.0.0"; x },
    function(x) { x$schema_version <- "5.0.0"; x },
    function(x) { x$kind <- "scalar"; x },
    function(x) { x$fields <- x$fields[1:2]; x },
    function(x) { names(x$fields) <- rev(names(x$fields)); x },
    function(x) { x$fields[[3L]] <- x$fields[[1L]]; x },
    function(x) { x$fields$CI23P339_FIELD$schema_version <- "2.0.0"; x },
    function(x) { x$normalized <- FALSE; x },
    function(x) { x$readiness$classified_cells <- 0L; x },
    function(x) { x$readiness$fields <- rev(x$readiness$fields); x },
    function(x) { x$extra <- TRUE; x })
  for (index in seq_along(mutations)) {
    bad <- result
    bad$source_nonresponse <- mutations[[index]](bad$source_nonresponse)
    expect_error(.nrb_validate(.nrb_reseal(bad)), class = "liss_source_nonresponse_blocked",
      info = paste("outer", index))
  }
  for (suffix in c("337", "338", "339")) {
    approved <- if (suffix == "339") -9999999999 else c(-9999999999, -9999999998)
    original <- as.numeric(fixture$raw[[paste0("ci23p", suffix)]])
    row <- which(is.finite(original) & original %in% approved)[1L]
    for (code in approved) {
      bad <- result
      bad$data[[paste0("s", suffix)]][row] <- code
      expect_error(.nrb_validate(bad), class = "liss_source_nonresponse_blocked",
        info = paste(suffix, code))
    }
  }
  for (form in .nrb_forms(result)[c("class_only", "missing", "contradictory", "readiness_only")])
    expect_error(lissr:::source_nonresponse_validate(form, result$source_nonresponse),
      class = "liss_source_nonresponse_blocked")
  for (key in c("source_nonresponse_readiness", "liss_source_nonresponse_readiness")) {
    bad <- result
    ready <- result$source_nonresponse_readiness
    ready$classified_cells <- 0L
    attr(bad$data, key) <- ready
    expect_error(.nrb_validate(bad), class = "liss_source_nonresponse_blocked")
  }
  bad <- result
  bad$source_nonresponse <- bad$source_nonresponse$fields
  expect_error(.nrb_validate(bad), class = "liss_source_nonresponse_blocked")
})

test_that("all v4 claims deny reverse scalar binding before new source reads", {
  fixture <- .nrb_fixture()
  result <- .nrb_merge(fixture)
  reads <- FALSE
  testthat::local_mocked_bindings(.liss_nonresponse_read_source = function(path) {
    reads <<- TRUE
    stop("lineage denial must precede source reads")
  }, .package = "lissr")
  for (name in names(.nrb_forms(result))) for (suffix in c("339", "337", "338")) {
    recipe <- if (suffix == "339") NULL else .nrb_saved(2)$fields[[suffix]]$recipe
    expect_error(lissr::liss_classify_source_nonresponse(.nrb_forms(result)[[name]],
      fixture$source, paste0("s", suffix), wave_col = "wave_id", recipe = recipe),
      class = "liss_source_nonresponse_blocked", info = paste(name, suffix))
    expect_false(reads)
  }
  old <- .nrb_saved(1)$inputs$canonical
  inputs <- c(list(old), lapply(c("337", "338"), function(suffix)
    .nrb_saved(2)$fields[[suffix]]$inputs[[paste0("s", suffix)]]),
    list(fixture$saved$orders[[1L]]$input, result))
  plan <- lissr:::.liss_nonresponse_plan(.nrb_recipe(c("339", "337", "338")))
  for (input in inputs) {
    frame <- input$data
    attr(frame, "liss_source_nonresponse") <- input$source_nonresponse
    for (form in list(input, frame)) {
      expect_error(lissr:::.liss_nonresponse_preflight(form, fixture$source, plan),
        class = "liss_source_nonresponse_blocked")
      expect_false(reads)
    }
  }
})

test_that("all collection cleaner modes overrides and consumer exports remain closed", {
  fixture <- .nrb_fixture()
  result <- .nrb_merge(fixture)
  ruleset <- lissr::liss_cleaning_ruleset()
  all_rules <- unlist(lapply(c("preparation_rules", "detection_rules", "correction_rules",
    "finalization_rules"), function(section) vapply(ruleset[[section]],
      function(rule) rule$rule_id, character(1))), use.names = FALSE)
  configurations <- list(list(), list(disable = c("P01", "P03", "P06")),
    list(enable_only = "D06"), list(disable = all_rules),
    list(params = list(P04 = list(code_share = 0.8))))
  reached <- character()
  testthat::local_mocked_bindings(
    attach_background_frame = function(...) { reached <<- c(reached, "background"); stop("background reached") },
    liss_cleaning_ruleset = function(...) { reached <<- c(reached, "ruleset"); stop("ruleset reached") },
    .package = "lissr")
  for (name in names(.nrb_forms(result))) for (mode in c("flag", "correct", "na_only")) {
    for (configuration in configurations) {
      args <- c(list(data = .nrb_forms(result)[[name]], mode = mode,
        background = result$data, variables = list(target = "absent_target"),
        verbose = FALSE), configuration)
      expect_error(do.call(lissr::liss_clean_income, args),
        class = "liss_source_nonresponse_blocked", info = paste(name, mode))
      expect_length(reached, 0)
    }
  }
  for (target in c("s339", "nethh", "ci00a339")) {
    renamed <- result
    names(renamed$data)[names(renamed$data) == "s337"] <- target
    expect_error(lissr::liss_clean_income(renamed, mode = "flag", verbose = FALSE,
      variables = list(target = target)), class = "liss_source_nonresponse_blocked")
    expect_length(reached, 0)
  }
  output <- file.path(fixture$directory, "blocked_three_export")
  expect_error(lissr::merge_liss_module(.nrb_recipe(), fixture$directory, output,
    source_nonresponse = TRUE), "output_dir = NULL")
  recipe <- .nrb_recipe()
  recipe$global$labelled_policy <- "preserve"
  expect_error(lissr::merge_liss_module(recipe, fixture$directory,
    output_dir = NULL, source_nonresponse = TRUE), "labelled_policy = to_numeric")
  for (form in .nrb_forms(result))
    expect_error(lissr::merge_liss_panel(list(ci = form)), "source_nonresponse.*lineage")
  expect_error(lissr::liss_cleaning_report(result, output), "guarded.*report export",
    class = "liss_source_nonresponse_blocked")
  expect_false(file.exists(output))
})

test_that("complete source rows remain mandatory even when no member classifies them", {
  fixture <- .nrb_fixture()
  sanitize <- lissr:::sanitize_spss_names
  # row six has no exact approved code in any of the three raw fields
  for (kind in c("drop", "move_wave", "replace_key")) local({
    testthat::local_mocked_bindings(sanitize_spss_names = function(data) {
      data <- sanitize(data)
      if (kind == "drop") data <- dplyr::slice(data, -6L)
      if (kind == "move_wave") data$wave_id[6L] <- "ci22o"
      if (kind == "replace_key") data$nomem_encr[6L] <- 99
      data
    }, .package = "lissr")
    expect_error(.nrb_merge(fixture), class = "liss_source_nonresponse_blocked", info = kind)
    expect_length(list.files(fixture$directory), 1)
  })
  testthat::local_mocked_bindings(sanitize_spss_names = function(data)
    dplyr::slice(sanitize(data), rev(seq_len(nrow(data)))), .package = "lissr")
  result <- .nrb_merge(fixture)
  expect_identical(as.numeric(result$data$nomem_encr), rev(as.numeric(fixture$raw$nomem_encr)))
  expect_identical(.nrb_validate(result)$state, result$source_nonresponse)
})

test_that("a plain numeric household source retains its complete native NULL metadata slot", {
  fixture <- .nrb_fixture()
  raw <- fixture$raw
  raw$ci23p339 <- as.numeric(raw$ci23p339)
  expect_null(attributes(raw$ci23p339))
  testthat::local_mocked_bindings(.liss_nonresponse_read_source = function(path) raw,
    read_wave_file = function(path, ...) raw, .package = "lissr")
  result <- .nrb_merge(fixture)
  leaf <- result$source_nonresponse$fields$CI23P339_FIELD
  expect_identical(leaf$schema_version, "1.0.0")
  expect_true("target_metadata" %in% names(leaf))
  expect_null(leaf$target_metadata)
  expect_null(leaf$metadata$attributes)
  expect_null(attributes(result$data$s339))
  expect_identical(.nrb_validate(result)$state, result$source_nonresponse)
})

test_that("household routing evidence remains from the original read after 229 changes", {
  fixture <- .nrb_fixture()
  sanitize <- lissr:::sanitize_spss_names
  baseline <- .nrb_merge(fixture)
  testthat::local_mocked_bindings(sanitize_spss_names = function(data) {
    data <- sanitize(data)
    data$s229[] <- NA_real_
    data
  }, .package = "lissr")
  result <- .nrb_merge(fixture)
  expect_true(all(is.na(result$data$s229)))
  expect_identical(result$source_nonresponse$fields$CI23P339_FIELD,
    baseline$source_nonresponse$fields$CI23P339_FIELD)
  expect_identical(result$source_nonresponse$fields$CI23P339_FIELD$ledger$candidate_answer_status,
    "dont_know")
  expect_identical(.nrb_validate(result)$state, result$source_nonresponse)
})

test_that("outer wrapper attribute contradictions block before unwrapping", {
  fixture <- .nrb_fixture()
  result <- .nrb_merge(fixture)
  expect_identical(lissr:::source_nonresponse_validate(result,
    result$source_nonresponse)$state, result$source_nonresponse)
  equivalent <- result
  attr(equivalent, "liss_source_nonresponse") <- result$source_nonresponse
  attr(equivalent, "source_nonresponse_readiness") <- result$source_nonresponse_readiness
  attr(equivalent, "liss_source_nonresponse_readiness") <- result$source_nonresponse_readiness
  expect_identical(lissr:::source_nonresponse_validate(equivalent,
    result$source_nonresponse)$state, result$source_nonresponse)
  for (key in c("liss_source_nonresponse", "source_nonresponse_readiness",
    "liss_source_nonresponse_readiness")) {
    bad <- result
    if (key == "liss_source_nonresponse") {
      attr(bad, key) <- list(schema_version = "3.0.0")
    } else {
      ready <- result$source_nonresponse_readiness
      ready$classified_cells <- 0L
      attr(bad, key) <- ready
    }
    expect_error(lissr:::source_nonresponse_validate(bad, bad$source_nonresponse),
      class = "liss_source_nonresponse_blocked", info = key)
  }
})


test_that("frozen v3 wrappers enforce the same declared attribute contradiction boundary", {
  for (captured in .nrb_saved()$orders) {
    result <- captured$input
    state <- result$source_nonresponse
    expect_identical(lissr:::source_nonresponse_validate(result, state), captured$validated)
    equivalent <- result
    attr(equivalent, "liss_source_nonresponse") <- state
    attr(equivalent, "source_nonresponse_readiness") <- state$readiness
    attr(equivalent, "liss_source_nonresponse_readiness") <- state$readiness
    expect_identical(lissr:::source_nonresponse_validate(equivalent, state), captured$validated)
    for (key in c("liss_source_nonresponse", "source_nonresponse_readiness",
      "liss_source_nonresponse_readiness")) {
      bad <- result
      if (key == "liss_source_nonresponse") {
        attr(bad, key) <- list(schema_version = "4.0.0")
      } else {
        ready <- state$readiness
        ready$classified_cells <- 0L
        attr(bad, key) <- ready
      }
      expect_error(lissr:::source_nonresponse_validate(bad, state),
        class = "liss_source_nonresponse_blocked", info = key)
    }
    expect_identical(result, captured$input)
  }
})


test_that("canonical ID labels can differ while every original ID receipt stays identical", {
  fixture <- .nrb_fixture()
  baseline <- .nrb_merge(fixture)
  sanitize <- lissr:::sanitize_spss_names
  testthat::local_mocked_bindings(sanitize_spss_names = function(data) {
    data <- sanitize(data)
    attr(data$nomem_encr, "label") <- "harmonized respondent key"
    data
  }, .package = "lissr")
  result <- .nrb_merge(fixture)
  expect_identical(attr(result$data$nomem_encr, "label"), "harmonized respondent key")
  expect_false(identical(attributes(result$data$nomem_encr), attributes(fixture$raw$nomem_encr)))
  for (leaf in result$source_nonresponse$fields)
    expect_identical(leaf$metadata$id_attributes, attributes(fixture$raw$nomem_encr))
  expect_identical(result$source_nonresponse, baseline$source_nonresponse)
  expect_identical(.nrb_validate(result)$state, result$source_nonresponse)
  relabelled <- result
  attr(relabelled$data$nomem_encr, "label") <- "current respondent key description"
  expect_identical(.nrb_validate(relabelled)$state, result$source_nonresponse)
  for (id in names(result$source_nonresponse$fields)) {
    bad <- result
    bad$source_nonresponse$fields[[id]]$metadata$id_attributes <- list(label = "different original source key")
    expect_error(.nrb_validate(.nrb_reseal(bad)),
      class = "liss_source_nonresponse_blocked", info = id)
  }
})
