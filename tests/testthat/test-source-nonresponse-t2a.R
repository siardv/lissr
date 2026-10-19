# all rows and frozen scalar receipts in this file are invented synthetic data
.nrc_saved <- function() {
  readRDS(testthat::test_path("fixtures", "source_nonresponse_v2_synthetic.rds"))
}

.nrc_fixture <- function(.local_envir = parent.frame()) {
  saved <- .nrc_saved()
  raw <- saved$raw
  directory <- withr::local_tempdir("joint_tax_", .local_envir = .local_envir)
  source <- file.path(directory, "ci23p_EN_1.0p.sav")
  haven::write_sav(raw, source)
  receipt <- saved$receipt
  testthat::local_mocked_bindings(
    .liss_nonresponse_source_state = function(path) receipt,
    .liss_nonresponse_read_source = function(path) raw,
    read_wave_file = function(path, ...) raw,
    .package = "lissr", .env = .local_envir)
  list(saved = saved, raw = raw, source = source, receipt = receipt,
       directory = directory)
}

.nrc_recipe <- function(order = c("337", "338")) {
  recipe <- .nrc_saved()$fields[["337"]]$recipe
  fields <- lissr:::.liss_nonresponse_registry()$fields
  recipe$source_nonresponse <- list(schema_version = "3.0.0",
    fields = lapply(order, function(suffix) {
      selected <- vapply(fields, function(field)
        identical(field$field_id, paste0("CI23P", suffix, "_FIELD")), logical(1))
      fields[[which(selected)]]
    }))
  recipe
}

.nrc_merge <- function(fixture, recipe = .nrc_recipe(), ...) {
  suppressWarnings(suppressMessages(lissr::merge_liss_module(recipe,
    fixture$directory, output_dir = NULL, source_nonresponse = TRUE, ...)))
}

.nrc_validate <- function(bound) {
  lissr:::source_nonresponse_validate(bound$data, bound$source_nonresponse)
}

.nrc_scalar_validate <- function(bound) {
  state <- bound$source_nonresponse
  lissr:::source_nonresponse_validate(bound$data, state, state$target, state$wave_col)
}

.nrc_deep_error <- function(error) {
  while (inherits(error$parent, "error")) error <- error$parent
  error
}

.nrc_rules <- function() {
  ruleset <- lissr::liss_cleaning_ruleset()
  unlist(lapply(c("preparation_rules", "detection_rules", "correction_rules",
    "finalization_rules"), function(section)
      vapply(ruleset[[section]], function(rule) rule$rule_id, character(1))),
    use.names = FALSE)
}

.nrc_marker_forms <- function(result) {
  frame <- result$data
  carrier_only <- list(data = frame, source_nonresponse = result$source_nonresponse)
  readiness_only <- list(data = frame,
    source_nonresponse_readiness = result$source_nonresponse_readiness)
  attr(readiness_only$data, "liss_source_nonresponse") <- NULL
  class_only <- result
  class_only$source_nonresponse <- NULL
  class_only$source_nonresponse_readiness <- NULL
  attr(class_only$data, "liss_source_nonresponse") <- NULL
  class_frame <- class_only$data
  class(class_frame) <- c("liss_source_nonresponse_collection_result", class(class_frame))
  readiness_frame <- class_only$data
  attr(readiness_frame, "source_nonresponse_readiness") <- result$source_nonresponse_readiness
  missing <- result
  missing$source_nonresponse <- NULL
  contradictory <- result
  attr(contradictory$data, "liss_source_nonresponse") <-
    .nrc_saved()$fields[["337"]]$inputs$s337$source_nonresponse
  list(intact = result, frame = frame, carrier_only = carrier_only,
    readiness_only = readiness_only, class_only = class_only,
    class_frame = class_frame, readiness_frame = readiness_frame,
    missing = missing, contradictory = contradictory)
}

test_that("frozen T1 v2 scalar states cross-read completely without migration", {
  fixture <- .nrc_fixture()
  expect_identical(fixture$saved$format, "invented_row_t1_v2_fixture")
  for (suffix in c("337", "338")) {
    saved <- fixture$saved$fields[[suffix]]
    for (name in names(saved$inputs)) {
      bound <- saved$inputs[[name]]
      expect_identical(.nrc_scalar_validate(bound), saved$validated[[name]],
        info = paste(suffix, name))
      expect_identical(bound$source_nonresponse$schema_version, "2.0.0")
      if (name == "normalized") {
        merged <- suppressWarnings(suppressMessages(lissr::merge_liss_module(
          saved$recipe, fixture$directory, output_dir = NULL,
          source_nonresponse = TRUE)))
        current <- merged[c("data", "source_nonresponse", "source_nonresponse_readiness")]
        class(current) <- class(merged)
      } else {
        current <- lissr::liss_classify_source_nonresponse(bound$data, fixture$source,
          bound$source_nonresponse$target, wave_col = bound$source_nonresponse$wave_col,
          recipe = if (name == "reordered") {
            recipe <- saved$recipe
            recipe$source_nonresponse$field <- bound$source_nonresponse$field
            recipe
          } else saved$recipe)
      }
      expect_identical(current, bound, info = paste(suffix, name))
    }
    for (name in names(saved$subsets))
      for (subset in saved$subsets[[name]])
        expect_identical(.nrc_scalar_validate(subset$input), subset$validated)
  }
  old <- readRDS(testthat::test_path("fixtures", "source_nonresponse_v1_synthetic.rds"))
  for (name in c("raw", "canonical", "normalized"))
    expect_identical(.nrc_scalar_validate(old$inputs[[name]]), old$validated[[name]])
})

test_that("explicit joint selection preserves either field order and complete mirrors", {
  fixture <- .nrc_fixture()
  for (order in list(c("337", "338"), c("338", "337"))) {
    recipe <- .nrc_recipe(order)
    recipe$source_nonresponse$fields <- lapply(recipe$source_nonresponse$fields,
      function(field) {
        field <- field[rev(seq_along(field))]
        field$readiness <- field$readiness[rev(seq_along(field$readiness))]
        field$cases <- lapply(field$cases, function(case) case[rev(seq_along(case))])
        field
      })
    expect_identical(lissr:::validate_source_nonresponse_contracts(recipe), character())
    result <- .nrc_merge(fixture, recipe)
    state <- result$source_nonresponse
    ids <- paste0("CI23P", order, "_FIELD")
    expect_s3_class(result, "liss_source_nonresponse_collection_result")
    expect_s3_class(result, "liss_source_nonresponse_result")
    expect_identical(state$schema_version, "3.0.0")
    expect_identical(state$kind, "source_nonresponse_collection")
    expect_identical(names(state), c("schema_version", "kind", "source", "fields",
      "normalized", "readiness", "digest"))
    expect_identical(names(state$fields), ids)
    expect_identical(names(state$readiness$fields), ids)
    expect_identical(state, attr(result$data, "liss_source_nonresponse", exact = TRUE))
    expect_identical(state$readiness, result$source_nonresponse_readiness)
    expect_identical(state$digest, lissr:::.liss_nonresponse_digest(state))
    expect_true(state$normalized)
    for (index in seq_along(ids)) {
      leaf <- state$fields[[index]]
      expect_identical(leaf$field, recipe$source_nonresponse$fields[[index]])
      expect_identical(leaf$field$field_id, ids[[index]])
      expect_identical(leaf$source, state$source)
      expect_identical(leaf$schema_version, "2.0.0")
      expect_true(leaf$normalized)
      expect_identical(leaf$digest, lissr:::.liss_nonresponse_digest(leaf))
      expect_identical(leaf$target, paste0("s", order[[index]]))
      expect_identical(leaf$wave_col, "wave_id")
      expect_identical(leaf$id_col, "nomem_encr")
      expect_identical(leaf$readiness, state$readiness$fields[[ids[[index]]]])
      expect_equal(nrow(leaf$input_rows), nrow(fixture$raw))
      expect_equal(nrow(leaf$coverage), nrow(fixture$raw))
    }
    checked <- .nrc_validate(result)
    expect_identical(checked$state, state)
    expect_identical(checked$readiness, state$readiness)
    expect_identical(state$readiness$status, "passed")
    expect_equal(state$readiness$evaluated_cells, 24)
    expect_equal(state$readiness$classified_cells, 8)
    expect_equal(state$readiness$unresolved_cells, 0)
    expect_match(state$readiness$scope, "field")
  }
})

test_that("joint recipes reject every incomplete ambiguous or altered selection", {
  fixture <- .nrc_fixture()
  recipe <- .nrc_recipe()
  changes <- list(
    function(x) { x$source_nonresponse$fields <- list(); x },
    function(x) { x$source_nonresponse$fields <- x$source_nonresponse$fields[1]; x },
    function(x) { x$source_nonresponse$fields[[3]] <- x$source_nonresponse$fields[[1]]; x },
    function(x) { x$source_nonresponse$fields[[2]] <- x$source_nonresponse$fields[[1]]; x },
    function(x) { names(x$source_nonresponse$fields) <- c("first", "second"); x },
    function(x) { x$source_nonresponse$fields[[2]] <- lissr:::.liss_nonresponse_rule(); x },
    function(x) { x$source_nonresponse$fields <- "337/338"; x },
    function(x) { x$source_nonresponse$extra <- TRUE; x },
    function(x) { x$source_nonresponse$schema_version <- "4.0.0"; x },
    function(x) { x$source_nonresponse$fields[[1]]$field_id <- "CI23P339_FIELD"; x },
    function(x) { x$source_nonresponse$fields[[1]]$raw_column <- "ci23p338"; x },
    function(x) { x$source_nonresponse$fields[[1]]$extra <- TRUE; x },
    function(x) { x$source_nonresponse$fields[[1]]$cases <- rev(x$source_nonresponse$fields[[1]]$cases); x },
    function(x) { x$source_nonresponse$fields[[1]]$cases[[3]] <- x$source_nonresponse$fields[[1]]$cases[[1]]; x },
    function(x) { x$source_nonresponse$fields[[2]]$cases[[2]]$code <- -9999999999; x },
    function(x) { x$source_nonresponse$fields[[2]]$cases[[2]]$rule_id <- "CI23P337_N02"; x },
    function(x) { x$source_nonresponse$fields[[2]]$cases[[1]]$code <- "-9999999999"; x },
    function(x) { x$source_nonresponse$fields[[1]]$source_sha256 <- strrep("0", 64); x },
    function(x) { x$source_nonresponse$fields[[2]]$consumers <- list(list(name = "liss_clean_income")); x })
  for (index in seq_along(changes)) {
    changed <- changes[[index]](recipe)
    expect_true(length(lissr:::validate_source_nonresponse_contracts(changed)) > 0,
      info = paste("selection", index))
    expect_error(.nrc_merge(fixture, changed), info = paste("selection", index))
  }
  reads <- FALSE
  testthat::local_mocked_bindings(.liss_nonresponse_read_source = function(path) {
    reads <<- TRUE
    stop("scalar schema rejection must precede source reads")
  }, .package = "lissr")
  expect_error(lissr::liss_classify_source_nonresponse(fixture$raw, fixture$source,
    "ci23p337", recipe = recipe), class = "liss_source_nonresponse_blocked")
  expect_false(reads)
})

test_that("joint selection requires the same-buffer reviewed registry authority", {
  fixture <- .nrc_fixture()
  recipe <- .nrc_recipe()
  registry <- lissr:::.liss_nonresponse_registry_path()
  missing <- file.path(fixture$directory, "missing_registry.yml")
  corrupt <- file.path(fixture$directory, "corrupt_registry.yml")
  writeLines(c(readLines(registry, warn = FALSE), "# changed bytes"), corrupt)
  for (path in c(missing, corrupt)) local({
    selected_path <- path
    testthat::local_mocked_bindings(.liss_nonresponse_registry_path = function() selected_path,
      .package = "lissr")
    expect_error(.nrc_merge(fixture, recipe))
  })
})

test_that("joint normalization records complete typed cells and preserves all controls", {
  fixture <- .nrc_fixture()
  recipe <- .nrc_recipe()
  recipe$harmonization_rules <- list(list(rule_id = "JOINT_ORDERING_PROBE",
    action = "value_recode", description = "synthetic ordering probe",
    suffixes = list("337", "338"), mapping = list(`-9999999999` = 111, `-9999999998` = 222)))
  result <- .nrc_merge(fixture, recipe)
  baseline <- suppressWarnings(suppressMessages(lissr::merge_liss_module(recipe,
    fixture$directory, output_dir = NULL, source_nonresponse = FALSE)))
  expect_null(baseline[["source_nonresponse"]])
  for (suffix in c("337", "338")) {
    field <- result$source_nonresponse$fields[[paste0("CI23P", suffix, "_FIELD")]]
    values <- as.numeric(fixture$raw[[paste0("ci23p", suffix)]])
    matched <- is.finite(values) & values %in% c(-9999999999, -9999999998)
    expect_true(all(is.na(as.numeric(result$data[[paste0("s", suffix)]])[matched])))
    expect_identical(as.numeric(result$data[[paste0("s", suffix)]])[!matched], values[!matched])
    expect_equal(nrow(field$ledger), sum(matched))
    expect_identical(field$ledger$original_value, values[matched])
    expect_identical(field$ledger$source_key, paste(fixture$receipt$sha256,
      which(matched), paste0("ci23p", suffix), sep = ":"))
    expect_false(anyDuplicated(field$ledger$source_key) > 0L)
    expect_identical(field$ledger$rule_id, paste0("CI23P", suffix,
      ifelse(values[matched] == -9999999999, "_N01", "_N02")))
    expect_identical(field$ledger$source_declared_missing, values[matched] == -9999999999)
    expect_true(all(field$ledger$no_numeric_proposal))
    expect_true(all(is.na(field$ledger$analysis_value_after)))
    expect_true(all(field$ledger$answer_status == "nonresponse_unknown"))
    expect_true(all(is.na(field$ledger$candidate_answer_status)))
    expect_true(all(field$ledger$subtype_evidence == "unresolved"))
    expect_identical(field$readiness$case_counts,
      stats::setNames(c(2L, 2L), paste0("CI23P", suffix, c("_N01", "_N02"))))
  }
  for (column in c("s339", "s226", "s229"))
    expect_identical(result$data[[column]], baseline$data[[column]], info = column)
  logs <- Filter(function(entry) identical(entry$action,
    "source_nonresponse:missing_amount"), result$log)
  expect_identical(vapply(logs, `[[`, character(1), "rule_id"),
    c("CI23P337_N01", "CI23P337_N02", "CI23P338_N01", "CI23P338_N02"))
  expect_equal(vapply(logs, `[[`, numeric(1), "rows_affected"), rep(2, 4))
  expect_equal(vapply(logs, `[[`, numeric(1), "values_changed"), rep(2, 4))
  expect_length(list.files(fixture$directory), 1)
})

test_that("both original fields pass before either original field can be normalized", {
  fixture <- .nrc_fixture()
  original <- lissr:::.liss_nonresponse_classify_field
  calls <- character()
  observations <- list()
  testthat::local_mocked_bindings(.liss_nonresponse_classify_field =
    function(data, source, target, wave_col, id_col, field) {
      calls <<- c(calls, field$field_id)
      observations[[length(observations) + 1L]] <<- data
      if (length(calls) == 2L)
        lissr:::.liss_nonresponse_abort("synthetic second original-source failure")
      original(data, source, target, wave_col, id_col, field)
    }, .package = "lissr")
  result <- tryCatch(.nrc_merge(fixture), error = identity)
  expect_s3_class(.nrc_deep_error(result), "liss_source_nonresponse_blocked")
  expect_identical(calls, c("CI23P337_FIELD", "CI23P338_FIELD"))
  expect_length(observations, 2)
  for (data in observations) {
    expect_identical(data$ci23p337, fixture$raw$ci23p337)
    expect_identical(data$ci23p338, fixture$raw$ci23p338)
    expect_null(attr(data, "liss_source_nonresponse", exact = TRUE))
  }
  expect_false(identical(result$source_nonresponse_readiness$status, "passed"))
  expect_length(list.files(fixture$directory), 1)
})

test_that("a second finalizer failure exposes no partial collection or artifact", {
  fixture <- .nrc_fixture()
  original <- lissr:::source_nonresponse_finalize
  completed <- character()
  testthat::local_mocked_bindings(source_nonresponse_finalize =
    function(data, state, target, wave_col = NULL, normalized = FALSE,
      expected_target_metadata = state$target_metadata) {
      if (length(completed) == 1L)
        lissr:::.liss_nonresponse_abort("synthetic second finalizer failure")
      answer <- original(data, state, target, wave_col, normalized, expected_target_metadata)
      completed <<- c(completed, state$field$field_id)
      expect_null(attr(data, "liss_source_nonresponse", exact = TRUE))
      answer
    }, .package = "lissr")
  result <- tryCatch(.nrc_merge(fixture), error = identity)
  expect_s3_class(.nrc_deep_error(result), "liss_source_nonresponse_blocked")
  expect_identical(completed, "CI23P337_FIELD")
  expect_null(result[["source_nonresponse"]])
  expect_false(identical(result$source_nonresponse_readiness$status, "passed"))
  expect_length(list.files(fixture$directory), 1)
})

test_that("present malformed fields and mismatched source identities block the whole merge", {
  fixture <- .nrc_fixture()
  for (suffix in c("337", "338")) for (kind in c("absent", "malformed")) local({
    raw <- fixture$raw
    column <- paste0("ci23p", suffix)
    raw[[column]] <- if (kind == "absent") NULL else as.character(raw[[column]])
    testthat::local_mocked_bindings(read_wave_file = function(path, ...) raw,
      .liss_nonresponse_read_source = function(path) raw, .package = "lissr")
    expect_error(.nrc_merge(fixture), class = "liss_source_nonresponse_blocked",
      info = paste(suffix, kind))
    expect_identical(fixture$raw, fixture$saved$raw)
  })
  for (key in c("filename", "sha256")) local({
    receipt <- fixture$receipt
    receipt[[key]] <- if (key == "filename") "ci23p_EN_1.1p.sav" else strrep("0", 64)
    testthat::local_mocked_bindings(.liss_nonresponse_source_state = function(path) receipt,
      .package = "lissr")
    expect_error(.nrc_merge(fixture), class = "liss_source_nonresponse_blocked")
  })
  expect_length(list.files(fixture$directory), 1)
})

test_that("joint preflight rejects inconsistent complete source receipts and metadata", {
  fixture <- .nrc_fixture()
  local({
    calls <- 0L
    testthat::local_mocked_bindings(.liss_nonresponse_source_state = function(path) {
      calls <<- calls + 1L
      receipt <- fixture$receipt
      if (calls > 2L) receipt$size_bytes <- receipt$size_bytes + 1
      receipt
    }, .package = "lissr")
    expect_error(.nrc_merge(fixture), class = "liss_source_nonresponse_blocked")
    expect_gte(calls, 4L)
  })
  for (kind in c("source_id", "source_metadata", "source_values")) local({
    raw <- fixture$raw
    if (kind == "source_id") raw$nomem_encr[2] <- raw$nomem_encr[1]
    if (kind == "source_metadata") attr(raw$ci23p338, "label") <- "changed source construct"
    if (kind == "source_values") raw$ci23p338[1] <- 123
    testthat::local_mocked_bindings(.liss_nonresponse_read_source = function(path) raw,
      .package = "lissr")
    expect_error(.nrc_merge(fixture), class = "liss_source_nonresponse_blocked", info = kind)
  })
  expect_identical(fixture$raw, fixture$saved$raw)
  expect_length(list.files(fixture$directory), 1)
})

test_that("late processing cannot mutate either field receipt or reintroduce either code", {
  fixture <- .nrc_fixture()
  sanitize <- lissr:::sanitize_spss_names
  for (suffix in c("337", "338")) {
    for (kind in c("metadata", "ordinary", "id", "wave", "missing", "alias", "N01", "N02")) local({
      target <- paste0("s", suffix)
      selected_kind <- kind
      testthat::local_mocked_bindings(sanitize_spss_names = function(data) {
        data <- sanitize(data)
        original <- as.numeric(fixture$raw[[paste0("ci23p", suffix)]])
        matched <- which(is.finite(original) & original %in% c(-9999999999, -9999999998))
        if (selected_kind == "metadata") attr(data[[target]], "label") <- "changed construct"
        if (selected_kind == "ordinary") data[[target]][which(original == 10000)] <- 10001
        if (selected_kind == "id") data$nomem_encr[1] <- 99
        if (selected_kind == "wave") data$wave_id[1] <- "ci22o"
        if (selected_kind == "missing") data[[target]] <- NULL
        if (selected_kind == "alias") names(data)[names(data) == target] <- paste0("ci_", target)
        if (selected_kind == "N01") data[[target]][matched[1]] <- -9999999999
        if (selected_kind == "N02") data[[target]][matched[1]] <- -9999999998
        data
      }, .package = "lissr")
      expect_error(.nrc_merge(fixture), class = "liss_source_nonresponse_blocked",
        info = paste(suffix, selected_kind))
      expect_length(list.files(fixture$directory), 1)
    })
  }
})

test_that("zero matches pass per field while a wholly absent selected wave stays unevaluated", {
  fixture <- .nrc_fixture()
  for (zero in list("337", "338", c("337", "338"))) local({
    raw <- fixture$raw
    for (suffix in zero) {
      column <- paste0("ci23p", suffix)
      matched <- is.finite(as.numeric(raw[[column]])) &
        as.numeric(raw[[column]]) %in% c(-9999999999, -9999999998)
      raw[[column]][matched] <- 19
    }
    testthat::local_mocked_bindings(read_wave_file = function(path, ...) raw,
      .liss_nonresponse_read_source = function(path) raw, .package = "lissr")
    result <- .nrc_merge(fixture)
    expect_identical(result$source_nonresponse_readiness$status, "passed")
    expect_equal(result$source_nonresponse_readiness$classified_cells, (2 - length(zero)) * 4)
    expect_equal(result$source_nonresponse_readiness$evaluated_cells, 24)
    for (suffix in zero) {
      field <- result$source_nonresponse$fields[[paste0("CI23P", suffix, "_FIELD")]]
      expect_identical(field$readiness$status, "passed")
      expect_equal(field$readiness$classified_cells, 0)
      expect_equal(nrow(field$ledger), 0)
    }
  })
  recipe <- .nrc_recipe()
  full <- lissr::liss_recipe("ci")
  recipe$wave_index <- Filter(function(wave) wave$id == "ci22o", full$wave_index)
  recipe$meta$covered_waves <- list("ci22o")
  raw <- fixture$raw
  names(raw) <- sub("^ci23p", "ci22o", names(raw))
  haven::write_sav(raw, file.path(fixture$directory, "ci22o_EN_1.0p.sav"))
  testthat::local_mocked_bindings(read_wave_file = function(path, ...) raw, .package = "lissr")
  result <- .nrc_merge(fixture, recipe)
  expect_null(result[["source_nonresponse"]])
  expect_null(attr(result$data, "liss_source_nonresponse", exact = TRUE))
  expect_identical(result$source_nonresponse_readiness$status, "not_evaluated")
  expect_identical(names(result$source_nonresponse_readiness$fields),
    c("CI23P337_FIELD", "CI23P338_FIELD"))
  expect_equal(result$source_nonresponse_readiness$evaluated_cells, 0)
  expect_equal(result$source_nonresponse_readiness$classified_cells, 0)
  expect_true(all(vapply(result$source_nonresponse_readiness$fields, function(field)
    identical(field$status, "not_evaluated"), logical(1))))
})

test_that("collection RDS subsets and reorderings reseal both leaves without changing the saved input", {
  fixture <- .nrc_fixture()
  result <- .nrc_merge(fixture)
  path <- file.path(fixture$directory, "synthetic_joint.rds")
  saveRDS(result, path, version = 2L)
  restored <- readRDS(path)
  expect_identical(restored, result)
  for (rows in list(rev(seq_len(nrow(result$data))), c(12L, 1L, 11L, 2L), c(8L, 9L), integer())) {
    subset <- restored
    subset$data <- dplyr::slice(subset$data, rows)
    checked <- .nrc_validate(subset)
    state <- checked$state
    expect_identical(names(state$fields), names(restored$source_nonresponse$fields))
    expected_count <- 0L
    for (suffix in c("337", "338")) {
      field <- state$fields[[paste0("CI23P", suffix, "_FIELD")]]
      values <- as.numeric(fixture$raw[[paste0("ci23p", suffix)]])[rows]
      matched <- is.finite(values) & values %in% c(-9999999999, -9999999998)
      expected_count <- expected_count + sum(matched)
      expect_equal(nrow(field$coverage), length(rows))
      expect_equal(nrow(field$input_rows), length(rows))
      expect_equal(nrow(field$ledger), sum(matched))
      expect_equal(field$readiness$classified_cells, sum(matched))
      expect_identical(field$digest, lissr:::.liss_nonresponse_digest(field))
      expect_identical(field$source, state$source)
    }
    expect_equal(state$readiness$evaluated_cells, 2 * length(rows))
    expect_equal(state$readiness$classified_cells, expected_count)
    expect_identical(state$digest, lissr:::.liss_nonresponse_digest(state))
    expect_identical(restored, result)
  }
  subset <- result
  subset$data <- dplyr::slice(subset$data, c(1L, 2L, 9L))
  attr(subset$data$s338, "label") <- NULL
  expect_error(.nrc_validate(subset), "target metadata changed",
    class = "liss_source_nonresponse_blocked")
})

test_that("collection validation rejects incomplete containers and tampered nested receipts", {
  fixture <- .nrc_fixture()
  result <- .nrc_merge(fixture)
  changes <- list(
    function(x) { x$schema_version <- "4.0.0"; x },
    function(x) { x$kind <- "scalar"; x },
    function(x) { x$fields <- x$fields[1]; x },
    function(x) { names(x$fields) <- rev(names(x$fields)); x },
    function(x) { x$fields[[2]]$field$field_id <- x$fields[[1]]$field$field_id; x },
    function(x) { x$source$size_bytes <- 0; x },
    function(x) { x$fields[[2]]$source$size_bytes <- 0; x },
    function(x) { x$fields[[2]]$normalized <- FALSE; x },
    function(x) { x$normalized <- FALSE; x },
    function(x) { x$readiness$classified_cells <- 0L; x },
    function(x) { x$fields[[2]]$ledger$no_numeric_proposal[1] <- FALSE; x },
    function(x) { x$fields[[2]]$coverage$original_value[1] <- 123; x },
    function(x) { x$fields[[2]]$input_rows$value[1] <- 123; x },
    function(x) { x$fields[[2]]$target_metadata$label <- "changed"; x },
    function(x) { x$fields[[2]]$field <- NULL; x },
    function(x) { x$fields[[2]]$source <- NULL; x },
    function(x) { x$fields[[2]]$coverage <- NULL; x },
    function(x) { x$fields[[2]]$ledger <- NULL; x },
    function(x) { x$fields[[2]]$input_rows <- NULL; x },
    function(x) { x$fields[[2]]$metadata <- NULL; x },
    function(x) { x$fields[[2]]$target_metadata <- NULL; x },
    function(x) { x$fields[[2]]$schema_version <- "4.0.0"; x })
  for (index in seq_along(changes)) {
    bad <- result
    bad$source_nonresponse <- changes[[index]](bad$source_nonresponse)
    attr(bad$data, "liss_source_nonresponse") <- bad$source_nonresponse
    expect_error(.nrc_validate(bad), class = "liss_source_nonresponse_blocked",
      info = paste("unsealed", index))
    bad$source_nonresponse <- lissr:::.liss_nonresponse_seal(bad$source_nonresponse)
    attr(bad$data, "liss_source_nonresponse") <- bad$source_nonresponse
    expect_error(.nrc_validate(bad), class = "liss_source_nonresponse_blocked",
      info = paste("outer resealed", index))
  }
  expect_identical(lissr:::source_nonresponse_validate(result,
    result$source_nonresponse)$state, result$source_nonresponse)
  for (name in c("readiness_only", "class_only", "missing", "contradictory")) {
    form <- .nrc_marker_forms(result)[[name]]
    expect_error(lissr:::source_nonresponse_validate(form, result$source_nonresponse),
      class = "liss_source_nonresponse_blocked", info = name)
  }
  bad <- result
  bad$source_nonresponse <- bad$source_nonresponse$fields
  expect_error(.nrc_validate(bad), class = "liss_source_nonresponse_blocked")
  for (suffix in c("337", "338")) for (code in c(-9999999999, -9999999998)) {
    bad <- result
    original <- as.numeric(fixture$raw[[paste0("ci23p", suffix)]])
    row <- which(is.finite(original) & original %in% c(-9999999999, -9999999998))[1]
    bad$data[[paste0("s", suffix)]][row] <- code
    expect_error(.nrc_validate(bad), class = "liss_source_nonresponse_blocked")
  }
})

test_that("collection lineage cannot be replaced through either scalar binding direction", {
  fixture <- .nrc_fixture()
  result <- .nrc_merge(fixture)
  forms <- .nrc_marker_forms(result)
  reads <- FALSE
  testthat::local_mocked_bindings(.liss_nonresponse_read_source = function(path) {
    reads <<- TRUE
    stop("lineage denial must precede source reads")
  }, .package = "lissr")
  for (name in names(forms)) for (suffix in c("339", "337", "338")) {
    recipe <- if (suffix == "339") NULL else fixture$saved$fields[[suffix]]$recipe
    expect_error(lissr::liss_classify_source_nonresponse(forms[[name]], fixture$source,
      paste0("s", suffix), wave_col = "wave_id", recipe = recipe),
      class = "liss_source_nonresponse_blocked", info = paste(name, suffix))
    expect_false(reads)
  }
  plan <- lissr:::.liss_nonresponse_plan(.nrc_recipe())
  old <- readRDS(testthat::test_path("fixtures", "source_nonresponse_v1_synthetic.rds"))
  scalars <- c(list(old$inputs$raw, old$inputs$canonical, old$inputs$normalized),
    lapply(c("337", "338"), function(suffix) fixture$saved$fields[[suffix]]$inputs$raw))
  for (scalar in scalars) {
    frame <- scalar$data
    attr(frame, "liss_source_nonresponse") <- scalar$source_nonresponse
    for (input in list(scalar, frame)) {
      expect_error(lissr:::.liss_nonresponse_preflight(input, fixture$source, plan),
        class = "liss_source_nonresponse_blocked")
      expect_false(reads)
    }
  }
})

test_that("the household cleaner rejects collections before target rules P01 or preparation", {
  fixture <- .nrc_fixture()
  result <- .nrc_merge(fixture)
  configurations <- list(list(), list(disable = c("P01", "P03", "P06")),
    list(enable_only = "D06"), list(disable = .nrc_rules()),
    list(params = list(P04 = list(code_share = 0.8))))
  reached <- character()
  testthat::local_mocked_bindings(
    attach_background_frame = function(...) { reached <<- c(reached, "background"); stop("background reached") },
    liss_cleaning_ruleset = function(...) { reached <<- c(reached, "ruleset"); stop("ruleset reached") },
    .package = "lissr")
  for (name in names(.nrc_marker_forms(result))) {
    form <- .nrc_marker_forms(result)[[name]]
    for (mode in c("flag", "correct", "na_only")) for (configuration in configurations) {
      args <- c(list(data = form, mode = mode, background = result$data,
        variables = list(target = "absent_target"), verbose = FALSE), configuration)
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
})

test_that("joint guards retain explicit no-export no-panel and no-report boundaries", {
  fixture <- .nrc_fixture()
  result <- .nrc_merge(fixture)
  output <- file.path(fixture$directory, "blocked_joint_export")
  expect_error(lissr::merge_liss_module(.nrc_recipe(), fixture$directory, output,
    source_nonresponse = TRUE), "output_dir = NULL")
  recipe <- .nrc_recipe()
  recipe$global$labelled_policy <- "preserve"
  expect_error(lissr::merge_liss_module(recipe, fixture$directory,
    output_dir = NULL, source_nonresponse = TRUE),
    "labelled_policy = to_numeric")
  for (form in .nrc_marker_forms(result))
    expect_error(lissr::merge_liss_panel(list(ci = form)), "source_nonresponse.*lineage")
  expect_error(lissr::liss_cleaning_report(result, output), "guarded.*report export",
    class = "liss_source_nonresponse_blocked")
  report_like <- result
  class(report_like) <- "liss_clean_result"
  expect_error(lissr::liss_cleaning_report(report_like, output), "guarded.*report export")
  expect_false(file.exists(output))
})
