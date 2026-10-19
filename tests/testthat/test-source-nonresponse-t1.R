# invented rows exercise scalar tax contracts; every source read is mocked
.nrt_fixture <- function(.local_envir = parent.frame()) {
  directory <- withr::local_tempdir("scalar_tax_", .local_envir = .local_envir)
  values <- c(-9999999999, -9999999998, -9999999999, -9999999998,
              -9999999999.5, -9999999998.5, -7, 0, 10000, NA, Inf, -Inf)
  raw <- data.frame(nomem_encr = as.numeric(seq_along(values)),
                    nohouse_encr = as.numeric(seq_along(values)), wavenr = 17)
  raw$ci23p337 <- haven::labelled_spss(values,
    labels = c("source label one" = 9999999, "source label two" = 9999998),
    na_values = -9999999999, label = "Respondent taxable income in 2022")
  raw$ci23p338 <- haven::labelled_spss(rev(values),
    labels = c("source label one" = 9999999, "source label two" = 9999998),
    na_values = -9999999999, label = "Respondent estimated taxable income in 2022")
  raw$ci23p339 <- haven::labelled_spss(c(-9999999999, rep(17000, length(values) - 1L)),
    label = "Household net income in 2022")
  raw$ci23p226 <- rep(2, length(values))
  raw$ci23p229 <- rep(2, length(values))
  source <- file.path(directory, "ci23p_EN_1.0p.sav")
  haven::write_sav(raw, source)
  receipt <- list(filename = basename(source),
    sha256 = "2e4578cafb649cf4bb3e89d7bc27ea50f8ea1f42038e2ba957a055eff4871862",
    size_bytes = as.numeric(file.info(source)$size))
  testthat::local_mocked_bindings(
    .liss_nonresponse_source_state = function(path) receipt,
    .liss_nonresponse_read_source = function(path) raw,
    read_wave_file = function(path, ...) raw,
    .package = "lissr", .env = .local_envir)
  list(raw = raw, source = source, receipt = receipt, directory = directory)
}

.nrt_recipe <- function(suffix = "337") {
  recipe <- lissr::liss_recipe("ci")
  fields <- lissr:::.liss_nonresponse_registry()$fields
  selected <- which(vapply(fields, function(field)
    identical(field$raw_column, paste0("ci23p", suffix)), logical(1)))
  stopifnot(length(selected) == 1L)
  recipe$source_nonresponse <- list(schema_version = "2.0.0", field = fields[[selected]])
  recipe$wave_index <- Filter(function(wave) wave$id == "ci23p", recipe$wave_index)
  recipe$meta$covered_waves <- list("ci23p")
  recipe$global$expected_presence <- NULL
  for (section in c("variable_rules", "harmonization_rules", "boundary_rules",
                    "derived_variables", "validation_checks"))
    recipe[[section]] <- list()
  recipe
}

.nrt_data <- function(fixture, suffix = "337", target = paste0("s", suffix)) {
  data <- fixture$raw[c("nomem_encr", "nohouse_encr", "wavenr")]
  data$wave_id <- rep("ci23p", nrow(data))
  data$wave_year <- rep(2023, nrow(data))
  data[[target]] <- fixture$raw[[paste0("ci23p", suffix)]]
  data
}

.nrt_bind <- function(fixture, suffix = "337", data = .nrt_data(fixture, suffix),
                      target = paste0("s", suffix), wave_col = "wave_id",
                      recipe = .nrt_recipe(suffix)) {
  lissr::liss_classify_source_nonresponse(data, fixture$source, target,
    wave_col = wave_col, recipe = recipe)
}

.nrt_validate <- function(bound) {
  state <- bound$source_nonresponse
  lissr:::source_nonresponse_validate(bound$data, state, state$target, state$wave_col)
}

.nrt_merge <- function(fixture, suffix = "337", recipe = .nrt_recipe(suffix), ...) {
  suppressWarnings(suppressMessages(lissr::merge_liss_module(recipe,
    fixture$directory, output_dir = NULL, source_nonresponse = TRUE, ...)))
}

.nrt_rule_ids <- function() {
  ruleset <- lissr::liss_cleaning_ruleset()
  unlist(lapply(c("preparation_rules", "detection_rules", "correction_rules",
                  "finalization_rules"), function(section)
    vapply(ruleset[[section]], function(rule) rule$rule_id, character(1))),
    use.names = FALSE)
}

test_that("explicit v2 fields have exactly two reviewed finite scalar cases", {
  registry <- lissr:::.liss_nonresponse_registry()
  expect_identical(lissr::liss_recipe("ci")$source_nonresponse$schema_version, "1.0.0")
  expect_identical(lissr:::.liss_nonresponse_rule()$rule_id, "CI23P339_N01")
  for (suffix in c("337", "338")) {
    field <- .nrt_recipe(suffix)$source_nonresponse$field
    expect_identical(lissr:::.liss_nonresponse_rule(.nrt_recipe(suffix)), field)
    expect_identical(field$field_id, paste0("CI23P", suffix, "_FIELD"))
    expect_identical(field$field_version, "1.0.0")
    expect_identical(field$targets, c(paste0("ci23p", suffix),
      paste0("s", suffix), paste0("ci_s", suffix)))
    expect_identical(field$source_release, "1.0p")
    expect_identical(field$metadata_policy, "retain_original_attributes")
    expect_null(field$routing)
    expect_identical(field$consumers, list())
    expect_length(field$cases, 2)
    expect_identical(vapply(field$cases, `[[`, character(1), "rule_id"),
      paste0("CI23P", suffix, c("_N01", "_N02")))
    expect_equal(vapply(field$cases, `[[`, numeric(1), "code"),
      c(-9999999999, -9999999998))
    expect_true(all(vapply(field$cases, function(case)
      identical(case$answer_status, "nonresponse_unknown") &&
        identical(case$response_validity, "source_nonresponse_encoding") &&
        identical(case$analysis_disposition, "missing_amount") &&
        isTRUE(case$no_numeric_proposal), logical(1))))
  }
  expect_identical(registry$fields[[1]]$construct, "respondent_taxable_income")
  expect_identical(registry$fields[[2]]$construct,
    "respondent_estimated_taxable_income")
})

test_that("standalone tax classification preserves values and per-case missing evidence", {
  fixture <- .nrt_fixture()
  for (suffix in c("337", "338")) {
    raw_column <- paste0("ci23p", suffix)
    bound <- .nrt_bind(fixture, suffix, fixture$raw, raw_column, NULL)
    state <- bound$source_nonresponse
    expected <- as.numeric(fixture$raw[[raw_column]])
    rows <- which(is.finite(expected) & expected %in% c(-9999999999, -9999999998))
    expect_identical(bound$data, fixture$raw)
    expect_s3_class(bound, "liss_source_nonresponse_result")
    expect_identical(state$schema_version, "2.0.0")
    expect_null(state$rule)
    expect_identical(state$field, .nrt_recipe(suffix)$source_nonresponse$field)
    expect_false(state$normalized)
    expect_equal(nrow(state$coverage), nrow(fixture$raw))
    expect_equal(nrow(state$input_rows), nrow(fixture$raw))
    expect_equal(nrow(state$ledger), 4)
    expect_false(anyDuplicated(state$ledger$source_key) > 0L)
    expect_identical(state$ledger$source_key, paste(fixture$receipt$sha256,
      rows, raw_column, sep = ":"))
    expect_equal(state$ledger$original_value, expected[rows])
    expect_equal(state$ledger$current_value_before, expected[rows])
    expect_true(all(is.na(state$ledger$analysis_value_after)))
    expect_identical(state$ledger$source_declared_missing,
      expected[rows] == -9999999999)
    expect_true(all(!state$ledger$system_missing_before))
    expect_identical(state$ledger$rule_id,
      paste0("CI23P", suffix, ifelse(expected[rows] == -9999999999, "_N01", "_N02")))
    expect_identical(state$ledger$rule_version, rep("1.0.0", 4))
    expect_identical(state$ledger$raw_storage_type, rep("double", 4))
    expect_identical(state$metadata$attributes, attributes(fixture$raw[[raw_column]]))
    expect_identical(state$target_metadata, attributes(fixture$raw[[raw_column]]))
    expect_identical(state$ledger$candidate_answer_status, rep(NA_character_, 4))
    expect_identical(state$ledger$subtype_evidence, rep("unresolved", 4))
    expect_true(all(state$ledger$no_numeric_proposal))
    expect_identical(state$readiness$status, "passed")
    expect_equal(state$readiness$evaluated_cells, 12)
    expect_equal(state$readiness$classified_cells, 4)
    expect_equal(state$readiness$unresolved_cells, 0)
    expect_identical(state$readiness$case_counts,
      stats::setNames(c(2L, 2L), paste0("CI23P", suffix, c("_N01", "_N02"))))
    checked <- .nrt_validate(bound)
    expect_identical(which(checked$lock), rows)
    expect_identical(checked$observed, expected)
  }
})

test_that("tax selection requires explicit field ownership across all allowed aliases", {
  fixture <- .nrt_fixture()
  for (suffix in c("337", "338")) {
    for (target in c(paste0("ci23p", suffix), paste0("s", suffix),
                     paste0("ci_s", suffix))) {
      data <- .nrt_data(fixture, suffix, target)
      bound <- .nrt_bind(fixture, suffix, data, target)
      expect_equal(sum(.nrt_validate(bound)$lock), 4)
      expect_identical(bound$source_nonresponse$target, target)
      expect_identical(bound$source_nonresponse$ledger$current_column, rep(target, 4))
      if (identical(target, paste0("ci23p", suffix)))
        expect_no_error(.nrt_bind(fixture, suffix, data, target, NULL))
      else expect_error(.nrt_bind(fixture, suffix, data, target, NULL),
        class = "liss_source_nonresponse_blocked")
    }
    for (target in c("s339", "ci23p339", "nethh", "ci00a339", "ci_s339",
                     paste0("s", if (suffix == "337") "338" else "337"), "other"))
      expect_error(.nrt_bind(fixture, suffix, .nrt_data(fixture, suffix, target), target),
        class = "liss_source_nonresponse_blocked", info = paste(suffix, target))
    expect_error(.nrt_bind(fixture, suffix, recipe = NULL),
      class = "liss_source_nonresponse_blocked")
  }
})

test_that("tax recipe mirrors cannot grant authority by changing identities or policies", {
  fixture <- .nrt_fixture()
  changes <- list(field_id = "CI23P339_FIELD", field_version = "2.0.0",
    module = "ca", wave = "ci22o", language = "NL", source_release = "1.1p",
    source_filename = "ci23p_EN_1.1p.sav", source_sha256 = strrep("0", 64),
    raw_column = "ci23p339", canonical_column = "s339", construct = "household_net_income",
    unit = "USD", measurement_period = 2023, targets = c("s337", "nethh"),
    metadata_policy = "drop_attributes", codebook_filename = "other.pdf",
    codebook_sha256 = strrep("0", 64), evidence_ref = "other:337",
    routing = list(column = "ci23p229"),
    readiness = list(role = "household_income_amount", scope = "all data"),
    consumers = list(list(name = "liss_clean_income")))
  for (suffix in c("337", "338")) {
    for (key in names(changes)) {
      recipe <- .nrt_recipe(suffix)
      recipe$source_nonresponse$field[[key]] <- changes[[key]]
      expect_error(.nrt_bind(fixture, suffix, recipe = recipe),
        class = "liss_source_nonresponse_blocked", info = paste(suffix, key))
    }
    for (change in list(list(rule_id = "OTHER_N01"), list(rule_version = "2.0.0"),
      list(code = -9999999998.5), list(code = "-9999999999"), list(code = NA_real_),
      list(code = Inf), list(answer_status = "dont_know"),
      list(response_validity = "valid_response"),
      list(analysis_disposition = "retain_amount"), list(no_numeric_proposal = FALSE),
      list(no_numeric_proposal = 1), list(reviewed = TRUE))) {
      recipe <- .nrt_recipe(suffix)
      key <- names(change)
      recipe$source_nonresponse$field$cases[[1]][[key]] <- change[[key]]
      expect_error(.nrt_bind(fixture, suffix, recipe = recipe),
        class = "liss_source_nonresponse_blocked", info = paste(suffix, key))
    }
  }
})

test_that("tax schema rejects collections, malformed types and self-approved mirrors", {
  fixture <- .nrt_fixture()
  changes <- list(
    function(x) { x$source_nonresponse$schema_version <- "3.0.0"; x },
    function(x) { x$source_nonresponse$field <- list(x$source_nonresponse$field); x },
    function(x) { x$source_nonresponse$fields <- list(x$source_nonresponse$field); x },
    function(x) { x$source_nonresponse$contracts <- list(x$source_nonresponse$field); x },
    function(x) { x$source_nonresponse$field$unit <- NULL; x },
    function(x) { x$source_nonresponse$field <- c(x$source_nonresponse$field,
      x$source_nonresponse$field["field_id"]); x },
    function(x) { x$source_nonresponse$field$cases[[3]] <- x$source_nonresponse$field$cases[[1]]; x },
    function(x) { x$source_nonresponse$field$cases[[2]]$code <- -9999999999; x },
    function(x) { x$source_nonresponse$field$cases[[2]]$rule_id <- x$source_nonresponse$field$cases[[1]]$rule_id; x },
    function(x) { x$source_nonresponse$field$cases[[1]]$answer_status <- NULL; x },
    function(x) { names(x$source_nonresponse$field$cases) <- c("first", "second"); x },
    function(x) { x$source_nonresponse$field$targets <- c("s337", "s337"); x },
    function(x) { x$source_nonresponse$field$measurement_period <- "2022"; x },
    function(x) { class(x$source_nonresponse$field) <- "caller_field"; x },
    function(x) { attr(x$source_nonresponse$field$cases[[1]]$code, "reviewed") <- TRUE; x },
    function(x) { x$source_nonresponse$reviewed <- TRUE;
      x$source_nonresponse$registry_sha256 <- as.character(openssl::sha256(
        serialize(x$source_nonresponse$field, NULL))); x },
    function(x) { x$meta$module <- "ca"; x })
  for (index in seq_along(changes))
    expect_error(.nrt_bind(fixture, recipe = changes[[index]](.nrt_recipe())),
      class = "liss_source_nonresponse_blocked", info = paste("schema case", index))
  overflow <- suppressWarnings(yaml::yaml.load("code: -9999999999"))$code
  expect_true(is.na(overflow))
  recipe <- .nrt_recipe()
  recipe$source_nonresponse$field$cases[[1]]$code <- overflow
  expect_error(.nrt_bind(fixture, recipe = recipe),
    class = "liss_source_nonresponse_blocked")
})

test_that("tax field and case mapping order is harmless and retained in its receipt", {
  fixture <- .nrt_fixture()
  for (suffix in c("337", "338")) {
    recipe <- .nrt_recipe(suffix)
    field <- recipe$source_nonresponse$field
    field <- field[rev(seq_along(field))]
    field$readiness <- field$readiness[rev(seq_along(field$readiness))]
    field$cases <- lapply(field$cases, function(case) case[rev(seq_along(case))])
    recipe$source_nonresponse$field <- field
    bound <- .nrt_bind(fixture, suffix, recipe = recipe)
    expect_identical(bound$source_nonresponse$field, field)
    expect_identical(.nrt_validate(bound)$state$field, field)
  }
})

test_that("tax registry validates field collections and rejects ambiguity", {
  registry <- lissr:::.liss_nonresponse_registry()
  expect_no_error(lissr:::.liss_nonresponse_registry_validate(registry))
  changes <- list(
    function(x) { x$fields <- list(); x },
    function(x) { x$fields[[3]] <- x$fields[[1]]; x },
    function(x) { names(x$fields) <- c("first", "second"); x },
    function(x) { x$fields[[2]]$field_id <- x$fields[[1]]$field_id; x },
    function(x) { x$fields[[1]]$field_version <- "9.0.0"; x },
    function(x) { x$fields[[1]]$extra <- TRUE; x },
    function(x) { x$fields[[1]]$codebook_sha256 <- NULL; x },
    function(x) { x$fields[[1]]$cases[[3]] <- x$fields[[1]]$cases[[1]]; x },
    function(x) { x$fields[[1]]$cases[[2]]$code <- x$fields[[1]]$cases[[1]]$code; x },
    function(x) { x$fields[[2]]$cases[[1]]$rule_id <- x$fields[[1]]$cases[[1]]$rule_id; x },
    function(x) { x$fields[[1]]$cases[[1]]$code <- Inf; x },
    function(x) { x$fields[[1]]$cases[[1]]$code <- "-9999999999"; x },
    function(x) { attr(x$fields[[1]]$cases[[1]]$code, "label") <- "code"; x },
    function(x) { x$fields[[1]]$cases[[1]]$rule_version <- NULL; x },
    function(x) { x$fields[[1]]$cases[[1]]$extra <- TRUE; x },
    function(x) { x$fields[[1]]$targets <- c("s337", "s337"); x },
    function(x) { x$fields[[1]]$consumers <- TRUE; x },
    function(x) { x$fields[[1]]$routing <- list(column = "ci23p229"); x })
  for (index in seq_along(changes))
    expect_error(lissr:::.liss_nonresponse_registry_validate(changes[[index]](registry)),
      class = "liss_source_nonresponse_blocked", info = paste("registry case", index))
})

test_that("missing or corrupt authority cannot approve a tax mirror", {
  fixture <- .nrt_fixture()
  recipe <- .nrt_recipe()
  path <- file.path(fixture$directory, "missing_registry.yml")
  testthat::local_mocked_bindings(.liss_nonresponse_registry_path = function() path,
    .package = "lissr")
  expect_error(.nrt_bind(fixture, recipe = recipe),
    class = "liss_source_nonresponse_blocked")
  writeLines(yaml::as.yaml(list(schema_version = "2.0.0", registry_version = "2.0.0",
    records = list(), fields = list(recipe$source_nonresponse$field))), path)
  expect_error(.nrt_bind(fixture, recipe = recipe),
    class = "liss_source_nonresponse_blocked")
})

test_that("tax source and complete binding identities fail closed", {
  fixture <- .nrt_fixture()
  recipe <- .nrt_recipe()
  for (key in c("filename", "sha256")) {
    receipt <- fixture$receipt
    receipt[[key]] <- if (key == "filename") "ci23p_EN_1.1p.sav" else strrep("0", 64)
    local({
      changed <- receipt
      testthat::local_mocked_bindings(.liss_nonresponse_source_state = function(path) changed,
        .package = "lissr")
      expect_error(.nrt_bind(fixture, recipe = recipe),
        class = "liss_source_nonresponse_blocked", info = key)
    })
  }
  for (kind in c("duplicate", "missing", "absent", "matched", "ordinary", "year", "wave_na")) {
    data <- .nrt_data(fixture)
    if (kind == "duplicate") data$nomem_encr[2] <- data$nomem_encr[1]
    if (kind == "missing") data$nomem_encr[2] <- NA_real_
    if (kind == "absent") data$nomem_encr[2] <- 99
    if (kind == "matched") data$s337[1] <- -9999999998
    if (kind == "ordinary") data$s337[9] <- 10001
    if (kind == "year") data$wave_id <- rep(2023, nrow(data))
    if (kind == "wave_na") data$wave_id[1] <- NA_character_
    expect_error(.nrt_bind(fixture, data = data),
      class = "liss_source_nonresponse_blocked", info = kind)
  }
})

test_that("tax rebinding rejects existing guarded lineage before source reads", {
  fixture <- .nrt_fixture()
  tax <- .nrt_bind(fixture)
  household_data <- .nrt_data(fixture, "339")
  household <- lissr::liss_classify_source_nonresponse(household_data,
    fixture$source, "s339", wave_col = "wave_id")
  claim <- list(status = "passed", field_id = "CI23P337_FIELD",
    role = "respondent_taxable_income_amount")
  inputs <- list(tax, tax$data, household, household$data,
    list(data = .nrt_data(fixture), source_nonresponse_readiness = claim),
    structure(list(data = .nrt_data(fixture)), class = "liss_source_nonresponse_result"))
  attr(inputs[[2]], "liss_source_nonresponse") <- tax$source_nonresponse
  attr(inputs[[4]], "liss_source_nonresponse") <- household$source_nonresponse
  disagreement <- tax
  attr(disagreement$data, "liss_source_nonresponse") <- household$source_nonresponse
  inputs <- c(inputs, list(disagreement))
  source_reached <- FALSE
  testthat::local_mocked_bindings(
    .liss_nonresponse_read_source = function(path) {
      source_reached <<- TRUE
      stop("lineage denial must precede a source read")
    }, .package = "lissr")
  for (index in seq_along(inputs)) {
    expect_error(.nrt_bind(fixture, data = inputs[[index]]),
      class = "liss_source_nonresponse_blocked", info = paste("lineage", index))
    expect_false(source_reached)
  }
})

test_that("reverse legacy rebinding cannot discard taxable lineage or claims", {
  fixture <- .nrt_fixture()
  tax <- .nrt_bind(fixture)
  tax$data$s339 <- fixture$raw$ci23p339
  frame <- tax$data
  attr(frame, "liss_source_nonresponse") <- tax$source_nonresponse
  claim <- list(data = tax$data, source_nonresponse_readiness = tax$source_nonresponse_readiness)
  missing <- tax
  missing$source_nonresponse <- NULL
  missing$source_nonresponse_readiness <- NULL
  inputs <- list(tax, frame, claim, missing)
  for (index in seq_along(inputs))
    expect_error(lissr::liss_classify_source_nonresponse(inputs[[index]],
      fixture$source, "s339", wave_col = "wave_id"),
      class = "liss_source_nonresponse_blocked", info = paste("reverse", index))
  legacy <- lissr::liss_classify_source_nonresponse(.nrt_data(fixture, "339"),
    fixture$source, "s339", wave_col = "wave_id")
  expect_no_error(lissr::liss_classify_source_nonresponse(legacy,
    fixture$source, "s339", wave_col = "wave_id"))
})

test_that("tax cleaners deny before target selection and background under all overrides", {
  fixture <- .nrt_fixture()
  background_reached <- FALSE
  testthat::local_mocked_bindings(attach_background_frame = function(...) {
    background_reached <<- TRUE
    stop("background must not be reached by taxable guards")
  }, .package = "lissr")
  configurations <- list(list(), list(disable = c("P01", "P03", "P06")),
    list(enable_only = "D06"), list(disable = .nrt_rule_ids()),
    list(params = list(P04 = list(code_share = 0.8))))
  for (suffix in c("337", "338")) {
    bound <- .nrt_bind(fixture, suffix)
    for (mode in c("flag", "correct", "na_only")) {
      for (configuration in configurations) {
        args <- c(list(data = bound, background = bound$data,
          mode = mode, verbose = FALSE), configuration)
        expect_error(do.call(lissr::liss_clean_income, args), "no approval.*consumer",
          class = "liss_source_nonresponse_blocked", info = paste(suffix, mode))
        expect_false(background_reached)
      }
    }
    for (target in c(paste0("ci23p", suffix), paste0("s", suffix),
      paste0("ci_s", suffix), "s339", "nethh", "ci00a339")) {
      renamed <- bound
      names(renamed$data)[names(renamed$data) == paste0("s", suffix)] <- target
      expect_error(lissr::liss_clean_income(renamed, mode = "flag", verbose = FALSE,
        variables = list(target = target)), "no approval.*consumer",
        class = "liss_source_nonresponse_blocked", info = target)
    }
  }
})

test_that("v2 RDS, reordering and metadata-preserving subsets retain scalar receipts", {
  fixture <- .nrt_fixture()
  for (suffix in c("337", "338")) {
    bound <- .nrt_bind(fixture, suffix)
    path <- file.path(fixture$directory, paste0("synthetic_", suffix, ".rds"))
    saveRDS(bound, path, version = 2L)
    restored <- readRDS(path)
    expect_identical(restored, bound)
    for (rows in list(rev(seq_len(nrow(bound$data))), c(9L, 1L, 2L), c(8L, 9L))) {
      subset <- restored
      subset$data <- subset$data[rows, , drop = FALSE]
      checked <- .nrt_validate(subset)
      expected <- as.numeric(subset$data[[paste0("s", suffix)]])
      matched <- is.finite(expected) & expected %in% c(-9999999999, -9999999998)
      expect_identical(checked$lock, matched)
      expect_equal(nrow(checked$state$coverage), length(rows))
      expect_equal(nrow(checked$state$input_rows), length(rows))
      expect_equal(nrow(checked$state$ledger), sum(matched))
      expect_equal(checked$readiness$evaluated_cells, length(rows))
      expect_equal(checked$readiness$classified_cells, sum(matched))
      expect_equal(sum(checked$readiness$case_counts), sum(matched))
      expect_identical(restored$source_nonresponse, bound$source_nonresponse)
      expect_identical(checked$state$schema_version, "2.0.0")
    }
  }
})

test_that("normalized v2 RDS subsets recover both codes without changing saved receipts", {
  fixture <- .nrt_fixture()
  for (suffix in c("337", "338")) {
    bound <- .nrt_merge(fixture, suffix)
    path <- file.path(fixture$directory, paste0("normalized_synthetic_", suffix, ".rds"))
    saveRDS(bound, path, version = 2L)
    restored <- readRDS(path)
    expect_identical(restored, bound)
    rows <- c(12L, 1L, 11L, 2L)
    subset <- restored
    subset$data <- dplyr::slice(subset$data, rows)
    checked <- .nrt_validate(subset)
    expected <- as.numeric(fixture$raw[[paste0("ci23p", suffix)]])[rows]
    expect_identical(checked$observed, expected)
    expect_equal(checked$readiness$classified_cells, 2)
    expect_identical(checked$readiness$case_counts,
      stats::setNames(c(1L, 1L), paste0("CI23P", suffix, c("_N01", "_N02"))))
    expect_true(all(is.na(as.numeric(subset$data[[paste0("s", suffix)]])[checked$lock])))
    expect_identical(restored$source_nonresponse, bound$source_nonresponse)
  }
})

test_that("external numeric subsetting that drops tax metadata remains blocked", {
  fixture <- .nrt_fixture()
  data <- .nrt_data(fixture)
  data$s337 <- as.numeric(data$s337)
  attr(data$s337, "label") <- "Respondent taxable income in 2022"
  bound <- .nrt_bind(fixture, data = data)
  expect_identical(bound$source_nonresponse$target_metadata, attributes(data$s337))
  bound$data <- bound$data[c(1L, 2L, 9L), , drop = FALSE]
  expect_null(attr(bound$data$s337, "label", exact = TRUE))
  expect_error(.nrt_validate(bound), "target metadata changed",
    class = "liss_source_nonresponse_blocked")
})

test_that("v2 validation rejects changed receipts, wave keys, values and metadata", {
  fixture <- .nrt_fixture()
  bound <- .nrt_bind(fixture)
  for (kind in c("source", "field", "ledger", "coverage", "metadata", "input_rows", "readiness")) {
    bad <- bound
    if (kind == "source") bad$source_nonresponse$source$sha256 <- strrep("0", 64)
    if (kind == "field") bad$source_nonresponse$field$cases[[2]]$code <- -9999999998.5
    if (kind == "ledger") bad$source_nonresponse$ledger$no_numeric_proposal[1] <- FALSE
    if (kind == "coverage") bad$source_nonresponse$coverage$original_value[1] <- 123
    if (kind == "metadata") bad$source_nonresponse$metadata$attributes <- NULL
    if (kind == "input_rows") bad$source_nonresponse$input_rows$value[1] <- 123
    if (kind == "readiness") bad$source_nonresponse$readiness$classified_cells <- 0L
    expect_error(.nrt_validate(bad), class = "liss_source_nonresponse_blocked", info = kind)
  }
  for (kind in c("matched", "ordinary", "id", "wave", "metadata", "duplicate", "mapping", "schema")) {
    bad <- bound
    if (kind == "matched") bad$data$s337[1] <- -9999999998
    if (kind == "ordinary") bad$data$s337[9] <- 10001
    if (kind == "id") bad$data$nomem_encr[1] <- 99
    if (kind == "wave") bad$data$wave_id[1] <- "ci22o"
    if (kind == "metadata") attr(bad$data$s337, "label") <- "different amount"
    if (kind == "duplicate") bad$data <- bad$data[c(1L, 1L), , drop = FALSE]
    if (kind == "mapping") bad$source_nonresponse$target <- "s338"
    if (kind == "schema") {
      bad$source_nonresponse$schema_version <- "3.0.0"
      bad$source_nonresponse <- lissr:::.liss_nonresponse_seal(bad$source_nonresponse)
    }
    expect_error(.nrt_validate(bad), class = "liss_source_nonresponse_blocked", info = kind)
  }
})

test_that("no-hit and absent-wave readiness certify only the selected field", {
  fixture <- .nrt_fixture()
  for (suffix in c("337", "338")) {
    data <- .nrt_data(fixture, suffix)
    values <- as.numeric(data[[paste0("s", suffix)]])
    data <- data[!(is.finite(values) & values %in% c(-9999999999, -9999999998)), , drop = FALSE]
    no_hit <- .nrt_bind(fixture, suffix, data)
    expect_identical(no_hit$source_nonresponse_readiness$status, "passed")
    expect_equal(no_hit$source_nonresponse_readiness$classified_cells, 0)
    expect_identical(no_hit$source_nonresponse_readiness$case_counts,
      stats::setNames(c(0L, 0L), paste0("CI23P", suffix, c("_N01", "_N02"))))
    data$wave_id <- "ci22o"
    absent <- .nrt_bind(fixture, suffix, data)
    expect_identical(absent$source_nonresponse_readiness$status, "not_evaluated")
    expect_equal(nrow(absent$source_nonresponse$coverage), 0)
    expect_equal(nrow(absent$source_nonresponse$ledger), 0)
    expect_equal(sum(.nrt_validate(absent)$lock), 0)
  }
})

test_that("scalar merge masks both selected cases before transformations and logs each case", {
  fixture <- .nrt_fixture()
  for (suffix in c("337", "338")) {
    recipe <- .nrt_recipe(suffix)
    recipe$harmonization_rules <- list(list(rule_id = "TAX_ORDERING_PROBE",
      action = "value_recode", description = "synthetic ordering probe",
      suffixes = list(suffix), mapping = list(`-9999999999` = 111, `-9999999998` = 222)))
    result <- .nrt_merge(fixture, suffix, recipe)
    state <- result$source_nonresponse
    expect_identical(state$schema_version, "2.0.0")
    expect_true(state$normalized)
    expect_identical(state$target, paste0("s", suffix))
    original <- as.numeric(fixture$raw[[paste0("ci23p", suffix)]])
    rows <- which(is.finite(original) & original %in% c(-9999999999, -9999999998))
    expect_true(all(is.na(as.numeric(result$data[[state$target]])[rows])))
    expect_equal(state$ledger$original_value, original[rows])
    expect_equal(sum(.nrt_validate(result)$lock), 4)
    other_suffix <- if (suffix == "337") "338" else "337"
    baseline <- suppressWarnings(suppressMessages(lissr::merge_liss_module(recipe,
      fixture$directory, output_dir = NULL, source_nonresponse = FALSE)))
    for (column in c(paste0("s", other_suffix), "s339", "s226", "s229"))
      expect_identical(result$data[[column]], baseline$data[[column]], info = column)
    logs <- Filter(function(entry) identical(entry$action,
      "source_nonresponse:missing_amount"), result$log)
    expect_length(logs, 2)
    expect_identical(vapply(logs, `[[`, character(1), "rule_id"),
      paste0("CI23P", suffix, c("_N01", "_N02")))
    expect_equal(vapply(logs, `[[`, numeric(1), "rows_affected"), c(2, 2))
    expect_equal(vapply(logs, `[[`, numeric(1), "values_changed"), c(2, 2))
    expect_identical(state, attr(result$data, "liss_source_nonresponse", exact = TRUE))
    expect_equal(length(list.files(fixture$directory)), 1)
  }
})

test_that("normalized scalar validation recovers either original code and rejects reintroduction", {
  fixture <- .nrt_fixture()
  for (suffix in c("337", "338")) {
    result <- .nrt_merge(fixture, suffix)
    state <- result$source_nonresponse
    checked <- .nrt_validate(result)
    expect_equal(checked$observed[checked$lock], state$ledger$original_value)
    for (code in c(-9999999999, -9999999998)) {
      bad <- result
      row <- which(checked$lock)[1]
      bad$data[[state$target]][row] <- code
      expect_error(lissr:::source_nonresponse_finalize(bad$data, state,
        state$target, state$wave_col, TRUE), class = "liss_source_nonresponse_blocked")
    }
  }
})

test_that("late merge processing cannot reintroduce a finite protected tax amount", {
  fixture <- .nrt_fixture()
  sanitize <- lissr:::sanitize_spss_names
  for (code in c(-9999999999, -9999999998)) {
    local({
      replacement <- code
      testthat::local_mocked_bindings(sanitize_spss_names = function(data) {
        data <- sanitize(data)
        data$s337[1] <- replacement
        data
      }, .package = "lissr")
      expect_error(.nrt_merge(fixture), class = "liss_source_nonresponse_blocked")
    })
  }
})

test_that("late merge processing cannot silently replace selected source metadata", {
  fixture <- .nrt_fixture()
  sanitize <- lissr:::sanitize_spss_names
  testthat::local_mocked_bindings(sanitize_spss_names = function(data) {
    data <- sanitize(data)
    attr(data$s337, "label") <- "Unexpected changed construct"
    data
  }, .package = "lissr")
  expect_error(.nrt_merge(fixture), class = "liss_source_nonresponse_blocked")
})

test_that("scalar finalization cannot silently substitute another approved alias", {
  fixture <- .nrt_fixture()
  for (suffix in c("337", "338")) {
    bound <- .nrt_bind(fixture, suffix)
    old_target <- paste0("s", suffix)
    new_target <- paste0("ci_s", suffix)
    renamed <- bound$data
    names(renamed)[names(renamed) == old_target] <- new_target
    expect_error(lissr:::source_nonresponse_finalize(renamed,
      bound$source_nonresponse, new_target, "wave_id", FALSE),
      class = "liss_source_nonresponse_blocked")
    expect_identical(.nrt_validate(bound)$state$target, old_target)
  }
})

test_that("unguarded frames do not imply tax ownership after every marker is stripped", {
  fixture <- .nrt_fixture()
  data <- .nrt_bind(fixture)$data
  expect_null(attr(data, "liss_source_nonresponse", exact = TRUE))
  result <- suppressWarnings(lissr::liss_clean_income(data,
    variables = list(target = "s337"), mode = "flag", verbose = FALSE,
    disable = .nrt_rule_ids()))
  expect_identical(result$source_nonresponse_readiness$status, "not_evaluated")
  expect_null(result[["source_nonresponse"]])
})

test_that("tax guards keep scalar export and panel limitations", {
  fixture <- .nrt_fixture()
  for (suffix in c("337", "338")) {
    recipe <- .nrt_recipe(suffix)
    result <- .nrt_merge(fixture, suffix, recipe)
    output <- file.path(fixture$directory, paste0("blocked_export_", suffix))
    expect_error(lissr::merge_liss_module(recipe, fixture$directory, output,
      source_nonresponse = TRUE), "output_dir = NULL")
    expect_false(file.exists(output))
    expect_error(lissr::merge_liss_panel(list(ci = result)), "source_nonresponse lineage")
    expect_error(lissr::merge_liss_panel(list(ci = result$data)), "source_nonresponse lineage")
    expect_error(lissr::liss_cleaning_report(result, output), "liss_clean_result")
    report_like <- result
    class(report_like) <- "liss_clean_result"
    expect_error(lissr::liss_cleaning_report(report_like, output), "guarded report export")
    expect_false(file.exists(output))
  }
})

test_that("absent tax source waves return no false merge carrier", {
  fixture <- .nrt_fixture()
  recipe <- .nrt_recipe()
  full_recipe <- lissr::liss_recipe("ci")
  recipe$wave_index <- Filter(function(wave) wave$id == "ci22o", full_recipe$wave_index)
  recipe$meta$covered_waves <- list("ci22o")
  source <- fixture$raw
  names(source) <- sub("^ci23p", "ci22o", names(source))
  haven::write_sav(source, file.path(fixture$directory, "ci22o_EN_1.0p.sav"))
  testthat::local_mocked_bindings(read_wave_file = function(path, ...) source,
    .package = "lissr")
  result <- .nrt_merge(fixture, recipe = recipe)
  expect_identical(result$source_nonresponse_readiness$status, "not_evaluated")
  expect_null(result[["source_nonresponse"]])
})
