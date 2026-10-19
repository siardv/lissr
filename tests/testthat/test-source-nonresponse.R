# synthetic fixtures mock only the verified-source I/O boundary; they are not
# empirical evidence about the original CI release or its measured exposure
.nr_fixture <- function(mock_source = TRUE, .local_envir = parent.frame()) {
  directory <- withr::local_tempdir("source_nonresponse_",
                                  .local_envir = .local_envir)
  raw <- data.frame(nomem_encr = as.numeric(1:8), nohouse_encr = as.numeric(1:8),
                    wavenr = 17, stringsAsFactors = FALSE)
  raw$ci23p339 <- haven::labelled_spss(
    c(-9999999999, -9999999998, -9999999998.5, -7, 0, 10000, 20000, NA),
    labels = c("source label one" = 9999999, "source label two" = 9999998),
    label = "Household net income in 2022")
  raw$ci23p337 <- rep(-9999999999, nrow(raw))
  raw$ci23p229 <- c(2, rep(NA_real_, nrow(raw) - 1L))
  source <- file.path(directory, "ci23p_EN_1.0p.sav")
  haven::write_sav(raw, source)
  if (mock_source) {
    testthat::local_mocked_bindings(
      .liss_nonresponse_source_state = function(path) list(
        filename = basename(path),
        sha256 = "2e4578cafb649cf4bb3e89d7bc27ea50f8ea1f42038e2ba957a055eff4871862",
        size_bytes = as.numeric(file.info(path)$size)),
      .liss_nonresponse_read_source = function(path) raw,
      .package = "lissr", .env = .local_envir)
  }
  list(raw = raw, source = source, directory = directory)
}

.nr_data <- function(fixture, target = "s339") {
  data <- fixture$raw[c("nomem_encr", "nohouse_encr", "wavenr")]
  data$wave_id <- rep("ci23p", nrow(data))
  data$wave_year <- rep(2023, nrow(data))
  data[[target]] <- fixture$raw$ci23p339
  data
}

.nr_bind <- function(fixture, data = .nr_data(fixture), target = "s339",
                     wave_col = "wave_id", recipe = NULL) {
  lissr::liss_classify_source_nonresponse(
    data, source = fixture$source, target = target,
    wave_col = wave_col, recipe = recipe)
}

.nr_clean <- function(bound, target = "s339", ...) {
  lissr::liss_clean_income(bound, variables = list(target = target),
                           verbose = FALSE, ...)
}

.nr_rule_ids <- function() {
  ruleset <- lissr::liss_cleaning_ruleset()
  unlist(lapply(c("preparation_rules", "detection_rules", "correction_rules",
                  "finalization_rules"), function(section)
    vapply(ruleset[[section]], function(rule) rule$rule_id, character(1))),
    use.names = FALSE)
}

.nr_recipe <- function() {
  recipe <- lissr::liss_recipe("ci")
  recipe$wave_index <- Filter(function(wave) wave$id == "ci23p", recipe$wave_index)
  recipe$meta$covered_waves <- list("ci23p")
  recipe$global$expected_presence <- NULL
  for (section in c("variable_rules", "harmonization_rules", "boundary_rules",
                    "derived_variables", "validation_checks"))
    recipe[[section]] <- list()
  recipe
}

test_that("exact source classification preserves raw values and typed audit", {
  fixture <- .nr_fixture()
  bound <- .nr_bind(fixture, data = fixture$raw, target = "ci23p339",
                    wave_col = NULL)
  expect_identical(bound$data, fixture$raw)
  expect_s3_class(bound, "liss_source_nonresponse_result")
  state <- bound$source_nonresponse
  expect_identical(state$metadata$attributes, attributes(fixture$raw$ci23p339))
  expect_identical(state$readiness$status, "passed")
  expect_equal(state$readiness$evaluated_cells, 8)
  expect_equal(state$readiness$classified_cells, 1)
  expect_equal(state$ledger$original_value, -9999999999)
  expect_identical(state$ledger$raw_column, "ci23p339")
  expect_identical(state$ledger$response_validity, "source_nonresponse_encoding")
  expect_identical(state$ledger$answer_status, "nonresponse_unknown")
  expect_identical(state$ledger$analysis_disposition, "missing_amount")
  expect_equal(state$ledger$current_value_before, -9999999999)
  expect_true(is.na(state$ledger$analysis_value_after))
  expect_identical(state$ledger$source_declared_missing, FALSE)
  expect_identical(state$ledger$system_missing_before, FALSE)
  expect_identical(state$ledger$raw_storage_type, "double")
  expect_true(all(state$ledger$no_numeric_proposal))
  expect_identical(state$ledger$candidate_answer_status, "dont_know")
  expect_identical(state$ledger$subtype_evidence, "routing_supported")
  expect_equal(state$ledger$source_row_index, 1)
  expect_false(anyDuplicated(state$ledger$source_key) > 0L)
  checked <- lissr:::source_nonresponse_validate(bound$data, state, "ci23p339")
  expect_identical(which(checked$lock), 1L)
  expect_identical(as.numeric(bound$data$ci23p337), rep(-9999999999, 8))
})

test_that("raw and explicitly rebound canonical targets share source decisions", {
  fixture <- .nr_fixture()
  for (target in c("ci23p339", "s339", "ci_s339", "nethh", "ci00a339")) {
    bound <- .nr_bind(fixture, data = .nr_data(fixture, target), target = target)
    checked <- lissr:::source_nonresponse_validate(
      bound$data, bound$source_nonresponse, target, "wave_id")
    expect_identical(which(checked$lock), 1L)
    expect_equal(checked$state$ledger$original_value, -9999999999)
    expect_identical(checked$state$ledger$current_column, target)
    result <- .nr_clean(bound, target = target, mode = "flag")
    expect_true(is.na(result$data[[paste0(target, "_proposed")]][1]))
    expect_equal(as.numeric(result$data[[target]])[1], -9999999999)
  }
  expect_error(.nr_bind(fixture, wave_col = NULL), "explicit.*wave_col")
  years <- .nr_data(fixture)
  years$wave_id <- rep(2023, nrow(years))
  expect_error(.nr_bind(fixture, data = years), "exact wave identifiers")
  no_scope <- .nr_data(fixture)
  no_scope$wave_id <- rep("ci22o", nrow(no_scope))
  absent <- .nr_bind(fixture, data = no_scope)
  expect_identical(absent$source_nonresponse$readiness$status, "not_evaluated")
  expect_equal(nrow(absent$source_nonresponse$ledger), 0)
})

test_that("coincident values in another construct cannot prove household binding", {
  fixture <- .nr_fixture()
  for (target in c("ci23p337", "s337", "s338", "ca_s339", "other_income")) {
    data <- .nr_data(fixture, target)[1, , drop = FALSE]
    expect_error(.nr_bind(fixture, data = data, target = target),
                 "outside the reviewed household-income names",
                 class = "liss_source_nonresponse_blocked")
  }
})

test_that("source identity and contract scope fail closed", {
  fixture <- .nr_fixture(mock_source = FALSE)
  expect_error(.nr_bind(fixture), "filename or SHA256")
  blocked <- tryCatch(.nr_bind(fixture), error = identity)
  expect_s3_class(blocked, "liss_source_nonresponse_blocked")
  expect_identical(blocked$source_nonresponse_readiness$status, "blocked")
  wrong_source <- file.path(fixture$directory, "ci23p_EN_1.1p.sav")
  expect_true(file.copy(fixture$source, wrong_source))
  expect_error(lissr::liss_classify_source_nonresponse(
    .nr_data(fixture), wrong_source, "s339", "wave_id"), "filename or SHA256")
  for (change in list(list(code = -9999999998), list(raw_column = "ci23p337"),
                     list(language = "NL"), list(no_numeric_proposal = FALSE),
                     list(source_sha256 = paste(rep("0", 64), collapse = "")))) {
    recipe <- .nr_recipe()
    key <- names(change)
    recipe$source_nonresponse$contracts[[1]][[key]] <- change[[key]]
    expect_error(.nr_bind(fixture, recipe = recipe), "invalid source nonresponse contract")
  }
})

test_that("binding rejects lost keys and changed values without guessing origins", {
  fixture <- .nr_fixture()
  for (kind in c("duplicate", "missing", "absent", "value", "ordinary_value")) {
    data <- .nr_data(fixture)
    if (kind == "duplicate") data$nomem_encr[2] <- data$nomem_encr[1]
    if (kind == "missing") data$nomem_encr[2] <- NA_real_
    if (kind == "absent") data$nomem_encr[2] <- 900
    if (kind == "value") data$s339[1] <- abs(data$s339[1])
    if (kind == "ordinary_value") data$s339[6] <- 10001
    expect_error(.nr_bind(fixture, data = data), "binding blocked")
  }
  ambiguous <- .nr_data(fixture, "nethh")
  ambiguous$ci00a339 <- ambiguous$nethh
  bound <- .nr_bind(fixture, data = ambiguous, target = "nethh")
  expect_error(lissr::liss_clean_income(bound, verbose = FALSE), "explicit target")
})

test_that("source locks survive all cleaner modes and rule overrides", {
  fixture <- .nr_fixture()
  bound <- .nr_bind(fixture)
  configurations <- list(list(), list(disable = c("P03", "P06")),
                         list(enable_only = "P03"), list(enable_only = "D06"),
                         list(disable = .nr_rule_ids()))
  for (mode in c("flag", "correct", "na_only")) {
    for (configuration in configurations) {
      result <- do.call(.nr_clean, c(list(bound = bound, mode = mode), configuration))
      analysis <- if (mode == "flag") result$data$s339_proposed else result$data$s339
      expect_true(is.na(analysis[1]))
      expect_equal(result$data$s339_observed[1], -9999999999)
      if (mode == "flag") expect_identical(result$data$s339, bound$data$s339)
      expect_false(any(result$decisions$row == 1 & is.finite(result$decisions$corrected)))
      expect_identical(result$source_nonresponse_readiness$status, "passed")
      expect_equal(result$summary$n_source_nonresponse, 1)
    }
  }
  untouched <- .nr_clean(bound, disable = .nr_rule_ids())
  expect_identical(as.numeric(untouched$data$s339[-1]), as.numeric(bound$data$s339[-1]))
})

test_that("exclusion removes the encoded value from shared numerical inputs", {
  fixture <- .nr_fixture()
  bound <- .nr_bind(fixture)
  reference <- bound$data
  reference$s339[1] <- NA_real_
  for (mode in c("flag", "correct", "na_only")) {
    guarded <- .nr_clean(bound, mode = mode)
    baseline <- .nr_clean(reference, mode = mode)
    analysis_column <- if (mode == "flag") "s339_proposed" else "s339"
    expect_identical(guarded$data[[analysis_column]],
                     baseline$data[[analysis_column]])
    expect_identical(guarded$decisions, baseline$decisions)
    expect_identical(guarded$summary$distribution_before,
                     baseline$summary$distribution_before)
    expect_identical(guarded$summary$distribution_after,
                     baseline$summary$distribution_after)
    expect_identical(baseline$source_nonresponse_readiness$status, "not_evaluated")
  }
  no_hit <- .nr_bind(fixture, data = bound$data[-1, , drop = FALSE])
  no_hit_guarded <- .nr_clean(no_hit, mode = "flag")
  no_hit_baseline <- .nr_clean(no_hit$data, mode = "flag")
  expect_identical(no_hit_guarded$data, no_hit_baseline$data)
  expect_identical(no_hit_guarded$decisions, no_hit_baseline$decisions)
  expect_equal(no_hit_guarded$summary$n_source_nonresponse, 0)
})

test_that("keyed subsets, repeats and RDS preserve sealed classifications", {
  fixture <- .nr_fixture()
  bound <- .nr_bind(fixture)
  path <- file.path(fixture$directory, "source_nonresponse.rds")
  saveRDS(bound, path)
  restored <- readRDS(path)
  expect_identical(restored, bound)
  subset <- restored
  subset$data <- subset$data[c(6, 1, 4), , drop = FALSE]
  result <- .nr_clean(subset, mode = "flag")
  expect_equal(result$data$s339_observed, c(10000, -9999999999, -7))
  expect_true(is.na(result$data$s339_proposed[2]))
  expect_equal(result$source_nonresponse$ledger$source_row_index, 1)
  expect_equal(nrow(result$source_nonresponse$ledger), 1)
  rebound <- .nr_bind(fixture, data = bound$data)
  expect_identical(rebound$source_nonresponse$ledger, bound$source_nonresponse$ledger)
  expect_error(.nr_clean(result), "cleaned.*already")
})

test_that("base subsets that drop numeric target metadata fail closed", {
  fixture <- .nr_fixture()
  data <- .nr_data(fixture)
  data$s339 <- as.numeric(data$s339)
  attr(data$s339, "label") <- "Household net income in 2022"
  bound <- .nr_bind(fixture, data = data)
  expect_identical(bound$source_nonresponse$target_metadata,
                   attributes(data$s339))
  expect_no_error(.nr_clean(bound, mode = "flag"))
  for (rows in list(c(6L, 1L, 4L), rev(seq_len(nrow(data))))) {
    subset <- bound
    subset$data <- subset$data[rows, , drop = FALSE]
    expect_null(attr(subset$data$s339, "label", exact = TRUE))
    expect_error(.nr_clean(subset, mode = "flag"), "target metadata changed",
                 class = "liss_source_nonresponse_blocked")
  }
})

test_that("a source-bound household amount cannot also be a category code", {
  fixture <- .nr_fixture()
  bound <- .nr_bind(fixture)
  configurations <- list(
    list(), list(disable = "P04"), list(disable = .nr_rule_ids()),
    list(params = list(P04 = list(code_share = 0.8))),
    list(enable_only = "D06", params = list(P04 = list(code_share = 0.8))))
  for (mode in c("flag", "correct", "na_only")) {
    for (configuration in configurations) {
      args <- c(list(data = bound, mode = mode, verbose = FALSE,
                     variables = list(target = "s339", category_code = "s339")),
                configuration)
      expect_error(do.call(lissr::liss_clean_income, args), "category-code",
                   class = "liss_source_nonresponse_blocked")
    }
    expect_error(lissr::liss_clean_income(
      bound, mode = mode, verbose = FALSE,
      variables = list(target = "s339", category_code = c("absent_code", "s339"))),
      "category-code", class = "liss_source_nonresponse_blocked")
  }
  background_reached <- FALSE
  testthat::local_mocked_bindings(
    attach_background_frame = function(...) {
      background_reached <<- TRUE
      stop("background attachment must not be reached")
    }, .package = "lissr")
  expect_error(lissr::liss_clean_income(
    bound, background = bound$data, mode = "flag", verbose = FALSE,
    variables = list(target = "s339", category_code = "s339")),
    "category-code", class = "liss_source_nonresponse_blocked")
  expect_false(background_reached)
  legacy <- bound$data
  legacy$s339 <- c(-9999999999, rep(1, nrow(legacy) - 1L))
  result <- suppressWarnings(lissr::liss_clean_income(
    legacy, mode = "flag", enable_only = "P04", verbose = FALSE,
    variables = list(target = "s339", category_code = "s339"),
    params = list(P04 = list(code_share = 0.8))))
  expect_identical(result$data$s339, legacy$s339)
  expect_identical(result$source_nonresponse_readiness$status, "not_evaluated")
  expect_true(any(vapply(result$log, function(entry)
    identical(entry$action, "map_category_bounds"), logical(1))))
})

test_that("tampered source, audit, value or metadata cannot regain readiness", {
  fixture <- .nr_fixture()
  bound <- .nr_bind(fixture)
  for (kind in c("source", "ledger", "source_metadata", "target_metadata", "coverage")) {
    bad <- bound
    if (kind == "source") bad$source_nonresponse$source$sha256 <- "changed"
    if (kind == "ledger") bad$source_nonresponse$ledger$no_numeric_proposal <- FALSE
    if (kind == "source_metadata") bad$source_nonresponse$metadata$attributes <- NULL
    if (kind == "target_metadata") bad$source_nonresponse$target_metadata <- NULL
    if (kind == "coverage") bad$source_nonresponse$coverage$original_value[1] <- 123
    expect_error(.nr_clean(bad), "tampered carrier digest")
  }
  value <- bound
  value$data$s339[1] <- 123
  expect_error(.nr_clean(value), "values changed after binding")
  metadata <- bound
  attr(metadata$data$s339, "label") <- "Different construct"
  expect_error(.nr_clean(metadata), "target metadata changed")
  duplicate <- bound
  duplicate$data <- duplicate$data[c(1, 1), , drop = FALSE]
  expect_error(.nr_clean(duplicate), "duplicated")
})

test_that("a classified wrapper cannot silently lose its guard carrier", {
  fixture <- .nr_fixture()
  for (kind in c("classified", "without_readiness", "unclassed_claim")) {
    bad <- .nr_bind(fixture)
    bad$source_nonresponse <- NULL
    if (kind == "without_readiness") bad$source_nonresponse_readiness <- NULL
    if (kind == "unclassed_claim") class(bad) <- NULL
    expect_error(.nr_clean(bad), "missing its carrier",
                 class = "liss_source_nonresponse_blocked")
  }
  bad <- .nr_bind(fixture)
  bad$source_nonresponse <- NULL
  expect_error(lissr::merge_liss_panel(list(ci = bad)), "source_nonresponse lineage")
  class(bad) <- NULL
  expect_error(lissr::merge_liss_panel(list(ci = bad)), "source_nonresponse lineage")
  cleaned <- .nr_clean(.nr_bind(fixture), mode = "flag")
  cleaned$source_nonresponse <- NULL
  output <- file.path(fixture$directory, "lost_carrier_export_must_not_exist")
  expect_error(lissr::liss_cleaning_report(cleaned, output), "guarded report export")
  expect_false(file.exists(output))
})

test_that("moving matched or ordinary rows out of the sealed wave scope blocks", {
  fixture <- .nr_fixture()
  bound <- .nr_bind(fixture)
  for (row in c(1L, 6L)) {
    for (wave in c("ci22o", NA_character_)) {
      moved <- bound
      moved$data$wave_id[row] <- wave
      expect_error(.nr_clean(moved, mode = "flag"),
                   class = "liss_source_nonresponse_blocked")
    }
  }
})

test_that("repeated people in distinct waves remain bound to their original rows", {
  fixture <- .nr_fixture()
  data <- .nr_data(fixture)
  other_wave <- data[1, , drop = FALSE]
  other_wave$wave_id <- "ci22o"
  other_wave$wave_year <- 2022
  other_wave$s339 <- 123
  bound <- .nr_bind(fixture, data = rbind(data, other_wave))
  result <- .nr_clean(bound, mode = "flag", disable = .nr_rule_ids())
  expect_true(is.na(result$data$s339_proposed[1]))
  expect_equal(result$data$s339_proposed[9], 123)
  expect_equal(result$source_nonresponse_readiness$classified_cells, 1)
  reordered <- bound
  reordered$data <- reordered$data[c(9, 6, 1), , drop = FALSE]
  expect_no_error(.nr_clean(reordered, mode = "flag"))
  moved <- bound
  moved$data$wave_id[1] <- "ci22o"
  expect_error(.nr_clean(moved, mode = "flag"),
               class = "liss_source_nonresponse_blocked")
  moved <- bound
  moved$data$wave_id[9] <- "ci23p"
  expect_error(.nr_clean(moved, mode = "flag"),
               class = "liss_source_nonresponse_blocked")
})

test_that("guarded merge classifies before harmonization and writes no data files", {
  fixture <- .nr_fixture()
  recipe <- .nr_recipe()
  recipe$harmonization_rules <- list(list(
    rule_id = "FIXTURE_RECODE", action = "value_recode", description = "ordering probe",
    suffixes = list("339"), mapping = list(`-9999999999` = 123)))
  result <- suppressWarnings(suppressMessages(lissr::merge_liss_module(
    recipe, fixture$directory, output_dir = NULL, source_nonresponse = TRUE)))
  expect_true(is.na(as.numeric(result$data$s339)[1]))
  expect_equal(result$source_nonresponse$ledger$original_value, -9999999999)
  expect_identical(result$source_nonresponse$target, "s339")
  expect_true(isTRUE(result$source_nonresponse$normalized))
  cleaned <- .nr_clean(result, mode = "flag")
  expect_identical(cleaned$data$s339, result$data$s339)
  expect_identical(attributes(cleaned$data$s339), attributes(result$data$s339))
  expect_true(is.na(as.numeric(cleaned$data$s339)[1]))
  expect_equal(cleaned$data$s339_observed[1], -9999999999)
  expect_true(is.na(cleaned$data$s339_proposed[1]))
  expect_equal(cleaned$source_nonresponse$ledger$original_value, -9999999999)
  expect_equal(length(list.files(fixture$directory)), 1)
  subset <- result
  subset$data <- subset$data[c(6, 1, 4), , drop = FALSE]
  subset_cleaned <- .nr_clean(subset, mode = "flag")
  expect_identical(subset_cleaned$source_nonresponse,
                   attr(subset_cleaned$data, "liss_source_nonresponse", exact = TRUE))
  expect_equal(subset_cleaned$source_nonresponse$readiness$evaluated_cells, 3)
  expect_equal(subset_cleaned$source_nonresponse$readiness$classified_cells, 1)
  output <- file.path(fixture$directory, "guarded_export_must_not_exist")
  expect_error(lissr::merge_liss_module(recipe, fixture$directory, output,
                                       source_nonresponse = TRUE), "output_dir = NULL")
  expect_false(file.exists(output))
  expect_error(lissr::merge_liss_panel(list(ci = result)), "source_nonresponse lineage")
  expect_error(lissr::merge_liss_panel(list(ci = result$data)), "source_nonresponse lineage")
  expect_error(lissr::merge_liss_panel(list(ci = list(data = result$data))),
               "source_nonresponse lineage")
})

test_that("guarded cleaner reports stop before creating incomplete audit exports", {
  fixture <- .nr_fixture()
  bound <- .nr_bind(fixture)
  output <- file.path(fixture$directory, "guarded_report_must_not_exist")
  expect_error(.nr_clean(bound, mode = "flag", output_dir = output),
               "guarded report export is not supported")
  expect_false(file.exists(output))
  result <- .nr_clean(bound, mode = "flag")
  expect_error(lissr::liss_cleaning_report(result, output, verbose = FALSE),
               "guarded report export is not supported")
  expect_false(file.exists(output))
})

test_that("absent source waves carry not_evaluated without becoming a false guard", {
  fixture <- .nr_fixture()
  recipe <- .nr_recipe()
  full_recipe <- lissr::liss_recipe("ci")
  recipe$wave_index <- Filter(function(wave) wave$id == "ci22o", full_recipe$wave_index)
  recipe$meta$covered_waves <- list("ci22o")
  source <- fixture$raw
  names(source)[names(source) == "ci23p339"] <- "ci22o339"
  haven::write_sav(source, file.path(fixture$directory, "ci22o_EN_1.0p.sav"))
  result <- suppressWarnings(suppressMessages(lissr::merge_liss_module(
    recipe, fixture$directory, output_dir = NULL, source_nonresponse = TRUE)))
  expect_identical(result$source_nonresponse_readiness$status, "not_evaluated")
  expect_null(result[["source_nonresponse"]])
  cleaned <- .nr_clean(result, mode = "flag")
  expect_identical(cleaned$source_nonresponse_readiness$status, "not_evaluated")
  expect_null(cleaned[["source_nonresponse"]])
  legacy <- list(data = .nr_data(fixture),
                 source_nonresponse_readiness = list(status = "not_evaluated"))
  expect_no_error(.nr_clean(legacy, mode = "flag"))
})
