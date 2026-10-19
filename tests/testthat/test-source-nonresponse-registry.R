# frozen v1 objects contain invented rows only; source I/O is mocked separately
.nrr_saved <- function() {
  readRDS(testthat::test_path("fixtures", "source_nonresponse_v1_synthetic.rds"))
}

.nrr_fixture <- function(.local_envir = parent.frame()) {
  saved <- .nrr_saved()
  raw <- saved$inputs$raw$data
  directory <- withr::local_tempdir("registry_source_",
                                   .local_envir = .local_envir)
  source <- file.path(directory, "ci23p_EN_1.0p.sav")
  haven::write_sav(raw, source)
  receipt <- saved$inputs$raw$source_nonresponse$source
  testthat::local_mocked_bindings(
    .liss_nonresponse_source_state = function(path) receipt,
    .liss_nonresponse_read_source = function(path) raw,
    .package = "lissr", .env = .local_envir)
  list(saved = saved, raw = raw, source = source, directory = directory)
}

.nrr_bind <- function(fixture, recipe = NULL, target = "s339") {
  data <- fixture$saved$inputs$canonical$data
  names(data)[names(data) == "s339"] <- target
  lissr::liss_classify_source_nonresponse(data, fixture$source, target,
    wave_col = "wave_id", recipe = recipe)
}

.nrr_stable <- function(result) {
  result$summary$duration_s <- NULL
  result$ruleset$meta$source_path <- NULL
  result$log <- lapply(result$log, function(entry) {
    entry$timestamp <- NULL
    entry
  })
  result
}

test_that("the installed registry preserves household authority and adds scalar tax fields", {
  path <- lissr:::.liss_nonresponse_registry_path()
  expect_identical(path, system.file("schema", "source_nonresponse_registry.yml",
                                    package = "lissr"))
  registry <- lissr:::.liss_nonresponse_registry()
  expect_identical(registry$schema_version, "2.0.0")
  expect_identical(registry$registry_version, "2.0.0")
  expect_length(registry$records, 1)
  expect_length(registry$fields, 2)
  record <- registry$records[[1]]
  rule <- lissr::liss_recipe("ci")$source_nonresponse$contracts[[1]]
  expect_identical(record$contract$rule_id, "CI23P339_N01")
  expect_identical(record$contract$raw_column, "ci23p339")
  expect_identical(record$contract$canonical_column, "s339")
  expect_equal(record$contract$code, -9999999999)
  expect_identical(record$source_release, "1.0p")
  expect_identical(record$targets,
                   c("ci23p339", "s339", "ci_s339", "nethh", "ci00a339"))
  expect_identical(record$routing$column, "ci23p229")
  expect_identical(record$evidence_ref, "codebook_ci23p_EN_1.1.pdf:339/229")
  expect_identical(record$metadata_policy, "retain_original_attributes")
  expect_length(record$consumers, 1)
  expect_identical(record$consumers[[1]]$name, "liss_clean_income")
  expect_identical(record$consumers[[1]]$construct, "household_net_income")
  expect_identical(record$consumers[[1]]$unit, "EUR")
  expect_equal(record$consumers[[1]]$measurement_period, 2022)
  expect_setequal(names(record$contract), names(rule))
  expect_no_error(lissr:::.liss_nonresponse_approved(rule))
  expect_no_error(lissr:::.liss_nonresponse_consumer(rule, "s339"))
})

test_that("missing or modified registry bytes block guarded calls only", {
  fixture <- .nrr_fixture()
  path <- file.path(fixture$directory, "missing_registry.yml")
  testthat::local_mocked_bindings(
    .liss_nonresponse_registry_path = function() path, .package = "lissr")
  missing <- tryCatch(.nrr_bind(fixture), error = identity)
  expect_s3_class(missing, "liss_source_nonresponse_blocked")
  expect_identical(missing$source_nonresponse_readiness$status, "blocked")
  path <- file.path(fixture$directory, "modified_registry.yml")
  writeLines("schema_version: '1.0.0'\nregistry_version: '1.0.0'\nrecords: []", path)
  expect_error(.nrr_bind(fixture), class = "liss_source_nonresponse_blocked")
  expect_error(lissr::liss_clean_income(fixture$saved$inputs$canonical,
    variables = list(target = "s339"), verbose = FALSE),
    class = "liss_source_nonresponse_blocked")
  legacy <- lissr::liss_clean_income(fixture$saved$inputs$unguarded,
    variables = list(target = "s339"), mode = "flag", verbose = FALSE)
  expect_identical(.nrr_stable(legacy), fixture$saved$expected$unguarded$flag)
})

test_that("registry structure rejects ambiguous or unsupported records", {
  registry <- lissr:::.liss_nonresponse_registry()
  expect_no_error(lissr:::.liss_nonresponse_registry_validate(registry))
  changes <- list(
    function(x) { x$schema_version <- "3.0.0"; x },
    function(x) { x$registry_version <- "3.0.0"; x },
    function(x) { x$caller_approved <- TRUE; x },
    function(x) { x$records <- list(); x },
    function(x) { x$records[[2]] <- x$records[[1]]; x },
    function(x) {
      x$records[[2]] <- x$records[[1]]
      x$records[[2]]$contract$rule_id <- "OVERLAPPING_CASE"
      x
    },
    function(x) { names(x$records) <- "named_record"; x },
    function(x) { x$records[[1]]$extra <- TRUE; x },
    function(x) { x$records[[1]]$contract$code <- NA_real_; x },
    function(x) { x$records[[1]]$contract$code <- Inf; x },
    function(x) { x$records[[1]]$contract$code <- "-9999999999"; x },
    function(x) {
      attr(x$records[[1]]$contract$code, "label") <- "a code"
      x
    },
    function(x) { x$records[[1]]$contract$no_numeric_proposal <- FALSE; x },
    function(x) { x$records[[1]]$metadata_policy <- "drop_attributes"; x },
    function(x) { x$records[[1]]$targets <- rep("s339", 2); x },
    function(x) { x$records[[1]]$targets <- c("s339", NA_character_); x },
    function(x) { x$records[[1]]$routing$column <- NULL; x },
    function(x) { x$records[[1]]$readiness$scope <- ""; x },
    function(x) { x$records[[1]]$consumers <- list(); x },
    function(x) {
      x$records[[1]]$consumers[[2]] <- x$records[[1]]$consumers[[1]]
      x
    },
    function(x) { x$records[[1]]$consumers[[1]]$targets <- "s337"; x })
  for (index in seq_along(changes))
    expect_error(lissr:::.liss_nonresponse_registry_validate(changes[[index]](registry)),
      class = "liss_source_nonresponse_blocked", info = paste("case", index))
})

test_that("caller recipes cannot approve changed semantic records", {
  fixture <- .nrr_fixture()
  recipe <- lissr::liss_recipe("ci")
  changes <- list(
    list(rule_id = "CI23P337_N01"), list(rule_version = "2.0.0"),
    list(module = "ca"), list(wave = "ci22o"), list(language = "NL"),
    list(source_filename = "ci23p_EN_1.1p.sav"),
    list(source_sha256 = paste(rep("0", 64), collapse = "")),
    list(raw_column = "ci23p337"), list(canonical_column = "s337"),
    list(code = -9999999998), list(code = -9999999998.5),
    list(code = NA_real_), list(code = "-9999999999"),
    list(construct = "respondent_taxable_income"), list(unit = "USD"),
    list(measurement_period = 2023), list(measurement_period = "2022"),
    list(codebook_filename = "other.pdf"),
    list(codebook_sha256 = paste(rep("0", 64), collapse = "")),
    list(response_validity = "valid_response"), list(answer_status = "dont_know"),
    list(analysis_disposition = "retain_amount"), list(no_numeric_proposal = FALSE),
    list(no_numeric_proposal = 1), list(reviewed = TRUE))
  for (change in changes) {
    bad <- recipe
    key <- names(change)
    bad$source_nonresponse$contracts[[1]][[key]] <- change[[key]]
    expect_error(.nrr_bind(fixture, recipe = bad),
      class = "liss_source_nonresponse_blocked", info = key)
  }
  for (kind in c("missing_key", "duplicate_key", "duplicate_id", "overlap",
                 "unsupported_schema", "extra_block_key", "wrong_module",
                 "classed_rule", "extra_rule_attribute", "classed_code")) {
    bad <- recipe
    if (kind == "missing_key") bad$source_nonresponse$contracts[[1]]$unit <- NULL
    if (kind == "duplicate_key") {
      rule <- bad$source_nonresponse$contracts[[1]]
      bad$source_nonresponse$contracts[[1]] <- c(rule, rule["rule_id"])
    }
    if (kind %in% c("duplicate_id", "overlap")) {
      bad$source_nonresponse$contracts[[2]] <- bad$source_nonresponse$contracts[[1]]
      if (kind == "overlap")
        bad$source_nonresponse$contracts[[2]]$rule_id <- "OVERLAPPING_CASE"
    }
    if (kind == "unsupported_schema") bad$source_nonresponse$schema_version <- "2.0.0"
    if (kind == "extra_block_key") {
      bad$source_nonresponse$reviewed <- TRUE
      bad$source_nonresponse$recipe_sha256 <- as.character(openssl::sha256(
        serialize(bad$source_nonresponse$contracts, NULL)))
    }
    if (kind == "wrong_module") bad$meta$module <- "ca"
    if (kind == "classed_rule")
      class(bad$source_nonresponse$contracts[[1]]) <- "unreviewed_contract"
    if (kind == "extra_rule_attribute")
      attr(bad$source_nonresponse$contracts[[1]], "reviewed") <- TRUE
    if (kind == "classed_code")
      class(bad$source_nonresponse$contracts[[1]]$code) <- "unreviewed_code"
    expect_error(.nrr_bind(fixture, recipe = bad),
      class = "liss_source_nonresponse_blocked", info = kind)
  }
  overflow <- suppressWarnings(yaml::yaml.load("code: -9999999999"))$code
  expect_true(is.na(overflow))
  bad <- recipe
  bad$source_nonresponse$contracts[[1]]$code <- overflow
  expect_error(.nrr_bind(fixture, recipe = bad),
    class = "liss_source_nonresponse_blocked")
})

test_that("approval preserves reordered v1 recipe objects and exact bindings", {
  fixture <- .nrr_fixture()
  recipe <- lissr::liss_recipe("ci")
  rule <- recipe$source_nonresponse$contracts[[1]]
  rule <- rule[rev(seq_along(rule))]
  recipe$source_nonresponse$contracts[[1]] <- rule
  bound <- .nrr_bind(fixture, recipe = recipe)
  expect_identical(bound$source_nonresponse$rule, rule)
  checked <- lissr:::source_nonresponse_validate(bound$data,
    bound$source_nonresponse, "s339", "wave_id")
  expect_identical(bound, fixture$saved$inputs$reordered)
  expect_identical(checked, fixture$saved$validated$reordered)
  expect_identical(names(checked$state),
                   names(fixture$saved$inputs$canonical$source_nonresponse))
  expect_false(any(c("registry", "registry_version", "approved") %in%
                     names(checked$state)))
  for (target in c("ci23p339", "s339", "ci_s339", "nethh", "ci00a339")) {
    bound <- .nrr_bind(fixture, target = target)
    checked <- lissr:::source_nonresponse_validate(bound$data,
      bound$source_nonresponse, target, "wave_id")
    expect_identical(which(checked$lock), c(1L, 2L))
    expect_identical(bound$source_nonresponse$ledger$answer_status,
                     rep("nonresponse_unknown", 2))
    expect_identical(bound$source_nonresponse$ledger$candidate_answer_status,
                     c("dont_know", NA_character_))
    expect_identical(bound$source_nonresponse$ledger$subtype_evidence,
                     c("routing_supported", "unresolved"))
  }
  for (target in c("ci23p337", "ci23p338", "s337", "s338", "ci_s337"))
    expect_error(.nrr_bind(fixture, target = target),
      class = "liss_source_nonresponse_blocked", info = target)
})

test_that("old raw and normalized v1 RDS cross-read without migration", {
  fixture <- .nrr_fixture()
  for (name in c("raw", "canonical", "normalized", "reordered")) {
    bound <- fixture$saved$inputs[[name]]
    path <- file.path(fixture$directory, paste0(name, "_old_v1.rds"))
    saveRDS(bound, path, version = 2L)
    restored <- readRDS(path)
    expect_identical(restored, bound)
    state <- restored$source_nonresponse
    checked <- lissr:::source_nonresponse_validate(restored$data, state,
      state$target, state$wave_col)
    expect_identical(restored$source_nonresponse, state)
    expect_identical(checked, fixture$saved$validated[[name]])
    expect_identical(checked$readiness, state$readiness)
    expect_identical(which(checked$lock), c(1L, 2L))
    expect_identical(checked$observed[c(1, 2)], rep(-9999999999, 2))
    for (mode in c("flag", "correct", "na_only")) {
      result <- lissr::liss_clean_income(restored,
        variables = list(target = state$target), mode = mode, verbose = FALSE)
      expect_identical(.nrr_stable(result), fixture$saved$expected[[name]][[mode]],
                       info = paste(name, mode))
    }
  }
  current_raw <- lissr::liss_classify_source_nonresponse(fixture$raw,
    fixture$source, "ci23p339")
  expect_identical(current_raw, fixture$saved$inputs$raw)
  current <- .nrr_bind(fixture)
  expect_identical(current, fixture$saved$inputs$canonical)
  normalized <- current
  normalized$data$s339[c(1, 2)] <- NA_real_
  normalized$source_nonresponse <- lissr:::source_nonresponse_finalize(
    normalized$data, current$source_nonresponse, "s339", "wave_id", TRUE)
  normalized$source_nonresponse_readiness <- normalized$source_nonresponse$readiness
  attr(normalized$data, "liss_source_nonresponse") <- normalized$source_nonresponse
  expect_identical(normalized, fixture$saved$inputs$normalized)
  for (mode in c("flag", "correct", "na_only")) {
    result <- lissr::liss_clean_income(fixture$saved$inputs$unguarded,
      variables = list(target = "s339"), mode = mode, verbose = FALSE)
    expect_identical(.nrr_stable(result), fixture$saved$expected$unguarded[[mode]])
  }
})

test_that("consumer capability is checked before numerical preparation", {
  saved <- .nrr_saved()
  bound <- saved$inputs$canonical
  record <- lissr:::.liss_nonresponse_approved(bound$source_nonresponse$rule)
  selected <- record
  background_reached <- FALSE
  testthat::local_mocked_bindings(
    .liss_nonresponse_approved = function(rule) selected,
    attach_background_frame = function(...) {
      background_reached <<- TRUE
      stop("P01 must not be reached before consumer denial")
    }, .package = "lissr")
  for (kind in c("absent", "name", "construct", "unit", "period", "target")) {
    selected <- record
    if (kind == "absent") selected$consumers <- list()
    if (kind == "name") selected$consumers[[1]]$name <- "another_consumer"
    if (kind == "construct")
      selected$consumers[[1]]$construct <- "respondent_taxable_income"
    if (kind == "unit") selected$consumers[[1]]$unit <- "USD"
    if (kind == "period") selected$consumers[[1]]$measurement_period <- 2021
    if (kind == "target") selected$consumers[[1]]$targets <- "s337"
    for (mode in c("flag", "correct", "na_only")) {
      for (configuration in list(list(), list(disable = c("P01", "P03", "P06")),
                                 list(enable_only = "D06"))) {
        args <- c(list(data = bound, background = bound$data,
          variables = list(target = "s339"), mode = mode, verbose = FALSE),
          configuration)
        expect_error(do.call(lissr::liss_clean_income, args),
          class = "liss_source_nonresponse_blocked", info = paste(kind, mode))
        expect_false(background_reached)
      }
    }
  }
})
