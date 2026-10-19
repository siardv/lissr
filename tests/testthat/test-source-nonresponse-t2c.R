# all rows and receipts in these fixtures are invented; source I/O is mocked
.nrd_saved <- function(version = 4L) {
  readRDS(testthat::test_path("fixtures",
    paste0("source_nonresponse_v", version, "_synthetic.rds")))
}

.nrd_fixture <- function(.local_envir = parent.frame()) {
  saved <- .nrd_saved()
  directory <- withr::local_tempdir("household_collection_", .local_envir = .local_envir)
  source <- file.path(directory, saved$mocked_receipt$filename)
  haven::write_sav(saved$raw, source)
  testthat::local_mocked_bindings(
    .liss_nonresponse_source_state = function(path) saved$mocked_receipt,
    .liss_nonresponse_read_source = function(path) saved$raw,
    read_wave_file = function(path, ...) {
      if (grepl("ci22o", basename(path), fixed = TRUE)) saved$raw_other_wave else saved$raw
    }, .package = "lissr", .env = .local_envir)
  list(saved = saved, directory = directory, source = source)
}

.nrd_clean <- function(input, ...) {
  suppressWarnings(lissr::liss_clean_income(input, mode = "flag",
    variables = list(target = "s339"), verbose = FALSE, ...))
}

.nrd_validate <- function(input) {
  lissr:::source_nonresponse_validate(input, input$source_nonresponse)
}

.nrd_scalar <- function(input) {
  # this private synthetic comparator is not a public collection projection
  checked <- .nrd_validate(input)
  state <- checked$state$fields$CI23P339_FIELD
  data <- input$data
  attr(data, "liss_source_nonresponse") <- state
  structure(list(data = data, source_nonresponse = state,
    source_nonresponse_readiness = state$readiness), class = "liss_source_nonresponse_result")
}

.nrd_stable_clean <- function(result) {
  result$summary$duration_s <- NULL
  result$ruleset$meta$source_path <- NULL
  result$log <- lapply(result$log, function(entry) {
    entry$timestamp <- NULL
    entry
  })
  result
}

.nrd_stable_merge <- function(result) {
  selected <- result[c("data", "source_nonresponse", "source_nonresponse_readiness")]
  class(selected) <- class(result)
  selected
}

.nrd_expect_preserved <- function(before, after) {
  expect_identical(names(after)[seq_along(before)], names(before))
  expect_identical(nrow(after), nrow(before))
  for (column in names(before)) expect_identical(after[[column]], before[[column]], info = column)
  expect_identical(row.names(after), row.names(before))
}

.nrd_expect_consumer <- function(input, result) {
  checked <- .nrd_validate(result)
  native <- checked$fields$CI23P339_FIELD
  ready <- result$source_nonresponse_consumer_readiness
  expect_identical(class(result), c("liss_clean_collection_flag_result", "liss_clean_result",
    "liss_source_nonresponse_collection_result", "liss_source_nonresponse_result"))
  expect_identical(names(ready), c("schema_version", "kind", "consumer", "policy_id",
    "policy_version", "policy_sha256", "capability_id", "capability_version",
    "carrier_schema_version", "carrier_digest", "field_id", "rule_id", "target",
    "mode", "status", "scope", "input_rows", "evaluated_cells", "classified_cells",
    "unresolved_cells", "result_digest", "digest"))
  expect_identical(ready$schema_version, "1.0.0")
  expect_identical(ready$kind, "source_nonresponse_consumer_readiness")
  expect_identical(ready$consumer, "liss_clean_income")
  expect_identical(ready$policy_id, "CI23P339_V4_FLAG_CONSUMER")
  expect_identical(ready$policy_version, "1.0.0")
  expect_identical(ready$policy_sha256,
    "41fe0edfa8a3e3f3ee467873a1350c50c112bcefbbf0664beceec393115d6d9f")
  expect_identical(ready$capability_id, "CI23P339_V4_FLAG")
  expect_identical(ready$capability_version, "1.0.0")
  expect_identical(ready$carrier_schema_version, "4.0.0")
  expect_identical(ready$carrier_digest, result$source_nonresponse$digest)
  expect_identical(ready$field_id, "CI23P339_FIELD")
  expect_identical(ready$rule_id, "CI23P339_N01")
  expect_identical(ready$target, "s339")
  expect_identical(ready$mode, "flag")
  expect_identical(ready$status,
    if (native$readiness$evaluated_cells > 0) "passed" else "not_evaluated")
  expect_identical(ready$scope, "household339_flag_source_exclusion_only")
  expect_equal(ready$input_rows, nrow(result$data))
  for (key in c("evaluated_cells", "classified_cells", "unresolved_cells"))
    expect_equal(ready[[key]], native$readiness[[key]], info = key)
  expect_identical(ready$result_digest,
    lissr:::.liss_nonresponse_consumer_result_digest(result))
  summary <- result$summary
  summary$duration_s <- NULL
  payload <- list(data = result$data, decisions = result$decisions, ruleset = result$ruleset,
    variables = result$variables, mode = result$mode, summary = summary)
  hash <- .nr_identity_hash(payload)
  expect_identical(ready$result_digest, hash)
  expect_identical(ready$digest, lissr:::.liss_nonresponse_digest(ready))
  expect_identical(checked$state, result$source_nonresponse)
  expect_identical(checked$readiness, result$source_nonresponse_readiness)
  expect_identical(result$source_nonresponse,
    attr(result$data, "liss_source_nonresponse", exact = TRUE))
  expect_identical(result$source_nonresponse_readiness,
    result$source_nonresponse$readiness)
  expect_null(attr(result$data, "source_nonresponse_consumer_readiness", exact = TRUE))
  expect_null(attr(result$data, "liss_source_nonresponse_consumer_readiness", exact = TRUE))
  expect_false(inherits(result$data, "liss_clean_collection_flag_result"))
  expect_true(all(is.na(result$data$s339_proposed[native$lock])))
  expect_identical(result$data$s339_proposed_status[native$lock],
    rep("source_nonresponse:missing_amount", sum(native$lock)))
  expect_identical(result$data$s339_observed, native$observed)
  .nrd_expect_preserved(input$data, result$data)
  source_only <- lissr:::source_nonresponse_validate(result$data, result$source_nonresponse)
  expect_identical(source_only$state, checked$state)
  expect_null(source_only$source_nonresponse_consumer_readiness)
}

.nrd_forms <- function(input) {
  clean <- input$data
  attr(clean, "liss_source_nonresponse") <- NULL
  class_only <- list(data = clean)
  class(class_only) <- class(input)
  frame_class <- clean
  class(frame_class) <- c("liss_source_nonresponse_collection_result", class(frame_class))
  readiness <- input$source_nonresponse_readiness
  frame_readiness <- clean
  attr(frame_readiness, "source_nonresponse_readiness") <- readiness
  frame_marker <- clean
  attr(frame_marker, "liss_source_nonresponse") <- list(schema_version = "4.0.0")
  list(class_only = class_only, class_frame = frame_class,
    readiness_only = list(data = clean, source_nonresponse_readiness = readiness),
    readiness_frame = frame_readiness,
    schema_only = list(data = clean, source_nonresponse = list(schema_version = "4.0.0")),
    schema_frame = frame_marker)
}

test_that("the frozen complete v4 fixture cross-reads all six orders and 27 subsets", {
  fixture <- .nrd_fixture()
  saved <- fixture$saved
  expect_identical(saved$format, "invented_row_t2b_v4_fixture")
  expect_true(saved$provenance$synthetic_only)
  expect_true(saved$provenance$source_io_mocked)
  expect_false(saved$provenance$original_data_read)
  validations <- 0L
  for (captured in c(saved$orders, list(saved$mixed_wave))) {
    expect_identical(.nrd_validate(captured$input), captured$validated)
    validations <- validations + 1L
    for (subset in captured$subsets) {
      expect_identical(.nrd_validate(subset$input), subset$validated)
      expect_identical(.nrd_validate(subset$resealed_input), subset$resealed_validated)
      validations <- validations + 2L
    }
  }
  expect_identical(validations, 61L)
  for (captured in saved$orders) {
    current <- suppressWarnings(suppressMessages(lissr::merge_liss_module(
      captured$recipe, fixture$directory, output_dir = NULL, source_nonresponse = TRUE)))
    expect_identical(.nrd_stable_merge(current), captured$input)
  }
})

test_that("new consumer authority leaves every frozen native v1 v2 and v3 validator unchanged", {
  old <- .nrd_saved(1)
  for (name in names(old$validated)) {
    input <- old$inputs[[name]]
    state <- input$source_nonresponse
    expect_identical(lissr:::source_nonresponse_validate(input$data, state,
      state$target, state$wave_col), old$validated[[name]], info = name)
    for (mode in names(old$expected[[name]])) {
      current <- suppressWarnings(lissr::liss_clean_income(input, mode = mode,
        variables = list(target = state$target), verbose = FALSE))
      expect_identical(.nrd_stable_clean(current), old$expected[[name]][[mode]],
        info = paste(name, mode))
    }
  }
  for (mode in names(old$expected$unguarded)) {
    current <- suppressWarnings(lissr::liss_clean_income(old$inputs$unguarded,
      mode = mode, variables = list(target = "s339"), verbose = FALSE))
    .nr_expect_frozen_unguarded(.nrd_stable_clean(current), old$expected$unguarded[[mode]])
  }
  for (suffix in c("337", "338")) {
    field <- .nrd_saved(2)$fields[[suffix]]
    for (name in names(field$inputs)) {
      input <- field$inputs[[name]]
      state <- input$source_nonresponse
      expect_identical(lissr:::source_nonresponse_validate(input$data, state,
        state$target, state$wave_col), field$validated[[name]], info = paste(suffix, name))
    }
    for (subsets in field$subsets) for (subset in subsets) {
      state <- subset$input$source_nonresponse
      expect_identical(lissr:::source_nonresponse_validate(subset$input$data, state,
        state$target, state$wave_col), subset$validated)
    }
  }
  for (captured in .nrd_saved(3)$orders) {
    expect_identical(.nrd_validate(captured$input), captured$validated)
    for (subset in captured$subsets) {
      expect_identical(.nrd_validate(subset$input), subset$validated)
      expect_identical(.nrd_validate(subset$resealed_input), subset$resealed_validated)
    }
  }
})

test_that("all explicit household projections retain complete source and outcome proof", {
  for (captured in .nrd_saved()$orders) {
    before <- captured$input
    for (input in list(before, before$data)) {
      result <- .nrd_clean(input)
      .nrd_expect_consumer(before, result)
      scalar <- .nrd_clean(.nrd_scalar(before))
      expect_identical(result$data$s339_observed, scalar$data$s339_observed)
      expect_identical(result$data$s339_proposed, scalar$data$s339_proposed)
      expect_identical(result$data$s339_proposed_status, scalar$data$s339_proposed_status)
      expect_identical(result$decisions, scalar$decisions)
      expect_identical(before, captured$input)
    }
  }
})

test_that("all scoped source inputs support flag simulation including empty and absent source rows", {
  saved <- .nrd_saved()
  for (captured in c(saved$orders, list(saved$mixed_wave))) {
    for (subset in captured$subsets) for (key in c("input", "resealed_input")) {
      input <- subset[[key]]
      result <- .nrd_clean(input)
      .nrd_expect_consumer(input, result)
      scalar <- .nrd_clean(.nrd_scalar(input))
      expect_identical(result$data$s339_proposed, scalar$data$s339_proposed)
      expect_identical(result$decisions, scalar$decisions)
    }
  }
  absent <- saved$mixed_wave$subsets$no_ci23p$input
  result <- .nrd_clean(absent)
  expect_identical(result$source_nonresponse_consumer_readiness$status, "not_evaluated")
  expect_equal(result$source_nonresponse_consumer_readiness$evaluated_cells, 0)
  expect_equal(result$summary$n_source_nonresponse, 0)
  unbound <- absent$data
  attr(unbound, "liss_source_nonresponse") <- NULL
  class(unbound) <- c("liss_source_nonresponse_collection_result", class(unbound))
  expect_error(.nrd_clean(unbound), class = "liss_source_nonresponse_blocked")
})

test_that("complete source and consumer RDS round trips preserve distinct proofs", {
  directory <- withr::local_tempdir("consumer_rds_")
  input <- .nrd_saved()$orders[[1L]]$input
  source_path <- file.path(directory, "invented_source.rds")
  saveRDS(input, source_path, version = 2L)
  restored <- readRDS(source_path)
  expect_identical(restored, input)
  result <- .nrd_clean(restored)
  output_path <- file.path(directory, "invented_consumer.rds")
  saveRDS(result, output_path, version = 2L)
  retained <- readRDS(output_path)
  expect_identical(retained, result)
  .nrd_expect_consumer(input, retained)
  frame_only <- retained$data
  frame_only$s339_proposed[1L] <- 123
  expect_no_error(lissr:::source_nonresponse_validate(frame_only, retained$source_nonresponse))
  for (rows in list(rev(seq_len(nrow(result$data))), seq_len(nrow(result$data))[-1L], integer())) {
    changed <- result
    changed$data <- changed$data[rows, , drop = FALSE]
    for (column in names(result$data)) attributes(changed$data[[column]]) <-
      attributes(result$data[[column]])
    expect_error(.nrd_validate(changed), class = "liss_source_nonresponse_blocked")
    expect_no_error(lissr:::source_nonresponse_validate(changed$data, result$source_nonresponse))
  }
})

test_that("flag overrides preserve source locks and native ordinary numerical decisions", {
  input <- .nrd_saved()$orders[[1L]]$input
  rs <- lissr::liss_cleaning_ruleset()
  rules <- unlist(lapply(c("preparation_rules", "detection_rules", "correction_rules",
    "finalization_rules"), function(section) vapply(rs[[section]], `[[`, character(1), "rule_id")),
    use.names = FALSE)
  configurations <- list(list(), list(disable = c("P01", "P03", "P06")),
    list(enable_only = "D06"), list(disable = rules),
    list(income_cap = 5000, min_income = 1),
    list(params = list(P04 = list(code_share = 0.8))))
  for (configuration in configurations) {
    result <- do.call(.nrd_clean, c(list(input = input), configuration))
    scalar <- do.call(.nrd_clean, c(list(input = .nrd_scalar(input)), configuration))
    .nrd_expect_consumer(input, result)
    expect_identical(result$data$s339_proposed, scalar$data$s339_proposed)
    expect_identical(result$decisions, scalar$decisions)
  }
})

test_that("ordinary P01 background only adds context and preserves every original column", {
  input <- .nrd_saved()$orders[[1L]]$input
  input$data$ordinary_context <- structure(seq_len(nrow(input$data)), label = "original context")
  background <- data.frame(nomem_encr = input$data$nomem_encr, wavenr = input$data$wavenr,
    ordinary_context = -1L, new_context = seq_len(nrow(input$data)) * 3L)
  result <- .nrd_clean(input, background = background)
  .nrd_expect_consumer(input, result)
  expect_identical(result$data$new_context, background$new_context)
  scalar <- .nrd_clean(.nrd_scalar(input), background = background)
  expect_identical(result$decisions, scalar$decisions)
  expect_identical(result$data$s339_proposed, scalar$data$s339_proposed)
  disabled <- .nrd_clean(input, background = background, disable = "P01")
  expect_false("new_context" %in% names(disabled$data))
  .nrd_expect_consumer(input, disabled)
  miss <- background
  miss$nomem_encr <- miss$nomem_encr + 10000
  zero <- .nrd_clean(input, background = miss)
  expect_true(all(is.na(zero$data$new_context)))
  .nrd_expect_consumer(input, zero)
})

test_that("entry requires a complete v4 explicit exact target and explicit flag mode", {
  input <- .nrd_saved()$orders[[1L]]$input
  cases <- list(list(), list(mode = "correct"), list(mode = "na_only"),
    list(mode = "flag"), list(mode = "flag", variables = list(targ = "s339")),
    list(mode = "flag", variables = list(target = "s339", target = "s339")),
    list(mode = "flag", variables = list(target = NA_character_)),
    list(mode = "flag", variables = list(target = c("s339", "s339"))),
    list(mode = "flag", variables = list(target = structure("s339", label = "claimed"))),
    list(mode = "flag", variables = list(target = factor("s339"))))
  for (target in c("s337", "s338", "nethh", "ci23p339", "ci_s339", "absent"))
    cases[[length(cases) + 1L]] <- list(mode = "flag", variables = list(target = target))
  reached <- character()
  testthat::local_mocked_bindings(liss_cleaning_ruleset = function(...) {
    reached <<- c(reached, "rules")
    stop("invalid entry reached rules loading")
  }, attach_background_frame = function(...) {
    reached <<- c(reached, "background")
    stop("invalid entry reached background")
  }, .package = "lissr")
  for (configuration in cases) {
    expect_error(do.call(lissr::liss_clean_income,
      c(list(data = input, verbose = FALSE), configuration)),
      class = "liss_source_nonresponse_blocked")
    expect_length(reached, 0L)
  }
  for (form in .nrd_forms(input)) {
    expect_error(.nrd_clean(form), class = "liss_source_nonresponse_blocked")
    expect_length(reached, 0L)
  }
  bad <- input
  bad$source_nonresponse$fields$CI23P337_FIELD$coverage$original_value[1L] <- 1
  expect_error(.nrd_clean(bad), class = "liss_source_nonresponse_blocked")
  expect_length(reached, 0L)
})

test_that("all sealed amounts are denied in every non-target role before P01", {
  input <- .nrd_saved()$orders[[1L]]$input
  roles <- c("person_id", "household_id", "wave", "personal_net", "personal_gross",
    "household_size", "category_code", "category_min", "category_max", "donor_keys",
    "birth_year", "age")
  reached <- FALSE
  testthat::local_mocked_bindings(attach_background_frame = function(...) {
    reached <<- TRUE
    stop("secondary role reached P01")
  }, .package = "lissr")
  for (column in c("s337", "s338", "s339")) for (role in roles) {
    variables <- list(target = "s339")
    variables[[role]] <- if (role == "donor_keys") c("nomem_encr", column) else column
    expect_error(suppressWarnings(lissr::liss_clean_income(input, mode = "flag",
      variables = variables, background = data.frame(nomem_encr = 1), verbose = FALSE)),
      class = "liss_source_nonresponse_blocked", info = paste(column, role))
    expect_false(reached)
  }
  for (column in c("s337", "s338", "s339"))
    expect_error(lissr::liss_clean_income(input, mode = "flag", variables = list(
      target = "s339", household_id = "absent", household_id_fallback = column),
      verbose = FALSE), class = "liss_source_nonresponse_blocked")
})

test_that("resolved role and original-column checks run again after P01", {
  input <- .nrd_saved()$orders[[1L]]$input
  original <- lissr:::resolve_cleaning_variables
  for (role in c("person", "household", "wave", "personal_net", "personal_gross",
    "household_size", "category_code", "category_min", "category_max", "donor_keys",
    "birth_year", "age", "target")) for (column in c("s337", "s338", "s339")) local({
      calls <- 0L
      testthat::local_mocked_bindings(resolve_cleaning_variables = function(data, vars) {
        calls <<- calls + 1L
        resolved <- original(data, vars)
        if (calls > 1L) resolved[[role]] <- column
        resolved
      }, .package = "lissr")
      if (!(role == "target" && column == "s339"))
        expect_error(.nrd_clean(input), class = "liss_source_nonresponse_blocked",
          info = paste("post-P01", role, column))
    })
  testthat::local_mocked_bindings(attach_background_frame = function(df, ...) {
    df$s226[1L] <- 99
    list(data = df, n_matched = nrow(df), n_added_cols = 0L, join_desc = "synthetic mutation")
  }, .package = "lissr")
  expect_error(.nrd_clean(input, background = data.frame(nomem_encr = 1)),
    class = "liss_source_nonresponse_blocked")
  expect_identical(input, .nrd_saved()$orders[[1L]]$input)
})

test_that("every guarded or claimed background is denied even with P01 disabled", {
  input <- .nrd_saved()$orders[[1L]]$input
  native <- list(.nrd_saved(1)$inputs$canonical,
    .nrd_saved(2)$fields[["337"]]$inputs$s337,
    .nrd_saved(2)$fields[["338"]]$inputs$s338,
    .nrd_saved(3)$orders[[1L]]$input, input)
  frames <- lapply(native, function(bound) {
    frame <- bound$data
    attr(frame, "liss_source_nonresponse") <- bound$source_nonresponse
    frame
  })
  backgrounds <- c(native, frames, .nrd_forms(input),
    list(list(nomem_encr = 1), matrix(1), "not a data frame"))
  consumer <- .nrd_clean(input)
  backgrounds <- c(backgrounds, list(consumer,
    structure(data.frame(nomem_encr = 1), class = c("liss_clean_collection_flag_result", "data.frame")),
    structure(data.frame(nomem_encr = 1), source_nonresponse_consumer_readiness =
      consumer$source_nonresponse_consumer_readiness)))
  for (key in c("liss_source_nonresponse", "source_nonresponse_readiness",
    "liss_source_nonresponse_readiness", "source_nonresponse_consumer_readiness",
    "liss_source_nonresponse_consumer_readiness")) for (status in c("passed", "blocked", "not_evaluated")) {
      frame <- data.frame(nomem_encr = 1)
      attr(frame, key) <- list(status = status, schema_version = "4.0.0")
      backgrounds[[length(backgrounds) + 1L]] <- frame
    }
  reached <- FALSE
  testthat::local_mocked_bindings(attach_background_frame = function(...) {
    reached <<- TRUE
    stop("guarded background reached conversion/join")
  }, .package = "lissr")
  for (background in backgrounds) for (disabled in list(NULL, "P01")) {
    expect_error(.nrd_clean(input, background = background, disable = disabled),
      class = "liss_source_nonresponse_blocked")
    expect_false(reached)
  }
})

test_that("helper and audit collisions cannot overwrite input or background columns", {
  input <- .nrd_saved()$orders[[1L]]$input
  helpers <- c(".lissr_join_person", ".lissr_join_wavenr")
  audits <- paste0("s339_", c("observed", "proposed", "proposed_status", "dataset_flag", "clean_status"))
  for (column in c(helpers, audits)) {
    bad <- input
    bad$data[[column]] <- seq_len(nrow(bad$data))
    expect_error(.nrd_clean(bad), class = "liss_source_nonresponse_blocked", info = column)
    background <- data.frame(nomem_encr = input$data$nomem_encr, wavenr = input$data$wavenr)
    background[[column]] <- seq_len(nrow(background))
    expect_error(.nrd_clean(input, background = background),
      class = "liss_source_nonresponse_blocked", info = paste("background", column))
  }
  for (column in audits) local({
    testthat::local_mocked_bindings(attach_background_frame = function(df, ...) {
      df[[column]] <- seq_len(nrow(df))
      list(data = df, n_matched = nrow(df), n_added_cols = 1L, join_desc = "synthetic collision")
    }, .package = "lissr")
    expect_error(.nrd_clean(input, background = data.frame(nomem_encr = 1)),
      class = "liss_source_nonresponse_blocked", info = paste("post-P01", column))
  })
  expect_identical(input, .nrd_saved()$orders[[1L]]$input)
})

test_that("the separate pinned consumer policy is exact and isolated from native paths", {
  fixture <- .nrd_fixture()
  policy <- lissr:::.liss_nonresponse_collection_consumers()
  registry <- lissr:::.liss_nonresponse_registry()
  adapter <- lissr:::.liss_nonresponse_collection_adapter()
  expect_identical(policy$policy_id, "CI23P339_V4_FLAG_CONSUMER")
  expect_length(policy$capabilities, 1L)
  expect_identical(adapter$consumers, list())
  expect_identical(policy$authority_registry$sha256,
    "a2e6694acc5855d2df389df46d66e0c94b705637626309cc309ceee8d840ddc3")
  expect_identical(policy$collection_adapter$sha256,
    "5e3235f1b4f78a589622ce27064eecf120c919e81fd95b84fe4a370126ac6c3f")
  changes <- list(function(x) { x$extra <- TRUE; x },
    function(x) { x$schema_version <- "2.0.0"; x },
    function(x) { x$policy_version <- "9.0.0"; x },
    function(x) { x$policy_id <- "caller_owned"; x },
    function(x) { x$authority_registry$sha256 <- strrep("0", 64); x },
    function(x) { x$collection_adapter$sha256 <- strrep("0", 64); x },
    function(x) { x$capabilities <- list(); x },
    function(x) { x$capabilities <- rep(x$capabilities, 2L); x },
    function(x) { names(x$capabilities) <- "grant"; x })
  for (key in names(policy$capabilities[[1L]])) changes[[length(changes) + 1L]] <- local({
    selected <- key
    function(x) {
      value <- x$capabilities[[1L]][[selected]]
      x$capabilities[[1L]][[selected]] <- if (is.numeric(value)) value + 1 else "altered"
      x
    }
  })
  for (index in seq_along(changes)) expect_error(
    lissr:::.liss_nonresponse_collection_consumers_validate(changes[[index]](policy), registry, adapter),
    class = "liss_source_nonresponse_blocked", info = paste("policy", index))
  for (key in names(policy)) {
    bad <- policy
    bad[[key]] <- NULL
    expect_error(lissr:::.liss_nonresponse_collection_consumers_validate(bad, registry, adapter),
      class = "liss_source_nonresponse_blocked", info = paste("missing policy", key))
  }
  for (record in c("authority_registry", "collection_adapter")) {
    bad <- policy
    bad[[record]]$extra <- TRUE
    expect_error(lissr:::.liss_nonresponse_collection_consumers_validate(bad, registry, adapter),
      class = "liss_source_nonresponse_blocked", info = record)
    for (key in names(policy[[record]])) {
      bad <- policy
      bad[[record]][[key]] <- "altered"
      expect_error(lissr:::.liss_nonresponse_collection_consumers_validate(bad, registry, adapter),
        class = "liss_source_nonresponse_blocked", info = paste(record, key))
    }
  }
  path <- lissr:::.liss_nonresponse_collection_consumers_path()
  missing <- file.path(fixture$directory, "missing_consumer.yml")
  corrupt <- file.path(fixture$directory, "corrupt_consumer.yml")
  writeLines(c(readLines(path, warn = FALSE), "# caller modified bytes"), corrupt)
  legacy <- .nrd_saved(1)$inputs$canonical
  legacy_result <- .nrd_stable_clean(.nrd_clean(legacy))
  for (selected in c(missing, corrupt)) local({
    selected_path <- selected
    testthat::local_mocked_bindings(.liss_nonresponse_collection_consumers_path =
      function() selected_path, .package = "lissr")
    expect_error(.nrd_clean(fixture$saved$orders[[1L]]$input),
      class = "liss_source_nonresponse_blocked")
    expect_identical(.nrd_stable_clean(.nrd_clean(legacy)), legacy_result)
    for (captured in fixture$saved$orders) {
      merged <- suppressWarnings(suppressMessages(lissr::merge_liss_module(captured$recipe,
        fixture$directory, output_dir = NULL, source_nonresponse = TRUE)))
      expect_identical(.nrd_stable_merge(merged), captured$input)
      expect_identical(.nrd_validate(captured$input), captured$validated)
    }
  })
})

test_that("complete outcome proof rejects every payload change and malformed record", {
  input <- .nrd_saved()$orders[[1L]]$input
  result <- .nrd_clean(input)
  changes <- list(
    function(x) { x$data$s339_proposed[2L] <- 123; x },
    function(x) { x$data$s226[2L] <- 123; x },
    function(x) { attr(x$data$s339, "label") <- "changed"; x },
    function(x) { x$decisions$changed <- rep(TRUE, nrow(x$decisions)); x },
    function(x) { x$ruleset$constraints$income_cap <- 123; x },
    function(x) { x$variables$personal_net <- "s337"; x },
    function(x) { x$summary$n_decisions <- x$summary$n_decisions + 1L; x },
    function(x) { x$mode <- "correct"; x },
    function(x) { x$source_nonresponse <- x$source_nonresponse$fields$CI23P339_FIELD; x },
    function(x) { x$source_nonresponse_consumer_readiness <- NULL; x },
    function(x) { x$extra <- TRUE; x },
    function(x) { attr(x, "caller_proof") <- TRUE; x },
    function(x) { class(x) <- setdiff(class(x), "liss_clean_collection_flag_result"); x },
    function(x) { attr(x$data, "source_nonresponse_consumer_readiness") <-
      x$source_nonresponse_consumer_readiness; x },
    function(x) { class(x$data) <- c("liss_clean_collection_flag_result", class(x$data)); x })
  for (index in seq_along(changes)) expect_error(.nrd_validate(changes[[index]](result)),
    class = "liss_source_nonresponse_blocked", info = paste("payload", index))
  ready <- result$source_nonresponse_consumer_readiness
  for (key in names(ready)) {
    bad <- result
    value <- ready[[key]]
    bad$source_nonresponse_consumer_readiness[[key]] <- if (is.numeric(value)) value + 1 else "altered"
    if (key != "digest") bad$source_nonresponse_consumer_readiness <-
      lissr:::.liss_nonresponse_seal(bad$source_nonresponse_consumer_readiness)
    expect_error(.nrd_validate(bad), class = "liss_source_nonresponse_blocked", info = key)
  }
  for (key in names(ready)) {
    bad <- result
    bad$source_nonresponse_consumer_readiness[[key]] <- NULL
    expect_error(.nrd_validate(bad), class = "liss_source_nonresponse_blocked", info = paste("missing", key))
  }
  bad <- result
  bad$source_nonresponse_consumer_readiness$extra <- TRUE
  expect_error(.nrd_validate(bad), class = "liss_source_nonresponse_blocked")
  for (key in c("input_rows", "evaluated_cells", "classified_cells", "unresolved_cells"))
    for (value in list(NA_integer_, Inf, -1L, 0.5, structure(1L, label = "claimed"), c(1L, 1L))) {
      bad <- result
      bad$source_nonresponse_consumer_readiness[[key]] <- value
      bad$source_nonresponse_consumer_readiness <-
        lissr:::.liss_nonresponse_seal(bad$source_nonresponse_consumer_readiness)
      expect_error(.nrd_validate(bad), class = "liss_source_nonresponse_blocked", info = key)
    }
  bad <- result
  ready <- bad$source_nonresponse_consumer_readiness
  names(ready)[2L] <- names(ready)[1L]
  bad$source_nonresponse_consumer_readiness <- ready
  expect_error(.nrd_validate(bad), class = "liss_source_nonresponse_blocked")
  for (column in c("s339_proposed", "s339_observed", "s339_proposed_status")) {
    bad <- result
    lock <- .nrd_validate(result)$fields$CI23P339_FIELD$lock
    bad$data[[column]][lock] <- if (column == "s339_proposed_status") "ordinary" else 123
    bad$source_nonresponse_consumer_readiness$result_digest <-
      lissr:::.liss_nonresponse_consumer_result_digest(bad)
    bad$source_nonresponse_consumer_readiness <-
      lissr:::.liss_nonresponse_seal(bad$source_nonresponse_consumer_readiness)
    expect_error(.nrd_validate(bad), class = "liss_source_nonresponse_blocked", info = column)
  }
  clocks <- result
  clocks$summary$duration_s <- clocks$summary$duration_s + 999
  clocks$log[[1L]]$timestamp <- "a different time"
  expect_identical(.nrd_validate(clocks), .nrd_validate(result))
})

test_that("new consumer claims cannot lose lineage through cleaning rebind report or panels", {
  fixture <- .nrd_fixture()
  result <- .nrd_clean(fixture$saved$orders[[1L]]$input)
  bare <- result$data
  attr(bare, "liss_source_nonresponse") <- NULL
  claimed <- structure(list(data = bare), class = "liss_clean_collection_flag_result")
  record_only <- list(data = bare, source_nonresponse_consumer_readiness =
    result$source_nonresponse_consumer_readiness)
  frame_record <- bare
  attr(frame_record, "source_nonresponse_consumer_readiness") <-
    result$source_nonresponse_consumer_readiness
  frame_class <- bare
  class(frame_class) <- c("liss_clean_collection_flag_result", class(frame_class))
  scalar <- result
  scalar$source_nonresponse <- scalar$source_nonresponse$fields$CI23P339_FIELD
  attr(scalar$data, "liss_source_nonresponse") <- scalar$source_nonresponse
  residual <- result
  class(residual) <- setdiff(class(residual), "liss_clean_collection_flag_result")
  residual$source_nonresponse_consumer_readiness <- NULL
  expect_true(lissr:::.liss_nonresponse_claims_consumer(residual))
  expect_error(lissr:::source_nonresponse_validate(residual, residual$source_nonresponse),
    class = "liss_source_nonresponse_blocked")
  expect_error(lissr:::source_nonresponse_validate(residual,
    result$source_nonresponse$fields$CI23P339_FIELD), class = "liss_source_nonresponse_blocked")
  legacy <- .nrd_clean(.nrd_saved(1)$inputs$canonical)
  expect_false(lissr:::.liss_nonresponse_claims_consumer(legacy))
  expect_no_error(lissr:::source_nonresponse_validate(legacy$data, legacy$source_nonresponse,
    legacy$source_nonresponse$target, legacy$source_nonresponse$wave_col))
  forms <- list(result, claimed, record_only, frame_record, frame_class, scalar, residual)
  reads <- FALSE
  testthat::local_mocked_bindings(.liss_nonresponse_read_source = function(path) {
    reads <<- TRUE
    stop("rebind must reject before source reads")
  }, .package = "lissr")
  output <- file.path(fixture$directory, "consumer_must_not_export")
  for (form in forms) {
    expect_true(lissr:::.liss_nonresponse_claims_consumer(form))
    expect_error(.nrd_clean(form), class = "liss_source_nonresponse_blocked")
    expect_error(lissr::merge_liss_panel(list(ci = form)))
    expect_error(lissr::liss_cleaning_report(form, output),
      class = "liss_source_nonresponse_blocked")
    for (suffix in c("337", "338", "339")) {
      recipe <- if (suffix == "339") NULL else .nrd_saved(2)$fields[[suffix]]$recipe
      expect_error(lissr::liss_classify_source_nonresponse(form, fixture$source,
        paste0("s", suffix), wave_col = "wave_id", recipe = recipe),
        class = "liss_source_nonresponse_blocked")
      expect_false(reads)
    }
  }
  expect_error(.nrd_clean(result$data))
  expect_error(.nrd_clean(fixture$saved$orders[[1L]]$input, output_dir = output),
    class = "liss_source_nonresponse_blocked")
  expect_false(file.exists(output))
})
