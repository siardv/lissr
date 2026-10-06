# derived target ownership applies only after an executed writer
.hmd_recipe <- function() suppressMessages(suppressWarnings(lissr::load_recipe(
  system.file("recipes", "cv_merge_recipe.yml", package = "lissr", mustWork = TRUE))))
.hmd_dv <- function() list(name = "s012", rule_id = "derived_metadata_control",
  method = "direct", sources = list("s012"), output_type = "double")
.hmd_merge <- function(recipe) {
  base <- withr::local_tempdir("harvested_metadata_")
  input <- file.path(base, "input")
  output <- file.path(base, "output")
  dir.create(input)
  raw <- data.frame(nomem_encr = as.numeric(1:2), cv23o_m1 = c(202301, 202302))
  raw$cv23o012 <- haven::labelled_spss(c(1, 2), labels = c("one" = 1, "two" = 2),
    na_values = 9, label = "substantive control")
  source <- file.path(input, "cv23o_EN_1.0p.sav")
  haven::write_sav(raw, source)
  before <- tools::md5sum(source)
  result <- suppressWarnings(suppressMessages(lissr::merge_liss_module(
    recipe, input, output, strict = FALSE)))
  result$saved <- haven::read_sav(file.path(output, "cv_merged.sav"), user_na = TRUE)
  expect_identical(tools::md5sum(source), before)
  result
}

test_that("harvested metadata accepts only plain keep/drop scalar policies", {
  recipe <- .hmd_recipe()
  for (policy in list("keep", "drop", NULL)) {
    dv <- .hmd_dv()
    dv["harvested_metadata"] <- list(policy)
    selected <- recipe
    selected$derived_variables <- c(recipe$derived_variables, list(dv))
    expect_no_error(suppressWarnings(suppressMessages(lissr::validate_recipe(selected))))
  }
  malformed <- list(c(policy = "drop"), structure("drop", class = "custom_policy"),
    matrix("drop", nrow = 1), structure("drop", comment = "hidden attribute"),
    "dorp", "", NA_character_, character(0), TRUE, 1, list("drop"), c("keep", "drop"))
  for (policy in malformed) {
    dv <- .hmd_dv()
    dv["harvested_metadata"] <- list(policy)
    selected <- recipe
    selected$derived_variables <- c(recipe$derived_variables, list(dv))
    expect_error(suppressWarnings(suppressMessages(lissr::validate_recipe(selected))), "harvested_metadata")
  }
})

test_that("harvested metadata policy is refused outside derived variables", {
  recipe <- .hmd_recipe()
  for (section in c("variable_rules", "harmonization_rules", "boundary_rules", "drop_retain_rules")) {
    selected <- recipe
    selected[[section]][[1]]$harvested_metadata <- "drop"
    expect_error(suppressWarnings(suppressMessages(lissr::validate_recipe(selected))), "harvested_metadata")
  }
})

test_that("derived metadata keep is the default and drop preserves values", {
  skip_if_not_installed("haven")
  recipe <- .hmd_recipe()
  for (policy in c("omitted", "null", "keep", "drop")) {
    selected <- recipe
    dv <- .hmd_dv()
    if (policy == "null") dv["harvested_metadata"] <- list(NULL)
    if (policy %in% c("keep", "drop")) dv$harvested_metadata <- policy
    selected$derived_variables <- c(recipe$derived_variables, list(dv))
    result <- .hmd_merge(selected)
    for (data in list(result$data, result$saved)) {
      expect_identical(as.numeric(data$s012), c(1, 2))
      expect_identical(inherits(data$s012, "haven_labelled_spss"), policy != "drop")
      if (policy == "drop") {
        for (key in c("labels", "na_values", "na_range"))
          expect_null(attr(data$s012, key, exact = TRUE))
      } else {
        expect_identical(attr(data$s012, "label", exact = TRUE), "substantive control")
        expect_identical(unname(attr(data$s012, "labels", exact = TRUE)), c(1, 2))
        expect_identical(as.numeric(attr(data$s012, "na_values", exact = TRUE)), 9)
      }
    }
    entries <- Filter(function(entry) identical(entry$rule_id, "derived_metadata_control"), result$log)
    expect_identical(vapply(entries, function(entry) entry$action, character(1)),
      c("derive", if (policy == "drop") "derive:TARGET_METADATA_DROPPED"))
  }
})

test_that("a skipped derived writer preserves harvested target metadata", {
  skip_if_not_installed("haven")
  recipe <- .hmd_recipe()
  for (variant in c("no_sources", "pending_spec")) {
    selected <- recipe
    dv <- .hmd_dv()
    dv$harvested_metadata <- "drop"
    if (variant == "no_sources") dv$sources <- list("999999") else dv$pending_spec <- TRUE
    selected$derived_variables <- c(recipe$derived_variables, list(dv))
    result <- .hmd_merge(selected)
    for (data in list(result$data, result$saved)) {
      expect_identical(as.numeric(data$s012), c(1, 2))
      expect_true(inherits(data$s012, "haven_labelled_spss"))
      expect_identical(as.numeric(attr(data$s012, "na_values", exact = TRUE)), 9)
    }
    entries <- Filter(function(entry) identical(entry$rule_id, "derived_metadata_control"), result$log)
    expect_identical(vapply(entries, function(entry) entry$action, character(1)),
      if (variant == "no_sources") "derive:SKIP_KEEP_EXISTING" else "derive:PENDING_SPEC")
  }
})

test_that("date writer withdraws only metadata for the target wave it writes", {
  rule <- list(rule_id = "metadata_ownership", action = "derive_fieldwork_month",
    source_column = "_m1", source_suffix = TRUE, target_column = "fieldwork_ym",
    if_absent = "warn_and_create_na")
  set <- function(wave, missing) list(labels = NULL, na_values = missing,
    na_range = NULL, wave = wave, vlab = "old target")
  registry <- new.env(parent = emptyenv())
  registry$fieldwork_ym <- list(set("cv23o", 202301), set("cv24p", 202401))
  registry$s_m1 <- list(set("cv23o", -9))
  before_source <- registry$s_m1
  data <- data.frame(s_m1 = c(202301, 202302))
  data$fieldwork_ym <- structure(c(202301, 202301), "_original_na_values" = 202301)
  out <- suppressWarnings(suppressMessages(lissr:::exec_variable_rule(
    data, rule, "cv23o", list(), "cv23o", list(), registry)))
  expect_identical(as.numeric(out$df$fieldwork_ym), c(202301, 202302))
  expect_identical(vapply(registry$fieldwork_ym, function(metadata) metadata$wave, character(1)), "cv24p")
  expect_identical(registry$s_m1, before_source)
  drop <- Filter(function(entry) identical(entry$action, "derive_fieldwork_month:TARGET_METADATA_DROPPED"), out$log)
  expect_length(drop, 1L)
  skip <- rule
  skip$if_absent <- "warn_and_skip"
  before <- registry$fieldwork_ym
  out <- suppressWarnings(suppressMessages(lissr:::exec_variable_rule(
    data["fieldwork_ym"], skip, "cv24p", list(), "cv24p", list(), registry)))
  expect_identical(registry$fieldwork_ym, before)
  expect_identical(out$df, data["fieldwork_ym"])
  # a rejected write must also preserve registry ownership atomically
  bad <- rule
  bad$target_column <- "s_m1"
  before <- as.list(registry)
  out <- suppressWarnings(suppressMessages(lissr:::exec_variable_rule(
    data, bad, "cv24p", list(), "cv24p", list(), registry)))
  expect_identical(out$df, data)
  expect_identical(as.list(registry), before)
  expect_true(any(vapply(out$log, function(entry) identical(entry$action,
    "ERROR:derive_fieldwork_month"), logical(1))))
})

.hmd_literal_rules <- function() {
  keys <- c("harvested_sample", "harvested_metadata", "harvested_metdata")
  named <- function(values) stats::setNames(as.list(values), keys)
  list(
    rename = list(section = "variable_rules", rule = list(rule_id = "literal_rename",
      action = "rename", description = "rename literal columns", mapping = named(c("sample", "policy", "typo")))),
    crosswalk = list(section = "harmonization_rules", rule = list(rule_id = "literal_crosswalk",
      action = "crosswalk", description = "map literal categories", variables = list("sample"),
      output_variable = "sample_code", crosswalk = named(c(1, 2, 3)))),
    labels = list(section = "harmonization_rules", rule = list(rule_id = "literal_labels",
      action = "conditional_label_swap", description = "replace literal value labels",
      variables = list("sample"), label_map = named(c("sample", "policy", "typo")))),
    group_sources = list(section = "variable_rules", rule = list(rule_id = "literal_group_sources",
      action = "derive_fieldwork_month", description = "designate dates for literal group names",
      group_column = "sample", source_by_group = named(rep("_m1", 3)),
      source_suffix = TRUE, target_column = "fieldwork_ym"))
  )
}

test_that("literal data-map keys are not harvested metadata configuration", {
  recipe <- .hmd_recipe()
  for (variant in .hmd_literal_rules()) {
    selected <- recipe
    selected[[variant$section]] <- c(selected[[variant$section]], list(variant$rule))
    result <- tryCatch(suppressWarnings(suppressMessages(lissr::validate_recipe(selected))),
      error = identity)
    expect_identical(result, TRUE, info = variant$rule$rule_id)
  }
})

test_that("literal rename, category and label maps keep their executable behavior", {
  variants <- .hmd_literal_rules()
  keys <- c("harvested_sample", "harvested_metadata", "harvested_metdata")
  raw <- stats::setNames(data.frame(as.numeric(1:2), as.numeric(3:4), as.numeric(5:6)), keys)
  out <- suppressWarnings(suppressMessages(lissr:::exec_variable_rule(
    raw, variants$rename$rule, "cv23o", list(), "cv23o", list())))
  expected <- raw
  names(expected) <- c("sample", "policy", "typo")
  expect_identical(out$df, expected)
  expect_identical(raw, stats::setNames(expected, keys))
  sample <- data.frame(sample = keys)
  out <- suppressWarnings(suppressMessages(lissr:::exec_harmonization_rule(
    sample, variants$crosswalk$rule, "cv23o", list(), "cv23o", list())))
  expect_identical(as.numeric(out$df$sample_code), c(1, 2, 3))
  expect_identical(out$df$sample, sample$sample)
  labelled <- data.frame(sample = haven::labelled(c(1, 2, 3), stats::setNames(c(1, 2, 3), keys)))
  out <- suppressWarnings(suppressMessages(lissr:::exec_harmonization_rule(
    labelled, variants$labels$rule, "cv23o", list(), "cv23o", list())))
  expect_identical(attr(out$df$sample, "labels", exact = TRUE), c(sample = 1, policy = 2, typo = 3))
  expect_identical(as.numeric(out$df$sample), c(1, 2, 3))
  expect_identical(attr(labelled$sample, "labels", exact = TRUE), stats::setNames(c(1, 2, 3), keys))
})

test_that("misplaced and misspelled config keys remain errors beside literal maps", {
  recipe <- .hmd_recipe()
  for (property in c("harvested_metadata", "harvested_metdata")) {
    selected <- recipe
    rule <- .hmd_literal_rules()$rename$rule
    rule[[property]] <- "drop"
    selected$variable_rules <- c(selected$variable_rules, list(rule))
    expect_error(suppressWarnings(suppressMessages(lissr::validate_recipe(selected))), "harvested_metadata")
    dv <- .hmd_dv()
    dv[[property]] <- "drop"
    dv$sources <- list(list(waves = list("cv23o"), suffixes = list("s012"),
      harvested_metdata = "drop"))
    selected <- recipe
    selected$derived_variables <- c(selected$derived_variables, list(dv))
    expect_error(suppressWarnings(suppressMessages(lissr::validate_recipe(selected))), "harvested_metadata")
  }
})

test_that("named record labels do not become harvested metadata properties", {
  recipe <- .hmd_recipe()
  sections <- c("wave_index", "variable_rules", "harmonization_rules", "boundary_rules",
    "drop_retain_rules", "derived_variables", "validation_checks")
  for (section in sections) {
    selected <- recipe
    labels <- paste0("record_", seq_along(selected[[section]]))
    labels[[1]] <- "harvested_metadata"
    if (length(labels) > 1L) labels[[2]] <- "harvested_metdata"
    names(selected[[section]]) <- labels
    result <- tryCatch(suppressWarnings(suppressMessages(lissr::validate_recipe(selected))),
      error = identity)
    expect_identical(result, TRUE, info = section)
  }
  dv <- .hmd_dv()
  dv$sources <- list(harvested_metadata = list(waves = list("cv23o"), suffixes = list("s012")))
  selected <- recipe
  selected$derived_variables <- c(recipe$derived_variables, list(dv))
  result <- tryCatch(suppressWarnings(suppressMessages(lissr::validate_recipe(selected))), error = identity)
  expect_identical(result, TRUE)
  recode <- list(rule_id = "named_recode_records", action = "recode_to_na",
    description = "recode structured records with literal labels", scope = list("s012"),
    recodes = list(harvested_metadata = list(waves = list("cv23o"), codes_to_na = list(
      list(code = -9, reason = "synthetic missing value")))))
  selected <- recipe
  selected$harmonization_rules <- c(recipe$harmonization_rules, list(recode))
  result <- tryCatch(suppressWarnings(suppressMessages(lissr::validate_recipe(selected))), error = identity)
  expect_identical(result, TRUE)
  # record labels do not exempt actual misplaced configuration properties
  selected$harmonization_rules[[length(selected$harmonization_rules)]]$recodes[[1]]$harvested_metdata <- "drop"
  expect_error(suppressWarnings(suppressMessages(lissr::validate_recipe(selected))), "harvested_metadata")
})

test_that("unrelated harvested annotation stays advisory while config typos fail", {
  recipe <- .hmd_recipe()
  selected <- recipe
  selected$variable_rules[[1]]$harvested_note <- "source review note"
  warnings <- character(0)
  result <- tryCatch(withCallingHandlers(suppressMessages(lissr::validate_recipe(selected)),
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }), error = identity)
  expect_identical(result, TRUE)
  expect_true(any(grepl("harvested_note", warnings, fixed = TRUE)))
  for (property in c("harvested_metadata", "harvested_metdata")) {
    selected <- recipe
    selected$variable_rules[[1]][[property]] <- "drop"
    expect_error(suppressWarnings(suppressMessages(lissr::validate_recipe(selected))), "harvested_metadata")
  }
})
