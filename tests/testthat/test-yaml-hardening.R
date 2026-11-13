# invented YAML and temporary sentinels only; no respondent files are read
.yaml_hardening_recipe <- function() {
  list(
    meta = list(module = "zz", module_label = "invented YAML fixture",
      schema_version = "1.0.0", recipe_version = "test", created = "test",
      source_spec = "invented", covered_waves = list("zz01a")),
    global = list(id_variable = "nomem_encr", wave_variable = "wave_id",
      year_variable = "wave_year", labelled_policy = "to_numeric",
      missing_variable_policy = "warn_and_create_na", strip_label_whitespace = TRUE),
    wave_index = list(list(id = "zz01a", year = 2001L, file_pattern = "zz01a_*")),
    logging = list(summary_artifact = FALSE))
}

.yaml_hardening_case <- function(object, tagged, .local_envir = parent.frame()) {
  directory <- withr::local_tempdir("yaml_hardening_", .local_envir = .local_envir)
  path <- file.path(directory, "invented.yml")
  sentinel <- file.path(directory, "sentinel.txt")
  expression <- paste0('{ writeLines("evaluated", ',
    encodeString(sentinel, quote = '"'), '); "yaml_probe" }')
  text <- yaml::as.yaml(object, precision = 17L)
  if (tagged) text <- c(text, "yaml_hardening_probe: !expr >-",
                        paste0("  ", expression))
  writeLines(text, path, useBytes = TRUE)
  if (!is.null(object$source_nonresponse)) {
    # verify fixture transport before testing either production loader
    parsed <- suppressWarnings(yaml::yaml.load_file(path, eval.expr = FALSE))
    actual_code <- parsed$source_nonresponse$contracts[[1L]]$code
    expected_code <- object$source_nonresponse$contracts[[1L]]$code
    expect_true(is.numeric(actual_code) && length(actual_code) == 1L &&
      is.finite(actual_code) && actual_code == expected_code,
      info = "invented YAML must preserve the exact source contract code")
  }
  list(path = path, sentinel = sentinel, expression = expression,
       directory = directory)
}

.yaml_hardening_unchanged <- function(fixture) {
  expect_false(file.exists(fixture$sentinel))
  expect_identical(getOption("yaml.eval.expr"), TRUE)
}

test_that("action registry YAML stays declarative with evaluation globally enabled", {
  withr::local_options(list(yaml.eval.expr = TRUE))
  registry <- list(actions = list(list(name = "note_only",
    sections = list("variable_rules"), status = "implemented")))
  for (tagged in c(FALSE, TRUE)) {
    fixture <- .yaml_hardening_case(registry, tagged)
    result <- suppressWarnings(lissr:::.load_action_registry(fixture$path))
    expect_identical(result$actions$variable_rules, "note_only")
    expect_identical(result$statuses, c(note_only = "implemented"))
    expect_null(result$payloads)
    .yaml_hardening_unchanged(fixture)
  }
})

test_that("merge recipe YAML stays declarative with evaluation globally enabled", {
  withr::local_options(list(yaml.eval.expr = TRUE))
  for (tagged in c(FALSE, TRUE)) {
    fixture <- .yaml_hardening_case(.yaml_hardening_recipe(), tagged)
    result <- suppressWarnings(suppressMessages(lissr::load_recipe(fixture$path)))
    expect_identical(result$meta$module, "zz")
    expect_identical(result$wave_index[[1L]]$id, "zz01a")
    if (tagged) expect_identical(result$yaml_hardening_probe, fixture$expression)
    .yaml_hardening_unchanged(fixture)
  }
})

test_that("income ruleset YAML stays declarative with evaluation globally enabled", {
  withr::local_options(list(yaml.eval.expr = TRUE))
  ruleset <- list(meta = list(ruleset = "invented", ruleset_version = "1.0.0",
    schema_version = "1.0.0"), variables = list(target = "amount", person_id = "id"),
    constraints = list(income_cap = 150000L, min_income = 8000L))
  for (tagged in c(FALSE, TRUE)) {
    fixture <- .yaml_hardening_case(ruleset, tagged)
    result <- suppressWarnings(lissr::liss_cleaning_ruleset(fixture$path))
    expect_s3_class(result, "liss_cleaning_ruleset")
    expect_identical(result$variables$target, "amount")
    expect_identical(result$meta$source_path, normalizePath(fixture$path))
    expect_true(lissr::validate_cleaning_ruleset(result, quiet = TRUE)$valid)
    if (tagged) expect_identical(result$yaml_hardening_probe, fixture$expression)
    .yaml_hardening_unchanged(fixture)
  }
})

test_that("recipe audit YAML stays declarative with evaluation globally enabled", {
  withr::local_options(list(yaml.eval.expr = TRUE))
  for (tagged in c(FALSE, TRUE)) {
    fixture <- .yaml_hardening_case(.yaml_hardening_recipe(), tagged)
    result <- suppressWarnings(lissr::audit_liss_recipes(fixture$path, quiet = TRUE))
    expect_identical(result$zz$module, "zz")
    expect_equal(result$zz$n_waves, 1L)
    expect_true(result$zz$covered_waves_match)
    expect_equal(result$totals$recipes, 1L)
    .yaml_hardening_unchanged(fixture)
  }
})

test_that("onboarding YAML stays declarative with evaluation globally enabled", {
  withr::local_options(list(yaml.eval.expr = TRUE))
  # isolate the loader; the invented new wave is never read from disk
  testthat::local_mocked_bindings(read_wave_file = function(path, ...) {
    expect_identical(basename(path), "zz02b_EN_1.0p.sav")
    data.frame(nomem_encr = numeric(), zz02b001 = numeric())
  }, .package = "lissr")
  for (tagged in c(FALSE, TRUE)) {
    fixture <- .yaml_hardening_case(.yaml_hardening_recipe(), tagged)
    result <- suppressWarnings(suppressMessages(lissr::onboard_new_wave(
      fixture$path, file.path(fixture$directory, "zz02b_EN_1.0p.sav"))))
    expect_identical(result$module, "zz")
    expect_identical(result$new_wave_id, "zz02b")
    expect_equal(result$rows, 0L)
    expect_identical(result$candidate_wave_index$id, "zz02b")
    .yaml_hardening_unchanged(fixture)
  }
})

.yaml_hardening_source_recipe <- function() {
  # the synthetic document uses public contract metadata, with no source rows
  rule <- lissr:::.liss_nonresponse_registry()$records[[1L]]$contract
  list(meta = list(module = "ci"), source_nonresponse = list(
    schema_version = "1.0.0", contracts = list(rule)))
}

test_that("scalar source recipe YAML stays declarative with evaluation globally enabled", {
  withr::local_options(list(yaml.eval.expr = TRUE))
  withr::local_options(list(digits = 3L))
  recipe <- .yaml_hardening_source_recipe()
  for (tagged in c(FALSE, TRUE)) {
    fixture <- .yaml_hardening_case(recipe, tagged)
    result <- suppressWarnings(lissr:::.liss_nonresponse_rule(fixture$path))
    expect_identical(result$rule_id, "CI23P339_N01")
    expect_true(lissr:::.liss_nonresponse_rule_equal(result,
      recipe$source_nonresponse$contracts[[1L]]))
    .yaml_hardening_unchanged(fixture)
  }
})

test_that("source plan YAML stays declarative with evaluation globally enabled", {
  withr::local_options(list(yaml.eval.expr = TRUE))
  withr::local_options(list(digits = 3L))
  recipe <- .yaml_hardening_source_recipe()
  for (tagged in c(FALSE, TRUE)) {
    fixture <- .yaml_hardening_case(recipe, tagged)
    result <- suppressWarnings(lissr:::.liss_nonresponse_plan(fixture$path))
    expect_identical(result$schema_version, "1.0.0")
    expect_identical(result$collection, FALSE)
    expect_identical(result$wave, "ci23p")
    expect_true(lissr:::.liss_nonresponse_rule_equal(result$rules[[1L]],
      recipe$source_nonresponse$contracts[[1L]]))
    .yaml_hardening_unchanged(fixture)
  }
})

test_that("the harmless expression sentinel is sensitive to parser evaluation", {
  withr::local_options(list(yaml.eval.expr = TRUE))
  fixture <- .yaml_hardening_case(list(invented = "control"), TRUE)
  expect_false(file.exists(fixture$sentinel))
  result <- suppressWarnings(yaml::yaml.load_file(fixture$path, eval.expr = TRUE))
  expect_identical(result$yaml_hardening_probe, "yaml_probe")
  expect_identical(readLines(fixture$sentinel), "evaluated")
})

test_that("declarative condition strings retain their restricted downstream evaluation", {
  withr::local_options(list(yaml.eval.expr = TRUE))
  recipe <- .yaml_hardening_recipe()
  recipe$validation_checks <- list(list(check_id = "C1", type = "na_rate",
    variable = "amount", condition = "group == 1 & !is.na(amount)",
    max_na_rate = 1, severity = "warning"))
  fixture <- .yaml_hardening_case(recipe, FALSE)
  result <- suppressWarnings(suppressMessages(lissr::load_recipe(fixture$path)))
  condition <- result$validation_checks[[1L]]$condition
  expect_identical(condition, "group == 1 & !is.na(amount)")
  expect_identical(lissr:::safe_eval_condition(condition,
    data.frame(group = c(1, 1, 2), amount = c(10, NA, 20))), c(TRUE, FALSE, FALSE))
  .yaml_hardening_unchanged(fixture)
})
