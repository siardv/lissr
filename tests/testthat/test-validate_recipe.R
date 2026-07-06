test_that("all ten bundled recipes pass validation with no unexpected warnings", {
  # every bundled recipe must load and validate error-free. one warning class
  # is allowlisted: the "unrecognized rule-level key(s)" drift between the
  # recipes and RECOGNIZED_RULE_KEYS, which belongs to the vocabulary
  # reconciliation work (roadmap T2.4). anything else fails the test, and
  # tightening to zero warnings later means deleting the allowlist line.
  modules <- c("ca", "cd", "cf", "ch", "ci", "cp", "cr", "cs", "cv", "cw")
  for (mod in modules) {
    path <- system.file(
      "recipes", paste0(mod, "_merge_recipe.yml"),
      package = "lissr"
    )
    expect_true(nzchar(path), info = paste0("recipe not found: ", mod))
    recipe <- yaml::yaml.load_file(path)
    warns <- testthat::capture_warnings(
      expect_no_error(suppressMessages(validate_recipe(recipe, path))))
    unexpected <- warns[!grepl("unrecognized rule-level key", warns)]
    expect_length(unexpected, 0)
    if (length(unexpected) > 0) {
      cat("module", mod, "unexpected warning(s):\n",
          paste(unexpected, collapse = "\n"), "\n")
    }
  }
})

test_that("recipe with missing meta section fails", {
  bad <- list(global = list(), wave_index = list(), logging = list())
  expect_error(validate_recipe(bad, "bad.yml"), "meta")
})

test_that("recipe with missing global fields fails", {
  bad <- list(
    meta = list(
      module = "xx", module_label = "Test", recipe_version = "1.0",
      created = "2026-01-01", source_spec = "test", covered_waves = list("xx01a")
    ),
    global = list(),
    wave_index = list(),
    logging = list()
  )
  expect_error(validate_recipe(bad, "bad.yml"), "global")
})

test_that("recipe with invalid action fails", {
  bad <- list(
    meta = list(
      module = "xx", module_label = "Test", recipe_version = "1.0",
      created = "2026-01-01", source_spec = "test", covered_waves = list("xx01a")
    ),
    global = list(
      id_variable = "nomem_encr", wave_variable = "wave_id",
      year_variable = "wave_year", labelled_policy = "to_numeric",
      missing_variable_policy = "warn_and_create_na",
      strip_label_whitespace = TRUE
    ),
    wave_index = list(list(id = "xx01a", year = 2020, file_pattern = "xx01a*")),
    variable_rules = list(
      list(rule_id = "bad1", action = "nonexistent_action", description = "bad rule")
    ),
    logging = list()
  )
  expect_error(validate_recipe(bad, "bad.yml"), "unknown action")
})

test_that("recipe with duplicate rule_id fails", {
  bad <- list(
    meta = list(
      module = "xx", module_label = "Test", recipe_version = "1.0",
      created = "2026-01-01", source_spec = "test", covered_waves = list("xx01a")
    ),
    global = list(
      id_variable = "nomem_encr", wave_variable = "wave_id",
      year_variable = "wave_year", labelled_policy = "to_numeric",
      missing_variable_policy = "warn_and_create_na",
      strip_label_whitespace = TRUE
    ),
    wave_index = list(list(id = "xx01a", year = 2020, file_pattern = "xx01a*")),
    variable_rules = list(
      list(rule_id = "dup1", action = "note_only", description = "first"),
      list(rule_id = "dup1", action = "note_only", description = "second")
    ),
    logging = list()
  )
  expect_error(validate_recipe(bad, "bad.yml"), "duplicate rule_id")
})
