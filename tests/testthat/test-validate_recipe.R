test_that("valid built-in recipe passes validation", {
  # all bundled recipes should pass schema validation
  modules <- c("ch", "cv", "cd", "cf", "cw", "cp", "cs", "ci")
  for (mod in modules) {
    path <- system.file(
      "recipes", paste0(mod, "_merge_recipe.yml"),
      package = "lissr"
    )
    skip_if(path == "", message = paste0("recipe not found: ", mod))
    recipe <- yaml::yaml.load_file(path)
    expect_no_error(suppressWarnings(validate_recipe(recipe, path)))
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
