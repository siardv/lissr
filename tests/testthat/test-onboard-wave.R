# onboard_new_wave: the step-3 diff must compare against the real previous
# wave file (resolved through the recipe's file_pattern, or an explicit
# prev_file), in both directions. before 1.3.0 the diff reconstructed the
# previous wave's names from the new wave's own names, so it always reported
# zero additions and hardcoded zero removals.

make_onboard_fixture <- function(dir) {
  recipe <- list(
    meta = list(module = "xx", covered_waves = list("xx01a", "xx02b")),
    wave_index = list(
      list(id = "xx01a", year = 2001, file_pattern = "xx01a_*"),
      list(id = "xx02b", year = 2002, file_pattern = "xx02b_*")
    )
  )
  recipe_path <- file.path(dir, "xx_merge_recipe.yml")
  yaml::write_yaml(recipe, recipe_path)
  prev <- data.frame(nomem_encr = double(0),
                     xx01a001 = double(0), xx01a002 = double(0))
  new <- data.frame(nomem_encr = double(0),
                    xx02b001 = double(0), xx02b003 = double(0))
  haven::write_sav(prev, file.path(dir, "xx01a_EN_1.0p.sav"))
  haven::write_sav(new, file.path(dir, "xx02b_EN_1.0p.sav"))
  list(recipe_path = recipe_path,
       new_file = file.path(dir, "xx02b_EN_1.0p.sav"),
       prev_file = file.path(dir, "xx01a_EN_1.0p.sav"))
}

test_that("onboarding diff is bidirectional against the real previous file", {
  dir <- tempfile("onboard"); dir.create(dir)
  fx <- make_onboard_fixture(dir)
  report <- suppressMessages(
    onboard_new_wave(fx$recipe_path, fx$new_file, prev_wave_id = "xx01a"))
  expect_identical(report$added_suffixes, "003")
  expect_identical(report$removed_suffixes, "002")
  expect_identical(basename(report$prev_file), "xx01a_EN_1.0p.sav")
})

test_that("missing previous file warns and marks the diff as skipped", {
  dir <- tempfile("onboard"); dir.create(dir)
  fx <- make_onboard_fixture(dir)
  lonely <- tempfile("lonely"); dir.create(lonely)
  file.copy(fx$new_file, file.path(lonely, basename(fx$new_file)))
  warns <- testthat::capture_warnings(
    report <- suppressMessages(onboard_new_wave(
      fx$recipe_path, file.path(lonely, basename(fx$new_file)),
      prev_wave_id = "xx01a")))
  expect_true(any(grepl("diff skipped", warns)))
  expect_true(isTRUE(report$diff_skipped))
  expect_null(report$added_suffixes)
})

test_that("an explicit prev_file overrides pattern resolution", {
  dir <- tempfile("onboard"); dir.create(dir)
  fx <- make_onboard_fixture(dir)
  lonely <- tempfile("lonely"); dir.create(lonely)
  new_moved <- file.path(lonely, basename(fx$new_file))
  file.copy(fx$new_file, new_moved)
  report <- suppressMessages(onboard_new_wave(
    fx$recipe_path, new_moved,
    prev_wave_id = "xx01a", prev_file = fx$prev_file))
  expect_identical(report$added_suffixes, "003")
  expect_identical(report$removed_suffixes, "002")
})
