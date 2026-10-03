.cd_recode_recipe <- function() {
  yaml::yaml.load_file(system.file("recipes", "cd_merge_recipe.yml",
                                  package = "lissr"))
}

.cd_recode_wave <- function(recipe, wave_id, columns) {
  data <- data.frame(nomem_encr = seq_along(columns[[1]]))
  for (suffix in names(columns)) data[[paste0(wave_id, suffix)]] <- columns[[suffix]]
  data <- lissr:::strip_wave_prefix(data, wave_id)
  wave_ids <- vapply(recipe$wave_index, function(wave) wave$id, character(1))
  wave_meta <- recipe$wave_index[[match(wave_id, wave_ids)]]
  entries <- list()
  for (rule in recipe$variable_rules) {
    result <- lissr:::exec_variable_rule(data, rule, wave_id, wave_meta,
                                        wave_ids, entries)
    data <- result$df
    entries <- result$log
  }
  for (rule in recipe$harmonization_rules) {
    result <- lissr:::exec_harmonization_rule(data, rule, wave_id, wave_meta,
                                             wave_ids, entries)
    data <- result$df
    entries <- result$log
  }
  list(data = data, log = entries)
}

test_that("cd satisfaction recodes follow renames without crossing eras", {
  recipe <- .cd_recode_recipe()
  for (wave in recipe$wave_index) {
    old_era <- identical(wave$era, "era1")
    suffixes <- if (old_era) c("001", "002") else c("091", "092")
    declared_code <- if (old_era) 999 else -9
    other_era_code <- if (old_era) -9 else 999
    input <- c(0, 10, declared_code, other_era_code, NA_real_)
    columns <- stats::setNames(rep(list(input), 2L), suffixes)
    result <- .cd_recode_wave(recipe, wave$id, columns)
    expected <- c(0, 10, NA_real_, other_era_code, NA_real_)
    for (name in c("h_satisfaction_dwelling", "h_satisfaction_vicinity")) {
      expect_equal(result$data[[name]], expected, info = paste(wave$id, name))
    }
    expect_false(any(paste0("s", suffixes) %in% names(result$data)))
  }
})

test_that("cd mortgage recodes cover first-wave and later renamed debt", {
  recipe <- .cd_recode_recipe()
  input <- c(0, 250000, 999999999, 9999999998, 9999999999, -9, -8, NA_real_)
  for (wave in recipe$wave_index) {
    old_era <- identical(wave$era, "era1")
    debt_suffix <- if (identical(wave$id, "cd08a")) "017" else "083"
    raw_suffix <- if (identical(debt_suffix, "017")) "083" else "017"
    columns <- stats::setNames(rep(list(input), 2L), c(debt_suffix, raw_suffix))
    result <- .cd_recode_wave(recipe, wave$id, columns)
    expected_debt <- input
    expected_debt[if (old_era) 4:5 else 6:7] <- NA_real_
    expect_equal(result$data$h_mortgage_debt, expected_debt, info = wave$id)
    # the original raw monetary scopes remain effective after the debt rename
    expected_raw <- input
    if (old_era) expected_raw[4:5] <- NA_real_
    expect_equal(result$data[[paste0("s", raw_suffix)]], expected_raw,
                 info = paste(wave$id, raw_suffix))
    expect_false(paste0("s", debt_suffix) %in% names(result$data))
  }
})

test_that("cd WOZ reference-year recodes retain their existing wave boundaries", {
  recipe <- .cd_recode_recipe()
  renamed_waves <- c("cd08a", "cd09b", "cd10c", "cd11d", "cd12e", "cd13f",
                     "cd14g", "cd15h", "cd16i", "cd17j", "cd18k", "cd20m",
                     "cd21n", "cd22o")
  input <- c(2018, 99999, 9999999999, -9, NA_real_)
  for (wave in recipe$wave_index) {
    result <- .cd_recode_wave(recipe, wave$id, list(`029` = input))
    expected <- input
    expected[if (identical(wave$era, "era1")) 2:3 else 4L] <- NA_real_
    target <- if (wave$id %in% renamed_waves) "h_woz_ref_year" else "s029"
    expect_equal(result$data[[target]], expected, info = wave$id)
    if (!(wave$id %in% renamed_waves)) {
      expect_false("h_woz_ref_year" %in% names(result$data))
    }
  }
})

test_that("cd latest WOZ amount recodes preserve money and pre-rename raw targets", {
  recipe <- .cd_recode_recipe()
  input <- c(325000, 98, 99, -9, -8, 9999999999, NA_real_)
  for (wave in recipe$wave_index) {
    result <- .cd_recode_wave(recipe, wave$id, list(`099` = input))
    expected <- input
    if (identical(wave$era, "era2")) expected[4:5] <- NA_real_
    target <- if (wave$id %in% c("cd23p", "cd24q", "cd25r")) {
      "h_woz_latest_value"
    } else "s099"
    expect_equal(result$data[[target]], expected, info = wave$id)
  }
})

test_that("cd existing raw recodes and substantive negative quantities are preserved", {
  recipe <- .cd_recode_recipe()
  input <- c(1, 98, 99, 999, 99999, 9999999998, 9999999999, -9, -8, -999, NA_real_)
  for (wave_id in c("cd08a", "cd18k", "cd19l", "cd25r")) {
    result <- .cd_recode_wave(recipe, wave_id, list(
      `009` = input, `020` = input, `014` = input, `008` = input,
      `082` = input, `215` = input))
    expected <- rep(list(input), 6L)
    names(expected) <- c("s009", "s020", "s014", "s008", "s082", "s215")
    old_era <- wave_id %in% c("cd08a", "cd18k")
    expected$s009[if (old_era) 3L else 8L] <- NA_real_
    expected$s020[if (old_era) 2:3 else 8:9] <- NA_real_
    expected$s014[if (old_era) c(5L, 7L) else 8L] <- NA_real_
    expected$s008[if (old_era) 6:7 else 8:9] <- NA_real_
    if (identical(wave_id, "cd25r")) expected$s215[10L] <- NA_real_
    for (name in names(expected)) {
      expect_equal(result$data[[name]], expected[[name]],
                   info = paste(wave_id, name))
    }
  }
})

test_that("cd recode repair does not admit substantive out-of-range values", {
  recipe <- .cd_recode_recipe()
  result <- .cd_recode_wave(recipe, "cd20m", list(
    `091` = c(-9, 11), `092` = c(0, 10), `083` = c(-8, 1000000000)))
  expect_equal(result$data$h_satisfaction_dwelling, c(NA_real_, 11))
  expect_equal(result$data$h_mortgage_debt, c(NA_real_, 1000000000))
  checks <- Filter(function(check) check$check_id %in%
    c("CHK02_satisfaction_range", "CHK05_mortgage_range"), recipe$validation_checks)
  validation <- suppressWarnings(suppressMessages(
    lissr:::run_validations(result$data, checks, list())))
  expect_length(validation$results, 2L)
  for (check in validation$results) {
    expect_identical(check$passed, FALSE)
    expect_identical(check$severity, "error")
  }
  expect_equal(validation$error_count, 2L)
})

test_that("cd renamed sentinel cleanup agrees in returned data and SAV output", {
  skip_if_not_installed("haven")
  recipe <- .cd_recode_recipe()
  waves <- c("cd08a", "cd09b", "cd18k", "cd20m", "cd23p")
  recipe$wave_index <- Filter(function(wave) wave$id %in% waves, recipe$wave_index)
  recipe$meta$covered_waves <- waves
  # keep the actual rename/recode phases and the two relevant executable checks
  recipe$boundary_rules <- NULL
  recipe$drop_retain_rules <- NULL
  recipe$derived_variables <- NULL
  recipe$validation_checks <- Filter(function(check) check$check_id %in%
    c("CHK02_satisfaction_range", "CHK05_mortgage_range"), recipe$validation_checks)
  fixture_dir <- withr::local_tempdir("lissr_cd_recode_")
  data_dir <- file.path(fixture_dir, "data")
  output_dir <- file.path(fixture_dir, "output")
  dir.create(data_dir)
  expected <- list()
  for (wave in recipe$wave_index) {
    old_era <- identical(wave$era, "era1")
    satisfaction <- c(0, 10, if (old_era) 999 else -9, NA_real_)
    mortgage <- c(0, 250000, if (old_era) 9999999999 else -8, NA_real_)
    year <- c(2018, 2019, if (old_era) 99999 else -9, NA_real_)
    columns <- list()
    columns[[if (old_era) "001" else "091"]] <- satisfaction
    columns[[if (old_era) "002" else "092"]] <- satisfaction
    columns[[if (identical(wave$id, "cd08a")) "017" else "083"]] <- mortgage
    if (wave$id != "cd23p") columns[["029"]] <- year
    if (wave$id == "cd23p") columns[["099"]] <- c(325000, 99, -9, NA_real_)
    input <- data.frame(nomem_encr = 1:4)
    input[[paste0(wave$id, "_m")]] <- rep(wave$year * 100 + 6, 4L)
    for (suffix in names(columns)) input[[paste0(wave$id, suffix)]] <- columns[[suffix]]
    haven::write_sav(input, file.path(data_dir, paste0(wave$id, "_EN_1.0p.sav")))
    expected[[wave$id]] <- list(
      h_satisfaction_dwelling = c(0, 10, NA_real_, NA_real_),
      h_satisfaction_vicinity = c(0, 10, NA_real_, NA_real_),
      h_mortgage_debt = c(0, 250000, NA_real_, NA_real_),
      h_woz_ref_year = if (wave$id == "cd23p") rep(NA_real_, 4L) else
        c(2018, 2019, NA_real_, NA_real_),
      h_woz_latest_value = if (wave$id == "cd23p") c(325000, 99, NA_real_, NA_real_)
        else rep(NA_real_, 4L))
  }
  # report-and-continue lets the baseline expose both invalid values and saved data
  expect_no_warning(result <- suppressMessages(lissr::merge_liss_module(
    recipe, data_dir, output_dir)))
  expect_true(result$valid_for_analysis)
  expect_true(all(vapply(result$validation, function(check) isTRUE(check$passed), logical(1))))
  saved <- haven::read_sav(file.path(output_dir, "cd_merged.sav"))
  expect_equal(as.character(saved$wave_id), as.character(result$data$wave_id))
  expect_equal(as.numeric(saved$nomem_encr), as.numeric(result$data$nomem_encr))
  for (wave_id in waves) {
    rows <- result$data$wave_id == wave_id
    for (name in names(expected[[wave_id]])) {
      expect_equal(as.numeric(result$data[[name]][rows]), expected[[wave_id]][[name]],
                   info = paste("returned", wave_id, name))
      expect_equal(as.numeric(saved[[name]][rows]), expected[[wave_id]][[name]],
                   info = paste("saved", wave_id, name))
    }
  }
  actions <- vapply(result$log, function(entry) entry$action, character(1))
  expect_false(any(grepl("^(ERROR:|SKIPPED:)", actions)))
})
