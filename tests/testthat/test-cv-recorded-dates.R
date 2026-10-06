# synthetic recorded dates distinguish designated sources from fallback guesses
.cvrd_recipe <- function() suppressWarnings(suppressMessages(lissr::load_recipe(
  system.file("recipes", "cv_merge_recipe.yml", package = "lissr", mustWork = TRUE))))
.cvrd_rule <- function(source = "_m1") list(rule_id = "recorded_date_test",
  action = "derive_fieldwork_month", source_column = source, source_suffix = TRUE,
  target_column = "fieldwork_ym", if_absent = "warn_and_create_na")
.cvrd_group_rule <- function() {
  rule <- .cvrd_rule()
  rule$source_column <- NULL
  rule$group_column <- "groep"
  rule$source_by_group <- list("0" = "maandnr", "1" = "maandnr_lang", "2" = "maandnr_lang")
  rule
}
.cvrd_run <- function(data, rule = .cvrd_rule(), wave = "cv23o", registry = NULL, source_types = NULL) {
  args <- list(data, rule, wave, list(), wave, list())
  if (!is.null(registry) || !is.null(source_types)) args <- c(args, list(registry))
  if (!is.null(source_types)) args <- c(args, list(source_types))
  suppressWarnings(suppressMessages(do.call(lissr:::exec_variable_rule, args)))
}
.cvrd_part <- function(x) haven::labelled_spss(x,
  labels = c("did not participate" = -9), na_values = -9, label = "part date")
.cvrd_log_count <- function(out, status) {
  entries <- Filter(function(entry) identical(entry$action,
    paste0("derive_fieldwork_month:", status)), out$log)
  sum(vapply(entries, function(entry) as.numeric(entry$rows_affected), numeric(1)))
}
.cvrd_plain_dates <- function(data, expected) {
  expect_identical(as.numeric(data$fieldwork_ym), expected)
  expect_identical(as.numeric(data$fieldwork_month), expected %% 100)
  expect_identical(is.na(data$fieldwork_month), is.na(data$fieldwork_ym))
  for (name in c("fieldwork_ym", "fieldwork_month")) {
    expect_false(inherits(data[[name]], "haven_labelled"))
    for (key in c("labels", "na_values", "na_range", "_original_labels",
                  "_original_na_values", "_original_na_range"))
      expect_null(attr(data[[name]], key, exact = TRUE))
  }
}
.cvrd_merge <- function(recipe, waves) {
  base <- withr::local_tempdir("cv_recorded_dates_")
  data_dir <- file.path(base, "input")
  output_dir <- file.path(base, "output")
  dir.create(data_dir)
  for (wave in names(waves)) haven::write_sav(waves[[wave]],
    file.path(data_dir, paste0(wave, "_EN_1.0p.sav")))
  paths <- list.files(data_dir, full.names = TRUE)
  before <- tools::md5sum(paths)
  result <- suppressWarnings(suppressMessages(lissr::merge_liss_module(
    recipe, data_dir, output_dir, strict = FALSE)))
  result$saved <- haven::read_sav(file.path(output_dir, "cv_merged.sav"), user_na = TRUE)
  result$saved_plain <- haven::read_sav(file.path(output_dir, "cv_merged.sav"), user_na = FALSE)
  expect_identical(tools::md5sum(paths), before)
  result
}

test_that("recorded dates require a calendar month and an explicit parsing envelope", {
  parse <- lissr:::parse_yyyymm
  good <- c(190001, 200712, 201512, 201601, 202212, 210012)
  expect_identical(parse(good)$value, good)
  expect_identical(parse(c(" 201512 ", "201601"))$value, c(201512, 201601))
  invalid <- c(-9, -8, 99, 999, 202300, 202313, 20231, 2024013, 202301.5, Inf, -Inf)
  expect_true(all(is.na(parse(invalid)$value)))
  expect_identical(parse(invalid)$class, rep("invalid", length(invalid)))
  for (text in c("2023-01", "2023/01", "2.02301e5", "+202301", "202301.0", "202,301"))
    expect_identical(parse(text)$class, "invalid")
  expect_identical(parse(c(189912, 210101))$class, rep("outside_window", 2))
  expect_identical(parse(c(189912, 210101))$value, c(NA_real_, NA_real_))
  expect_identical(parse(c(NA_real_, NaN, haven::tagged_na("a")))$class,
                   rep("source_missing", 3))
  expect_identical(parse(c(NA_character_, "", "  "))$class, rep("source_missing", 3))
  expect_identical(sum(parse(c(201512, NA, -9, 202313, 210101))$counts), 5L)
  withr::local_options(list(OutDec = ","))
  expect_identical(parse(c(201512, 201601))$value, c(201512, 201601))
  expect_identical(parse(c("201512", "201601"))$value, c(201512, 201601))
  expect_identical(lissr:::transform_apply(c(-9, 202313), "modulo", 100), c(91, 13))
})

test_that("date parsing respects labels, declared missingness and unsupported types", {
  parse <- lissr:::parse_yyyymm
  x <- haven::labelled_spss(c(201512, -9, -3, 201601),
    labels = c("december" = 201512), na_values = -9, na_range = c(-5, -1))
  before <- x
  parsed <- parse(x, na_values = attr(x, "na_values", exact = TRUE),
    na_range = attr(x, "na_range", exact = TRUE))
  expect_identical(parsed$value, c(201512, NA_real_, NA_real_, 201601))
  expect_identical(parsed$class, c("valid", "declared_missing", "declared_missing", "valid"))
  expect_identical(x, before)
  expect_identical(parse(factor(c("201512", "201601")))$value, c(201512, 201601))
  expect_identical(parse(c(FALSE, TRUE, NA))$class, c("invalid", "invalid", "source_missing"))
  unsupported <- list(as.Date(c("2015-12-01", "2016-01-01")),
    as.POSIXct(c("2015-12-01", "2016-01-01"), tz = "UTC"),
    list(201512, 201601), c(201512 + 0i, 201601 + 0i), matrix(c(201512, 201601), ncol = 1),
    structure(c(201512, 201601), class = "custom_numeric"))
  for (x in unsupported) {
    expect_identical(parse(x)$value, c(NA_real_, NA_real_))
    expect_identical(parse(x)$class, rep("unsupported", 2))
  }
  data <- data.frame(s_m1 = c(201512, -9, -8, -3, 201601))
  attr(data$s_m1, "_original_na_values") <- -9
  attr(data$s_m1, "na_values") <- -8
  attr(data$s_m1, "na_range") <- c(-5, -1)
  before <- data
  out <- .cvrd_run(data)
  expect_identical(as.numeric(out$df$fieldwork_ym), c(201512, NA_real_, NA_real_, NA_real_, 201601))
  expect_identical(out$df$s_m1, before$s_m1)
  expect_identical(data, before)
})

test_that("multipart dates never borrow another part or a generic month", {
  data <- data.frame(s_m = rep(202303, 4), s_m1 = .cvrd_part(c(202212, -9, NA, 202313)),
    s_m2 = rep(202301, 4), s_m3 = rep(202302, 4), fieldwork_ym = rep(202303, 4))
  before <- data
  out <- .cvrd_run(data)
  expect_identical(as.numeric(out$df$fieldwork_ym), c(202212, NA_real_, NA_real_, NA_real_))
  expect_identical(out$df[setdiff(names(data), "fieldwork_ym")],
                   before[setdiff(names(data), "fieldwork_ym")])
  missing_source <- data
  missing_source$s_m1 <- NULL
  expect_identical(.cvrd_run(missing_source)$df$fieldwork_ym, rep(NA_real_, 4))
  skip <- .cvrd_rule()
  skip$if_absent <- "warn_and_skip"
  expect_identical(.cvrd_run(missing_source, skip)$df, missing_source)
  empty <- data[FALSE, , drop = FALSE]
  expect_identical(as.numeric(.cvrd_run(empty)$df$fieldwork_ym), numeric(0))
})

test_that("cv16h dates follow group-specific ownership rather than coalescing", {
  data <- data.frame(groep = c(0, 1, 2, 0, 1, 2, 3, NA, 0.5),
    maandnr = c(201512, NA, NA, 201512, 201512, NA, 201512, 201512, 201512),
    maandnr_lang = c(NA, 201512, 201601, 201601, NA, 201601, 201601, 201601, 201601))
  expected <- c(201512, 201512, 201601, 201512, NA_real_, 201601, NA_real_, NA_real_, NA_real_)
  before <- data
  out <- .cvrd_run(data, .cvrd_group_rule(), "cv16h")
  expect_identical(as.numeric(out$df$fieldwork_ym), expected)
  expect_equal(.cvrd_log_count(out, "CONFLICT"), 1)
  expect_equal(.cvrd_log_count(out, "NONDESIGNATED_VALID"), 4)
  expect_equal(.cvrd_log_count(out, "GROUP_UNMAPPED"), 2)
  expect_equal(.cvrd_log_count(out, "GROUP_MISSING"), 1)
  expect_equal(.cvrd_log_count(out, "SOURCE"), 5)
  expect_equal(.cvrd_log_count(out, "SOURCE_MISSING"), 1)
  expect_identical(out$df[names(data)], before)
  expect_identical(data, before)
  # an invalid alternate source cannot make a valid designated date invalid
  data$maandnr_lang[[1]] <- 201513
  data$maandnr[[2]] <- 189912
  out <- .cvrd_run(data, .cvrd_group_rule(), "cv16h")
  expect_identical(as.numeric(out$df$fieldwork_ym), expected)
  expect_equal(.cvrd_log_count(out, "CONFLICT"), 1)
  # the designated source is absent, although the alternate is populated
  data$maandnr_lang <- NULL
  expect_identical(as.numeric(.cvrd_run(data, .cvrd_group_rule(), "cv16h")$df$fieldwork_ym),
    c(201512, NA_real_, NA_real_, 201512, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
  data$groep <- NULL
  expect_identical(.cvrd_run(data, .cvrd_group_rule(), "cv16h")$df$fieldwork_ym, rep(NA_real_, 9))
})

test_that("cv16h missing or unsupported group values cannot designate a source", {
  for (group in list(c(NA_real_, haven::tagged_na("a")), c(TRUE, FALSE),
                    as.Date(c("2015-12-01", "2016-01-01")), list(0, 1))) {
    data <- data.frame(maandnr = c(201512, 201512), maandnr_lang = c(201601, 201601))
    data$groep <- group
    expect_identical(.cvrd_run(data, .cvrd_group_rule(), "cv16h")$df$fieldwork_ym,
                     c(NA_real_, NA_real_))
  }
  data <- data.frame(groep = haven::labelled_spss(c(0, 1), na_values = 1),
    maandnr = c(201512, 201512), maandnr_lang = c(201601, 201601))
  expect_identical(as.numeric(.cvrd_run(data, .cvrd_group_rule(), "cv16h")$df$fieldwork_ym),
                   c(201512, NA_real_))
  data$groep <- structure(c(0, 2), "_original_na_values" = 2)
  expect_identical(as.numeric(.cvrd_run(data, .cvrd_group_rule(), "cv16h")$df$fieldwork_ym),
                   c(201512, NA_real_))
})

test_that("CV declares exactly one designated date writer per covered wave", {
  recipe <- .cvrd_recipe()
  ids <- vapply(recipe$wave_index, function(wave) wave$id, character(1))
  for (wave in recipe$wave_index) {
    writers <- Filter(function(rule) identical(rule$action, "derive_fieldwork_month") &&
      wave$id %in% lissr:::resolve_waves(if (is.null(rule$waves)) rule$wave else rule$waves, ids), recipe$variable_rules)
    expect_length(writers, 1L)
    if (length(writers) != 1L) next
    rule <- writers[[1]]
    expect_identical(rule$target_column, "fieldwork_ym")
    expect_true(rule$source_suffix)
    expect_null(rule$sources)
    if (identical(wave$id, "cv16h")) {
      expect_identical(rule$group_column, "groep")
      expect_identical(unlist(rule$source_by_group), c("0" = "maandnr", "1" = "maandnr_lang", "2" = "maandnr_lang"))
      expect_null(rule$source_column)
    } else expect_identical(rule$source_column,
      if (identical(wave$admin_structure, "three_part")) "_m1" else "_m")
  }
  month <- Filter(function(dv) identical(dv$name, "fieldwork_month"), recipe$derived_variables)
  expect_length(month, 1L)
  expect_identical(month[[1]]$harvested_metadata, "drop")
})

test_that("full CV merge preserves rows, other columns and returned/SAV dates", {
  skip_if_not_installed("haven")
  recipe <- .cvrd_recipe()
  waves <- list()
  expected <- numeric(0)
  for (wave in recipe$wave_index) {
    data <- data.frame(nomem_encr = as.numeric(1:6))
    data[[paste0(wave$id, "012")]] <- haven::labelled_spss(c(1, 2, 1, 2, 1, 2),
      labels = c("one" = 1, "two" = 2), na_values = 9, label = "substantive control")
    if (identical(wave$id, "cv16h")) {
      data$groep <- c(0, 1, 2, 0, 1, 3)
      data$maandnr <- c(201512, NA, NA, 201512, 201512, 201512)
      data$maandnr_lang <- c(NA, 201512, 201601, 201601, NA, 201601)
      dates <- c(201512, 201512, 201601, 201512, NA_real_)
    } else if (identical(wave$admin_structure, "three_part")) {
      ym <- as.numeric(wave$year * 100 + 11)
      data[[paste0(wave$id, "_m1")]] <- .cvrd_part(c(ym, ym, -9, NA, ym, ym))
      data[[paste0(wave$id, "_m2")]] <- .cvrd_part(rep(wave$year * 100 + 12, 6))
      data[[paste0(wave$id, "_m3")]] <- .cvrd_part(rep((wave$year + 1) * 100 + 1, 6))
      dates <- c(ym, ym, NA_real_, NA_real_, ym, ym)
      for (suffix in c("301", "302", "303")) data[[paste0(wave$id, suffix)]] <- c(99, 999, 998, 99, 999, 998)
    } else {
      ym <- as.numeric(wave$year * 100 + 12)
      data[[paste0(wave$id, "_m")]] <- c(ym, (wave$year + 1) * 100 + 1, ym, ym, ym, ym)
      dates <- as.numeric(data[[paste0(wave$id, "_m")]])
    }
    # stale target metadata must not be reapplied to either canonical date
    data$fieldwork_ym <- haven::labelled_spss(rep("201512", 6), na_values = "201512")
    data$fieldwork_month <- haven::labelled_spss(rep("12", 6), na_values = "12")
    waves[[wave$id]] <- data
    expected <- c(expected, dates)
  }
  result <- .cvrd_merge(recipe, waves)
  expect_equal(nrow(result$data), 107L)
  expect_identical(as.character(result$data$wave_id),
    unlist(lapply(recipe$wave_index, function(wave) rep(wave$id, if (wave$id == "cv16h") 5 else 6)), use.names = FALSE))
  expect_identical(as.numeric(result$data$nomem_encr),
    unlist(lapply(recipe$wave_index, function(wave) as.numeric(if (wave$id == "cv16h") 1:5 else 1:6)), use.names = FALSE))
  for (data in list(result$data, result$saved, result$saved_plain)) .cvrd_plain_dates(data, expected)
  expect_type(result$data$fieldwork_ym, "double")
  expect_type(result$data$fieldwork_month, "integer")
  reference <- recipe
  reference$variable_rules <- Filter(function(rule) !identical(rule$action, "derive_fieldwork_month"), reference$variable_rules)
  reference$derived_variables <- Filter(function(dv) !identical(dv$name, "fieldwork_month"), reference$derived_variables)
  control <- .cvrd_merge(reference, waves)
  other <- setdiff(names(result$data), c("fieldwork_ym", "fieldwork_month"))
  expect_identical(other, setdiff(names(control$data), c("fieldwork_ym", "fieldwork_month")))
  for (name in other) {
    expect_identical(result$data[[name]], control$data[[name]], info = name)
    expect_identical(result$saved[[name]], control$saved[[name]], info = name)
  }
  # removing the harvested raw month places the derived month after other DVs
  expect_gt(match("fieldwork_month", names(result$data)), match("fieldwork_ym", names(result$data)))
  for (suffix in c("s301", "s302", "s303")) {
    rows <- as.character(result$data$wave_id) == "cv26r"
    expect_identical(as.numeric(result$data[[suffix]][rows]), c(99, 999, 998, 99, 999, 998))
  }
})

test_that("legacy parse_time retains its character-copy contract", {
  rule <- list(rule_id = "legacy_parse_time", action = "parse_time", sources = list("_m1"), target = "legacy_time")
  data <- data.frame(s_m1 = c(202313, -9, NA_real_))
  expect_identical(.cvrd_run(data, rule)$df$legacy_time, c("202313", "-9", NA_character_))
})

test_that("original text type prevents accepting grammar lost by labelled conversion", {
  raw <- data.frame(nomem_encr = as.numeric(1:3))
  raw$s_m1 <- haven::labelled(c("202301.0", "202302e0", "202303"), label = "recorded month")
  original_types <- vapply(raw, typeof, character(1))
  converted <- suppressWarnings(lissr:::apply_labelled_policy(raw, "to_numeric"))
  before <- converted
  expect_identical(as.numeric(converted$s_m1), c(202301, 202302, 202303))
  out <- .cvrd_run(converted, source_types = original_types)
  expect_identical(as.numeric(out$df$fieldwork_ym), rep(NA_real_, 3))
  expect_equal(.cvrd_log_count(out, "UNSUPPORTED_TYPE"), 3)
  expect_identical(out$df[names(converted)], before)
  numeric <- data.frame(s_m1 = c(202301, 202302, 202303))
  expect_identical(as.numeric(.cvrd_run(numeric, source_types = vapply(numeric, typeof,
    character(1)))$df$fieldwork_ym), c(202301, 202302, 202303))
  # retained source text still has its grammar and can be parsed directly
  plain <- data.frame(s_m1 = c("202301.0", "202302e0", "202303"))
  expect_identical(as.numeric(.cvrd_run(plain, source_types = vapply(plain, typeof,
    character(1)))$df$fieldwork_ym), c(NA_real_, NA_real_, 202303))
})

test_that("SAV text dates remain strict without changing source columns", {
  skip_if_not_installed("haven")
  recipe <- .cvrd_recipe()
  for (kind in c("bare", "value_labelled", "user_missing")) {
    values <- c("202301.0", "202302e0", "202303")
    source <- switch(kind,
      bare = haven::labelled(values, label = "recorded month"),
      value_labelled = haven::labelled(values, labels = c("valid" = "202303"),
        label = "recorded month"),
      user_missing = haven::labelled_spss(values, na_values = "-9", label = "recorded month"))
    raw <- data.frame(nomem_encr = as.numeric(1:3))
    raw$cv23o_m1 <- source
    result <- .cvrd_merge(recipe, list(cv23o = raw))
    expected <- if (kind == "bare") c(NA_real_, NA_real_, 202303) else rep(NA_real_, 3)
    for (data in list(result$data, result$saved, result$saved_plain)) .cvrd_plain_dates(data, expected)
    control_recipe <- recipe
    control_recipe$variable_rules <- Filter(function(rule)
      !identical(rule$action, "derive_fieldwork_month"), recipe$variable_rules)
    control_recipe$derived_variables <- Filter(function(dv)
      !identical(dv$name, "fieldwork_month"), recipe$derived_variables)
    control <- .cvrd_merge(control_recipe, list(cv23o = raw))
    expect_identical(result$data$s_m1, control$data$s_m1)
    expect_identical(result$saved$s_m1, control$saved$s_m1)
  }
})

test_that("renames and collisions preserve original text date provenance", {
  skip_if_not_installed("haven")
  recipe <- .cvrd_recipe()
  writer <- which(vapply(recipe$variable_rules, function(rule)
    identical(rule$rule_id, "VR06b_fieldwork_ym_part1"), logical(1)))
  expect_length(writer, 1L)
  if (length(writer) != 1L) return(invisible(NULL))
  for (kind in c("plain_coerced", "value_labelled")) {
    for (collision in c(FALSE, TRUE)) {
      selected <- recipe
      rename <- list(rule_id = "date_source_rename_control", action = "rename",
        waves = c("cv23o"), mapping = list(s_m1 = "timing"), log = TRUE)
      upstream <- list(rename)
      if (kind == "plain_coerced") upstream <- c(upstream, list(list(
        rule_id = "date_source_coerce_control", action = "coerce_numeric",
        waves = c("cv23o"), suffixes = list("timing"), log = TRUE)))
      selected$variable_rules[[writer]]$source_column <- "timing"
      selected$variable_rules[[writer]]$source_suffix <- FALSE
      selected$variable_rules <- append(selected$variable_rules, upstream, after = writer - 1L)
      raw <- data.frame(nomem_encr = as.numeric(1:3))
      values <- c("202301.0", "202302e0", "202303")
      raw$cv23o_m1 <- if (kind == "plain_coerced") values else haven::labelled(values,
        labels = c("first" = "202301.0"), label = "recorded month")
      if (collision) raw$timing <- c(NA_real_, 202302, NA_real_)
      result <- .cvrd_merge(selected, list(cv23o = raw))
      for (data in list(result$data, result$saved, result$saved_plain))
        .cvrd_plain_dates(data, rep(NA_real_, 3))
      control_recipe <- selected
      control_recipe$variable_rules <- Filter(function(rule)
        !identical(rule$action, "derive_fieldwork_month"), selected$variable_rules)
      control_recipe$derived_variables <- Filter(function(dv)
        !identical(dv$name, "fieldwork_month"), selected$derived_variables)
      control <- .cvrd_merge(control_recipe, list(cv23o = raw))
      other <- setdiff(names(result$data), c("fieldwork_ym", "fieldwork_month"))
      expect_identical(other, setdiff(names(control$data), c("fieldwork_ym", "fieldwork_month")))
      for (name in other) {
        expect_identical(result$data[[name]], control$data[[name]], info = name)
        expect_identical(result$saved[[name]], control$saved[[name]], info = name)
      }
    }
  }
})
