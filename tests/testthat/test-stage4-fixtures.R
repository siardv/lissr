# ============================================================================
# stage 4 (v1.4 development): per-module synthetic end-to-end fixtures.
# every bundled recipe merges a generated multi-wave dataset; assertions:
# the merge completes, no rule rolls back (ERROR:) or hits an unimplemented
# action (SKIPPED:), every declared flag column is non-degenerate in the
# OUTPUT, no validation check skips, and no error-severity check fails.
# plus: provenance fields, the overwrite guard, and expected_release pins.
# ============================================================================

# ---- fixture generator ------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

.collect_suffixes <- function(x, acc = character(0)) {
  # harvest plausible 3-digit suffix references from rule/check payloads
  if (is.list(x)) {
    for (k in c("suffixes", "variables", "scope", "items", "stems")) {
      v <- x[[k]]
      if (!is.null(v) && !is.list(v[[1]])) acc <- c(acc, as.character(unlist(v)))
    }
    for (el in x) if (is.list(el)) acc <- .collect_suffixes(el, acc)
  }
  acc
}

.plant_specs <- function(recipe) {
  # value constraints implied by executable checks, so the fixture passes
  # error-severity checks by construction
  allowed <- list(); present <- list()
  for (chk in (recipe$validation_checks %||% list())) {
    ty <- paste0(chk$type %||% "", collapse = "")
    cols <- as.character(unlist(chk$suffixes %||% chk$variables %||%
                                  chk$scope %||% list()))
    if (ty %in% c("value_in_set", "value_set", "assert_values")) {
      av <- suppressWarnings(as.numeric(unlist(chk$allowed_values %||%
                                                 chk$allowed %||% list())))
      av <- av[!is.na(av)]
      if (length(av)) for (cc in cols) allowed[[cc]] <- av
    }
    if (ty %in% c("value_range", "value_in_range", "assert_range")) {
      lo <- chk$min %||% 0; hi <- chk$max %||% 3
      for (cc in cols) allowed[[cc]] <- unique(pmin(pmax(c(1, 2), lo), hi))
    }
    if (ty %in% c("value_present", "value_present_per_wave")) {
      pv <- suppressWarnings(as.numeric(chk$value))
      if (!is.na(pv)) for (cc in cols) present[[cc]] <- pv
    }
  }
  list(allowed = allowed, present = present)
}

.expand_rng <- function(items) {
  out <- character(0)
  for (it in as.character(unlist(items %||% list()))) {
    if (grepl("^[0-9]+-[0-9]+$", it)) {
      parts <- strsplit(it, "-")[[1]]
      out <- c(out, sprintf(paste0("%0", nchar(parts[1]), "d"),
                            seq(as.integer(parts[1]), as.integer(parts[2]))))
    } else out <- c(out, it)
  }
  out
}

.absent_specs <- function(recipe) {
  # suffix x wave combinations that checks declare structurally all-NA;
  # the generator must not plant values there
  ab <- list()
  add <- function(sfx, waves) {
    for (s in sfx) ab[[s]] <<- unique(c(ab[[s]], waves))
  }
  for (chk in (recipe$validation_checks %||% list())) {
    ty <- paste0(chk$type %||% "", collapse = "")
    if (ty %in% c("structural_missingness", "structural_absence", "all_na",
                  "structural_na_count", "missingness_check")) {
      sfx <- as.character(unlist(chk$suffixes %||% chk$variables %||%
                                   chk$variable %||% chk$scope %||% list()))
      waves <- as.character(unlist(chk$waves_must_be_all_na %||%
                                     chk$must_be_na_in %||%
                                     chk$expected_na_waves %||%
                                     chk$wave_filter %||% chk$waves %||%
                                     list()))
      if (length(sfx) && length(waves)) add(sfx, waves)
    }
    if (ty %in% c("na_rate", "na_rate_above") &&
        identical(chk$direction %||%
                    (if (identical(ty, "na_rate_above")) "above" else "below"),
                  "above") &&
        (chk$threshold %||% 0) >= 1) {
      sfx <- .expand_rng(chk$items %||% chk$suffixes %||% chk$variables %||%
                           chk$scope)
      waves <- as.character(unlist(chk$waves %||% chk$wave_filter %||% list()))
      if (length(sfx) && length(waves)) add(sfx, waves)
    }
  }
  ab
}

.gen_module_fixture <- function(recipe, data_dir) {
  sfx <- unique(unlist(c(
    lapply(recipe$variable_rules %||% list(), .collect_suffixes),
    lapply(recipe$harmonization_rules %||% list(), .collect_suffixes),
    lapply(recipe$validation_checks %||% list(), .collect_suffixes)
  )))
  sfx <- sfx[grepl("^[0-9]{3}$", sfx)]
  sfx <- utils::head(sort(unique(sfx)), 40)
  plant <- .plant_specs(recipe)
  absent <- .absent_specs(recipe)
  # planted/constrained suffixes must exist even beyond the cap
  extra <- setdiff(grep("^[0-9]{3}$",
                        c(names(plant$allowed), names(plant$present)),
                        value = TRUE), sfx)
  sfx <- c(sfx, extra)
  # boundary split_variable sources must exist for era-scoped outputs
  bnd <- unlist(lapply(recipe$boundary_rules %||% list(), function(r) {
    c(r$suffix %||% character(0),
      vapply(r$output_vars %||% list(),
             function(ov) as.character(ov$source_suffix %||% ""),
             character(1)))
  }))
  sfx <- unique(c(sfx, bnd[grepl("^[0-9]{3}$", bnd)]))

  for (w in recipe$wave_index) {
    wid <- w$id
    n <- 3L
    df <- data.frame(nomem_encr = seq_len(n))
    df[[paste0(wid, "_m")]] <- rep(as.numeric(paste0(w$year, "03")), n)
    for (s in sfx) {
      col <- paste0(wid, s)
      if (wid %in% (absent[[s]] %||% character(0))) {
        df[[col]] <- rep(NA_real_, n)
        next
      }
      pool <- plant$allowed[[s]] %||% c(1, 2, 3)
      vals <- rep_len(pool, n)
      if (!is.null(plant$present[[s]])) vals[1] <- plant$present[[s]]
      df[[col]] <- as.numeric(vals)
    }
    pat <- as.character(w$file_pattern %||% paste0(wid, "_*"))
    fname <- if (grepl("\\*$", pat)) {
      if (grepl("_EN_", pat) || grepl("p\\*$", pat))
        sub("\\*$", ".sav", pat) else sub("\\*$", "EN_1.0p.sav", pat)
    } else paste0(wid, "_EN_1.0p.sav")
    haven::write_sav(df, file.path(data_dir, fname))
  }
  invisible(sfx)
}

.flag_cols_declared <- function(recipe) {
  out <- character(0)
  for (rule in (recipe$boundary_rules %||% list())) {
    act <- rule$action %||% ""
    fc <- if (act %in% c("add_flag", "add_era_flag")) {
      rule$flag_name %||% rule$flag_variable
    } else if (act == "add_period_flag") {
      rule$flag_column %||% paste0(rule$rule_id, "_period")
    } else if (act == "structural_na") {
      rule$flag_column
    } else NULL
    if (!is.null(fc)) out <- c(out, fc)
  }
  unique(out)
}

test_that("every bundled recipe merges a synthetic panel end to end", {
  skip_if_not_installed("haven")
  mods <- c("ca", "cd", "cf", "ch", "ci", "cp", "cr", "cs", "cv", "cw")
  problems <- character(0)

  for (mod in mods) {
    recipe_path <- system.file("recipes", paste0(mod, "_merge_recipe.yml"),
                               package = "lissr")
    recipe <- yaml::yaml.load_file(recipe_path)
    data_dir <- file.path(tempdir(), paste0("lissr_fix_", mod))
    out_dir  <- file.path(tempdir(), paste0("lissr_fixo_", mod))
    unlink(c(data_dir, out_dir), recursive = TRUE)
    dir.create(data_dir, recursive = TRUE)
    dir.create(out_dir, recursive = TRUE)

    .gen_module_fixture(recipe, data_dir)
    res <- tryCatch(
      suppressWarnings(suppressMessages(
        merge_liss_module(recipe_path, data_dir, out_dir))),
      error = function(e) e)

    if (inherits(res, "error")) {
      problems <- c(problems, paste0(mod, ": merge errored: ",
                                     conditionMessage(res)))
      next
    }

    acts <- vapply(res$log, function(e) as.character(e$action), character(1))
    if (any(grepl("^ERROR:", acts)))
      problems <- c(problems, paste0(mod, ": rolled-back rule(s): ",
        paste(unique(vapply(res$log[grepl("^ERROR:", acts)],
                            function(e) as.character(e$rule_id),
                            character(1))), collapse = ", ")))
    if (any(grepl("^SKIPPED:", acts)))
      problems <- c(problems, paste0(mod, ": unimplemented action(s) hit: ",
        paste(unique(acts[grepl("^SKIPPED:", acts)]), collapse = ", ")))

    for (fc in .flag_cols_declared(recipe)) {
      if (!(fc %in% names(res$data)) || all(is.na(res$data[[fc]])))
        problems <- c(problems, paste0(mod, ": degenerate flag in output: ", fc))
    }

    n_skip <- sum(vapply(res$validation, function(r)
      !isTRUE(r$passed) && !isFALSE(r$passed) && !isTRUE(r$documentary),
      logical(1)))
    if (n_skip > 0)
      problems <- c(problems, paste0(mod, ": ", n_skip, " skipped check(s)"))

    err_fails <- vapply(res$validation, function(r)
      identical(r$severity, "error") && isFALSE(r$passed), logical(1))
    if (any(err_fails))
      problems <- c(problems, paste0(mod, ": error-severity FAIL: ",
        paste(vapply(res$validation[err_fails],
                     function(r) r$check_id, character(1)), collapse = ", ")))

    unlink(c(data_dir, out_dir), recursive = TRUE)
  }

  expect_identical(problems, character(0))
})

# ---- provenance, valid_for_analysis, overwrite, release pins ----------------

.mini_fixture <- function(pin = NULL, extra_file = FALSE) {
  data_dir <- file.path(tempdir(), "lissr_s4_data")
  out_dir  <- file.path(tempdir(), "lissr_s4_out")
  unlink(c(data_dir, out_dir), recursive = TRUE)
  dir.create(data_dir, recursive = TRUE)
  dir.create(out_dir, recursive = TRUE)
  haven::write_sav(data.frame(nomem_encr = 1:2, yy01a005 = c(1, 2)),
                   file.path(data_dir, "yy01a_EN_1.0p.sav"))
  if (extra_file)
    haven::write_sav(data.frame(nomem_encr = 1:2, yy01a005 = c(1, 2)),
                     file.path(data_dir, "yy01a_EN_1.1p.sav"))
  wave <- list(id = "yy01a", year = 2001, file_pattern = "yy01a_*")
  if (!is.null(pin)) wave$expected_release <- pin
  recipe <- list(
    meta = list(module = "yy", module_label = "F", schema_version = "1.0.0",
                recipe_version = "9.9.9", created = "t", source_spec = "t",
                covered_waves = list("yy01a")),
    global = list(id_variable = "nomem_encr", wave_variable = "wave_id",
                  year_variable = "wave_year", labelled_policy = "to_numeric",
                  missing_variable_policy = "warn_and_create_na",
                  strip_label_whitespace = TRUE),
    wave_index = list(wave),
    logging = list(summary_artifact = FALSE))
  list(recipe = recipe, data_dir = data_dir, out_dir = out_dir)
}

test_that("provenance and valid_for_analysis are attached to the result", {
  skip_if_not_installed("haven")
  fx <- .mini_fixture()
  on.exit(unlink(c(fx$data_dir, fx$out_dir), recursive = TRUE), add = TRUE)
  res <- suppressWarnings(suppressMessages(
    merge_liss_module(fx$recipe, fx$data_dir, fx$out_dir)))
  expect_true(res$valid_for_analysis)
  expect_identical(res$provenance$recipe_version, "9.9.9")
  expect_identical(res$provenance$package_version,
                   as.character(utils::packageVersion("lissr")))
  expect_identical(res$provenance$inputs$file, "yy01a_EN_1.0p.sav")
  expect_match(res$provenance$inputs$md5, "^[a-f0-9]{32}$")
  rpt <- readLines(file.path(fx$out_dir, "yy_merge_report.txt"))
  expect_true(any(grepl("Valid for analysis: TRUE", rpt)))
  expect_true(any(grepl("md5", rpt)))
})

test_that("the overwrite guard refuses to clobber an existing output", {
  skip_if_not_installed("haven")
  fx <- .mini_fixture()
  on.exit(unlink(c(fx$data_dir, fx$out_dir), recursive = TRUE), add = TRUE)
  suppressWarnings(suppressMessages(
    merge_liss_module(fx$recipe, fx$data_dir, fx$out_dir)))
  expect_error(
    suppressWarnings(suppressMessages(
      merge_liss_module(fx$recipe, fx$data_dir, fx$out_dir,
                        overwrite = FALSE))),
    "already exists")
})

test_that("expected_release pins report, invalidate, and abort under strict", {
  skip_if_not_installed("haven")
  # release ranking keeps 1.1p; a 1.0p pin is then violated
  fx <- .mini_fixture(pin = "1.0p", extra_file = TRUE)
  on.exit(unlink(c(fx$data_dir, fx$out_dir), recursive = TRUE), add = TRUE)
  res <- suppressWarnings(suppressMessages(
    merge_liss_module(fx$recipe, fx$data_dir, fx$out_dir)))
  expect_false(res$valid_for_analysis)
  expect_identical(res$provenance$release_violations, "yy01a")
  # the multi-file ranking decision is recorded
  expect_length(res$provenance$release_decisions, 1)
  expect_identical(res$provenance$release_decisions[[1]]$selected,
                   "yy01a_EN_1.1p.sav")
  expect_error(
    suppressWarnings(suppressMessages(
      merge_liss_module(fx$recipe, fx$data_dir, fx$out_dir, strict = TRUE))),
    "expected_release")

  # a matching pin stays valid
  fx2 <- .mini_fixture(pin = "1.0p")
  on.exit(unlink(c(fx2$data_dir, fx2$out_dir), recursive = TRUE), add = TRUE)
  res2 <- suppressWarnings(suppressMessages(
    merge_liss_module(fx2$recipe, fx2$data_dir, fx2$out_dir)))
  expect_true(res2$valid_for_analysis)
})

# ---- stage 5b: output-changing recipe semantics (ca, cv, cr) ----------------

.merge_bundled <- function(mod) {
  recipe_path <- system.file("recipes", paste0(mod, "_merge_recipe.yml"),
                             package = "lissr")
  recipe <- yaml::yaml.load_file(recipe_path)
  data_dir <- file.path(tempdir(), paste0("lissr_5b_", mod))
  out_dir  <- file.path(tempdir(), paste0("lissr_5bo_", mod))
  unlink(c(data_dir, out_dir), recursive = TRUE)
  dir.create(data_dir, recursive = TRUE)
  dir.create(out_dir, recursive = TRUE)
  .gen_module_fixture(recipe, data_dir)
  res <- suppressWarnings(suppressMessages(
    merge_liss_module(recipe_path, data_dir, out_dir)))
  unlink(c(data_dir, out_dir), recursive = TRUE)
  res
}

test_that("ca wave_year is the fieldwork year; DV00 keeps the reference year", {
  skip_if_not_installed("haven")
  d <- .merge_bundled("ca")$data
  yr <- vapply(split(as.numeric(d$wave_year), as.character(d$wave_id)),
               unique, numeric(1))
  expect_equal(yr[["ca08a"]], 2008)  # fieldwork year, no longer 2007
  expect_equal(yr[["ca24i"]], 2024)
  expect_equal(yr[["ca25j"]], 2025)
  expect_true("asset_reference_year" %in% names(d))
  ref <- vapply(split(as.numeric(d$asset_reference_year),
                      as.character(d$wave_id)), unique, numeric(1))
  expect_equal(ref[["ca08a"]], 2007)
  expect_equal(ref[["ca25j"]], 2024)
  # the reference year sits one year before fieldwork in every wave
  expect_true(all(ref == yr[names(ref)] - 1))
})

test_that("cv fieldwork_month executes as fieldwork_ym mod 100", {
  skip_if_not_installed("haven")
  d <- .merge_bundled("cv")$data
  expect_true("fieldwork_month" %in% names(d))
  ok <- !is.na(d$fieldwork_ym)
  expect_gt(sum(ok), 0)
  expect_equal(as.numeric(d$fieldwork_month[ok]),
               as.numeric(d$fieldwork_ym[ok]) %% 100)
})

test_that("cr scale-break splits populate era-scoped output columns", {
  skip_if_not_installed("haven")
  d <- .merge_bundled("cr")$data
  splits <- list(
    c(pre = "attendance_pre2019_8pt", post = "attendance_post2019_6pt"),
    c(pre = "prayer_pre2019_8pt", post = "prayer_post2019_6pt"),
    c(pre = "afterlife_pre2019_4pt", post = "afterlife_post2019_3pt"))
  pre_rows  <- as.character(d$wave_id) <= "cr18k"
  post_rows <- !pre_rows
  for (sp in splits) {
    expect_true(all(c(sp[["pre"]], sp[["post"]]) %in% names(d)))
    # pre column carries the source values on pre-2019 rows, NA after
    expect_true(all(is.na(d[[sp[["pre"]]]][post_rows])))
    expect_true(all(is.na(d[[sp[["post"]]]][pre_rows])))
    expect_gt(sum(!is.na(d[[sp[["pre"]]]][pre_rows])), 0)
    expect_gt(sum(!is.na(d[[sp[["post"]]]][post_rows])), 0)
  }
})
