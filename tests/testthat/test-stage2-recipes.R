# ============================================================================
# stage 2 (v1.4 development) regressions: every bundled boundary flag rule
# produces a non-degenerate column, the re-keyed rules execute with their
# documented semantics, and file discovery handles the archive's real naming
# variants through the normalized {wave_id}_* patterns.
# ============================================================================

.load_bundled <- function(mod) {
  path <- system.file("recipes", paste0(mod, "_merge_recipe.yml"),
                      package = "lissr", mustWork = TRUE)
  yaml::yaml.load_file(path)
}

.flag_col_for <- function(rule) {
  if (rule$action %in% c("add_flag", "add_era_flag")) {
    rule$flag_name %||% rule$flag_variable %||% paste0(rule$rule_id, "_flag")
  } else if (rule$action == "add_period_flag") {
    rule$flag_column %||% paste0(rule$rule_id, "_period")
  } else if (rule$action == "structural_na") {
    rule$flag_column
  } else {
    NULL
  }
}
`%||%` <- function(x, y) if (is.null(x)) y else x

test_that("every bundled boundary flag rule yields a non-degenerate column", {
  mods <- c("ca", "cd", "cf", "ch", "ci", "cp", "cr", "cs", "cv", "cw")
  degenerate <- character(0)

  for (mod in mods) {
    recipe <- .load_bundled(mod)
    wave_ids <- vapply(recipe$wave_index, function(w) w$id, character(1))
    # two synthetic rows per wave; enough for any wave-keyed flag assignment
    df <- data.frame(wave_id = rep(wave_ids, each = 2),
                     nomem_encr = seq_len(2 * length(wave_ids)))

    for (rule in (recipe$boundary_rules %||% list())) {
      if (!(rule$action %in% c("add_flag", "add_era_flag",
                               "add_period_flag", "structural_na"))) next
      fc <- .flag_col_for(rule)
      if (is.null(fc)) next   # structural_na without flag_column is a no-op
      out <- suppressWarnings(suppressMessages(
        lissr:::exec_boundary_rule(df, rule, wave_ids, list())))
      col_ok <- fc %in% names(out$df) && any(!is.na(out$df[[fc]]))
      if (!col_ok) degenerate <- c(degenerate, paste0(mod, ":", rule$rule_id,
                                                      " (", fc, ")"))
    }
  }
  expect_identical(degenerate, character(0))
})

test_that("the re-keyed flags carry their documented values", {
  cr <- .load_bundled("cr")
  wave_ids <- vapply(cr$wave_index, function(w) w$id, character(1))
  df <- data.frame(wave_id = wave_ids, nomem_encr = seq_along(wave_ids))

  rules <- cr$boundary_rules
  by_id <- function(id) Filter(function(r) identical(r$rule_id, id), rules)[[1]]

  out <- suppressMessages(lissr:::exec_boundary_rule(df, by_id("BR01"), wave_ids, list()))$df
  expect_identical(sort(unique(out$redesign_2019)), c(0L, 1L))
  expect_true(all(out$redesign_2019[out$wave_id %in% c("cr19l", "cr25r")] == 1))
  expect_true(all(out$redesign_2019[out$wave_id %in% c("cr08a", "cr18k")] == 0))

  out <- suppressMessages(lissr:::exec_boundary_rule(df, by_id("BR02"), wave_ids, list()))$df
  expect_identical(unname(out$instrument_phase[out$wave_id == "cr08a"]), "1")
  expect_identical(unname(out$instrument_phase[out$wave_id == "cr20m"]), "2")
  expect_identical(unname(out$instrument_phase[out$wave_id == "cr25r"]), "3")

  out <- suppressMessages(lissr:::exec_boundary_rule(df, by_id("BR10"), wave_ids, list()))$df
  expect_identical(unname(out$religion_coding_era[out$wave_id == "cr14g"]), "2")

  out <- suppressMessages(lissr:::exec_boundary_rule(df, by_id("BR80"), wave_ids, list()))$df
  expect_identical(unname(out$fieldwork_season[out$wave_id == "cr08a"]), "winter")
  expect_identical(unname(out$fieldwork_season[out$wave_id == "cr15h"]), "summer")

  # ch B12 materializes under its own name, not the rid_period fallback
  ch <- .load_bundled("ch")
  ch_ids <- vapply(ch$wave_index, function(w) w$id, character(1))
  b12 <- Filter(function(r) identical(r$rule_id, "B12"), ch$boundary_rules)[[1]]
  dfc <- data.frame(wave_id = ch_ids, nomem_encr = seq_along(ch_ids))
  out <- suppressMessages(lissr:::exec_boundary_rule(dfc, b12, ch_ids, list()))$df
  expect_true("s020_wording_period" %in% names(out))
  expect_false("B12_period" %in% names(out))
  expect_identical(unname(out$s020_wording_period[out$wave_id == "ch24q"]),
                   "reworded_with_examples")
})

test_that("cw HR03 recodes cw25r pension-date categories to years", {
  cw <- .load_bundled("cw")
  hr03 <- Filter(function(r) identical(r$rule_id, "HR03_pension_dates"),
                 cw$harmonization_rules)[[1]]
  df <- data.frame(s149 = c(1, 2, 5))
  out <- suppressWarnings(suppressMessages(
    lissr:::exec_harmonization_rule(df, hr03, "cw25r", list(),
                                    c("cw24q", "cw25r"), list())))
  expect_equal(out$df$s149, c(2023, 2024, 5))
  # and it is scoped: cw24q rows are untouched
  out24 <- suppressWarnings(suppressMessages(
    lissr:::exec_harmonization_rule(df, hr03, "cw24q", list(),
                                    c("cw24q", "cw25r"), list())))
  expect_equal(out24$df$s149, c(1, 2, 5))
})

test_that("re-keyed crosswalk_rename rules coalesce into harmonized columns", {
  ch <- .load_bundled("ch")
  b06 <- Filter(function(r) identical(r$rule_id, "B06"), ch$boundary_rules)[[1]]
  df <- data.frame(wave_id = c("ch08b", "ch09c"),
                   s244 = c(3, NA), s261 = c(NA, 4))
  out <- suppressMessages(lissr:::exec_boundary_rule(
    df, b06, c("ch08b", "ch09c"), list()))$df
  expect_true("h_premium_period" %in% names(out))
  expect_equal(as.numeric(out$h_premium_period), c(3, 4))

  ci <- .load_bundled("ci")
  a09 <- Filter(function(r) identical(r$rule_id, "A-09"), ci$boundary_rules)[[1]]
  dfi <- data.frame(wave_id = c("ci13f", "ci14g"),
                    s066 = c(7, NA), s363 = c(NA, 8))
  outi <- suppressMessages(lissr:::exec_boundary_rule(
    dfi, a09, c("ci13f", "ci14g"), list()))$df
  expect_true(all(c("h_q363", "h_q364", "h_q371") %in% names(outi)))
  expect_equal(as.numeric(outi$h_q363), c(7, 8))
})

test_that("normalized patterns resolve the archive's real naming variants", {
  d <- file.path(tempdir(), "lissr_s2_disc")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  # the four variants observed in the local archive inventory
  for (f in c("aa08a_EN_1.0p.sav",   # standard
              "aa09b_2.0p_EN.sav",   # reversed order
              "aa10c_2p_EN.sav",     # short version
              "aa11d_EN_2.0.sav"))   # p-less
    file.create(file.path(d, f))

  recipe <- list(wave_index = list(
    list(id = "aa08a", year = 2008, file_pattern = "aa08a_*"),
    list(id = "aa09b", year = 2009, file_pattern = "aa09b_*"),
    list(id = "aa10c", year = 2010, file_pattern = "aa10c_*"),
    list(id = "aa11d", year = 2011, file_pattern = "aa11d_*")))
  hits <- suppressMessages(lissr:::discover_wave_files(recipe, d))
  expect_length(hits, 4)
  expect_true(all(vapply(hits, function(h) length(h$paths) == 1L, logical(1))))
})

test_that("no bundled file_pattern is extension-locked or bare", {
  mods <- c("ca", "cd", "cf", "ch", "ci", "cp", "cr", "cs", "cv", "cw")
  bad <- character(0)
  for (mod in mods) {
    recipe <- .load_bundled(mod)
    for (w in recipe$wave_index) {
      pat <- as.character(w$file_pattern %||% "")
      ok <- identical(pat, paste0(w$id, "_*")) ||
            identical(pat, "cd10c_EN_1.1p*")   # documented supersession pin
      if (!ok) bad <- c(bad, paste0(mod, ":", w$id, " '", pat, "'"))
    }
  }
  expect_identical(bad, character(0))
})
