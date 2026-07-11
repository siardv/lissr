# ============================================================================
# stage 6 (v1.4 development): adversarial fixtures for the income cleaner
# ============================================================================
# each fixture targets one repaired policy defect (C1-C9) or exercises the
# candidate-selection machinery beyond the trivial household-median winner;
# the A8 tests pin the ruleset-validator value checks. regexes matched
# against cli-rendered messages use \s+ between words (cli wraps at the
# console width).

q6 <- function(data, ...) lissr::liss_clean_income(data, verbose = FALSE, ...)

hh6 <- function(house, person, waves, nethh, code = NA_real_,
                nettoink = NA_real_, brutoink = NA_real_,
                aantalhh = NA_real_, with_donor_keys = FALSE) {
  k <- length(waves)
  df <- data.frame(
    nomem_encr = rep(person, k), nohouse_encr = rep(house, k),
    wavenr = waves, nethh = nethh, nethh_min = rep_len(code, k),
    nettoink = rep_len(nettoink, k), brutoink = rep_len(brutoink, k),
    aantalhh = rep_len(aantalhh, k), stringsAsFactors = FALSE
  )
  if (with_donor_keys) {
    df$positiehh <- 1; df$belbezig <- 1; df$leeftijd <- 40; df$oplmet <- 4
    df$gebjaar <- NA_real_
  }
  df
}

r6 <- function(res, person, wave, col) {
  d <- res$data
  d[[col]][d$nomem_encr == person & d$wavenr == wave]
}

# ---- C2: anchor outside the admissible range voids instead of midpoint ------

test_that("C2: a below-minimum household is voided, not midpoint-imputed", {
  # every admissible candidate for the 10x entry error 50000 is filtered
  # as below min_income (8000); the old fallback imputed the range
  # midpoint 79000, fifteen times the household's truth
  fix <- hh6(1, 101, 1:4, c(5000, 5200, 50000, 5100))
  res <- q6(fix)
  expect_identical(r6(res, 101, 3, "nethh_clean_status"), "voided:D06")
  expect_true(is.na(r6(res, 101, 3, "nethh")))
  row <- res$decisions[res$decisions$rule_id == "D06" &
                         res$decisions$action == "set_na", ]
  expect_identical(nrow(row), 1L)
  expect_match(row$justification, "outside the admissible range")
  expect_match(row$justification, "voided")
  # the untouched waves survive as observed
  expect_identical(r6(res, 101, 1, "nethh"), 5000)
})

test_that("C2: the midpoint fallback still serves cells with an in-range anchor", {
  # no candidate survives (all generators disabled), but the household
  # median 25000 lies inside the bracket bounds [24000, 36000], so the
  # bounded midpoint fallback remains defensible and applies
  fix <- hh6(2, 201, 1:4, c(25000, 24500, 58000, 25500), code = 4)
  res <- q6(fix, disable = c("C01", "C02", "C03", "C04", "C05"))
  expect_identical(r6(res, 201, 3, "nethh_clean_status"), "corrected:D07")
  expect_identical(r6(res, 201, 3, "nethh"), 30000)
  row <- res$decisions[res$decisions$rule_id == "D07" &
                         res$decisions$action == "correct", ]
  expect_identical(row$candidate_source, "range_midpoint")
})

# ---- C3: single-person echoes and zero gross income --------------------------

test_that("C3: a single-person household's echo is legitimate and spared", {
  single <- hh6(3, 301, 1:2, c(9000, 9100), nettoink = 8950, aantalhh = 1)
  multi <- hh6(4, 401, 1:2, c(9000, 9100), nettoink = 8950, aantalhh = 3)
  res <- q6(rbind(single, multi))
  # the single-person report survives untouched
  expect_true(is.na(r6(res, 301, 1, "nethh_clean_status")))
  expect_identical(r6(res, 301, 1, "nethh"), 9000)
  # the same numbers in a three-person household are a keying error
  expect_identical(r6(res, 401, 1, "nethh_clean_status"), "voided:D04")
  expect_true(is.na(r6(res, 401, 1, "nethh")))
  d <- res$decisions
  expect_match(d$evidence[d$rule_id == "D04"][1], "household of 3")
})

test_that("C3: an unknown household size spares the echo", {
  fix <- hh6(5, 501, 1:2, c(9000, 9100), nettoink = 8950, aantalhh = NA_real_)
  res <- q6(fix)
  expect_true(is.na(r6(res, 501, 1, "nethh_clean_status")))
  expect_identical(r6(res, 501, 1, "nethh"), 9000)
})

test_that("C3: a gross personal income of zero no longer triggers D03", {
  zero <- hh6(6, 601, 1:2, c(80, NA), brutoink = 0)
  pos <- hh6(7, 701, 1:2, c(80, NA), brutoink = 30000)
  res <- q6(rbind(zero, pos))
  expect_true(is.na(r6(res, 601, 1, "nethh_clean_status")))
  expect_identical(r6(res, 601, 1, "nethh"), 80)
  expect_identical(r6(res, 701, 1, "nethh_clean_status"), "voided:D03")
})

# ---- C4: bracket-code column classification ----------------------------------

test_that("C4: one stray sentinel no longer disables bracket mapping", {
  # nine code-4 waves plus one stray 99: the old max()-gate silently
  # skipped the whole column; now the codes map per value, the stray is
  # warned about, and D07 still catches the bound violation
  fix <- hh6(8, 801, 1:10,
             c(30000, 30500, 58000, 30200, 29800, 30100, 30400, 29900,
               30300, 31000),
             code = c(rep(4, 9), 99))
  expect_warning(res <- q6(fix), "treated\\s+as\\s+missing\\s+brackets")
  expect_identical(r6(res, 801, 3, "nethh_clean_status"), "corrected:D07")
  v <- r6(res, 801, 3, "nethh")
  expect_true(v >= 24000 && v <= 36000)
  # the stray-coded row has no bounds, so no bound rule touches it
  expect_true(is.na(r6(res, 801, 10, "nethh_clean_status")))
})

test_that("C4: an ambiguous code/euro mixture is declined loudly", {
  fix <- hh6(9, 901, 1:4, c(30000, 30200, 30100, 30300),
             code = c(4, 24000, 4, 24000))
  expect_warning(res <- q6(fix), "mapping\\s+declined")
  # nothing is corrected off phantom bounds
  st <- res$data$nethh_clean_status[res$data$nomem_encr == 901]
  expect_true(all(is.na(st)))
})

test_that("C4: a euro-bounds column still passes through silently", {
  fix <- hh6(10, 1001, 1:3, c(30000, 30200, 30100))
  fix$nethh_min <- c(24000, 24000, 24000)
  fix$nethh_max <- c(36000, 36000, 36000)
  expect_no_warning(res <- q6(fix))
  st <- res$data$nethh_clean_status[res$data$nomem_encr == 1001]
  expect_true(all(is.na(st)))
})

# ---- C5: missing household ids and background joins ---------------------------

test_that("C5: rows with NA household ids fall back to person-id grouping", {
  fix <- hh6(11, 1101, 1:6, c(25000, 25500, 2600, 26000, 25800, 26200))
  fix$nohouse_encr <- NA_real_
  res <- q6(fix)
  # the decimal-shift error is corrected despite the missing household id
  expect_identical(r6(res, 1101, 3, "nethh_clean_status"), "corrected:D06")
  expect_identical(res$summary$n_hid_fallback, 6L)
  expect_identical(res$summary$n_ungroupable, 0L)
})

test_that("C5: a year-keyed background joins on the correct scale", {
  inc <- data.frame(nomem_encr = c(1, 1, 2), nohouse_encr = c(1, 1, 2),
                    wavenr = c(1, 2, 1), nethh = c(21000, 21500, 30000))
  # calendar-year keys: the old yyyymm assumption produced wavenr
  # -1985 and an all-NA join that was logged as success
  bck_year <- data.frame(nomem_encr = c(1, 1, 2),
                         wave = c(2008, 2009, 2008),
                         oplmet = c(4, 6, 3))
  res <- q6(inc, background = bck_year)
  expect_identical(res$data$oplmet, c(4, 6, 3))
  # annual-wavenr keys are recognized directly
  bck_wnr <- data.frame(nomem_encr = c(1, 1, 2), wave = c(1, 2, 1),
                        belbezig = c(1, 2, 3))
  res2 <- q6(inc, background = bck_wnr)
  expect_identical(res2$data$belbezig, c(1, 2, 3))
})

test_that("C5: a zero-match background join warns instead of passing silently", {
  inc <- data.frame(nomem_encr = c(1, 2), nohouse_encr = c(1, 2),
                    wavenr = c(1, 1), nethh = c(21000, 30000))
  bck <- data.frame(nomem_encr = c(99, 98), wave = c(200801, 200801),
                    oplmet = c(4, 5))
  expect_warning(res <- q6(inc, background = bck), "matched\\s+0")
  expect_true(all(is.na(res$data$oplmet)))
})

# ---- C6: enable_only is scoped per section ------------------------------------

test_that("C6: enable_only on a detection rule keeps corrections running", {
  fix <- rbind(
    hh6(12, 1201, 1:6, c(25000, 25500, 2600, 26000, 25800, 26200)),
    hh6(13, 1301, 1:4, c(1600, 1650, 1700, 1680), code = 3),
    hh6(14, 1401, 1:3, c(-21000, 21500, 22000))
  )
  res <- q6(fix, enable_only = "D06")
  # D06 detects and, with the correction machinery still enabled, the
  # cell is corrected rather than voided (the old all-section
  # restriction disabled C01-C06 and F01 too)
  expect_identical(r6(res, 1201, 3, "nethh_clean_status"), "corrected:D06")
  v <- r6(res, 1201, 3, "nethh")
  expect_true(v >= 25000 && v <= 26500)
  # other detection rules are off: the D05 low-magnitude household stays
  st <- res$data$nethh_clean_status[res$data$nomem_encr == 1301]
  expect_true(all(is.na(st)))
  # preparation is untouched: the sign error still rectifies
  expect_identical(r6(res, 1401, 1, "nethh_clean_status"), "rectified:P03")
  expect_match(paste(res$summary$overrides, collapse = " "),
               "scoped to detection_rules")
})

# ---- C7: F01 disposition ------------------------------------------------------

test_that("C7: F01 dispositions void, winsorise, or flag a lone cap exceedance", {
  # a single-observation household never enters the household stage, so
  # the finalization rule is the only line that touches it
  fix <- hh6(15, 1501, 1, 200000)

  res_v <- q6(fix)
  expect_identical(r6(res_v, 1501, 1, "nethh_clean_status"), "capped:F01")
  expect_true(is.na(r6(res_v, 1501, 1, "nethh")))

  res_w <- q6(fix, params = list(F01 = list(disposition = "winsorise")))
  expect_identical(r6(res_w, 1501, 1, "nethh_clean_status"), "winsorised:F01")
  expect_identical(r6(res_w, 1501, 1, "nethh"), 150000)
  expect_identical(res_w$summary$n_winsorised, 1L)
  d <- res_w$decisions
  expect_identical(d$corrected[d$action == "cap_winsorise"], 150000)

  res_f <- q6(fix, params = list(F01 = list(disposition = "flag")))
  expect_true(is.na(r6(res_f, 1501, 1, "nethh_clean_status")))
  expect_identical(r6(res_f, 1501, 1, "nethh"), 200000)
  expect_identical(res_f$summary$n_cap_flagged, 1L)
  expect_true(all(res_f$decisions$action[res_f$decisions$rule_id == "F01"] ==
                    "cap_flag"))
})

test_that("C7: winsorised values still satisfy the ledger-coverage invariant", {
  fix <- hh6(16, 1601, 1, 200000)
  res <- q6(fix, params = list(F01 = list(disposition = "winsorise")))
  before <- res$data$nethh_observed
  after <- res$data$nethh
  diff_rows <- which(xor(is.na(before), is.na(after)) |
                       (!is.na(before) & !is.na(after) & before != after))
  mut <- res$decisions[res$decisions$applied &
                         res$decisions$action %in%
                           c("correct", "set_na", "rectify_sign", "cap_na",
                             "cap_winsorise"), ]
  expect_setequal(diff_rows, unique(mut$row))
})

# ---- selection machinery beyond the trivial winner -----------------------------

test_that("C02 category_midpoint wins when the household center is out of range", {
  # others' median 20500 falls below the bracket floor 24000, so every
  # series-based candidate is filtered and the respondent's own bracket
  # answer supplies the correction
  fix <- hh6(17, 1701, 1:4, c(20000, 20500, 58000, 21000), code = 4)
  res <- q6(fix)
  row <- res$decisions[res$decisions$action == "correct" &
                         res$decisions$applied, ]
  expect_identical(nrow(row), 1L)
  expect_identical(row$candidate_source, "category_midpoint")
  expect_identical(r6(res, 1701, 3, "nethh"), 30000)
})

test_that("C04 temporal smoothing wins once the household center is disabled", {
  fix <- hh6(18, 1801, 1:5, c(20000, 21000, 90000, 22000, 23000))
  res <- q6(fix, disable = "C01")
  row <- res$decisions[res$decisions$action == "correct" &
                         res$decisions$applied, ]
  expect_identical(row$candidate_source, "temporal_smoothing")
  expect_identical(r6(res, 1801, 3, "nethh"), 21500)
})

test_that("C05 donor pool wins for a short contaminated series", {
  target <- hh6(19, 1901, 1:2, c(25000, 99000), with_donor_keys = TRUE,
                aantalhh = 2)
  donors <- hh6(20, 2001, 1:3, c(30000, 30500, 31000),
                with_donor_keys = TRUE, aantalhh = 2)
  res <- q6(rbind(target, donors), disable = c("C01", "C04"))
  row <- res$decisions[res$decisions$action == "correct" &
                         res$decisions$applied, ]
  expect_identical(nrow(row), 1L)
  expect_identical(row$candidate_source, "donor_pool")
  # the pool is every key-matched finite row except the cell itself,
  # including the target household's other wave: median(25000, 30000,
  # 30500, 31000)
  expect_identical(r6(res, 1901, 2, "nethh"), 30250)
})

# ---- C1: the selection anchor is dispatched from the ruleset -------------------

test_that("C1: selection.anchor household_mean changes execution and the report", {
  fix <- hh6(21, 2101, 1:4, c(20000, 21000, 25000, 200000))

  rs <- yaml::read_yaml(system.file("cleaning", "income_cleaning_rules.yml",
                                    package = "lissr"))
  rs$selection$anchor <- "household_mean"
  path <- tempfile(fileext = ".yml")
  yaml::write_yaml(rs, path)

  res_med <- q6(fix)
  res_mean <- q6(fix, ruleset = path)
  # median anchor 21000 selects the household median; mean anchor 22000
  # selects the household mean candidate
  expect_identical(r6(res_med, 2101, 4, "nethh"), 21000)
  expect_identical(r6(res_mean, 2101, 4, "nethh"), 22000)
  row <- res_mean$decisions[res_mean$decisions$action == "correct" &
                              res_mean$decisions$applied, ]
  expect_identical(row$candidate_source, "household_mean")
  expect_match(row$justification, "household_mean")

  # the generated report names the executed anchor
  dir <- file.path(tempdir(), "stage6-anchor-report")
  paths <- lissr::liss_cleaning_report(res_mean, dir, verbose = FALSE)
  rpt <- readLines(paths$report)
  expect_true(any(grepl("closest to the household_mean", rpt)))
})

# ---- C9: equivalisation guards --------------------------------------------------

test_that("C9: zero-adult compositions and length mismatches are guarded", {
  # zero adults: household of two children pushes the oecd divisor to
  # 1 + 0.5 * (0 - 1) + 0.3 * 2 = 1.1, but the composition is invalid
  expect_warning(
    out <- lissr::liss_equivalise_income(30000, 2, 2, scale = "oecd_modified"),
    "invalid\\s+household\\s+composition"
  )
  expect_true(is.na(out))
  # intermediate lengths are an error, not silent recycling
  expect_error(
    lissr::liss_equivalise_income(c(1, 2, 3, 4, 5) * 10000, c(2, 3),
                                  verbose = FALSE),
    "length"
  )
  # length-1 composition still broadcasts
  expect_identical(
    lissr::liss_equivalise_income(c(10000, 20000), 4, 0, scale = "sqrt",
                                  verbose = FALSE),
    c(5000, 10000)
  )
})

# ---- A8: ruleset-validator value checks ------------------------------------------

test_that("A8: disposition, scope, and stage values are controlled", {
  rs <- lissr::liss_cleaning_ruleset()
  bad <- rs
  bad$detection_rules[[2]]$disposition <- "delete"
  bad$detection_rules[[3]]$scope <- "house"
  bad$detection_rules[[4]]$stage <- "prelim"
  v <- lissr::validate_cleaning_ruleset(bad, quiet = TRUE)
  expect_false(v$valid)
  expect_true(any(grepl("disposition 'delete'", v$errors)))
  expect_true(any(grepl("scope 'house'", v$errors)))
  expect_true(any(grepl("stage 'prelim'", v$errors)))
})

test_that("A8: the finalization disposition and selection anchors are controlled", {
  rs <- lissr::liss_cleaning_ruleset()
  bad <- rs
  bad$finalization_rules[[1]]$disposition <- "discard"
  bad$selection$anchor <- "household_mode"
  bad$selection$fallback_anchor <- "zero"
  v <- lissr::validate_cleaning_ruleset(bad, quiet = TRUE)
  expect_false(v$valid)
  expect_true(any(grepl("disposition 'discard'", v$errors)))
  expect_true(any(grepl("selection.anchor", v$errors)))
  expect_true(any(grepl("selection.fallback_anchor", v$errors)))
})

test_that("A8: methods, consensus, and nested thresholds are validated", {
  rs <- lissr::liss_cleaning_ruleset()
  bad <- rs
  for (i in seq_along(bad$detection_rules)) {
    if (bad$detection_rules[[i]]$rule_id == "D09") {
      bad$detection_rules[[i]]$params$methods <- list("iqr", "lof")
      bad$detection_rules[[i]]$params$consensus <- 3
      bad$detection_rules[[i]]$params$thresholds <- list(iqr = -1, zscore = 2)
    }
  }
  v <- lissr::validate_cleaning_ruleset(bad, quiet = TRUE)
  expect_false(v$valid)
  expect_true(any(grepl("unknown method", v$errors)))
  expect_true(any(grepl("consensus", v$errors)))
  expect_true(any(grepl("thresholds.iqr", v$errors)))
  expect_true(any(grepl("not in", v$warnings)))
})

test_that("A8: an invalid anchor is rejected at load time", {
  rs <- yaml::read_yaml(system.file("cleaning", "income_cleaning_rules.yml",
                                    package = "lissr"))
  rs$selection$anchor <- "household_mode"
  path <- tempfile(fileext = ".yml")
  yaml::write_yaml(rs, path)
  expect_error(lissr::liss_cleaning_ruleset(path), "invalid")
})
