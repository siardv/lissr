# regression and unit tests for the income-cleaning framework
# (liss_clean_executors.R + liss_clean_income.R). kernels are pure and
# run everywhere; engine tests use a synthetic fixture with one
# household per known error pattern.

num <- function(x) as.numeric(unclass(x))

# ---- fixture ---------------------------------------------------------------
# one household per error signature; ids encode the scenario.
make_income_fixture <- function() {
  hh <- function(house, person, waves, nethh, code = NA_real_,
                 nettoink = NA_real_, brutoink = NA_real_) {
    k <- length(waves)
    data.frame(
      nomem_encr = rep(person, k), nohouse_encr = rep(house, k),
      wavenr = waves, nethh = nethh, nethh_min = rep_len(code, k),
      nettoink = rep_len(nettoink, k), brutoink = rep_len(brutoink, k),
      aantalhh = 2, positiehh = 1, belbezig = 1, leeftijd = 40,
      oplmet = 4, gebjaar = NA_real_, stringsAsFactors = FALSE
    )
  }
  rbind(
    hh(1, 101, 1:6, c(25000, 25500, 2600, 26000, 25800, 26200)),  # D06 decimal shift
    hh(2, 201, 1:4, c(1600, 1650, 1700, 1680), code = 3),         # D05 monthly-for-annual
    hh(3, 301, 1:4, c(30000, 58000, 31000, 30500), code = 4),     # D07 bound violation
    hh(4, 401, 1:3, c(120000, 160000, 130000)),                   # D08 cap exceedance
    hh(5, 501, 1:6, c(20000, 21000, 20500, 42000, 21500, 20800)), # D09 robust consensus
    hh(6, 601, 1:3, c(20000, 20200, 34000)),                      # D10 extreme z
    hh(7, 701, 1:2, c(5, NA)),                                    # D02 absolute floor
    hh(8, 801, 1:2, c(80, NA), brutoink = 30000),                 # D03 contextual floor
    hh(9, 901, 1:2, c(2000, NA), nettoink = 1985),                # D04 personal echo
    hh(10, 1001, 1:3, c(-21000, 21500, 22000)),                   # P03 sign entry
    hh(11, 1101, 1:3, c(9999999999, 23000, 23500)),               # P06 sentinel
    hh(12, 1201, 1:4, c(30000, 30500, 31000, 30200)),             # control
    hh(13, 1301, 1:5, c(30000, 30100, 30200, 29900, 27600))       # tight, mild dip
  )
}

clean_quiet <- function(data, ...) {
  lissr::liss_clean_income(data, verbose = FALSE, ...)
}

cell <- function(res, person, wave, col) {
  d <- res$data
  d[[col]][d$nomem_encr == person & d$wavenr == wave]
}

# ---- kernels: magnitude and volatility --------------------------------------

test_that("power10_magnitude returns full-length output with NA at invalid positions", {
  out <- lissr:::power10_magnitude(c(74, 76, 0, -5, NA, 1000))
  expect_identical(out, c(10, 100, NA, NA, NA, 1000))
  expect_identical(lissr:::power10_magnitude(c(74, 76), mode = "floor"),
                   c(10, 10))
  expect_identical(lissr:::power10_magnitude(c(74, 76), mode = "ceiling"),
                   c(100, 100))
})

test_that("local_log_volatility matches a hand-computed jump and skips NA gaps", {
  expect_identical(lissr:::local_log_volatility(c(10, 10, 10)), c(0, 0, 0))
  expect_equal(lissr:::local_log_volatility(c(100, 1000, 100)),
               rep(signif(log(10), 2), 3))
  expect_equal(lissr:::local_log_volatility(c(100, NA, 1000, 100)),
               c(2.3, 0, 2.3, 2.3))
})

test_that("robust_zscore propagates NA and falls back to sd when mad is zero", {
  z <- lissr:::robust_zscore(c(1, 1, 1, 10))
  expect_equal(z[4], 2)  # mad 0, sd 4.5, (10 - 1) / 4.5
  expect_true(is.na(lissr:::robust_zscore(c(1, 2, NA))[3]))
  expect_identical(lissr:::robust_zscore(c(3, 3, 3)), c(0, 0, 0))
})

# ---- kernels: detectors ------------------------------------------------------

test_that("detect_univariate_outliers needs four finite values and honors thresholds", {
  none <- lissr:::detect_univariate_outliers(c(1, 2, 3), method = "iqr")
  expect_false(any(none))
  expect_true(all(is.na(attr(none, "bounds"))))

  x <- c(10, 11, 12, 13, 100)
  flag <- lissr:::detect_univariate_outliers(x, method = "iqr")
  expect_identical(which(flag), 5L)
  loose <- lissr:::detect_univariate_outliers(x, method = "iqr",
                                              threshold = 50)
  expect_false(any(loose))
})

test_that("detect_outliers_consensus counts method agreement", {
  x <- c(10, 11, 12, 13, 100)
  res <- lissr:::detect_outliers_consensus(x, methods = c("iqr", "mad"),
                                           consensus = 1)
  expect_identical(res$counts[5], 2L)
  expect_identical(res$outliers, res$counts >= 1)
  res2 <- lissr:::detect_outliers_consensus(x, methods = c("iqr", "mad"),
                                            consensus = 2)
  expect_identical(which(res2$outliers), 5L)
})

# ---- kernels: candidate generators -------------------------------------------

test_that("wma_impute_at matches linear weighting, widens, and needs support", {
  expect_equal(lissr:::wma_impute_at(c(10, 20, 30, 40, 50), 3, k = 2), 30)
  expect_equal(lissr:::wma_impute_at(c(10, 20, 30, 40, 50), 3, k = 2,
                                     weighting = "simple"), 30)
  # both immediate neighbours missing: window widens to the ends
  expect_equal(lissr:::wma_impute_at(c(10, NA, 99, NA, 40), 3, k = 1), 25)
  # fewer than two finite values elsewhere: no trend-based candidate
  expect_true(is.na(lissr:::wma_impute_at(c(NA, 5, NA), 2)))
  expect_equal(lissr:::wma_impute_at(c(5, 7, NA), 3, k = 2), (1 * 5 + 2 * 7) / 3)
})

test_that("donor_pool_value narrows hierarchically and never self-donates", {
  d <- data.frame(v = c(10, 20, 30, 40), a = c(1, 1, 2, 2), b = c(1, 2, 1, 2))
  t12 <- data.frame(a = 1, b = 2)
  expect_equal(lissr:::donor_pool_value(t12, d, "v", c("a", "b")), 20)
  # excluding the matching donor keeps the wider pool from key a
  expect_equal(lissr:::donor_pool_value(t12, d, "v", c("a", "b"),
                                        exclude_row = 2L), 10)
  # min_donors blocks a narrowing that would leave a single donor
  expect_equal(lissr:::donor_pool_value(t12, d, "v", c("a", "b"),
                                        min_donors = 2), 15)
  # NA target key is skipped
  tna <- data.frame(a = NA_real_, b = 1)
  expect_equal(lissr:::donor_pool_value(tna, d, "v", c("a", "b")), 20)
})

test_that("category_bounds_from_codes maps valid codes and NAs the rest", {
  lo <- c(0, 8000, 16000, 24000, 36000, 48000, 60000)
  hi <- c(8000, 16000, 24000, 36000, 48000, 60000, 120000)
  m <- lissr:::category_bounds_from_codes(c(1, 7, 0, 8, NA), lo, hi)
  expect_identical(m$lower, c(0, 60000, NA, NA, NA))
  expect_identical(m$upper, c(8000, 120000, NA, NA, NA))
})

test_that("filter_candidates dedupes to the first source and select picks the closest", {
  flt <- lissr:::filter_candidates(c(5, 50, 50, 500), c("a", "b", "c", "d"),
                                   10, 400)
  expect_identical(flt$values, 50)
  expect_identical(flt$sources, "b")
  sel <- lissr:::select_candidate(c(40, 80), c("x", "y"), anchor = 60)
  expect_identical(sel$value, 40)  # equal distance, first wins
  expect_identical(sel$source, "x")
})

test_that("bound_deviation_ratio ranks violations", {
  expect_equal(lissr:::bound_deviation_ratio(90000, 24000, 36000), 2.5)
  expect_equal(lissr:::bound_deviation_ratio(10000, 24000, 36000), 2.4)
  expect_equal(lissr:::bound_deviation_ratio(30000, 24000, 36000), 1)
  expect_identical(lissr:::bound_deviation_ratio(-5, 24000, 36000), Inf)
})

test_that("equivalise kernel reproduces the analysis-script scale and guards composition", {
  # stand_inc = nethh / ((aantalhh - aantalki + 0.8 * aantalki)^0.5)
  expect_equal(lissr:::equivalise_income_kernel(30000, 3, 1),
               30000 / sqrt(2.8))
  expect_equal(lissr:::equivalise_income_kernel(30000, 3, 1,
                                                scale = "oecd_modified"),
               30000 / 1.8)
  expect_equal(lissr:::equivalise_income_kernel(30000, 4, 0, scale = "sqrt"),
               15000)
  expect_true(is.na(lissr:::equivalise_income_kernel(30000, 0, 0)))
  expect_true(is.na(lissr:::equivalise_income_kernel(30000, 2, 3)))
})

# ---- ruleset loading, validation, overrides -----------------------------------

test_that("the packaged ruleset loads, validates, and prints", {
  rs <- lissr::liss_cleaning_ruleset()
  expect_s3_class(rs, "liss_cleaning_ruleset")
  val <- lissr::validate_cleaning_ruleset(rs, quiet = TRUE)
  expect_true(val$valid)
  expect_length(val$warnings, 0)
  expect_no_error(print(rs))
})

test_that("validation rejects schema violations and warns on authoring slips", {
  rs <- lissr::liss_cleaning_ruleset()
  bad <- rs
  bad$detection_rules[[1]]$action <- "bogus_action"
  bad$detection_rules[[2]]$rule_id <- bad$detection_rules[[1]]$rule_id
  bad$constraints$min_income <- 999999
  v <- lissr::validate_cleaning_ruleset(bad, quiet = TRUE)
  expect_false(v$valid)
  expect_true(any(grepl("unknown detection_rules action", v$errors)))
  expect_true(any(grepl("duplicate rule_id", v$errors)))
  expect_true(any(grepl("min_income must be below", v$errors)))

  slips <- rs
  slips$detection_rules[[1]]$applies_to_waves <- list("w1")
  slips$detection_rules[[2]]$references <- list("nosuchref")
  v2 <- lissr::validate_cleaning_ruleset(slips, quiet = TRUE)
  expect_true(v2$valid)
  expect_true(any(grepl("unrecognized key", v2$warnings)))
  expect_true(any(grepl("nosuchref", v2$warnings)))
})

test_that("overrides toggle rules, merge params, and are recorded", {
  rs <- lissr::liss_cleaning_ruleset()
  rs2 <- lissr:::apply_ruleset_overrides(
    rs, income_cap = 175000, disable = c("D10"),
    params = list(D06 = list(volatility_min = 0.7))
  )
  expect_identical(rs2$constraints$income_cap, 175000)
  d10 <- Filter(function(r) r$rule_id == "D10", rs2$detection_rules)[[1]]
  expect_false(isTRUE(d10$enabled))
  d06 <- Filter(function(r) r$rule_id == "D06", rs2$detection_rules)[[1]]
  expect_identical(d06$params$volatility_min, 0.7)
  expect_true(any(grepl("income_cap = 175000", rs2$meta$overrides)))
  expect_error(lissr:::apply_ruleset_overrides(rs, disable = "Z99"),
               "unknown rule id")
})

# ---- engine: one scenario per rule ---------------------------------------------

test_that("D06 corrects a decimal-shift wave toward the household center", {
  res <- clean_quiet(make_income_fixture())
  expect_identical(cell(res, 101, 3, "nethh_clean_status"), "corrected:D06")
  v <- cell(res, 101, 3, "nethh")
  expect_true(v >= 25000 && v <= 26500)
  expect_identical(cell(res, 101, 3, "nethh_observed"), 2600)
  d <- res$decisions
  row <- d[d$rule_id == "D06" & d$applied, ]
  expect_identical(nrow(row), 1L)
  expect_identical(row$observed, 2600)
  expect_match(row$evidence, "modal magnitude")
  expect_match(row$justification, "closest of")
})

test_that("D05 scales a whole low-magnitude household inside its brackets", {
  res <- clean_quiet(make_income_fixture())
  st <- res$data$nethh_clean_status[res$data$nomem_encr == 201]
  expect_identical(st, rep("corrected:D05", 4))
  expect_identical(res$data$nethh[res$data$nomem_encr == 201],
                   c(16000, 16500, 17000, 16800))
  d <- res$decisions
  expect_identical(unique(d$candidate_source[d$rule_id == "D05"]), "scale_x10")
})

test_that("D07 pulls a bracket violation back inside the reported bounds", {
  res <- clean_quiet(make_income_fixture())
  expect_identical(cell(res, 301, 2, "nethh_clean_status"), "corrected:D07")
  d <- res$decisions
  row <- d[d$rule_id == "D07" & d$applied, ]
  expect_identical(row$observed, 58000)
  expect_identical(row$valid_min, 24000)
  expect_identical(row$valid_max, 36000)
  v <- cell(res, 301, 2, "nethh")
  expect_true(v >= 24000 && v <= 36000)
})

test_that("D08 corrects a cap exceedance the magnitude test cannot see", {
  res <- clean_quiet(make_income_fixture())
  expect_identical(cell(res, 401, 2, "nethh_clean_status"), "corrected:D08")
  expect_identical(cell(res, 401, 2, "nethh"), 125000)
  row <- res$decisions[res$decisions$rule_id == "D08" &
                         res$decisions$applied, ]
  expect_match(row$evidence, "exceeds the income cap")
})

test_that("D09 needs both robust-method agreement and volatility confirmation", {
  res <- clean_quiet(make_income_fixture())
  expect_identical(cell(res, 501, 4, "nethh_clean_status"), "corrected:D09")
  row <- res$decisions[res$decisions$rule_id == "D09" &
                         res$decisions$applied, ]
  expect_identical(nrow(row), 1L)
  expect_identical(row$observed, 42000)
  expect_match(row$evidence, "iqr|mad")
  # the household's remaining mild dispersion is not chased
  st <- res$data$nethh_clean_status[res$data$nomem_encr == 501]
  expect_identical(sum(!is.na(st)), 1L)
})

test_that("D10 catches an extreme modified z-score in a three-wave household", {
  res <- clean_quiet(make_income_fixture())
  expect_identical(cell(res, 601, 3, "nethh_clean_status"), "corrected:D10")
  row <- res$decisions[res$decisions$rule_id == "D10" &
                         res$decisions$applied, ]
  expect_identical(row$observed, 34000)
  expect_match(row$evidence, "modified z-score")
})

test_that("D10's relative-deviation gate spares tight households with mild dips", {
  # a small MAD pushes the 27600 dip past |z| > 3, but it sits only 8%
  # from the household median; the gate must leave it alone
  res <- clean_quiet(make_income_fixture())
  st <- res$data$nethh_clean_status[res$data$nomem_encr == 1301]
  expect_true(all(is.na(st)))
  expect_identical(res$data$nethh[res$data$nomem_encr == 1301],
                   c(30000, 30100, 30200, 29900, 27600))
  # loosening the gate makes the same cell correctable, proving the
  # parameter is live
  res2 <- clean_quiet(make_income_fixture(),
                      params = list(D10 = list(min_relative_deviation = 0.05)))
  expect_identical(cell(res2, 1301, 5, "nethh_clean_status"),
                   "corrected:D10")
})

test_that("D02, D03, and D04 void unrecoverable values with per-cell evidence", {
  res <- clean_quiet(make_income_fixture())
  expect_identical(cell(res, 701, 1, "nethh_clean_status"), "voided:D02")
  expect_identical(cell(res, 801, 1, "nethh_clean_status"), "voided:D03")
  expect_identical(cell(res, 901, 1, "nethh_clean_status"), "voided:D04")
  expect_true(all(is.na(c(cell(res, 701, 1, "nethh"),
                          cell(res, 801, 1, "nethh"),
                          cell(res, 901, 1, "nethh")))))
  d <- res$decisions
  expect_match(d$evidence[d$rule_id == "D03"], "gross personal income")
  expect_match(d$evidence[d$rule_id == "D04"], "personal net income")
})

test_that("P03 rectifies a sign-entry error and ledgers the flip", {
  res <- clean_quiet(make_income_fixture())
  expect_identical(cell(res, 1001, 1, "nethh_clean_status"), "rectified:P03")
  expect_identical(cell(res, 1001, 1, "nethh"), 21000)
  row <- res$decisions[res$decisions$rule_id == "P03", ]
  expect_identical(row$observed, -21000)
  expect_identical(row$corrected, 21000)
})

test_that("P06 sweeps configured sentinel codes and declared SPSS user-missing values", {
  res <- clean_quiet(make_income_fixture())
  expect_identical(cell(res, 1101, 1, "nethh_clean_status"), "voided:P06")
  expect_true(is.na(cell(res, 1101, 1, "nethh")))

  skip_if_not_installed("haven")
  df <- data.frame(nomem_encr = 1:3, nohouse_encr = 1, wavenr = 1:3)
  df$nethh <- haven::labelled_spss(c(21000, 22000, 8888),
                                   labels = c(dk = 8888), na_values = 8888)
  res2 <- clean_quiet(df)
  expect_true(is.na(res2$data$nethh[3]))
  expect_identical(res2$data$nethh_clean_status[3], "voided:P06")
  expect_identical(res2$data$nethh_observed[3], 8888)
})

test_that("D01 voids corrupt euro bounds without touching the income value", {
  df <- data.frame(
    nomem_encr = 1:3, nohouse_encr = 1, wavenr = 1:3,
    nethh = c(28000, 29000, 28500),
    nethh_min = c(24000, -5000, 26000),
    nethh_max = c(36000, 36000, 500000)
  )
  res <- clean_quiet(df)
  d <- res$decisions
  expect_identical(sort(d$row[d$rule_id == "D01"]), c(2L, 3L))
  expect_identical(unique(d$action[d$rule_id == "D01"]), "void_bounds")
  expect_identical(res$data$nethh, c(28000, 29000, 28500))
})

test_that("a stable control household is left untouched", {
  res <- clean_quiet(make_income_fixture())
  ctrl <- res$data[res$data$nomem_encr == 1201, ]
  expect_true(all(is.na(ctrl$nethh_clean_status)))
  expect_identical(ctrl$nethh, ctrl$nethh_observed)
  expect_false(any(res$decisions$person_id == "1201" &
                     res$decisions$action != "flag"))
})

test_that("D11 annotates dataset-level extremes without modifying them", {
  res <- clean_quiet(make_income_fixture())
  h4 <- res$data[res$data$nomem_encr == 401, ]
  expect_true(all(!is.na(h4$nethh_dataset_flag)))
  ctrl <- res$data[res$data$nomem_encr == 1201, ]
  expect_true(all(is.na(ctrl$nethh_dataset_flag)))
  # flags never change values: flagged cells equal their post-correction state
  d <- res$decisions
  fl <- d[d$action == "flag", ]
  expect_identical(res$data$nethh[fl$row], fl$observed)
})

# ---- engine: ledger and mode invariants ------------------------------------------

test_that("the observed column preserves the input and the ledger covers every change", {
  fix <- make_income_fixture()
  res <- clean_quiet(fix)
  expect_identical(res$data$nethh_observed, num(fix$nethh))

  before <- res$data$nethh_observed
  after <- res$data$nethh
  diff_rows <- which(xor(is.na(before), is.na(after)) |
                       (!is.na(before) & !is.na(after) & before != after))
  mut <- res$decisions[res$decisions$applied &
                         res$decisions$variable == "nethh" &
                         res$decisions$action %in%
                           c("correct", "set_na", "rectify_sign", "cap_na"), ]
  expect_setequal(diff_rows, unique(mut$row))
  st <- res$data$nethh_clean_status
  expect_setequal(which(!is.na(st)), diff_rows)
})

test_that("flag mode changes nothing and proposes exactly the correct-mode result", {
  fix <- make_income_fixture()
  res_c <- clean_quiet(fix)
  res_f <- clean_quiet(fix, mode = "flag")
  expect_identical(res_f$data$nethh, num(fix$nethh))
  expect_identical(res_f$data$nethh_proposed, res_c$data$nethh)
  mut <- res_f$decisions[res_f$decisions$action %in%
                           c("correct", "set_na", "rectify_sign", "cap_na"), ]
  expect_true(all(!mut$applied))
  expect_true("nethh_proposed_status" %in% names(res_f$data))
  expect_false("nethh_clean_status" %in% names(res_f$data))
})

test_that("na_only mode voids detected cells instead of imputing", {
  fix <- make_income_fixture()
  res_c <- clean_quiet(fix)
  res_n <- clean_quiet(fix, mode = "na_only")
  corrected_rows <- res_c$decisions$row[res_c$decisions$action == "correct" &
                                          res_c$decisions$applied]
  expect_true(all(is.na(res_n$data$nethh[corrected_rows])))
  expect_false(any(res_n$decisions$action == "correct"))
})

test_that("cleaning is deterministic and row-order independent within households", {
  fix <- make_income_fixture()
  r1 <- clean_quiet(fix)
  r2 <- clean_quiet(fix)
  expect_identical(r1$data, r2$data)
  expect_identical(r1$decisions[setdiff(names(r1$decisions), "decision_id")],
                   r2$decisions[setdiff(names(r2$decisions), "decision_id")])

  set.seed(42)
  shuf <- fix[sample(nrow(fix)), ]
  r3 <- clean_quiet(shuf)
  v3 <- r3$data$nethh[r3$data$nomem_encr == 101 & r3$data$wavenr == 3]
  expect_identical(v3, cell(r1, 101, 3, "nethh"))
})

test_that("re-cleaning is guarded, and a stripped re-run reaches a steady state", {
  fix <- make_income_fixture()
  res <- clean_quiet(fix)
  expect_error(clean_quiet(res$data), "cleaned already")

  stripped <- res$data
  stripped$nethh_observed <- NULL
  stripped$nethh_clean_status <- NULL
  stripped$nethh_dataset_flag <- NULL
  res2 <- clean_quiet(stripped)
  s <- res2$summary
  expect_identical(s$n_corrected + s$n_voided + s$n_rectified + s$n_capped, 0L)
})

# ---- engine: configurability -------------------------------------------------------

test_that("disabling the household detectors leaves the target cell alone", {
  fix <- make_income_fixture()
  res <- clean_quiet(fix, disable = c("D06", "D09", "D10"))
  expect_identical(cell(res, 101, 3, "nethh"), 2600)
  expect_true(is.na(cell(res, 101, 3, "nethh_clean_status")))
})

test_that("a params override reroutes a decision to the widened rule", {
  fix <- make_income_fixture()
  res <- clean_quiet(fix, params = list(D02 = list(threshold = 100)))
  # the contextual-floor case now falls to the widened absolute floor
  expect_identical(cell(res, 801, 1, "nethh_clean_status"), "voided:D02")
})

test_that("an income_cap override changes what counts as implausible", {
  fix <- make_income_fixture()
  res <- clean_quiet(fix, income_cap = 200000)
  expect_identical(cell(res, 401, 2, "nethh"), 160000)
  expect_true(is.na(cell(res, 401, 2, "nethh_clean_status")))
  expect_true(any(grepl("income_cap = 200000", res$summary$overrides)))
})

test_that("a custom ruleset file is honored end to end", {
  rs <- yaml::read_yaml(system.file("cleaning", "income_cleaning_rules.yml",
                                    package = "lissr"))
  for (i in seq_along(rs$detection_rules)) {
    if (rs$detection_rules[[i]]$rule_id == "D02") {
      rs$detection_rules[[i]]$params$threshold <- 100
    }
  }
  path <- tempfile(fileext = ".yml")
  yaml::write_yaml(rs, path)
  res <- clean_quiet(make_income_fixture(), ruleset = path)
  expect_identical(cell(res, 801, 1, "nethh_clean_status"), "voided:D02")
})

# ---- engine: inputs, resolution, attachment ------------------------------------------

test_that("merge-result lists and tibbles round-trip", {
  fix <- make_income_fixture()
  res <- clean_quiet(list(data = fix, log = list()))
  expect_s3_class(res, "liss_clean_result")
  res_tb <- clean_quiet(tibble::as_tibble(fix))
  expect_s3_class(res_tb$data, "tbl_df")
  expect_identical(num(res_tb$data$nethh), res$data$nethh)
})

test_that("a missing household id falls back to person-level grouping with a note", {
  fix <- make_income_fixture()
  fix$nohouse_encr <- NULL
  res <- clean_quiet(fix)
  expect_true(any(grepl("household id absent", res$summary$notes)))
  expect_identical(cell(res, 101, 3, "nethh_clean_status"), "corrected:D06")
})

test_that("the target resolves through its alias and names the output columns", {
  fix <- make_income_fixture()
  names(fix)[names(fix) == "nethh"] <- "ci00a339"
  res <- clean_quiet(fix)
  expect_true(any(grepl("alias 'ci00a339'", res$summary$notes)))
  expect_true(all(c("ci00a339_observed", "ci00a339_clean_status") %in%
                    names(res$data)))
})

test_that("a missing wave column degrades to input order with a note", {
  fix <- make_income_fixture()
  fix$wavenr <- NULL
  res <- clean_quiet(fix)
  expect_true(any(grepl("no wave column", res$summary$notes)))
  expect_identical(res$summary$n_corrected > 0, TRUE)
})

test_that("P01 attaches a monthly background on the annual scale, keeping the latest month", {
  inc <- data.frame(nomem_encr = c(1, 1, 2), nohouse_encr = c(1, 1, 2),
                    wavenr = c(1, 2, 1), nethh = c(21000, 21500, 30000))
  bck <- data.frame(
    nomem_encr = c(1, 1, 1, 2),
    wave = c(200801, 200807, 200901, 200803),
    oplmet = c(4, 5, 6, 3), aantalhh = 2
  )
  res <- clean_quiet(inc, background = bck)
  expect_identical(nrow(res$data), 3L)
  expect_identical(res$data$oplmet, c(5, 6, 3))  # 200807 wins the 2008 year
  # single-snapshot backgrounds join on the person id only
  bck2 <- data.frame(nomem_encr = c(1, 2), oplmet = c(9, 8))
  res2 <- clean_quiet(inc, background = bck2)
  expect_identical(res2$data$oplmet, c(9, 9, 8))
})

# ---- report artifacts ------------------------------------------------------------------

test_that("liss_cleaning_report writes a methodology-bearing report, CSV ledger, and JSONL log", {
  res <- clean_quiet(make_income_fixture())
  dir <- file.path(tempdir(), "clean-report")
  paths <- lissr::liss_cleaning_report(res, dir, verbose = FALSE)
  expect_true(all(file.exists(unlist(paths))))
  rpt <- readLines(paths$report)
  expect_true(any(grepl("^## Methodology$", rpt)))
  for (rid in c("P03", "D02", "D06", "C01", "F01")) {
    expect_true(any(grepl(paste0("#### ", rid, " "), rpt)))
  }
  led <- utils::read.csv(paths$decisions, stringsAsFactors = FALSE)
  expect_identical(nrow(led), nrow(res$decisions))
  lg <- readLines(paths$log)
  parsed <- lapply(lg, jsonlite::fromJSON)
  expect_true(all(vapply(parsed, function(e) is.character(e$rule_id),
                         logical(1))))
})

test_that("print and summary methods run cleanly", {
  res <- clean_quiet(make_income_fixture())
  expect_no_error(print(res))
  expect_output(s <- summary(res), "Income Cleaning Summary")
  expect_identical(s$n_rows, nrow(make_income_fixture()))
})

# ---- equivalisation wrapper ---------------------------------------------------------------

test_that("liss_equivalise_income matches the manual scale and warns on bad composition", {
  expect_equal(lissr::liss_equivalise_income(30000, 3, 1, verbose = FALSE),
               30000 / ((3 - 1 + 0.8 * 1)^0.5))
  expect_warning(
    out <- lissr::liss_equivalise_income(c(30000, 30000), c(3, 0), c(1, 0)),
    "invalid household composition"
  )
  expect_true(is.na(out[2]))
})
