# the saved compatibility object and all numerical witnesses are invented
.nr_portable_expected <- function() {
  saved <- readRDS(testthat::test_path("fixtures", "source_nonresponse_v1_synthetic.rds"))
  saved$expected$unguarded$flag
}

.nr_portable_double <- function(bytes) {
  readBin(as.raw(bytes), "double", n = 1L, size = 8L, endian = "big")
}

test_that("frozen unguarded comparison permits only the observed one-ULP mean rounding", {
  expected <- .nr_portable_expected()
  baseline <- .nr_portable_double(c(0xc1, 0xf0, 0x8e, 0x8c, 0xa1, 0x72, 0xaa, 0xaa))
  one_ulp <- .nr_portable_double(c(0xc1, 0xf0, 0x8e, 0x8c, 0xa1, 0x72, 0xaa, 0xab))
  two_ulps <- .nr_portable_double(c(0xc1, 0xf0, 0x8e, 0x8c, 0xa1, 0x72, 0xaa, 0xac))
  expect_identical(expected$summary$distribution_before$mean, baseline)
  expect_identical(abs(one_ulp - baseline), 2^-20)
  expect_identical(abs(two_ulps - baseline), 2^-19)
  tolerance <- .Machine$double.eps * max(1, abs(baseline))
  expect_lte(abs(one_ulp - baseline), tolerance)
  expect_gt(abs(two_ulps - baseline), tolerance)
  expect_identical(.nr_expect_frozen_unguarded(expected, expected), expected)
  actual <- expected
  actual$summary$distribution_before$mean <- one_ulp
  before <- serialize(actual, NULL, version = 2L)
  expected_before <- serialize(expected, NULL, version = 2L)
  expect_identical(.nr_expect_frozen_unguarded(actual, expected), actual)
  expect_identical(serialize(actual, NULL, version = 2L), before)
  expect_identical(serialize(expected, NULL, version = 2L), expected_before)
  actual$summary$distribution_before$mean <- two_ulps
  testthat::expect_failure(.nr_expect_frozen_unguarded(actual, expected))
})

test_that("portable mean comparison rejects changed type shape attributes and missingness", {
  expected <- .nr_portable_expected()
  mean <- expected$summary$distribution_before$mean
  malformed <- list(1L, as.character(mean), list(mean), numeric(), c(mean, mean),
    structure(mean, label = "changed"), c(changed = mean),
    structure(mean, class = "changed"), NA_real_, NaN, Inf, -Inf)
  for (index in seq_along(malformed)) {
    actual <- expected
    actual$summary$distribution_before$mean <- malformed[[index]]
    testthat::expect_failure(.nr_expect_frozen_unguarded(actual, expected,
      info = paste("malformed mean", index)))
  }
  actual <- expected
  actual$summary$distribution_before$mean <- NULL
  testthat::expect_failure(.nr_expect_frozen_unguarded(actual, expected))
  for (mean in list(NA_real_, Inf, numeric(), 1L, structure(1, label = "changed"))) {
    actual <- expected
    actual$summary$distribution_before$mean <- mean
    testthat::expect_failure(.nr_expect_frozen_unguarded(actual, actual))
  }
})

test_that("portable frozen comparison retains exact data decisions statistics counts and rules", {
  expected <- .nr_portable_expected()
  mutations <- list(
    data = function(x) { x$data$s339[1L] <- 123; x },
    decisions = function(x) { attr(x$decisions, "changed") <- TRUE; x },
    other_mean = function(x) { x$summary$distribution_after$mean <- 123; x },
    standard_deviation = function(x) { x$summary$distribution_before$sd <- 123; x },
    distribution_count = function(x) {
      x$summary$distribution_before$n <- x$summary$distribution_before$n + 1L
      x
    },
    summary_count = function(x) { x$summary$n_rows <- x$summary$n_rows + 1L; x },
    rules = function(x) { x$ruleset$constraints$income_cap <- 123; x },
    readiness = function(x) { x$source_nonresponse_readiness$status <- "passed"; x },
    result_metadata = function(x) { attr(x, "changed") <- TRUE; x })
  for (name in names(mutations)) {
    actual <- mutations[[name]](expected)
    actual$summary$distribution_before$mean <- .nr_portable_double(
      c(0xc1, 0xf0, 0x8e, 0x8c, 0xa1, 0x72, 0xaa, 0xab))
    testthat::expect_failure(.nr_expect_frozen_unguarded(actual, expected, info = name))
  }
  expected$ruleset$meta$policy_sha256 <- strrep("0", 64L)
  actual <- expected
  actual$ruleset$meta$policy_sha256 <- strrep("1", 64L)
  testthat::expect_failure(.nr_expect_frozen_unguarded(actual, expected))
})
