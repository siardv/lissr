# only the frozen v1 unguarded descriptive mean permits machine rounding
.nr_expect_frozen_unguarded <- function(actual, expected, info = NULL) {
  actual_mean <- actual[["summary"]][["distribution_before"]][["mean"]]
  expected_mean <- expected[["summary"]][["distribution_before"]][["mean"]]
  mean_shape <- identical(typeof(actual_mean), "double") &&
    identical(typeof(expected_mean), "double") &&
    length(actual_mean) == 1L && length(expected_mean) == 1L &&
    is.null(attributes(expected_mean)) &&
    identical(attributes(actual_mean), attributes(expected_mean)) &&
    is.finite(actual_mean) && is.finite(expected_mean)
  if (!mean_shape) {
    testthat::expect_true(mean_shape,
      info = paste("frozen unguarded mean must retain its finite double shape", info))
    return(invisible(actual))
  }
  tolerance <- .Machine$double.eps * max(1, abs(expected_mean))
  mean_within_tolerance <- abs(actual_mean - expected_mean) <= tolerance
  if (!mean_within_tolerance) {
    testthat::expect_true(mean_within_tolerance,
      info = paste("frozen unguarded mean exceeds machine rounding", info))
    return(invisible(actual))
  }
  comparison <- actual
  comparison[["summary"]][["distribution_before"]][["mean"]] <- expected_mean
  testthat::expect_identical(comparison, expected, info = info)
  invisible(actual)
}
