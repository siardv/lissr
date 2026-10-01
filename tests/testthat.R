# run the installed package tests and check every recorded failure or error
library(testthat)
library(lissr)

test_results <- testthat::test_check("lissr")
test_has_failure <- vapply(test_results, function(test_result) {
  any(vapply(test_result[["results"]], function(result) {
    inherits(result, c("expectation_error", "expectation_failure"))
  }, logical(1)))
}, logical(1))
if (any(test_has_failure)) stop("Test failures.", call. = FALSE)
