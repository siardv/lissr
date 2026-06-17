test_that("liss_store_credentials requires keyring package", {
  skip_if(requireNamespace("keyring", quietly = TRUE),
          message = "keyring is installed; skip error-path test")
  expect_error(liss_store_credentials("test"), "keyring")
})

test_that("liss_list_credentials requires keyring package", {
  skip_if(requireNamespace("keyring", quietly = TRUE),
          message = "keyring is installed; skip error-path test")
  expect_error(liss_list_credentials(), "keyring")
})

test_that("liss_delete_credentials requires keyring package", {
  skip_if(requireNamespace("keyring", quietly = TRUE),
          message = "keyring is installed; skip error-path test")
  expect_error(liss_delete_credentials("test"), "keyring")
})
