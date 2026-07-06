test_that("find_col locates exact column names", {
  df <- data.frame(nomem_encr = 1, s001 = 2, s002 = 3)
  expect_equal(lissr:::find_col(df, "s001"), "s001")
  expect_equal(lissr:::find_col(df, "nomem_encr"), "nomem_encr")
})

test_that("find_col returns NULL for absent columns", {
  df <- data.frame(nomem_encr = 1, s001 = 2)
  expect_null(lissr:::find_col(df, "999"))
})

test_that("find_col tries s-prefix fallback", {
  df <- data.frame(nomem_encr = 1, s001 = 2)
  expect_equal(lissr:::find_col(df, "001"), "s001")
})

test_that("resolve_waves handles 'all' keyword", {
  ids <- c("ch07a", "ch08b", "ch09c")
  expect_equal(lissr:::resolve_waves("all", ids), ids)
  expect_equal(lissr:::resolve_waves(NULL, ids), ids)
})

test_that("resolve_waves intersects with available waves", {
  ids <- c("ch07a", "ch08b", "ch09c")
  expect_equal(
    lissr:::resolve_waves(c("ch08b", "ch09c", "ch10d"), ids),
    c("ch08b", "ch09c")
  )
})

test_that("coerce_column handles all target types", {
  x <- c("1", "2", "3")
  expect_type(lissr:::coerce_column(x, "integer"), "integer")
  expect_type(lissr:::coerce_column(x, "numeric"), "double")
  expect_type(lissr:::coerce_column(x, "character"), "character")
  expect_type(lissr:::coerce_column(c(1, 0, 1), "logical"), "logical")
})

test_that("strip_wave_prefix removes known prefix", {
  df <- data.frame(nomem_encr = 1, ch07a001 = 2, ch07a002 = 3)
  result <- lissr:::strip_wave_prefix(df, "ch07a", "nomem_encr")
  expect_true("nomem_encr" %in% names(result))
  expect_true("s001" %in% names(result))
  expect_true("s002" %in% names(result))
})

test_that("strip_wave_prefix preserves id_vars", {
  df <- data.frame(nomem_encr = 1, nohouse_encr = 2, ch07aXY = 3)
  result <- lissr:::strip_wave_prefix(df, "ch07a", "nomem_encr")
  expect_true("nomem_encr" %in% names(result))
  expect_true("nohouse_encr" %in% names(result))
})

test_that("make_log creates structured entry", {
  entry <- lissr:::make_log("r1", "ch07a", "s001", "recode_to_na", 10L)
  expect_equal(entry$rule_id, "r1")
  expect_equal(entry$wave_id, "ch07a")
  expect_equal(entry$action, "recode_to_na")
  expect_equal(entry$rows_affected, 10L)
  expect_true(nchar(entry$timestamp) > 0)
})

test_that("liss_recipe loads bundled recipe", {
  skip_on_cran()
  recipe <- liss_recipe("ch")
  expect_equal(recipe$meta$module, "ch")
  expect_true(length(recipe$wave_index) > 0)
})


test_that("crosswalk_map maps named codes and NAs the rest", {
  m <- list(`1` = 10, `2` = 20)
  expect_identical(lissr:::crosswalk_map(c(1, 2, 3, NA), m), c(10, 20, NA, NA))
  expect_identical(lissr:::crosswalk_map(c(1, 2), list()), c(NA_real_, NA_real_))
})

test_that("crosswalk_map_scheme routes rows through their scheme's mapping", {
  cw <- list(scheme_1 = list(`1` = 100), scheme_2 = list(`1` = 200))
  out <- lissr:::crosswalk_map_scheme(c(1, 1, 1, 2), c(1, 2, NA, 1), cw)
  expect_identical(out, c(100, 200, NA, NA))
})

test_that("crosswalk_coverage reports excess and the unmapped codes", {
  cov <- lissr:::crosswalk_coverage(c(1, 2, NA, 3), c(10, NA, NA, NA))
  expect_equal(cov$excess, 2)
  expect_identical(cov$unmapped_codes, c(2, 3))
  expect_identical(cov$severity, "error")
  ok <- lissr:::crosswalk_coverage(c(1, NA), c(10, NA))
  expect_equal(ok$excess, 0)
  expect_identical(ok$severity, "ok")
})

test_that("dv_aggregate honors method and missing_as_zero", {
  src <- list(c(1, NA, NA), c(2, 3, NA))
  expect_identical(lissr:::dv_aggregate(src, "sum"), c(3, 3, NA))
  expect_identical(lissr:::dv_aggregate(src, "sum", missing_as_zero = TRUE),
                   c(3, 3, 0))
  expect_identical(lissr:::dv_aggregate(src, "coalesce"), c(1, 3, NA))
  expect_identical(lissr:::dv_aggregate(src, "presence"), c(1, 1, 0))
  expect_identical(lissr:::dv_aggregate(src, "max"), c(2, 3, NA))
  expect_error(lissr:::dv_aggregate(src, "median"), "unknown")
})
