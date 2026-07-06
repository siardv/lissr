# the cv party-scheme taxonomy must stay consistent with the cv recipe's
# per-wave scheme declarations, and the facts catalogued from the archive's
# value labels must hold as regressions: unique codes per scheme, the
# per-scheme actual-to-hypothetical offsets, the registry stability that
# begins at scheme 3, the named retired codes, and the pre-registry
# renumbering of schemes 1 and 2.

load_party_taxonomy <- function() {
  path <- system.file("recipes", "taxonomies", "cv_party_scheme.yml",
                      package = "lissr")
  expect_true(nzchar(path), info = "taxonomy file not installed")
  yaml::yaml.load_file(path)
}

scheme_by_id <- function(tax, id) {
  Filter(function(s) s$scheme == id, tax$schemes)[[1]]
}

party_map <- function(scheme, table = "parties") {
  stats::setNames(scheme[[table]],
                  vapply(scheme[[table]], function(p) p$key, character(1)))
}

test_that("every scheme declared by a cv wave exists in the taxonomy", {
  tax <- load_party_taxonomy()
  recipe <- yaml::yaml.load_file(system.file(
    "recipes", "cv_merge_recipe.yml", package = "lissr"))
  declared <- vapply(recipe$wave_index,
                     function(w) w$party_scheme, integer(1))
  ids <- vapply(tax$schemes, function(s) s$scheme, integer(1))
  expect_true(all(declared %in% ids))
  expect_true(setequal(ids, unlist(tax$meta$catalogued_schemes)))
  for (s in tax$schemes) {
    rec_waves <- vapply(
      Filter(function(w) w$party_scheme == s$scheme, recipe$wave_index),
      function(w) w$id, character(1))
    expect_true(setequal(unlist(s$waves), rec_waves),
                info = paste("scheme", s$scheme))
    expect_true(all(unlist(s$verified_waves) %in% unlist(s$waves)),
                info = paste("scheme", s$scheme))
  }
})

test_that("catalogued schemes have unique keys and codes per table", {
  tax <- load_party_taxonomy()
  for (s in tax$schemes) {
    expect_identical(s$status, "catalogued",
                     info = paste("scheme", s$scheme))
    tables <- intersect(c("parties", "parties_actual", "parties_hypo"),
                        names(s))
    expect_gt(length(tables), 0)
    for (tb in tables) {
      keys <- vapply(s[[tb]], function(p) p$key, character(1))
      expect_false(anyDuplicated(keys) > 0,
                   info = paste("scheme", s$scheme, tb))
      code_fields <- intersect(c("code", "code_actual", "code_hypo"),
                               names(s[[tb]][[1]]))
      for (cf in code_fields) {
        codes <- vapply(s[[tb]], function(p) p[[cf]], integer(1))
        expect_false(anyDuplicated(codes) > 0,
                     info = paste("scheme", s$scheme, tb, cf))
      }
    }
  }
})

test_that("per-scheme hypothetical offsets hold for every party", {
  tax <- load_party_taxonomy()
  for (id in c(2L, 3L, 4L, 5L)) {
    s <- scheme_by_id(tax, id)
    off <- s$hypo_offset
    ca <- vapply(s$parties, function(p) p$code_actual, integer(1))
    ch <- vapply(s$parties, function(p) p$code_hypo, integer(1))
    expect_identical(ch, ca + as.integer(off), info = paste("scheme", id))
  }
  s1 <- scheme_by_id(tax, 1L)
  expect_null(s1$hypo_offset)
  expect_true(all(c("parties_actual", "parties_hypo") %in% names(s1)))
})

test_that("scheme 1's two items carry different party sets", {
  tax <- load_party_taxonomy()
  s1 <- scheme_by_id(tax, 1L)
  actual <- names(party_map(s1, "parties_actual"))
  hypo <- names(party_map(s1, "parties_hypo"))
  expect_true(setequal(setdiff(actual, hypo), c("lpf", "een_nl")))
  expect_identical(setdiff(hypo, actual), "ton")
})

test_that("schemes 1 and 2 renumber; anchor codes are as catalogued", {
  tax <- load_party_taxonomy()
  s1 <- party_map(scheme_by_id(tax, 1L), "parties_actual")
  s2 <- party_map(scheme_by_id(tax, 2L))
  s3 <- party_map(scheme_by_id(tax, 3L))
  expect_identical(s1[["cda"]]$code, 1L)
  expect_identical(s2[["cda"]]$code_actual, 5L)
  expect_identical(s3[["cda"]]$code_actual, 3L)
  expect_identical(s1[["vvd"]]$code, 3L)
  expect_identical(s2[["vvd"]]$code_actual, 1L)
  expect_identical(s3[["vvd"]]$code_actual, 1L)
})

test_that("the registry is stable from scheme 3 onward", {
  tax <- load_party_taxonomy()
  s3 <- party_map(scheme_by_id(tax, 3L))
  s4 <- party_map(scheme_by_id(tax, 4L))
  s5 <- party_map(scheme_by_id(tax, 5L))
  for (k in intersect(names(s3), names(s4))) {
    expect_identical(s3[[k]]$code_actual, s4[[k]]$code_actual, info = k)
  }
  for (k in intersect(names(s4), names(s5))) {
    expect_identical(s4[[k]]$code_actual, s5[[k]]$code_actual, info = k)
    expect_identical(s4[[k]]$label, s5[[k]]$label, info = k)
  }
  expect_true(setequal(setdiff(names(s5), names(s4)), "fifty_plus"))
  expect_identical(setdiff(names(s4), names(s5)), "nsc")
  expect_identical(s5[["fifty_plus"]]$code_actual,
                   s3[["fifty_plus"]]$code_actual)
})

test_that("retired codes are named consistently with their evidence scheme", {
  tax <- load_party_taxonomy()
  s3 <- party_map(scheme_by_id(tax, 3L))
  for (id in c(4L, 5L)) {
    s <- scheme_by_id(tax, id)
    parties <- party_map(s)
    codes <- vapply(s$parties, function(p) p$code_actual, integer(1))
    for (rc in s$retired_codes) {
      expect_false(rc$code_actual %in% codes,
                   info = paste("scheme", id, "code", rc$code_actual))
      if (!is.null(rc$key) && rc$key %in% names(s3)) {
        expect_identical(s3[[rc$key]]$code_actual, rc$code_actual,
                         info = paste("scheme", id, rc$key))
      }
    }
  }
})
