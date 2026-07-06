# the cv party-scheme taxonomy must stay consistent with the cv recipe's
# per-wave scheme declarations, and the registry invariants established when
# schemes 4 and 5 were catalogued from the archive's value labels must hold:
# unique codes per scheme, code_308 = code_307 + 1 for every entry, and
# stability of every code shared across schemes.

load_party_taxonomy <- function() {
  path <- system.file("recipes", "taxonomies", "cv_party_scheme.yml",
                      package = "lissr")
  expect_true(nzchar(path), info = "taxonomy file not installed")
  yaml::yaml.load_file(path)
}

test_that("every scheme declared by a cv wave exists in the taxonomy", {
  tax <- load_party_taxonomy()
  recipe <- yaml::yaml.load_file(system.file(
    "recipes", "cv_merge_recipe.yml", package = "lissr"))
  declared <- vapply(recipe$wave_index,
                     function(w) w$party_scheme, integer(1))
  ids <- vapply(tax$schemes, function(s) s$scheme, integer(1))
  expect_true(all(declared %in% ids))
  for (s in tax$schemes) {
    rec_waves <- vapply(
      Filter(function(w) w$party_scheme == s$scheme, recipe$wave_index),
      function(w) w$id, character(1))
    expect_true(setequal(unlist(s$waves), rec_waves),
                info = paste("scheme", s$scheme))
  }
})

test_that("catalogued schemes satisfy the registry invariants", {
  tax <- load_party_taxonomy()
  cat_ids <- unlist(tax$meta$catalogued_schemes)
  schemes <- Filter(function(s) s$scheme %in% cat_ids, tax$schemes)
  expect_length(schemes, length(cat_ids))
  for (s in schemes) {
    expect_identical(s$status, "catalogued",
                     info = paste("scheme", s$scheme))
    c307 <- vapply(s$parties, function(p) p$code_307, integer(1))
    c308 <- vapply(s$parties, function(p) p$code_308, integer(1))
    keys <- vapply(s$parties, function(p) p$key, character(1))
    expect_false(anyDuplicated(keys) > 0, info = paste("scheme", s$scheme))
    expect_false(anyDuplicated(c307) > 0, info = paste("scheme", s$scheme))
    expect_false(anyDuplicated(c308) > 0, info = paste("scheme", s$scheme))
    expect_identical(c308, c307 + 1L, info = paste("scheme", s$scheme))
    unassigned <- unlist(s$unassigned_codes_307)
    expect_length(intersect(unassigned, c307), 0)
  }
})

test_that("schemes 4 and 5 differ only by 50PLUS in and NSC out", {
  tax <- load_party_taxonomy()
  by_key <- function(id) {
    s <- Filter(function(x) x$scheme == id, tax$schemes)[[1]]
    stats::setNames(s$parties,
                    vapply(s$parties, function(p) p$key, character(1)))
  }
  s4 <- by_key(4)
  s5 <- by_key(5)
  expect_identical(setdiff(names(s5), names(s4)), "fifty_plus")
  expect_identical(setdiff(names(s4), names(s5)), "nsc")
  for (k in intersect(names(s4), names(s5))) {
    expect_identical(s4[[k]]$code_307, s5[[k]]$code_307, info = k)
    expect_identical(s4[[k]]$code_308, s5[[k]]$code_308, info = k)
    expect_identical(s4[[k]]$label, s5[[k]]$label, info = k)
  }
  expect_identical(s5[["fifty_plus"]]$code_307, 10L)
  expect_identical(s4[["nsc"]]$code_307, 21L)
})
