# regression tests for the 1.1.0 engine and recipe fixes.
# unit tests run everywhere; the empirical block needs the real LISS files and
# activates when LISSR_VERIFICATION_DIR points at the verification bundle.

num <- function(x) as.numeric(unclass(x))

test_that("value_recode uses snapshot semantics and cannot chain", {
  df <- data.frame(nomem_encr = 1:5, s013 = c(1, 2, 3, 4, 5))
  rule <- list(rule_id = "T", action = "value_recode", description = "t",
               suffixes = list("013"), mapping = list(`1` = 2, `2` = 3))
  r <- lissr:::exec_harmonization_rule(df, rule, "w1", list(), c("w1"), list())
  expect_identical(r$df$s013, c(2, 3, 3, 4, 5))
})

test_that("recode_to_na honors the recode alias and wave-scoped exclude blocks", {
  df <- data.frame(nomem_encr = 1:3, s001 = c(9, 1, 9), s243 = c(9, 9, 2))
  rule <- list(rule_id = "T2", action = "recode_to_na", description = "t",
               scope = "all_numeric", recode = list(`9` = ".dk"),
               exclude = list(list(suffixes = list("243"), waves = list("w1"))))
  r <- lissr:::exec_harmonization_rule(df, rule, "w1", list(), c("w1", "w2"), list())
  expect_equal(sum(r$df$s001 == 9, na.rm = TRUE), 0)
  expect_equal(sum(r$df$s243 == 9, na.rm = TRUE), 2)
  r2 <- lissr:::exec_harmonization_rule(df, rule, "w2", list(), c("w1", "w2"), list())
  expect_equal(sum(r2$df$s243 == 9, na.rm = TRUE), 0)
})

test_that("a rule that resolves no targets leaves an audit trace", {
  df <- data.frame(nomem_encr = 1:3, s001 = c(1, 2, 3))
  rule <- list(rule_id = "T3", action = "value_recode", description = "t",
               suffix = "013", mapping = list(`1` = 2))
  r <- lissr:::exec_harmonization_rule(df, rule, "w1", list(), c("w1"), list())
  acts <- vapply(r$log, function(e) e$action, character(1))
  expect_true("value_recode:NO_TARGETS" %in% acts)
})

test_that("read_wave_file rejects unknown extensions instead of csv-parsing", {
  bad <- tempfile(fileext = ".pdf")
  writeLines("x", bad)
  expect_error(lissr:::read_wave_file(bad))
})

test_that("unimplemented validation types report SKIP, never PASS or error", {
  v <- lissr:::run_validations(
    data.frame(nomem_encr = 1),
    list(list(check_id = "C", type = "bogus_type", severity = "error")), list())
  expect_true(is.na(v$results[[1]]$passed))
  expect_equal(v$n_skip, 1)
  expect_equal(v$error_count, 0)
})

test_that("uniqueness checks execute against a key within a grouping variable", {
  df <- data.frame(nomem_encr = c(1, 2, 2), wave_id = c("a", "a", "a"))
  v <- lissr:::run_validations(
    df, list(list(check_id = "U", type = "uniqueness", column = "nomem_encr",
                  within = "wave_id", severity = "error")), list())
  expect_false(isTRUE(v$results[[1]]$passed))
  expect_equal(v$error_count, 1)
})

test_that("strict mode aborts before outputs; non-strict still writes", {
  dd <- file.path(tempdir(), "strictd"); dir.create(dd, showWarnings = FALSE)
  haven::write_sav(data.frame(nomem_encr = 1:3, xx01a005 = c(1, 99, 2)),
                   file.path(dd, "xx01a_EN_1.0p.sav"))
  rec <- list(
    meta = list(module = "xx", module_label = "t", schema_version = "1.0.0",
                recipe_version = "t", created = "t", source_spec = "t",
                covered_waves = list("xx01a")),
    global = list(id_variable = "nomem_encr", wave_variable = "wave_id",
                  year_variable = "wave_year", labelled_policy = "to_numeric",
                  missing_variable_policy = "warn_and_create_na",
                  strip_label_whitespace = TRUE),
    wave_index = list(list(id = "xx01a", year = 2001, file_pattern = "xx01a_*")),
    validation_checks = list(list(check_id = "V1", type = "value_absence",
                                  suffixes = list("005"),
                                  forbidden_values = list(99),
                                  severity = "error")),
    logging = list(log_file = "t.jsonl"))
  outd <- file.path(tempdir(), "stricto")
  expect_error(suppressWarnings(merge_liss_module(rec, dd, outd, strict = TRUE)))
  expect_false(file.exists(file.path(outd, "xx_merged.sav")))
  suppressWarnings(merge_liss_module(rec, dd, outd, strict = FALSE))
  expect_true(file.exists(file.path(outd, "xx_merged.sav")))
})

# ---- empirical block: real LISS files -------------------------------------

vdir <- Sys.getenv("LISSR_VERIFICATION_DIR", "")
skip_note <- "set LISSR_VERIFICATION_DIR to the verification bundle to run"

rec_path <- function(mod) system.file("recipes", paste0(mod, "_merge_recipe.yml"),
                                      package = "lissr")
stage <- function(name, files) {
  d <- file.path(tempdir(), name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  for (f in files) file.copy(file.path(vdir, f), d, overwrite = TRUE)
  d
}

test_that("cd10c superseded release: highest version wins, no duplication", {
  skip_if(!nzchar(vdir) || !dir.exists(vdir), skip_note)
  dd <- stage("t_cd", c("01_cd10c_versions/cd10c_EN_1.0p.sav",
                        "01_cd10c_versions/cd10c_EN_1.1p.sav"))
  od <- file.path(tempdir(), "t_cdo")
  res <- suppressWarnings(
    merge_liss_module(load_recipe(rec_path("cd")), dd, od))
  out <- haven::read_sav(file.path(od, "cd_merged.sav"), user_na = TRUE)
  expect_equal(sum(out$wave_id == "cd10c"), 3626)
  expect_false("s059" %in% names(out))
  expect_equal(anyDuplicated(out$nomem_encr[out$wave_id == "cd10c"]), 0)
  u <- Filter(function(r) grepl("^CHK0[67]", r$check_id), res$validation)
  expect_true(length(u) == 2 && all(vapply(u, function(r) isTRUE(r$passed), logical(1))))
})

test_that("cr three-era harmonization reproduces the verified distributions", {
  skip_if(!nzchar(vdir) || !dir.exists(vdir), skip_note)
  dd <- stage("t_cr", c("02_cr_religion_eras/cr08a_EN_2.0p.sav",
                        "02_cr_religion_eras/cr14g_1.0p_EN.sav",
                        "02_cr_religion_eras/cr19l_EN_1.1p.sav"))
  od <- file.path(tempdir(), "t_cro")
  suppressWarnings(merge_liss_module(load_recipe(rec_path("cr")), dd, od))
  out <- haven::read_sav(file.path(od, "cr_merged.sav"), user_na = TRUE)
  e1 <- c(`1` = 1530, `2` = 473, `3` = 567, `4` = 9, `5` = 73, `6` = 157,
          `7` = 125, `8` = 10, `9` = 7, `10` = 8, `11` = 4, `12` = 5, `13` = 24)
  g1 <- table(num(out$s013[out$wave_id == "cr08a"]))
  expect_equal(as.integer(g1[names(e1)]), unname(e1))
  e2 <- c(`1` = 1059, `2` = 715, `4` = 9, `5` = 64, `6` = 211, `7` = 115,
          `8` = 14, `9` = 6, `10` = 6, `12` = 2, `13` = 21)
  g2 <- table(num(out$s013[out$wave_id == "cr14g"]))
  expect_equal(as.integer(g2[names(e2)]), unname(e2))
  expect_false("3" %in% names(g2))
  expect_equal(sum(num(out$s013) %in% c(99, 999), na.rm = TRUE), 0)
})

test_that("cp DK recode is scoped to its items and spares paradata", {
  skip_if(!nzchar(vdir) || !dir.exists(vdir), skip_note)
  dd <- stage("t_cp", "03_cp_sentinel_blast/cp08a_1p_EN.sav")
  od <- file.path(tempdir(), "t_cpo")
  suppressWarnings(merge_liss_module(load_recipe(rec_path("cp")), dd, od))
  out <- haven::read_sav(file.path(od, "cp_merged.sav"), user_na = TRUE)
  n999 <- sum(vapply(c("s010", "s011", "s019"),
                     function(c) sum(num(out[[c]]) == 999, na.rm = TRUE), numeric(1)))
  expect_equal(n999, 0)
  expect_equal(sum(num(out$s193) == 999, na.rm = TRUE), 3)
})

test_that("cs DK 999 becomes -9 on exactly the verified suffixes", {
  skip_if(!nzchar(vdir) || !dir.exists(vdir), skip_note)
  dd <- stage("t_cs", "04_cs_dk_labels/cs08a_2p_EN.sav")
  od <- file.path(tempdir(), "t_cso")
  suppressWarnings(merge_liss_module(load_recipe(rec_path("cs")), dd, od))
  out <- haven::read_sav(file.path(od, "cs_merged.sav"), user_na = TRUE)
  cols <- c("s001", "s002", "s283")
  expect_equal(sum(vapply(cols, function(c)
    sum(num(out[[c]]) == 999, na.rm = TRUE), numeric(1))), 0)
  expect_equal(sum(vapply(cols, function(c)
    sum(num(out[[c]]) == -9, na.rm = TRUE), numeric(1))), 211)
})

test_that("cv HR01 fires, exclude carve-outs survive the residual sweep", {
  skip_if(!nzchar(vdir) || !dir.exists(vdir), skip_note)
  dd <- stage("t_cv", c("05_sentinel_regimes/cv08a_1.1p_EN.sav",
                        "05_sentinel_regimes/cv20l_EN_1.0p.sav"))
  od <- file.path(tempdir(), "t_cvo")
  suppressWarnings(merge_liss_module(load_recipe(rec_path("cv")), dd, od))
  out <- haven::read_sav(file.path(od, "cv_merged.sav"), user_na = TRUE)
  w08 <- out[out$wave_id == "cv08a", ]
  n99 <- sum(vapply(paste0("s", c("008", "053", "102", "103", "104", "105")),
                    function(c) sum(num(w08[[c]]) == 99, na.rm = TRUE), numeric(1)))
  expect_equal(n99, 0)
  w20 <- out[out$wave_id == "cv20l", ]
  expect_equal(sum(num(w20$s243) == -9, na.rm = TRUE), 154)
  lab_cols <- sum(vapply(out, function(c) inherits(c, "haven_labelled"), logical(1)))
  expect_gt(lab_cols, 0)
})
