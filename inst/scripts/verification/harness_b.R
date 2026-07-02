# part b harness: end-to-end module merges against the real LISS files.
# paths are taken from the environment with in-container defaults:
#   LISSR_ENGINE_DIR        directory containing the (patched) R sources to test
#   LISSR_RECIPE_DIR        directory containing the revised recipe YAMLs
#   LISSR_VERIFICATION_DIR  the verification bundle root
#   LISSR_ORIG_RECIPE_DIR   the upstream recipes (for before/after comparisons)
# expected values below were independently verified with pyreadstat
# (user_missing = TRUE) and the wave codebooks; see lissr-verification-report.md
options(warn = 1)
suppressPackageStartupMessages({library(haven); library(dplyr)})
setwd(Sys.getenv("LISSR_ENGINE_DIR", "local/lissr/R"))
source("liss_merge_engine.R")

B   <- Sys.getenv("LISSR_VERIFICATION_DIR", "local/lissr/tests/verification-bundle")
REC <- Sys.getenv("LISSR_RECIPE_DIR", "local/lissr/inst/recipes")
ORI <- Sys.getenv("LISSR_ORIG_RECIPE_DIR", "local/lissr/inst/recipes-orig")
ok  <- function(cond, msg) { cat(if (isTRUE(cond)) "PASS " else "FAIL ", msg, "\n"); stopifnot(isTRUE(cond)) }
num <- function(x) as.numeric(unclass(x))
mkdir <- function(...) { d <- file.path(tempdir(), ...); dir.create(d, recursive = TRUE, showWarnings = FALSE); d }
stage <- function(dst, srcs) { for (s in srcs) file.copy(file.path(B, s), dst, overwrite = TRUE); dst }
run_mod <- function(recipe_path, dd, od, patch = identity) {
  rec <- patch(load_recipe(recipe_path))
  suppressWarnings(merge_liss_module(rec, dd, od))
}
read_out <- function(od, mod) read_sav(file.path(od, paste0(mod, "_merged.sav")), user_na = TRUE)
dist_of <- function(v) { t <- table(num(v)); setNames(as.integer(t), names(t)) }
same_dist <- function(got, want) {
  all(names(want) %in% names(got)) && all(got[names(want)] == want) &&
    sum(got) == sum(want)
}

cat("== B1 cr end-to-end: three-era religion harmonization ==\n")
dd <- stage(mkdir("crd"), c("02_cr_religion_eras/cr08a_EN_2.0p.sav",
                            "02_cr_religion_eras/cr14g_1.0p_EN.sav",
                            "02_cr_religion_eras/cr19l_EN_1.1p.sav"))
od <- mkdir("cro")
res <- run_mod(file.path(REC, "cr_merge_recipe.yml"), dd, od)
out <- read_out(od, "cr")
e1 <- c(`1`=1530,`2`=473,`3`=567,`4`=9,`5`=73,`6`=157,`7`=125,`8`=10,`9`=7,`10`=8,`11`=4,`12`=5,`13`=24)
e2 <- c(`1`=1059,`2`=715,`4`=9,`5`=64,`6`=211,`7`=115,`8`=14,`9`=6,`10`=6,`12`=2,`13`=21)
g1 <- dist_of(out$s013[out$wave_id == "cr08a"])
g2 <- dist_of(out$s013[out$wave_id == "cr14g"])
ok(same_dist(g1, e1), "era-1 (cr08a) harmonized distribution exact")
ok(same_dist(g2, e2), "era-2 (cr14g) harmonized distribution exact")
ok(!("3" %in% names(g2)) && g2[["13"]] == 21,
   "era-2 shows no chaining signature (code 3 absent, code 13 == 21)")
w3 <- out[out$wave_id == "cr19l", ]
c3 <- if ("s013" %in% names(w3) && any(!is.na(w3$s013))) w3$s013 else w3$s144
ok(sum(!is.na(num(c3))) == 1417, "era-3 answered count preserved (1417)")
ok(sum(num(out$s013) %in% c(99, 999), na.rm = TRUE) == 0,
   "no raw DK sentinels remain on the harmonized column")

cat("== B2 cd end-to-end: superseded release handled, no duplication ==\n")
dd <- stage(mkdir("cdd"), c("01_cd10c_versions/cd10c_EN_1.0p.sav",
                            "01_cd10c_versions/cd10c_EN_1.1p.sav"))
od <- mkdir("cdo")
res <- run_mod(file.path(REC, "cd_merge_recipe.yml"), dd, od)
out <- read_out(od, "cd")
ok(sum(out$wave_id == "cd10c") == 3626, "wave cd10c has 3626 rows (no stacking)")
ok(!("s059" %in% names(out)), "redacted open-text location item absent")
ok(anyDuplicated(out$nomem_encr[out$wave_id == "cd10c"]) == 0, "respondent ids unique within wave")
u_chk <- Filter(function(r) grepl("^CHK0[67]", r$check_id), res$validation)
ok(length(u_chk) == 2 && all(vapply(u_chk, function(r) isTRUE(r$passed), logical(1))),
   "uniqueness checks CHK06/CHK07 executed and passed")
# widen the pattern in memory: the engine must rank versions and pick 1.1p
widen <- function(rec) { rec$wave_index <- lapply(rec$wave_index, function(w) {
  if (w$id == "cd10c") w$file_pattern <- "cd10c_*"; w }); rec }
warns <- character(0)
rec_w <- widen(load_recipe(file.path(REC, "cd_merge_recipe.yml")))
res2 <- withCallingHandlers(
  merge_liss_module(rec_w, dd, mkdir("cdo2")),
  warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") })
ok(any(grepl("highest", warns) & grepl("1\\.1p", warns)),
   "wide pattern triggers version disambiguation toward 1.1p")
ok(sum(res2$data$wave_id == "cd10c") == 3626, "disambiguated merge also yields 3626 rows")

cat("== B3 cp end-to-end: scoped DK recode spares paradata ==\n")
dd <- stage(mkdir("cpd"), "03_cp_sentinel_blast/cp08a_1p_EN.sav")
od <- mkdir("cpo")
res <- run_mod(file.path(REC, "cp_merge_recipe.yml"), dd, od)
out <- read_out(od, "cp")
raw <- read_sav(file.path(B, "03_cp_sentinel_blast/cp08a_1p_EN.sav"), user_na = TRUE)
ok(sum(num(out$s010) == 999, na.rm = TRUE) + sum(num(out$s011) == 999, na.rm = TRUE) +
     sum(num(out$s019) == 999, na.rm = TRUE) == 0,
   "999 cleared on intended items 010/011/019 (234 cells)")
ok(sum(num(out$s193) == 999, na.rm = TRUE) == sum(num(raw$cp08a193) == 999, na.rm = TRUE),
   "duration paradata untouched (3 legitimate 999 s remain)")

cat("== B4 cs end-to-end: DK 999 -> -9 on the verified suffixes ==\n")
dd <- stage(mkdir("csd"), "04_cs_dk_labels/cs08a_2p_EN.sav")
od <- mkdir("cso")
raw <- read_sav(file.path(B, "04_cs_dk_labels/cs08a_2p_EN.sav"), user_na = TRUE)
pre_neg9 <- sum(vapply(c("cs08a001","cs08a002","cs08a283"),
                       function(c) sum(num(raw[[c]]) == -9, na.rm = TRUE), numeric(1)))
res <- run_mod(file.path(REC, "cs_merge_recipe.yml"), dd, od)
out <- read_out(od, "cs")
post999 <- sum(vapply(c("s001","s002","s283"),
                      function(c) sum(num(out[[c]]) == 999, na.rm = TRUE), numeric(1)))
post_neg9 <- sum(vapply(c("s001","s002","s283"),
                        function(c) sum(num(out[[c]]) == -9, na.rm = TRUE), numeric(1)))
ok(post999 == 0, "no raw 999 remains on suffixes 001/002/283")
ok(post_neg9 == pre_neg9 + 211, "exactly the 211 verified DK cells became -9")

cat("== B5 cv end-to-end: inert HR01 now fires; exclude blocks honored ==\n")
dd <- stage(mkdir("cvd"), c("05_sentinel_regimes/cv08a_1.1p_EN.sav",
                            "05_sentinel_regimes/cv20l_EN_1.0p.sav"))
od <- mkdir("cvo")
raw20 <- read_sav(file.path(B, "05_sentinel_regimes/cv20l_EN_1.0p.sav"), user_na = TRUE)
res <- run_mod(file.path(REC, "cv_merge_recipe.yml"), dd, od)
out <- read_out(od, "cv")
w08 <- out[out$wave_id == "cv08a", ]
hr01 <- paste0("s", c("008","053","102","103","104","105"))
n99 <- sum(vapply(hr01, function(c) sum(num(w08[[c]]) == 99, na.rm = TRUE), numeric(1)))
ok(n99 == 0, "all 1147 DK 99 cells cleared on the six HR01 targets")
w20 <- out[out$wave_id == "cv20l", ]
keep243 <- sum(num(raw20$cv20l243) == -9, na.rm = TRUE)
ok(sum(num(w20$s243) == -9, na.rm = TRUE) == keep243 && keep243 > 0,
   sprintf("excluded suffix 243 keeps its %d eligibility -9 s in cv20l", keep243))
excl <- c("243","245","160","242")
other <- setdiff(grep("^s\\d{3}$", names(w20), value = TRUE), paste0("s", excl))
resid <- sum(vapply(other, function(c) sum(num(w20[[c]]) == -9, na.rm = TRUE), numeric(1)))
ok(resid == 0, "no -9 survives outside the recipe's exclude carve-outs in cv20l")

cat("== B6 label round-trip and residual sweep ==\n")
logf <- readLines(file.path(od, load_recipe(file.path(REC,"cv_merge_recipe.yml"))$logging$log_file %||% "cv_merge_log.jsonl"))
ok(any(grepl('"LABEL_RESTORE"', logf)), "LABEL_RESTORE entries present in the jsonl log")
lab_cols <- sum(vapply(out, function(c) inherits(c, "haven_labelled_spss") || inherits(c, "haven_labelled"), logical(1)))
ok(lab_cols > 0, sprintf("output .sav carries value labels again (%d labelled columns)", lab_cols))
ok(any(grepl('"NA_SWEEP"', logf)), "residual user-missing sweep logged")

cat("== B7 validate_recipe: unknown-key rule count drops on revised recipes ==\n")
flagged_rules <- function(path) {
  msgs <- character(0)
  withCallingHandlers(validate_recipe(load_recipe(path), path),
    warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") })
  msgs <- gsub("\\s+", " ", msgs)
  m <- regmatches(msgs, regexpr("in [0-9]+ rule", msgs))
  if (length(m) == 0) return(0L)
  max(as.integer(gsub("[^0-9]", "", m)))
}
tot_a <- 0L; tot_b <- 0L
for (m in c("cd","cp","cr","cs","cv")) {
  a <- flagged_rules(file.path(ORI, paste0(m, "_merge_recipe.yml")))
  b <- flagged_rules(file.path(REC, paste0(m, "_merge_recipe.yml")))
  tot_a <- tot_a + a; tot_b <- tot_b + b
  cat(sprintf("  %s: rules with unrecognized keys %d -> %d\n", m, a, b))
  ok(b <= a, paste0(m, " revised recipe adds no unknown-key noise"))
}
ok(tot_b < tot_a, sprintf("net unknown-key noise drops across revised recipes (%d -> %d)", tot_a, tot_b))
for (m in c("ca","cf","ch","ci","cw")) {
  a <- flagged_rules(file.path(ORI, paste0(m, "_merge_recipe.yml")))
  cat(sprintf("  %s (untouched recipe under revised engine): %d rules still flagged\n", m, a))
}
cat("all part b checks passed\n")
