# empirical harness: runs the revised engine against the real LISS files
# paths are taken from the environment with defaults relative to the package root:
#   LISSR_ENGINE_DIR        directory containing the (patched) R sources to test
#   LISSR_RECIPE_DIR        directory containing the revised recipe YAMLs
#   LISSR_VERIFICATION_DIR  the verification bundle root
#   LISSR_ORIG_RECIPE_DIR   the upstream recipes (for before/after comparisons)
options(warn = 1)
suppressPackageStartupMessages({library(haven); library(dplyr)})

# env var wins; else compute a default under the package root, discovered on demand
resolve_lissr_path <- function(envvar, ...) {
  v <- Sys.getenv(envvar, "")
  if (nzchar(v)) return(v)
  root <- tryCatch(
    rprojroot::find_root(rprojroot::has_file("DESCRIPTION")),
    error = function(e) stop(
      envvar, " not set and package root not found; ",
      "run from within the lissr checkout or set the env var explicitly",
      call. = FALSE
    )
  )
  file.path(root, ...)
}

setwd(resolve_lissr_path("LISSR_ENGINE_DIR", "R"))
source("liss_merge_engine.R")

B   <- resolve_lissr_path("LISSR_VERIFICATION_DIR", "tests", "verification-bundle")
REC <- resolve_lissr_path("LISSR_RECIPE_DIR",       "inst", "recipes")
ok  <- function(cond, msg) { cat(if (isTRUE(cond)) "PASS " else "FAIL ", msg, "\n"); stopifnot(isTRUE(cond)) }

cat("== A1 chaining: snapshot semantics ==\n")
df <- data.frame(nomem_encr = 1:5, s013 = c(1, 2, 3, 4, 5))
rule <- list(rule_id = "T", action = "value_recode", description = "t",
             suffixes = list("013"), mapping = list(`1` = 2, `2` = 3))
r <- exec_harmonization_rule(df, rule, "w1", list(), c("w1"), list())
ok(identical(r$df$s013, c(2, 3, 3, 4, 5)), "map {1:2,2:3} does not chain")

cat("== A2 recode_to_na: recode alias + wave-scoped exclude ==\n")
df <- data.frame(nomem_encr = 1:3, s001 = c(9, 1, 9), s243 = c(9, 9, 2))
rule <- list(rule_id = "T2", action = "recode_to_na", description = "t",
             scope = "all_numeric", recode = list(`9` = ".dk"),
             exclude = list(list(suffixes = list("243"), waves = list("w1"))))
r <- exec_harmonization_rule(df, rule, "w1", list(), c("w1", "w2"), list())
ok(sum(r$df$s001 == 9, na.rm = TRUE) == 0 && sum(r$df$s243 == 9, na.rm = TRUE) == 2,
   "recode alias fires; excluded suffix untouched in scoped wave")
r2 <- exec_harmonization_rule(df, rule, "w2", list(), c("w1", "w2"), list())
ok(sum(r2$df$s243 == 9, na.rm = TRUE) == 0, "exclude limited to its waves")

cat("== A3 NO_TARGETS trace ==\n")
rule <- list(rule_id = "T3", action = "value_recode", description = "t",
             suffix = "013", mapping = list(`1` = 2))  # mis-keyed on purpose
r <- exec_harmonization_rule(df, rule, "w1", list(), c("w1"), list())
acts <- vapply(r$log, function(e) e$action, character(1))
ok("value_recode:NO_TARGETS" %in% acts, "mis-keyed rule leaves an audit trace")

cat("== A4 read_wave_file whitelist ==\n")
bad <- tempfile(fileext = ".pdf"); writeLines("x", bad)
ok(inherits(try(read_wave_file(bad), silent = TRUE), "try-error"),
   "non-data extension aborts instead of csv-parsing")

cat("== A5 discovery: version disambiguation on the real cd10c pair ==\n")
dd <- file.path(tempdir(), "cd"); dir.create(dd, showWarnings = FALSE)
file.copy(file.path(B, "01_cd10c_versions/cd10c_EN_1.0p.sav"), dd, overwrite = TRUE)
file.copy(file.path(B, "01_cd10c_versions/cd10c_EN_1.1p.sav"), dd, overwrite = TRUE)
stub <- list(wave_index = list(list(id = "cd10c", year = 2010, file_pattern = "cd10c_*")))
wf <- withCallingHandlers(discover_wave_files(stub, dd),
        warning = function(w) { cat("  [warn] ", conditionMessage(w), "\n"); invokeRestart("muffleWarning") })
ok(length(wf) == 1 && basename(wf[[1]]$paths) == "cd10c_EN_1.1p.sav" &&
     length(wf[[1]]$aux_paths) == 0, "highest version selected, 1.0p ignored")

cat("== A6 aux overlap contract aborts (declaring 1.0p as aux) ==\n")
mini <- list(
  meta = list(module = "cd", module_label = "Housing", schema_version = "1.0.0",
              recipe_version = "t", created = "t", source_spec = "t",
              covered_waves = list("cd10c")),
  global = list(id_variable = "nomem_encr", wave_variable = "wave_id",
                year_variable = "wave_year", labelled_policy = "to_numeric",
                missing_variable_policy = "warn_and_create_na",
                strip_label_whitespace = TRUE),
  wave_index = list(list(id = "cd10c", year = 2010,
                         file_pattern = "cd10c_EN_1.1p*",
                         aux_files = list("cd10c_EN_1.0p.sav"))),
  logging = list(log_file = "t.jsonl"))
e <- try(suppressWarnings(merge_liss_module(mini, dd, tempfile())), silent = TRUE)
ok(inherits(e, "try-error") && grepl("shares 3626 respondent", e), "shared-id aux stack blocked")

cat("== A7 strict mode gates on severity=error failure ==\n")
sy <- file.path(tempdir(), "sy"); dir.create(sy, showWarnings = FALSE)
haven::write_sav(data.frame(nomem_encr = 1:3, xx01a005 = c(1, 99, 2)),
                 file.path(sy, "xx01a_EN_1.0p.sav"))
mini2 <- mini
mini2$meta$module <- "xx"; mini2$meta$covered_waves <- list("xx01a")
mini2$wave_index <- list(list(id = "xx01a", year = 2001, file_pattern = "xx01a_*"))
mini2$validation_checks <- list(list(check_id = "V1", type = "value_absence",
                                     suffixes = list("005"),
                                     forbidden_values = list(99), severity = "error"))
outd <- file.path(tempdir(), "sout")
e <- try(suppressWarnings(merge_liss_module(mini2, sy, outd, strict = TRUE)), silent = TRUE)
ok(inherits(e, "try-error") && !file.exists(file.path(outd, "xx_merged.sav")),
   "strict aborts before writing outputs")
r <- suppressWarnings(merge_liss_module(mini2, sy, outd, strict = FALSE))
ok(file.exists(file.path(outd, "xx_merged.sav")), "non-strict still writes")

cat("== A8 unknown check type reports SKIP not PASS ==\n")
v <- run_validations(data.frame(nomem_encr = 1), 
                     list(list(check_id = "C", type = "bogus_type", severity = "error")), list())
ok(is.na(v$results[[1]]$passed) && v$n_skip == 1 && v$error_count == 0,
   "unimplemented type is NA/SKIP and never a hard error")
cat("all unit checks passed\n")
