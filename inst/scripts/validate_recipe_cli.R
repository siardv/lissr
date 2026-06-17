#!/usr/bin/env Rscript
# ============================================================================
# validate_recipe_cli.R - schema validator CLI for LISS merge recipes
# ============================================================================
# validates recipes against the canonical schema with lissr's load_recipe(),
# which parses and fail-fast validates each recipe (aborting on a violation).
# arguments may be recipe file paths, globs, or bare module codes (e.g. "ch"),
# the latter resolved against the recipes bundled with the installed package.
# suitable for CI pipelines or a pre-commit hook; exits non-zero on any failure.
#
# usage:
#   Rscript validate_recipe_cli.R ch_merge_recipe.yml
#   Rscript validate_recipe_cli.R *.yml
#   Rscript validate_recipe_cli.R ch cv ci

suppressPackageStartupMessages(library(lissr))

# resolve one argument to zero or more recipe paths. an existing path or a
# glob match is used as-is; otherwise the argument is treated as a module code
# and looked up among the package's bundled recipes.
resolve_arg <- function(arg) {
  hits <- Sys.glob(arg)
  if (length(hits) > 0) return(hits)
  if (file.exists(arg)) return(arg)
  bundled <- system.file("recipes", paste0(arg, "_merge_recipe.yml"),
                         package = "lissr")
  if (nzchar(bundled)) return(bundled)
  cat(sprintf("  SKIP  %s (no matching file, glob, or bundled module)\n", arg))
  character(0)
}

# validate a single recipe; load_recipe() aborts on any schema violation.
validate_one <- function(path) {
  tryCatch({
    load_recipe(path)
    cat(sprintf("  PASS  %s\n", path))
    TRUE
  }, error = function(e) {
    cat(sprintf("  FAIL  %s\n        %s\n", path, conditionMessage(e)))
    FALSE
  })
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cat("usage: Rscript validate_recipe_cli.R <recipe.yml | module_code> [...]\n")
    quit(status = 1)
  }

  paths <- unique(unlist(lapply(args, resolve_arg)))
  if (length(paths) == 0) {
    cat("no recipes to validate\n")
    quit(status = 1)
  }

  cat(sprintf("validating %d recipe(s) with lissr %s\n",
              length(paths), as.character(utils::packageVersion("lissr"))))

  results <- vapply(paths, validate_one, logical(1))

  cat(sprintf("results: %d pass, %d fail\n", sum(results), sum(!results)))
  if (any(!results)) quit(status = 1)
}
