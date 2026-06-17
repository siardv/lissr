# ============================================================================
# run_merge.R - example usage of the lissr panel merge engine
# ============================================================================
# attaches the installed package (engine, executors, and bundled recipes) and
# demonstrates the single-module, batch, and validate-only workflows. LISS
# wave data is restricted and is NOT shipped with the package; set data_root
# and output_dir to your own paths before running.

library(lissr)

# point these at your local LISS data and a writable output directory
data_root  <- "~/liss/data"      # root holding one subdirectory per module
output_dir <- "~/liss/output"

# -- single module merge -----------------------------------------------------
# merge_liss_module loads and validates the recipe against the canonical
# schema before any merge work begins (fail-fast), then writes to output_dir:
#   ch_merged.sav          merged data (SPSS .sav, variable/value labels kept)
#   ch_merge_log.jsonl     audit-grade structured log
#   ch_merge_summary.json  per-run summary
#   ch_merge_report.txt    human-readable report
ch_recipe <- system.file("recipes", "ch_merge_recipe.yml", package = "lissr")
result <- merge_liss_module(ch_recipe,
                            data_dir   = file.path(data_root, "ch"),
                            output_dir = output_dir)

# -- multi-module batch merge ------------------------------------------------
# every recipe is validated before any merge begins. data_root holds one
# subdirectory per module (data_root/ch, data_root/cv, ...).
modules <- c("ca", "cd", "cf", "ch", "ci", "cp", "cr", "cs", "cv", "cw")
recipe_paths <- system.file("recipes", paste0(modules, "_merge_recipe.yml"),
                            package = "lissr")
recipe_paths <- recipe_paths[nzchar(recipe_paths)]

results <- merge_liss_modules(recipe_paths,
                              data_dir   = data_root,
                              output_dir = output_dir)

# -- validate recipes without merging ----------------------------------------
# load_recipe() parses and validates a recipe, aborting on the first schema
# violation. for batch pass/fail reporting across many recipes, use the
# validate_recipe_cli.R script.
invisible(lapply(recipe_paths, load_recipe))

# -- new wave onboarding -----------------------------------------------------
# onboard_new_wave() offers a semi-automated workflow for extending a recipe
# to a newly released wave; see ?onboard_new_wave.
