## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(lissr)
#
# results <- merge_liss_modules(
#   data_dir   = "liss",
#   output_dir = "./output"
# )

## -----------------------------------------------------------------------------
# recipe <- liss_recipe("ch")
# result <- merge_liss_module(
#   recipe,
#   data_dir   = "liss/ch",
#   output_dir = "./output"
# )

## -----------------------------------------------------------------------------
# custom_results <- merge_liss_modules(
#   recipe_paths = "my_ch_recipe.yml",
#   data_dir   = "liss",
#   output_dir = "./output"
# )

## -----------------------------------------------------------------------------
# panel <- merge_liss_panel(results, write_to = "./output/liss_panel.sav")
#
# # only respondent-years present in all modules
# panel_inner <- merge_liss_panel(results, join_type = "inner")

## -----------------------------------------------------------------------------
# recipe <- liss_recipe("ch")
# validate_recipe(recipe, "ch_merge_recipe.yml")

## -----------------------------------------------------------------------------
# onboard_new_wave(
#   recipe_path  = system.file("recipes", "ch_merge_recipe.yml", package = "lissr"),
#   new_file     = "ch25r_EN_1.0p.sav",
#   prev_wave_id = "ch24q"
# )

## -----------------------------------------------------------------------------
# # example: attach age to a Health survey using selected monthly snapshots
# survey <- haven::read_sav("output/ch_merged.sav")
# table(survey$fieldwork_ym, useNA = "ifany")
#
# sources <- data.frame(
#   sav_path = c(
#     "data/avars/avars_202411_EN_1.0p.sav",
#     "data/avars/avars_202412_EN_1.0p.sav"
#   ),
#   expected_month = c(202411L, 202412L)
# )
#
# attachment <- liss_attach_background(
#   data = survey,
#   sources = sources,
#   month_col = "fieldwork_ym",
#   variables = "leeftijd"
# )
#
# merged <- attachment$data
# stopifnot(nrow(merged) == nrow(survey))
# attachment$audit
# attachment$provenance
