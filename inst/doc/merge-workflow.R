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
# # example: merge Health survey with background variables
# survey <- haven::read_sav("output/ch_merged.sav")
# table(survey$fieldwork_ym, useNA = "ifany")
#
# # read local monthly SPSS files; verify each observed period
# bg_files <- list.files("data/avars/", pattern = "\\.sav$", full.names = TRUE)
# stopifnot(length(bg_files) > 0L)
# bg_data <- purrr::map_dfr(bg_files, function(f) {
#   bg <- haven::read_sav(f)
#   bg_month <- unique(as.integer(bg$wave))
#   stopifnot(length(bg_month) == 1L, !anyNA(bg_month),
#             bg_month %in% survey$fieldwork_ym)
#   dplyr::mutate(bg, fieldwork_ym = as.integer(wave))
# })
#
# bg_keys <- bg_data[c("nomem_encr", "fieldwork_ym")]
# stopifnot(!anyNA(bg_keys), !anyDuplicated(bg_keys))
# merged <- dplyr::left_join(
#   survey, bg_data,
#   by = c("nomem_encr", "fieldwork_ym"), na_matches = "never"
# )
# stopifnot(nrow(merged) == nrow(survey))
