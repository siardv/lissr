## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(lissr)
# 
# recipe <- liss_recipe("ch")
# result <- merge_liss_module(
#   recipe,
#   data_dir   = "liss/ch",
#   output_dir = "./output"
# )

## -----------------------------------------------------------------------------
# modules <- c("ch", "cv", "cd", "cf", "cw", "cp", "cs", "ci")
# recipe_paths <- purrr::map_chr(modules, ~ {
#   system.file("recipes", paste0(.x, "_merge_recipe.yml"), package = "lissr")
# })
# 
# results <- merge_liss_modules(
#   recipe_paths,
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
# 
# # read avars files and tag each with YYYYMM from the filename
# bg_files <- list.files("data/avars/", pattern = "\\.sav$", full.names = TRUE)
# bg_data  <- purrr::map_dfr(bg_files, function(f) {
#   ym <- as.integer(stringr::str_extract(basename(f), "\\d{6}"))
#   haven::read_sav(f) |> dplyr::mutate(fieldwork_ym = ym)
# })
# 
# merged <- dplyr::left_join(
#   survey, bg_data,
#   by = c("nomem_encr", "fieldwork_ym")
# )

