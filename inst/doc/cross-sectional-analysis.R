## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----download-single----------------------------------------------------------
# library(lissr)
# 
# # authenticate (once per session)
# liss_login()
# 
# # build the file inventory
# bp <- liss_blueprint()
# 
# # filter to the latest Health wave, SPSS format
# latest_health <- bp |>
#   dplyr::filter(
#     module == "Health",
#     type   == "spss"
#   ) |>
#   dplyr::filter(wave == max(wave))
# 
# latest_health
# #> # A tibble: 1 × 8
# #>   module module_id  wave wave_id type  name       file              path
# #>   <chr>      <int> <int>   <int> <chr> <chr>      <chr>             <chr>
# #> 1 Health        18    17    1054 spss  ch24q 1.0p ch24q_1_0p_EN.sav /down…
# 
# # download just that one file
# liss_download(latest_health, .dir = "data/ch")

## ----clean-via-recipe---------------------------------------------------------
# library(haven)
# library(dplyr)
# 
# # option A: read raw and clean yourself
# raw <- haven::read_sav("data/ch/ch24q_1_0p_EN.sav")
# dim(raw)
# #> [1] 4892  271
# 
# # option B: use the merge engine for a single wave
# # (this applies prefix stripping, sentinel recoding, and labelled policy)
# recipe <- liss_recipe("ch")
# 
# # temporarily trim the recipe to just the wave you need
# recipe$wave_index <- purrr::keep(
#   recipe$wave_index,
#   ~ .x$id == "ch24q"
# )
# 
# result <- merge_liss_module(recipe, data_dir = "data/ch", output_dir = "output")
# health <- result$data
# dim(health)
# #> [1] 4892  265

## ----attach-demographics------------------------------------------------------
# # download the background variables file for the same fieldwork period
# # (health wave 24q was fielded around November 2024)
# bg_files <- bp |>
#   dplyr::filter(
#     module == "Background Variables",
#     type   == "spss",
#     wave   == 202411  # YYYYMM matching fieldwork period
#   )
# 
# liss_download(bg_files, .dir = "data/avars")
# 
# avars <- haven::read_sav("data/avars/avars_202411_EN_1_0p.sav") |>
#   haven::zap_labels() |>
#   dplyr::select(
#     nomem_encr,
#     age       = leeftijd,
#     sex       = geslacht,
#     edu_level = oplcat,
#     hh_income = nettohh_f,
#     urban     = sted
#   )
# 
# analysis_df <- health |>
#   dplyr::left_join(avars, by = "nomem_encr")
# 
# nrow(analysis_df)
# #> [1] 4892

## ----model--------------------------------------------------------------------
# analysis_df <- analysis_df |>
#   dplyr::mutate(
#     srh = factor(s001, levels = 1:5,
#                  labels = c("poor", "moderate", "good", "very good", "excellent")),
#     female = as.integer(sex == 2)
#   )
# 
# fit <- MASS::polr(srh ~ edu_level + age + female, data = analysis_df)
# summary(fit)

