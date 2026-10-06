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
# #> 1 Health        18    18    1102 spss  ch25r 1.0p ch25r_1_0p_EN.sav /down…
#
# # download just that one file
# liss_download(latest_health, .dir = "data/ch")

## ----clean-via-recipe---------------------------------------------------------
# library(haven)
# library(dplyr)
#
# # option A: read raw and clean yourself
# raw <- haven::read_sav("data/ch/ch25r_1_0p_EN.sav")
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
#   ~ .x$id == "ch25r"
# )
#
# result <- merge_liss_module(recipe, data_dir = "data/ch", output_dir = "output")
# health <- result$data
# dim(health)
# #> [1] 4892  265

## ----attach-demographics------------------------------------------------------
# expected_month <- 202511L
# survey_month <- unique(health$fieldwork_ym[!is.na(health$fieldwork_ym)])
# stopifnot(length(survey_month) == 1L, survey_month == expected_month)
# bg_raw <- haven::read_sav("data/avars/avars_202511_EN_1_0p.sav")
# bg_month <- unique(as.integer(bg_raw$wave))
# stopifnot(length(bg_month) == 1L, !anyNA(bg_month),
#           bg_month == expected_month)
#
# avars <- haven::zap_labels(bg_raw) %>%
#   dplyr::mutate(fieldwork_ym = as.integer(wave)) %>%
#   dplyr::select(
#     nomem_encr, fieldwork_ym,
#     age       = leeftijd,
#     sex       = geslacht,
#     edu_level = oplcat,
#     hh_income = nettohh_f,
#     urban     = sted
#   )
#
# bg_keys <- avars[c("nomem_encr", "fieldwork_ym")]
# stopifnot(!anyNA(bg_keys), !anyDuplicated(bg_keys))
# analysis_df <- dplyr::left_join(
#   health, avars,
#   by = c("nomem_encr", "fieldwork_ym"), na_matches = "never"
# )
# stopifnot(nrow(analysis_df) == nrow(health))
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

