## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----download-all-------------------------------------------------------------
# library(lissr)
# library(dplyr)
#
# liss_login()
# bp <- liss_blueprint()
#
# # all SPSS files for the Health module
# health_files <- bp |>
#   filter(module == "Health", type == "spss")
#
# liss_download(health_files, .dir = "data/ch")

## ----merge--------------------------------------------------------------------
# recipe <- liss_recipe("ch")
# result <- merge_liss_module(recipe, data_dir = "data/ch", output_dir = "output")
#
# panel <- result$data
#
# # the stacked panel has one row per person-wave
# panel |> count(wave_id) |> print(n = 20)
# #> # A tibble: 18 × 2
# #>    wave_id     n
# #>    <chr>   <int>
# #>  1 ch07a    6871
# #>  2 ch08b    6386
# #>  3 ch09c    6222
# #>  ...  (18 waves, ch07a through ch25r; counts are illustrative)

## ----participation------------------------------------------------------------
# # how many waves did each respondent participate in?
# participation <- panel |>
#   group_by(nomem_encr) |>
#   summarise(n_waves = n_distinct(wave_id), .groups = "drop")
#
# # distribution of participation (illustrative counts)
# table(participation$n_waves)
# #>    1    2    3   ...   16   17   18
# #> 1842  801  602   ...  482  723 2103
#
# # balanced sub-panel: respondents present in all 18 waves
# balanced_ids <- participation |>
#   filter(n_waves == max(n_waves)) |>
#   pull(nomem_encr)
#
# length(balanced_ids)
# #> [1] 2103

## ----attrition----------------------------------------------------------------
# # tag respondents who appear in wave 1 but not in the final wave
# first_wave <- "ch07a"
# last_wave  <- "ch25r"
#
# baseline <- panel |>
#   filter(wave_id == first_wave) |>
#   mutate(
#     survived = nomem_encr %in%
#       (panel |> filter(wave_id == last_wave) |> pull(nomem_encr))
#   )
#
# # compare baseline self-rated health between survivors and attriters
# # (age is not a survey-module column; attach it from the Background
# # Variables file if you need it in this comparison)
# baseline |>
#   group_by(survived) |>
#   summarise(
#     n         = n(),
#     mean_srh  = mean(s001, na.rm = TRUE),
#     .groups   = "drop"
#   )
# #> # A tibble: 2 × 3
# #>   survived     n mean_srh
# #>   <lgl>    <int>    <dbl>
# #> 1 FALSE     4480     3.08
# #> 2 TRUE      2103     3.25

## ----fixed-effects------------------------------------------------------------
# library(fixest)
#
# # self-rated health (s001) regressed on a time-varying predictor with
# # person and year fixed effects. pick the predictor from your module's
# # codebook and use its harmonised suffix; `x_var` is a placeholder
# panel <- panel |> mutate(x_var = s004)  # replace s004 with your item
#
# fe_model <- fixest::feols(
#   s001 ~ x_var | nomem_encr + wave_year,
#   data = panel
# )
#
# summary(fe_model)

## ----boundary-check-----------------------------------------------------------
# # inspect boundary flags created by the merge engine
# flag_cols <- grep("_flag$|_period$|_era$|_present$", names(panel),
#                   value = TRUE)
# flag_cols
# #> [1] "cancer_definition_era"        "alzheimer_definition_era"
# #> [3] "ch_hca_items_present"         "ch_ecig_dental_items_present"
# #> [5] "ch_years_not_working_present" "ch_covid_n2o_items_present"
# #> [7] "ch_menopause_module_present"  "s020_wording_period"
#
# # if your analysis touches the e-cigarette items, restrict to waves
# # where the block was fielded — or include the flag as a control
# panel |>
#   filter(ch_ecig_dental_items_present) |>
#   count(wave_year)

## ----event-study--------------------------------------------------------------
# # define treatment: respondents exposed to the policy, identified from
# # a baseline covariate you attach (for example province of residence
# # from the Background Variables file). `treated_ids` is your design's
# # treatment group
# treated_ids <- c( )  # fill from your design
#
# panel <- panel |>
#   mutate(
#     treated  = as.integer(nomem_encr %in% treated_ids),
#     post     = as.integer(wave_year >= 2016),
#     rel_year = wave_year - 2016
#   )
#
# # event-study with staggered treatment
# es_model <- fixest::feols(
#   s001 ~ i(rel_year, treated, ref = -1) | nomem_encr + wave_year,
#   data = panel
# )
#
# fixest::iplot(es_model, main = "Event study: self-rated health")

## ----growth-curve-------------------------------------------------------------
# library(lme4)
#
# # linear growth curve: health trajectories over time
# panel <- panel |>
#   mutate(year_centered = wave_year - 2007)
#
# growth <- lme4::lmer(
#   s001 ~ year_centered + (1 + year_centered | nomem_encr),
#   data = panel
# )
#
# summary(growth)

## ----subset-waves-------------------------------------------------------------
# recipe <- liss_recipe("ch")
#
# # keep only 2015-2018 (ch skipped 2014; there is no ch14 wave)
# target_waves <- c("ch15h", "ch16i", "ch17j", "ch18k")
# recipe$wave_index <- purrr::keep(
#   recipe$wave_index,
#   ~ .x$id %in% target_waves
# )
#
# result <- merge_liss_module(recipe, data_dir = "data/ch", output_dir = "output")

## ----audit--------------------------------------------------------------------
# log <- jsonlite::stream_in(file("output/ch_merge_log.jsonl"), verbose = FALSE)
#
# # how many values were recoded to NA?
# log |>
#   filter(action == "recode_to_na") |>
#   summarise(total_recoded = sum(values_changed, na.rm = TRUE))

