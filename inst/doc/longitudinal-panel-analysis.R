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
# #> # A tibble: 17 × 2
# #>    wave_id     n
# #>    <chr>   <int>
# #>  1 ch07a    6871
# #>  2 ch08b    6386
# #>  3 ch09c    6222
# #>  ...

## ----participation------------------------------------------------------------
# # how many waves did each respondent participate in?
# participation <- panel |>
#   group_by(nomem_encr) |>
#   summarise(n_waves = n_distinct(wave_id), .groups = "drop")
# 
# # distribution of participation
# table(participation$n_waves)
# #>    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17
# #> 1842  801  602  489  431  378  354  307  301  285  284  300  343  388  482  723 2391
# 
# # balanced sub-panel: respondents present in all 17 waves
# balanced_ids <- participation |>
#   filter(n_waves == max(n_waves)) |>
#   pull(nomem_encr)
# 
# length(balanced_ids)
# #> [1] 2391

## ----attrition----------------------------------------------------------------
# # tag respondents who appear in wave 1 but not in the final wave
# first_wave <- "ch07a"
# last_wave  <- "ch24q"
# 
# baseline <- panel |>
#   filter(wave_id == first_wave) |>
#   mutate(
#     survived = nomem_encr %in%
#       (panel |> filter(wave_id == last_wave) |> pull(nomem_encr))
#   )
# 
# # compare baseline self-rated health between survivors and attriters
# baseline |>
#   group_by(survived) |>
#   summarise(
#     n         = n(),
#     mean_srh  = mean(s001, na.rm = TRUE),
#     mean_age  = mean(s002, na.rm = TRUE),
#     .groups   = "drop"
#   )
# #> # A tibble: 2 × 4
# #>   survived     n mean_srh mean_age
# #>   <lgl>    <int>    <dbl>    <dbl>
# #> 1 FALSE     4480     3.08     48.5
# #> 2 TRUE      2391     3.25     43.2

## ----fixed-effects------------------------------------------------------------
# library(fixest)
# 
# # self-rated health (s001) regressed on a time-varying predictor
# # e.g. BMI (s038) with person and year fixed effects
# fe_model <- fixest::feols(
#   s001 ~ s038 | nomem_encr + wave_year,
#   data = panel
# )
# 
# summary(fe_model)

## ----boundary-check-----------------------------------------------------------
# # inspect boundary flags created by the merge engine
# flag_cols <- grep("_flag$|_period$|_era$", names(panel), value = TRUE)
# flag_cols
# #> [1] "ecig_era_flag"  "work_capacity_period"
# 
# # if your analysis touches e-cigarette variables, restrict to waves
# # on one side of the boundary — or include the era flag as a control
# panel |>
#   filter(!is.na(ecig_era_flag)) |>
#   count(ecig_era_flag, wave_year)

## ----event-study--------------------------------------------------------------
# # define treatment: respondents living in province X at baseline
# # (you would attach this from background variables)
# panel <- panel |>
#   mutate(
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
# # keep only 2014–2018
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

