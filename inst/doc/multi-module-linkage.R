## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----merge-all----------------------------------------------------------------
# library(lissr)
# library(dplyr)
# library(purrr)
# 
# liss_login()
# 
# # download all waves for three modules
# bp <- liss_blueprint()
# for (mod in c("Health", "Economic Integration: Income", "Politics and Values")) {
#   files <- bp |> filter(module == mod, type == "spss")
#   mod_dir <- file.path("data", tolower(substr(mod, 1, 2)))
#   liss_download(files, .dir = mod_dir)
# }
# 
# # merge each module with its own recipe
# modules <- c("ch", "ci", "cv")
# results <- purrr::map(modules, function(mod) {
#   recipe <- liss_recipe(mod)
#   merge_liss_module(
#     recipe,
#     data_dir   = file.path("data", mod),
#     output_dir = file.path("output", mod)
#   )
# }) |> purrr::set_names(modules)

## ----batch-merge--------------------------------------------------------------
# recipe_paths <- purrr::map_chr(modules, ~ {
#   system.file("recipes", paste0(.x, "_merge_recipe.yml"), package = "lissr")
# })
# 
# results <- merge_liss_modules(recipe_paths, data_dir = "data", output_dir = "output")

## ----select-vars--------------------------------------------------------------
# # health: self-rated health (s001) and BMI (s038)
# health <- results$ch$data |>
#   select(nomem_encr, wave_year,
#          srh = s001,
#          bmi = s038)
# 
# # income: main employment status (s001 in CI = net personal income)
# income <- results$ci$data |>
#   select(nomem_encr, wave_year,
#          net_income = s001,
#          employed   = s006)
# 
# # politics: voted in last election (s012 in CV)
# politics <- results$cv$data |>
#   select(nomem_encr, wave_year,
#          voted_last_election = s012,
#          political_trust     = s002)

## ----check-alignment----------------------------------------------------------
# # which years does each module cover?
# purrr::map(results, ~ sort(unique(.x$data$wave_year)))
# #> $ch
# #>  [1] 2007 2008 2009 2010 2011 2012 2013 2015 2016 2017 2018 2019 2020 2021
# #>      2022 2023 2024
# #>
# #> $ci
# #>  [1] 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020
# #>      2021 2022 2023 2024
# #>
# #> $cv
# #>  [1] 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021
# #>      2022 2023 2024
# 
# # find the overlapping years
# common_years <- Reduce(
#   intersect,
#   purrr::map(results, ~ unique(.x$data$wave_year))
# )
# common_years
# #> [1] 2008 2009 2010 2011 2012 2013 2015 2016 2017 2018 2019 2020 2021 2022
# #>     2023 2024

## ----join---------------------------------------------------------------------
# # full join preserves all person-years from any module
# linked <- health |>
#   full_join(income,   by = c("nomem_encr", "wave_year")) |>
#   full_join(politics, by = c("nomem_encr", "wave_year"))
# 
# dim(linked)
# #> [1] 125438      7
# 
# # restrict to the common years for a balanced design
# linked_balanced <- linked |>
#   filter(wave_year %in% common_years)

## ----attach-bg----------------------------------------------------------------
# # read all monthly background variable files
# bg_files <- list.files("data/avars", pattern = "\\.sav$", full.names = TRUE)
# bg <- purrr::map_dfr(bg_files, \(f) {
#   haven::read_sav(f) |>
#     haven::zap_labels() |>
#     select(nomem_encr,
#            fieldwork_ym = wave,
#            age       = leeftijd,
#            sex       = geslacht,
#            edu       = oplcat,
#            hh_income = nettohh_f,
#            urban     = sted)
# })
# 
# # create a wave_year column to join on
# bg <- bg |>
#   mutate(wave_year = as.integer(substr(fieldwork_ym, 1, 4))) |>
#   # keep one snapshot per person-year (e.g. November of each year)
#   group_by(nomem_encr, wave_year) |>
#   slice_max(fieldwork_ym, n = 1, with_ties = FALSE) |>
#   ungroup()
# 
# linked <- linked |>
#   left_join(bg, by = c("nomem_encr", "wave_year"))

## ----model--------------------------------------------------------------------
# library(fixest)
# 
# linked <- linked |>
#   mutate(job_loss = lag(employed) == 1 & employed == 0,
#          .by = nomem_encr)
# 
# fe <- feols(
#   srh ~ job_loss + age + I(age^2) | nomem_encr + wave_year,
#   data = linked
# )
# 
# summary(fe)

## ----era-flags----------------------------------------------------------------
# # list all era/period/flag columns across modules
# purrr::map(results, ~ {
#   grep("_flag$|_period$|_era$", names(.x$data), value = TRUE)
# })

## ----full-batch---------------------------------------------------------------
# all_modules <- c("ch", "cv", "cd", "cf", "cw", "cp", "cs", "ci")
# recipe_paths <- purrr::map_chr(all_modules, ~ {
#   system.file("recipes", paste0(.x, "_merge_recipe.yml"), package = "lissr")
# })
# 
# all_results <- merge_liss_modules(
#   recipe_paths, data_dir = "data", output_dir = "output"
# )
# 
# # join all modules progressively
# linked_all <- all_results[[1]]$data |>
#   select(nomem_encr, wave_year, srh = s001)
# 
# for (mod in names(all_results)[-1]) {
#   vars_to_keep <- c("nomem_encr", "wave_year",
#                      # pick a few variables from each
#                      head(setdiff(names(all_results[[mod]]$data),
#                                   c("nomem_encr", "wave_id", "wave_year")), 5))
#   linked_all <- linked_all |>
#     full_join(
#       all_results[[mod]]$data |> select(all_of(vars_to_keep)),
#       by = c("nomem_encr", "wave_year")
#     )
# }
# 
# dim(linked_all)

