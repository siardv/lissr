## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----01-download--------------------------------------------------------------
# # R/01_download.R
# library(lissr)
# 
# liss_login()
# bp <- liss_blueprint()
# 
# # download Health module (all waves, SPSS format)
# health_files <- dplyr::filter(bp, module == "Health", type == "spss")
# liss_download(health_files, .dir = "data/ch")
# 
# # download background variables for all available months
# bg_files <- dplyr::filter(bp, module == "Background Variables", type == "spss")
# liss_download(bg_files, .dir = "data/avars")

## ----02-merge-----------------------------------------------------------------
# # R/02_merge.R
# library(lissr)
# 
# # use a custom recipe stored in the project (version-controlled)
# recipe <- load_recipe("recipes/ch_custom.yml")
# 
# result <- merge_liss_module(
#   recipe,
#   data_dir   = "data/ch",
#   output_dir = "output"
# )

## ----read-log-----------------------------------------------------------------
# log <- jsonlite::stream_in(file("output/ch_merge_log.jsonl"), verbose = FALSE)
# 
# # total transformations
# nrow(log)
# #> [1] 287
# 
# # breakdown by action type
# table(log$action)
# #> add_era_flag      drop   note_only recode_to_na  strip_prefix
# #>            4         3          12          136            17
# 
# # total values recoded to NA across all waves
# sum(log$values_changed, na.rm = TRUE)
# #> [1] 48293

## ----read-summary-------------------------------------------------------------
# summary <- jsonlite::read_json("output/ch_merge_summary.json")
# summary$total_rows
# #> [1] 92847
# summary$total_cols
# #> [1] 265
# summary$total_waves
# #> [1] 17

## ----create-golden------------------------------------------------------------
# # run after the first successful merge
# merged <- haven::read_sav("output/ch_merged.sav")
# 
# golden <- list(
#   row_count          = nrow(merged),
#   col_count          = ncol(merged),
#   wave_count         = length(unique(merged$wave_id)),
#   unique_respondents = length(unique(merged$nomem_encr)),
#   column_names       = sort(names(merged)),
#   wave_ids           = sort(unique(merged$wave_id)),
#   wave_row_counts    = as.list(table(merged$wave_id)),
#   na_rates           = lapply(merged, \(x) round(mean(is.na(x)), 4)),
#   column_types       = vapply(merged, \(x) class(x)[1], character(1))
# )
# 
# jsonlite::write_json(golden, "golden/ch_golden.json",
#                      pretty = TRUE, auto_unbox = TRUE)

## ----test-golden--------------------------------------------------------------
# # tests/test_merge_regression.R
# library(testthat)
# library(jsonlite)
# 
# test_that("CH merge output matches golden reference", {
#   merged <- haven::read_sav("output/ch_merged.sav")
#   golden <- jsonlite::read_json("golden/ch_golden.json")
# 
#   expect_equal(nrow(merged), golden$row_count)
#   expect_equal(ncol(merged), golden$col_count)
#   expect_equal(length(unique(merged$wave_id)), golden$wave_count)
#   expect_equal(sort(names(merged)), golden$column_names)
# 
#   # check per-wave row counts
#   actual_wave_counts <- as.list(table(merged$wave_id))
#   for (w in names(golden$wave_row_counts)) {
#     expect_equal(
#       actual_wave_counts[[w]],
#       golden$wave_row_counts[[w]],
#       label = paste("wave", w, "row count")
#     )
#   }
# 
#   # check NA rate drift (warn at 5 percentage points)
#   for (col in names(golden$na_rates)) {
#     if (col %in% names(merged)) {
#       actual_na <- round(mean(is.na(merged[[col]])), 4)
#       expected_na <- golden$na_rates[[col]]
#       expect_lt(
#         abs(actual_na - expected_na), 0.05,
#         label = paste("NA rate drift in", col)
#       )
#     }
#   }
# })

## ----onboard------------------------------------------------------------------
# report <- onboard_new_wave(
#   recipe_path  = "recipes/ch_custom.yml",
#   new_file     = "data/ch/ch25r_EN_1.0p.sav",
#   prev_wave_id = "ch24q"
# )

