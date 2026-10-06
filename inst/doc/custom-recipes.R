## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----fork-recipe--------------------------------------------------------------
# library(lissr)
#
# # load the built-in Health recipe as a list
# recipe <- liss_recipe("ch")
#
# # inspect its structure
# str(recipe, max.level = 1)
# #> List of 10
# #>  $ meta               :List of 8
# #>  $ global             :List of 7
# #>  $ wave_index         :List of 18
# #>  $ variable_rules     :List of 17
# #>  $ harmonization_rules:List of 4
# #>  $ boundary_rules     :List of 12
# #>  $ drop_retain_rules  :List of 3
# #>  $ derived_variables  :List of 6
# #>  $ validation_checks  :List of 10
# #>  $ logging            :List of 5

## ----subset-waves-------------------------------------------------------------
# # keep only post-2015 waves for an analysis of recent trends
# recipe$wave_index <- purrr::keep(
#   recipe$wave_index,
#   ~ as.integer(.x$year) >= 2015
# )
#
# # update the metadata to reflect the change
# recipe$meta$covered_waves <- purrr::map_chr(recipe$wave_index, "id")
# recipe$meta$notes <- "forked from built-in ch recipe; restricted to 2015+"

## ----labelled-policy----------------------------------------------------------
# recipe$global$labelled_policy <- "to_factor"

## ----add-rule-----------------------------------------------------------------
# recipe$harmonization_rules <- append(
#   recipe$harmonization_rules,
#   list(list(
#     rule_id     = "CUSTOM_01_srh_binary",
#     action      = "value_recode",
#     description = "recode SRH to binary: 1-2 -> 0, 3-5 -> 1",
#     suffixes    = list("001"),
#     mapping     = list("1" = 0, "2" = 0, "3" = 1, "4" = 1, "5" = 1),
#     waves       = "all"
#   ))
# )

## ----validate-run-------------------------------------------------------------
# # the schema validator catches typos in action names, missing fields, etc.
# validate_recipe(recipe, "custom_ch_recipe")
#
# # run the merge
# result <- merge_liss_module(recipe, data_dir = "data/ch", output_dir = "output/custom")

## ----save-recipe--------------------------------------------------------------
# yaml::write_yaml(recipe, "my_ch_recipe.yml")
#
# # later, reload it
# recipe <- load_recipe("my_ch_recipe.yml")

## ----from-scratch-------------------------------------------------------------
# my_recipe <- list(
#   meta = list(
#     module        = "covid",
#     module_label  = "COVID Attitudes Special Study",
#     recipe_version = "1.0.0",
#     created       = format(Sys.Date()),
#     source_spec   = "covid_codebook_EN.pdf",
#     covered_waves = list("covid20a", "covid20b", "covid21c"),
#     schema_version = "1.0.0"
#   ),
#
#   global = list(
#     id_variable             = "nomem_encr",
#     wave_variable           = "wave_id",
#     year_variable           = "wave_year",
#     labelled_policy         = "to_numeric",
#     missing_variable_policy = "warn_and_create_na",
#     strip_label_whitespace  = TRUE
#   ),
#
#   wave_index = list(
#     list(id = "covid20a", year = 2020, file_pattern = "covid20a_*"),
#     list(id = "covid20b", year = 2020, file_pattern = "covid20b_*"),
#     list(id = "covid21c", year = 2021, file_pattern = "covid21c_*")
#   ),
#
#   variable_rules = list(
#     list(
#       rule_id     = "V01_strip_prefix",
#       action      = "strip_prefix",
#       description = "remove wave prefix from all columns"
#     )
#   ),
#
#   harmonization_rules = list(
#     list(
#       rule_id     = "H01_sentinel_recode",
#       action      = "recode_to_na",
#       description = "recode -9 (DK) and -8 (PNTS) to NA",
#       scope       = "all_numeric",
#       codes       = list(-9, -8)
#     )
#   ),
#
#   boundary_rules = list(),
#   drop_retain_rules = list(),
#   derived_variables = list(),
#   validation_checks = list(),
#
#   logging = list(
#     log_file        = "covid_merge_log.jsonl",
#     report_file     = "covid_merge_report.txt",
#     summary_artifact = list(enabled = TRUE)
#   )
# )
#
# # validate before first use
# validate_recipe(my_recipe, "covid_recipe.yml")
#
# # save to disk
# yaml::write_yaml(my_recipe, "covid_merge_recipe.yml")

## ----keep-sentinels-----------------------------------------------------------
# recipe <- liss_recipe("ch")
#
# # remove all recode_to_na rules
# recipe$harmonization_rules <- purrr::discard(
#   recipe$harmonization_rules,
#   ~ .x$action == "recode_to_na"
# )
#
# # optionally add a rule to rename sentinels instead of dropping them
# recipe$harmonization_rules <- append(
#   recipe$harmonization_rules,
#   list(list(
#     rule_id     = "CUSTOM_keep_dk",
#     action      = "value_recode",
#     description = "recode -9 to 97 (DK) and -8 to 98 (PNTS) for explicit modelling",
#     suffixes    = list("001"),
#     mapping     = list("-9" = 97, "-8" = 98),
#     waves       = "all"
#   ))
# )
#
# validate_recipe(recipe, "ch_keep_sentinels")
# result <- merge_liss_module(recipe, data_dir = "data/ch", output_dir = "output/sentinels")

## ----comparability------------------------------------------------------------
# recipe <- liss_recipe("cs")
#
# recipe$boundary_rules <- append(
#   recipe$boundary_rules,
#   list(list(
#     rule_id     = "CUSTOM_B01_sport_freq_redesign",
#     action      = "add_period_flag",
#     description = "sport frequency question redesigned in cs20m",
#     flag_column = "sport_freq_era",
#     waves_early = list("cs08a", "cs09b", "cs10c", "cs11d", "cs12e",
#                        "cs13f", "cs14g", "cs15h", "cs16i", "cs17j",
#                        "cs18k", "cs19l"),
#     waves_late  = list("cs20m", "cs21n", "cs22o", "cs23p", "cs24q",
#                        "cs25r"),
#     early_label = "open_numeric",
#     late_label  = "categorical",
#     comparability = list(
#       status    = "non_comparable",
#       method    = "no_pool",
#       rationale = paste(
#         "pre-cs20m used open numeric entry conditional on participation;",
#         "post-cs20m uses categorical scale asked unconditionally.",
#         "do not pool without explicit period interaction."
#       )
#     )
#   ))
# )
#
# validate_recipe(recipe, "cs_with_custom_boundary")

## ----vocab--------------------------------------------------------------------
# # inspect the vocabulary programmatically
# lissr:::VALID_ACTIONS
# #> $variable_rules
# #>  [1] "strip_prefix"                "type_coerce"
# #>  [3] "coerce_type"                 "coerce_numeric"
# #>  [5] "rename"                      "rename_to_suffix"
# #>  [7] "set_label"                   "apply_labelled_policy"
# #>  [9] "strip_value_labels"          "strip_labels_coerce_numeric"
# #> [11] "ensure_column"               "use_global_policy"
# #> [13] "identity"                    "safe_integer_cast"
# #> [15] "derive_fieldwork_month"      "parse_time"
# #> [17] "note_only"
# #>
# #> $harmonization_rules
# #>  [1] "type_coerce"            "derive_fieldwork_month" "parse_time"
# #>  [4] "recode_to_na"           "recode_sentinels_to_na" "na_recode"
# #>  [7] "recode"                 "value_recode"           "fix_label"
# #> [10] "strip_question_stem"    "lowercase_labels"       "normalize_labels"
# #> [13] "flag_only"              "flag_absence"           "subtract"
# #> [16] "crosswalk"              "conditional_label_swap" "label_to_string"
# #> [19] "derive_combined_party"  "extend_categories"      "transform"
# #> [22] "note_only"
# #>
# #> $boundary_rules
# #>  [1] "safe_integer_cast"     "fix_label"             "derive_combined_party"
# #>  [4] "add_era_flag"          "add_flag"              "add_period_flag"
# #>  [7] "split_variable"        "structural_na"         "filter_rows"
# #> [10] "drop_outside"          "crosswalk_rename"      "structural_note"
# #> [13] "stack_aux_files"       "retain_where_present"  "retain_but_flag"
# #> [16] "note_only"
# #>
# #> $drop_retain_rules
# #> [1] "flag_only"               "flag_absence"
# #> [3] "drop_outside"            "retain_but_flag"
# #> [5] "drop"                    "retain"
# #> [7] "retain_if_present"       "retain_as_metadata_only"
# #> [9] "note_only"

