## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)


## ----source-nonresponse-merge-------------------------------------------------
guarded <- lissr::merge_liss_module(
  lissr::liss_recipe("ci"), data_dir = "data/ci",
  output_dir = NULL, source_nonresponse = TRUE
)
dry_guarded <- lissr::liss_clean_income(
  guarded, variables = list(target = "s339"), mode = "flag"
)


## ----source-nonresponse-rebind------------------------------------------------
legacy_merge <- lissr::merge_liss_module(
  lissr::liss_recipe("ci"), data_dir = "data/ci", output_dir = NULL
)
bound <- lissr::liss_classify_source_nonresponse(
  legacy_merge,
  source = "data/ci/ci23p_EN_1.0p.sav",
  target = "s339", wave_col = "wave_id"
)
dry_bound <- lissr::liss_clean_income(
  bound, variables = list(target = "s339"), mode = "flag"
)


## ----taxable-source-selection-------------------------------------------------
reviewed_registry <- yaml::yaml.load_file(system.file(
  "schema", "source_nonresponse_registry.yml", package = "lissr"
))
selected_fields <- Filter(function(field) field$field_id == "CI23P337_FIELD",
                          reviewed_registry$fields)
stopifnot(length(selected_fields) == 1L)
tax_recipe <- lissr::liss_recipe("ci")
tax_recipe$source_nonresponse <- list(
  schema_version = "2.0.0", field = selected_fields[[1L]]
)
tax_audit <- lissr::liss_classify_source_nonresponse(
  legacy_merge, source = "data/ci/ci23p_EN_1.0p.sav", target = "s337",
  wave_col = "wave_id", recipe = tax_recipe
)
tax_normalized <- lissr::merge_liss_module(
  tax_recipe, data_dir = "data/ci", output_dir = NULL,
  source_nonresponse = TRUE
)


## ----joint-taxable-selection--------------------------------------------------
joint_recipe <- lissr::liss_recipe("ci")
joint_recipe$source_nonresponse <- list(
  schema_version = "3.0.0", fields = unname(reviewed_registry$fields)
)
joint_taxable <- lissr::merge_liss_module(
  joint_recipe, data_dir = "data/ci", output_dir = NULL,
  source_nonresponse = TRUE
)
joint_taxable$source_nonresponse_readiness


## ----joint-three-field-selection----------------------------------------------
three_recipe <- lissr::liss_recipe("ci")
three_fields <- lapply(reviewed_registry$fields, function(field) list(
  field_id = field$field_id,
  scalar = list(schema_version = "2.0.0", field = field)
))
household_records <- Filter(function(record) {
  identical(record$contract$rule_id, "CI23P339_N01")
}, reviewed_registry$records)
stopifnot(length(household_records) == 1L)
three_fields[[3L]] <- list(
  field_id = "CI23P339_FIELD",
  scalar = list(schema_version = "1.0.0",
                contracts = list(household_records[[1L]]$contract))
)
three_recipe$source_nonresponse <- list(
  schema_version = "4.0.0", fields = three_fields
)
three_guarded <- lissr::merge_liss_module(
  three_recipe, data_dir = "data/ci", output_dir = NULL,
  source_nonresponse = TRUE
)


## ----collection-household-flag------------------------------------------------
three_flag <- lissr::liss_clean_income(
  three_guarded, mode = "flag", variables = list(target = "s339"),
  output_dir = NULL
)
saveRDS(three_flag, "three_field_household_flag.rds")


## ----collection-household-applied---------------------------------------------
three_correct <- lissr::liss_clean_income(
  three_guarded, mode = "correct", variables = list(target = "s339"),
  background = NULL, output_dir = NULL
)
lissr::liss_validate_applied_income(three_correct)
saveRDS(three_correct, "three_field_household_applied.rds")
three_reloaded <- readRDS("three_field_household_applied.rds")
lissr::liss_validate_applied_income(three_reloaded, replay = TRUE)


## ----collection-household-applied-view----------------------------------------
selected_keys <- rev(three_correct$applied_lineage$row_keys)
three_view <- lissr::liss_create_applied_income_view(three_correct, selected_keys)
lissr::liss_validate_applied_income_view(three_view)
saveRDS(three_view, "three_field_household_applied_view.rds")
view_reloaded <- readRDS("three_field_household_applied_view.rds")
lissr::liss_validate_applied_income_view(view_reloaded, replay = TRUE)


## ----ruleset------------------------------------------------------------------
library(lissr)

rs <- liss_cleaning_ruleset()
print(rs)

system.file("cleaning", "income_cleaning_rules.yml", package = "lissr")


## ----flag-mode----------------------------------------------------------------
result <- merge_liss_module(liss_recipe("ci"), data_dir = "data/ci",
                            output_dir = "output/ci")

dry <- liss_clean_income(result, mode = "flag")

head(dry$decisions)
summary(dry)


## ----correct-mode-------------------------------------------------------------
cleaned <- liss_clean_income(result, output_dir = "output/ci")

cleaned$data$nethh          # cleaned values
cleaned$data$nethh_observed # untouched input, always preserved
table(cleaned$data$nethh_clean_status, useNA = "ifany")


## ----ledger-------------------------------------------------------------------
d <- cleaned$decisions
d[d$rule_id == "D06", c("person_id", "wave", "observed", "corrected",
                        "candidate_source", "evidence")]

# the justification column carries the full sentence, for example:
# "detected by scale_error; replaced with the household_median candidate
#  25800 (closest of 4 admissible candidate(s) to the household_median
#  25800; constrained to [8000, 150000])"


## ----report-------------------------------------------------------------------
liss_cleaning_report(cleaned, output_dir = "output/ci")


## ----overrides----------------------------------------------------------------
# a stricter volatility requirement for scale-error detection,
# no extreme-z net, and a higher plausibility cap
cleaned2 <- liss_clean_income(
  result,
  income_cap = 175000,
  disable = c("D10"),
  params = list(D06 = list(volatility_min = 0.7))
)


## ----custom-ruleset-----------------------------------------------------------
cleaned3 <- liss_clean_income(result, ruleset = "my_income_rules.yml")


## ----background---------------------------------------------------------------
background <- haven::read_sav("data/avars_201801_EN_1.0p.sav")
cleaned <- liss_clean_income(result, background = background)


## ----equivalise---------------------------------------------------------------
library(magrittr)

panel <- cleaned$data %>%
  dplyr::mutate(
    stand_inc = liss_equivalise_income(nethh, aantalhh, aantalki)
  )
