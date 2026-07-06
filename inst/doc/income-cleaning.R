## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)


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

