# Multi-Module Linkage

## When you need data from multiple modules

Many research questions span LISS domains. For instance:

- **Health and income**: does job loss predict a decline in self-rated
  health? This needs the Health (CH) and Economic Integration (CI)
  modules.
- **Personality and political participation**: do conscientiousness or
  openness predict voter turnout? This needs Personality (CP) and
  Politics and Values (CV).
- **Housing, family structure, and well-being**: does moving into
  home-ownership improve life satisfaction? This needs Housing (CD),
  Family (CF), and a well-being item from Health (CH).

The LISS panel’s key advantage is that all modules share the same
respondent identifier (`nomem_encr`) and can be linked at the
person-wave level.

## Architecture of a multi-module analysis

    Background Variables (CA) ──── demographics, income, education
              │
              ├── Health (CH) ──── self-rated health, BMI, smoking
              │
              ├── Income (CI) ──── employment status, labour income
              │
              └── Politics (CV) ── voting, political trust

Each module is merged independently using its own recipe (different
sentinel codes, different boundary rules, different variable numbering),
then the cleaned outputs are joined on `nomem_encr` + `wave_year`.

## Step 1 — merge each module separately

``` r

library(lissr)
library(dplyr)
library(purrr)

liss_login()

# download all waves for three modules
bp <- liss_blueprint()
for (mod in c("Health", "Economic Integration: Income", "Politics and Values")) {
  files <- bp |> filter(module == mod, type == "spss")
  mod_dir <- file.path("data", tolower(substr(mod, 1, 2)))
  liss_download(files, .dir = mod_dir)
}

# merge each module with its own recipe
modules <- c("ch", "ci", "cv")
results <- purrr::map(modules, function(mod) {
  recipe <- liss_recipe(mod)
  merge_liss_module(
    recipe,
    data_dir   = file.path("data", mod),
    output_dir = file.path("output", mod)
  )
}) |> purrr::set_names(modules)
```

Alternatively, use the batch interface:

``` r

recipe_paths <- purrr::map_chr(modules, ~ {
  system.file("recipes", paste0(.x, "_merge_recipe.yml"), package = "lissr")
})

results <- merge_liss_modules(recipe_paths, data_dir = "data", output_dir = "output")
```

## Step 2 — select variables from each module

After merging, each module produces a wide data frame with hundreds of
columns. For a focused analysis, select only the variables you need
before joining — this keeps the linked dataset manageable and avoids
column-name collisions.

``` r

# health: self-rated health (s001) and BMI (s038)
health <- results$ch$data |>
  select(nomem_encr, wave_year,
         srh = s001,
         bmi = s038)

# income: main employment status (s001 in CI = net personal income)
income <- results$ci$data |>
  select(nomem_encr, wave_year,
         net_income = s001,
         employed   = s006)

# politics: voted in last election (s012 in CV)
politics <- results$cv$data |>
  select(nomem_encr, wave_year,
         voted_last_election = s012,
         political_trust     = s002)
```

## Step 3 — align wave years

Not all modules are fielded in exactly the same year. The Health module
skipped 2014 (there is no ch14 wave); the Work module has 15 waves while
Health has 17. When joining on `wave_year`, you get `NA` in modules that
did not have a wave that year. This is expected, not an error.

``` r

# which years does each module cover?
purrr::map(results, ~ sort(unique(.x$data$wave_year)))
#> $ch
#>  [1] 2007 2008 2009 2010 2011 2012 2013 2015 2016 2017 2018 2019 2020 2021
#>      2022 2023 2024
#>
#> $ci
#>  [1] 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020
#>      2021 2022 2023 2024
#>
#> $cv
#>  [1] 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021
#>      2022 2023 2024

# find the overlapping years
common_years <- Reduce(
  intersect,
  purrr::map(results, ~ unique(.x$data$wave_year))
)
common_years
#> [1] 2008 2009 2010 2011 2012 2013 2015 2016 2017 2018 2019 2020 2021 2022
#>     2023 2024
```

## Step 4 — join modules

``` r

# full join preserves all person-years from any module
linked <- health |>
  full_join(income,   by = c("nomem_encr", "wave_year")) |>
  full_join(politics, by = c("nomem_encr", "wave_year"))

dim(linked)
#> [1] 125438      7

# restrict to the common years for a balanced design
linked_balanced <- linked |>
  filter(wave_year %in% common_years)
```

If you prefer a left join anchored on one module (e.g. Health is the
primary outcome, the others are predictors), use
[`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
so you keep only person-years present in Health.

## Step 5 — attach background variables

Background variables supply time-varying demographics (age, education
level, household income) that are not part of any survey module.

``` r

# read all monthly background variable files
bg_files <- list.files("data/avars", pattern = "\\.sav$", full.names = TRUE)
bg <- purrr::map_dfr(bg_files, \(f) {
  haven::read_sav(f) |>
    haven::zap_labels() |>
    select(nomem_encr,
           fieldwork_ym = wave,
           age       = leeftijd,
           sex       = geslacht,
           edu       = oplcat,
           hh_income = nettohh_f,
           urban     = sted)
})

# create a wave_year column to join on
bg <- bg |>
  mutate(wave_year = as.integer(substr(fieldwork_ym, 1, 4))) |>
  # keep one snapshot per person-year (e.g. November of each year)
  group_by(nomem_encr, wave_year) |>
  slice_max(fieldwork_ym, n = 1, with_ties = FALSE) |>
  ungroup()

linked <- linked |>
  left_join(bg, by = c("nomem_encr", "wave_year"))
```

## Step 6 — example analysis

Does job loss predict a decline in self-rated health, controlling for
person fixed effects?

``` r

library(fixest)

linked <- linked |>
  mutate(job_loss = lag(employed) == 1 & employed == 0,
         .by = nomem_encr)

fe <- feols(
  srh ~ job_loss + age + I(age^2) | nomem_encr + wave_year,
  data = linked
)

summary(fe)
```

## Handling comparability across modules

Each module has its own boundary rules and era flags. When you join
modules, you inherit *all* of those constraints. Practical guidance:

- **Check each module’s merge report** for comparability contracts
  before running a multi-module analysis. If Health changed the
  e-cigarette block in 2015 and Income changed the employment
  classification in 2014, your pooled design must respect both
  boundaries.
- **Carry forward era flags** into the linked dataset. They are present
  in each module’s merged output and transfer through the join.
- **Document which recipe versions** you used for each module.

``` r

# list all era/period/flag columns across modules
purrr::map(results, ~ {
  grep("_flag$|_period$|_era$", names(.x$data), value = TRUE)
})
```

## Scaling to all eight modules

If you need variables from every module, the batch merge handles it:

``` r

all_modules <- c("ch", "cv", "cd", "cf", "cw", "cp", "cs", "ci")
recipe_paths <- purrr::map_chr(all_modules, ~ {
  system.file("recipes", paste0(.x, "_merge_recipe.yml"), package = "lissr")
})

all_results <- merge_liss_modules(
  recipe_paths, data_dir = "data", output_dir = "output"
)

# join all modules progressively
linked_all <- all_results[[1]]$data |>
  select(nomem_encr, wave_year, srh = s001)

for (mod in names(all_results)[-1]) {
  vars_to_keep <- c("nomem_encr", "wave_year",
                     # pick a few variables from each
                     head(setdiff(names(all_results[[mod]]$data),
                                  c("nomem_encr", "wave_id", "wave_year")), 5))
  linked_all <- linked_all |>
    full_join(
      all_results[[mod]]$data |> select(all_of(vars_to_keep)),
      by = c("nomem_encr", "wave_year")
    )
}

dim(linked_all)
```

## Checklist

Merged each module independently with its own recipe before joining.

Renamed ambiguous columns (e.g. `s001` means different things in
different modules) before the join.

Used `wave_year` (not `wave_id`) as the time key for cross-module joins
— wave IDs are module-specific strings.

Checked wave-year coverage across modules and handled gaps.

Carried forward era flags from every module into the linked data.

Attached background variables from the correct fieldwork month.

Reported all module wave identifiers and recipe versions used.
