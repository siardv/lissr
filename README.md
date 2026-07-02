# lissr

<!-- badges: start -->
[![R-CMD-check](https://github.com/siardv/lissr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/siardv/lissr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Programmatic access to the [LISS Data Archive](https://www.lissdata.nl/).
Authenticate, browse available modules and waves, interactively select and
download data files, and merge longitudinal waves using recipe-driven YAML
specifications.

## Installation

```r
# install from GitHub
# install.packages("remotes")  # if not already installed
remotes::install_github("siardv/lissr")
```

## Quick start

```r
library(lissr)

# 1. store credentials (once, prompts for password)
liss_store_credentials("1234")

# 2. log in (credentials retrieved from keyring + 2FA prompt)
liss_login()

# 3. explore
liss_modules()
liss_wave_matrix()

# 4. interactively select modules, waves, file types
selection <- liss_select()

# 5. download
liss_download(selection)

# 6. merge a module using the built-in recipe
recipe <- liss_recipe("ch")
result <- merge_liss_module(recipe, data_dir = "liss", output_dir = "./output")

# 7. batch merge all core modules
modules <- c("ch", "cv", "cd", "cf", "cw", "cp", "cs", "ci", "ca", "cr")
results <- merge_liss_modules(
  purrr::map_chr(modules, ~ system.file("recipes", paste0(.x, "_merge_recipe.yml"),
                                         package = "lissr")),
  data_dir = "liss",
  output_dir = "./output"
)
```

## Vignettes

Worked examples ship with the package. After installing, list them with
`browseVignettes("lissr")` or open one by its name, for example
`vignette("getting-started", package = "lissr")`. You can also read the
rendered versions in your browser without installing:

- [Getting Started with lissr](https://htmlpreview.github.io/?https://github.com/siardv/lissr/blob/main/inst/doc/getting-started.html):
  a short orientation to the package and its workflow.
- [Merging LISS Panel Data](https://htmlpreview.github.io/?https://github.com/siardv/lissr/blob/main/inst/doc/merge-workflow.html):
  the core single-module merge, from recipe to merged output.
- [Longitudinal Panel Analysis](https://htmlpreview.github.io/?https://github.com/siardv/lissr/blob/main/inst/doc/longitudinal-panel-analysis.html):
  assembling and analyzing data across multiple waves.
- [Cross-Sectional Analysis with a Single Wave](https://htmlpreview.github.io/?https://github.com/siardv/lissr/blob/main/inst/doc/cross-sectional-analysis.html):
  working with one wave and attaching the Background Variables.
- [Multi-Module Linkage](https://htmlpreview.github.io/?https://github.com/siardv/lissr/blob/main/inst/doc/multi-module-linkage.html):
  joining several modules on the respondent id.
- [Custom Merge Recipes](https://htmlpreview.github.io/?https://github.com/siardv/lissr/blob/main/inst/doc/custom-recipes.html):
  writing or adapting a YAML recipe against the canonical schema.
- [Reproducible Research Pipelines](https://htmlpreview.github.io/?https://github.com/siardv/lissr/blob/main/inst/doc/reproducible-pipelines.html):
  structuring the workflow as a reproducible pipeline.

## Merge system

The merge engine processes YAML recipes conforming to `CANONICAL_SCHEMA.md`
(v1.0.0). Each recipe encodes every merge-relevant decision for a module:
wave file patterns, variable harmonization rules, boundary handling,
comparability contracts, and validation checks.

Built-in recipes are included for all ten core LISS modules:
CH (Health), CV (Politics and Values), CD (Housing), CF (Family and
Household), CW (Work and Schooling), CP (Personality), CS (Culture and
Sports), CI (Economic Integration), CA (Assets), and CR (Religion and
Ethnicity).

## Background variables

The merge engine covers the ten core study modules above; it does not fetch or
attach the LISS Background Variables (the monthly `avars` file). Demographics
such as age, sex, education, income, and household composition live in that
separate file, and you join them yourself after merging.

Two identifier columns are preserved through every merge: `nomem_encr` (the
respondent id, used as the merge key) and `nohouse_encr` (the encrypted
household id). `nohouse_encr` is present only in early waves of most modules and
is dropped later, so for recent waves the household id has to come from the
Background Variables file.

To attach demographics, download the Background Variables file for the same
fieldwork month as your merged data, then left-join on `nomem_encr`:

```r
# the Background Variables file appears as the "Background Variables" module
# in liss_select() / the blueprint; download it, then read and join
avars <- haven::read_sav("data/avars/avars_202411_EN_1_0p.sav")
merged_with_demographics <- dplyr::left_join(result, avars, by = "nomem_encr")
```

Join on `nomem_encr` only, never `nohouse_encr` (the household id is not a
stable person-level key and changes when household composition changes), and
match the Background Variables fieldwork month to your wave data. The
`cross-sectional-analysis` and `multi-module-linkage` vignettes show the full
workflow.

## File formats

The package has been developed and tested only with SPSS `.sav` files, which is
the default format throughout. The downloader can also fetch Stata `.dta` files,
and the engine includes a read path for them (`haven::read_dta`), but `.dta`
input has never been tested. Treat `.dta` support as experimental: there is no
guarantee the merge pipeline produces correct results from `.dta` sources, so
validate any `.dta`-based output yourself. The engine can also read `.csv` files
via `readr`.

A built-in fallback matches a wave file by its `wave_id` prefix when a recipe's
`file_pattern` extension does not match the file on disk (for example a recipe
written for `.sav` run against a downloaded `.dta`). This only locates the file;
it does not validate that a non-`.sav` format is handled correctly downstream.

## Validate recipes without merging

```r
recipe <- liss_recipe("ch")
validate_recipe(recipe, "ch_merge_recipe.yml")
```

`validate_recipe()` also emits a non-fatal warning listing any
rule-level key the engine neither consults nor sanctions as documentation, so a
mis-named key is surfaced at authoring time instead of being silently ignored.
The recognized and sanctioned key sets are documented in `CANONICAL_SCHEMA.md`.

## Onboard a new wave

```r
onboard_new_wave(
  recipe_path = system.file("recipes", "ch_merge_recipe.yml", package = "lissr"),
  new_file    = "ch25r_EN_1_0p.csv",
  prev_wave_id = "ch24q"
)
```

## License

MIT
