# Package index

## Authentication and credentials

- [`liss_store_credentials()`](https://siardv.github.io/lissr/reference/liss_store_credentials.md)
  : store LISS Data Archive credentials in the system keyring
- [`liss_login()`](https://siardv.github.io/lissr/reference/liss_login.md)
  : log in to the LISS Data Archive
- [`liss_is_logged_in()`](https://siardv.github.io/lissr/reference/liss_is_logged_in.md)
  : check whether the current session is still authenticated
- [`liss_list_credentials()`](https://siardv.github.io/lissr/reference/liss_list_credentials.md)
  : list stored LISS Data Archive credentials
- [`liss_delete_credentials()`](https://siardv.github.io/lissr/reference/liss_delete_credentials.md)
  : delete stored LISS Data Archive credentials

## Browse and select

- [`liss_modules()`](https://siardv.github.io/lissr/reference/liss_modules.md)
  : list available LISS panel modules
- [`liss_wave_matrix()`](https://siardv.github.io/lissr/reference/liss_wave_matrix.md)
  : display a module-by-wave availability matrix
- [`liss_select()`](https://siardv.github.io/lissr/reference/liss_select.md)
  : interactively select modules, waves, and file types
- [`liss_server_status()`](https://siardv.github.io/lissr/reference/liss_server_status.md)
  : check LISS Data Archive availability

## Download

- [`liss_download()`](https://siardv.github.io/lissr/reference/liss_download.md)
  : download files from the LISS Data Archive

## Recipes

- [`liss_recipe()`](https://siardv.github.io/lissr/reference/liss_recipe.md)
  : load a built-in merge recipe by module code
- [`load_recipe()`](https://siardv.github.io/lissr/reference/load_recipe.md)
  : load and validate a canonical YAML merge recipe
- [`validate_recipe()`](https://siardv.github.io/lissr/reference/validate_recipe.md)
  : validate a merge recipe against the canonical schema
- [`liss_blueprint()`](https://siardv.github.io/lissr/reference/liss_blueprint.md)
  : build a complete file inventory of the LISS Data Archive
- [`onboard_new_wave()`](https://siardv.github.io/lissr/reference/onboard_new_wave.md)
  : onboard a new wave into a merge recipe

## Merge engine

- [`merge_liss_module()`](https://siardv.github.io/lissr/reference/merge_liss_module.md)
  : run the full merge pipeline for a single module
- [`merge_liss_modules()`](https://siardv.github.io/lissr/reference/merge_liss_modules.md)
  : merge multiple modules sequentially
- [`merge_liss_panel()`](https://siardv.github.io/lissr/reference/merge_liss_panel.md)
  : merge multiple LISS modules into a single panel dataset

## Income cleaning

- [`liss_clean_income()`](https://siardv.github.io/lissr/reference/liss_clean_income.md)
  : Detect and correct implausible household-income values
- [`liss_cleaning_ruleset()`](https://siardv.github.io/lissr/reference/liss_cleaning_ruleset.md)
  : Load an income-cleaning ruleset
- [`validate_cleaning_ruleset()`](https://siardv.github.io/lissr/reference/validate_cleaning_ruleset.md)
  : Validate an income-cleaning ruleset
- [`liss_cleaning_report()`](https://siardv.github.io/lissr/reference/liss_cleaning_report.md)
  : Write the income-cleaning report and audit artifacts
- [`liss_equivalise_income()`](https://siardv.github.io/lissr/reference/liss_equivalise_income.md)
  : Equivalise household income

## Package overview

- [`lissr`](https://siardv.github.io/lissr/reference/lissr-package.md)
  [`lissr-package`](https://siardv.github.io/lissr/reference/lissr-package.md)
  : lissr: View, Download, and Merge LISS Panel Data
