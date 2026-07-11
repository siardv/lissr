# Custom Merge Recipes

## When to write a custom recipe

The built-in recipes cover the eight core LISS modules as distributed by
the archive. You might need a custom recipe when:

- You are working with a **special-purpose LISS study** (not one of the
  eight core modules) that has no built-in recipe.
- You want to **subset waves** or **add project-specific harmonization**
  on top of the standard recipe.
- You have **downloaded data in a non-standard format** (e.g. Stata
  instead of SPSS, or English vs Dutch labels).
- You want to **change the sentinel-code policy** — perhaps keeping
  “don’t know” as a distinct category rather than recoding to `NA`.

## Anatomy of a recipe

A recipe is a YAML file with these top-level sections:

| Section | Required | Purpose |
|----|----|----|
| `meta` | yes | module name, version, covered waves |
| `global` | yes | id variable, labelled policy, sentinel policy |
| `wave_index` | yes | one entry per wave: id, year, file pattern |
| `variable_rules` | no | prefix stripping, type coercion, renaming |
| `harmonization_rules` | no | sentinel recoding, value mapping, label fixes |
| `boundary_rules` | no | era flags, split variables, structural NA |
| `drop_retain_rules` | no | columns to drop or force-keep |
| `derived_variables` | no | new columns computed from existing ones |
| `validation_checks` | no | post-merge assertions |
| `logging` | yes | log file names, summary artifact toggle |

Every rule must have a unique `rule_id`, a non-empty `action` from the
controlled vocabulary, and a `description`.

## Scenario A — fork and modify a built-in recipe

The most common path is to start from a built-in recipe and adapt it.

``` r

library(lissr)

# load the built-in Health recipe as a list
recipe <- liss_recipe("ch")

# inspect its structure
str(recipe, max.level = 1)
#> List of 10
#>  $ meta               :List of 8
#>  $ global             :List of 6
#>  $ wave_index         :List of 17
#>  $ variable_rules     :List of 5
#>  $ harmonization_rules:List of 8
#>  $ boundary_rules     :List of 4
#>  $ drop_retain_rules  :List of 3
#>  $ derived_variables  :List of 2
#>  $ validation_checks  :List of 4
#>  $ logging            :List of 3
```

### Subset to specific waves

``` r

# keep only post-2015 waves for an analysis of recent trends
recipe$wave_index <- purrr::keep(
  recipe$wave_index,
  ~ as.integer(.x$year) >= 2015
)

# update the metadata to reflect the change
recipe$meta$covered_waves <- purrr::map_chr(recipe$wave_index, "id")
recipe$meta$notes <- "forked from built-in ch recipe; restricted to 2015+"
```

### Change the labelled policy

By default, the recipe converts haven-labelled columns to numeric. If
your analysis needs factor levels (e.g. for ordinal logistic regression
in R where factor ordering matters), switch the policy:

``` r

recipe$global$labelled_policy <- "to_factor"
```

### Add a custom harmonization rule

Suppose you want to recode self-rated health (suffix 001) from its
original 1–5 scale to a binary indicator (1–2 = poor/moderate vs 3–5 =
good/very good/excellent):

``` r

recipe$harmonization_rules <- append(
  recipe$harmonization_rules,
  list(list(
    rule_id     = "CUSTOM_01_srh_binary",
    action      = "value_recode",
    description = "recode SRH to binary: 1-2 -> 0, 3-5 -> 1",
    suffixes    = list("001"),
    mapping     = list("1" = 0, "2" = 0, "3" = 1, "4" = 1, "5" = 1),
    waves       = "all"
  ))
)
```

### Validate and run

``` r

# the schema validator catches typos in action names, missing fields, etc.
validate_recipe(recipe, "custom_ch_recipe")

# run the merge
result <- merge_liss_module(recipe, data_dir = "data/ch", output_dir = "output/custom")
```

### Save the modified recipe to disk

``` r

yaml::write_yaml(recipe, "my_ch_recipe.yml")

# later, reload it
recipe <- load_recipe("my_ch_recipe.yml")
```

## Scenario B — write a recipe from scratch

For a non-core LISS study (e.g. a special-purpose module on COVID
attitudes), you write the recipe from scratch.

``` r

my_recipe <- list(
  meta = list(
    module        = "covid",
    module_label  = "COVID Attitudes Special Study",
    recipe_version = "1.0.0",
    created       = format(Sys.Date()),
    source_spec   = "covid_codebook_EN.pdf",
    covered_waves = list("covid20a", "covid20b", "covid21c"),
    schema_version = "1.0.0"
  ),

  global = list(
    id_variable             = "nomem_encr",
    wave_variable           = "wave_id",
    year_variable           = "wave_year",
    labelled_policy         = "to_numeric",
    missing_variable_policy = "warn_and_create_na",
    strip_label_whitespace  = TRUE
  ),

  wave_index = list(
    list(id = "covid20a", year = 2020, file_pattern = "covid20a_*"),
    list(id = "covid20b", year = 2020, file_pattern = "covid20b_*"),
    list(id = "covid21c", year = 2021, file_pattern = "covid21c_*")
  ),

  variable_rules = list(
    list(
      rule_id     = "V01_strip_prefix",
      action      = "strip_prefix",
      description = "remove wave prefix from all columns"
    )
  ),

  harmonization_rules = list(
    list(
      rule_id     = "H01_sentinel_recode",
      action      = "recode_to_na",
      description = "recode -9 (DK) and -8 (PNTS) to NA",
      scope       = "all_numeric",
      codes       = list(-9, -8)
    )
  ),

  boundary_rules = list(),
  drop_retain_rules = list(),
  derived_variables = list(),
  validation_checks = list(),

  logging = list(
    log_file        = "covid_merge_log.jsonl",
    report_file     = "covid_merge_report.txt",
    summary_artifact = list(enabled = TRUE)
  )
)

# validate before first use
validate_recipe(my_recipe, "covid_recipe.yml")

# save to disk
yaml::write_yaml(my_recipe, "covid_merge_recipe.yml")
```

## Scenario C — keep sentinel codes as distinct values

Some analyses treat “don’t know” and “prefer not to say” as informative
categories (e.g. in survey methodology research studying item
non-response). In that case, skip the harmonization rules that recode
sentinels to `NA`.

``` r

recipe <- liss_recipe("ch")

# remove all recode_to_na rules
recipe$harmonization_rules <- purrr::discard(
  recipe$harmonization_rules,
  ~ .x$action == "recode_to_na"
)

# optionally add a rule to rename sentinels instead of dropping them
recipe$harmonization_rules <- append(
  recipe$harmonization_rules,
  list(list(
    rule_id     = "CUSTOM_keep_dk",
    action      = "value_recode",
    description = "recode -9 to 97 (DK) and -8 to 98 (PNTS) for explicit modelling",
    suffixes    = list("001"),
    mapping     = list("-9" = 97, "-8" = 98),
    waves       = "all"
  ))
)

validate_recipe(recipe, "ch_keep_sentinels")
result <- merge_liss_module(recipe, data_dir = "data/ch", output_dir = "output/sentinels")
```

## Scenario D — add comparability contracts to your custom rules

If your custom recipe introduces a boundary that future users should
know about, annotate it with a comparability contract:

``` r

recipe <- liss_recipe("cs")

recipe$boundary_rules <- append(
  recipe$boundary_rules,
  list(list(
    rule_id     = "CUSTOM_B01_sport_freq_redesign",
    action      = "add_period_flag",
    description = "sport frequency question redesigned in cs20m",
    flag_column = "sport_freq_era",
    waves_early = list("cs08a", "cs09b", "cs10c", "cs11d", "cs12e",
                       "cs13f", "cs14g", "cs15h", "cs16i", "cs17j",
                       "cs18k", "cs19l"),
    waves_late  = list("cs20m", "cs21n", "cs22o", "cs23p", "cs24q"),
    early_label = "open_numeric",
    late_label  = "categorical",
    comparability = list(
      status    = "non_comparable",
      method    = "no_pool",
      rationale = paste(
        "pre-cs20m used open numeric entry conditional on participation;",
        "post-cs20m uses categorical scale asked unconditionally.",
        "do not pool without explicit period interaction."
      )
    )
  ))
)

validate_recipe(recipe, "cs_with_custom_boundary")
```

## Controlled action vocabulary reference

Consult the canonical schema for the full list of allowed actions. The
validator rejects any action not in this vocabulary:

``` r

# inspect the vocabulary programmatically
lissr:::VALID_ACTIONS
#> $variable_rules
#> [1] "strip_prefix"         "type_coerce"          "rename"
#> [4] "set_label"            "apply_labelled_policy" "strip_value_labels"
#> [7] "note_only"
#>
#> $harmonization_rules
#> [1] "recode_to_na"        "value_recode"        "fix_label"
#> [4] "crosswalk"           "strip_question_stem" "lowercase_labels"
#> [7] "flag_only"           "note_only"
#>
#> $boundary_rules
#> [1] "add_era_flag"     "add_flag"         "add_period_flag"
#> [4] "split_variable"   "structural_na"    "filter_rows"
#> [7] "crosswalk_rename" "stack_aux_files"  "note_only"
#>
#> $drop_retain_rules
#> [1] "drop"                    "retain"
#> [3] "retain_if_present"       "retain_as_metadata_only"
#> [5] "note_only"
```

## Sharing recipes with collaborators

If you publish an analysis using LISS data, include your recipe YAML in
the replication package. Anyone with `lissr` installed can then
reproduce your exact merge pipeline:

``` r

# in the collaborator's R session
recipe <- lissr::load_recipe("custom_ch_recipe.yml")
result <- lissr::merge_liss_module(recipe, data_dir = "data/ch", output_dir = "output")
```

The recipe file is the single source of truth for every data-cleaning
decision, replacing the typical sequence of ad-hoc R scripts that are
hard to audit and easy to break.
