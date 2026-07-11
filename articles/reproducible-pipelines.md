# Reproducible Research Pipelines

## The reproducibility problem in survey data

Published analyses of longitudinal survey data are notoriously hard to
reproduce. The raw files contain labelled-value quirks, sentinel codes,
variable renumbering across waves, and undocumented instrument changes.
Every research team writes its own cleaning scripts, and small
differences in how sentinel codes are handled can change substantive
conclusions.

lissr’s recipe system addresses this by encoding all cleaning decisions
in a single, declarative YAML file that:

- is version-controlled alongside your analysis code,
- is validated against a formal schema before any data is touched,
- produces a structured audit log of every transformation applied.

This vignette shows how to build a full reproducible pipeline around
these tools.

## Project structure

A recommended layout for a LISS-based research project:

    my-project/
    ├── data/                    # raw LISS downloads (gitignored)
    │   ├── ch/
    │   └── avars/
    ├── recipes/                 # YAML merge recipes (version-controlled)
    │   └── ch_custom.yml
    ├── output/                  # merged outputs (gitignored)
    │   ├── ch_merged.sav
    │   ├── ch_merge_log.jsonl
    │   ├── ch_merge_report.txt
    │   └── ch_merge_summary.json
    ├── golden/                  # golden reference files for regression tests
    │   └── ch_golden.json
    ├── R/
    │   ├── 01_download.R
    │   ├── 02_merge.R
    │   ├── 03_analysis.R
    │   └── 04_figures.R
    ├── tests/
    │   └── test_merge_regression.R
    └── README.md

`data/` and `output/` are gitignored — they contain individual-level
data that cannot be shared. The recipes, analysis scripts, golden
references, and test files are committed and form the replication
package.

## Step 1 — scripted download

``` r

# R/01_download.R
library(lissr)

liss_login()
bp <- liss_blueprint()

# download Health module (all waves, SPSS format)
health_files <- dplyr::filter(bp, module == "Health", type == "spss")
liss_download(health_files, .dir = "data/ch")

# download background variables for all available months
bg_files <- dplyr::filter(bp, module == "Background Variables", type == "spss")
liss_download(bg_files, .dir = "data/avars")
```

## Step 2 — recipe-driven merge with full audit trail

``` r

# R/02_merge.R
library(lissr)

# use a custom recipe stored in the project (version-controlled)
recipe <- load_recipe("recipes/ch_custom.yml")

result <- merge_liss_module(
  recipe,
  data_dir   = "data/ch",
  output_dir = "output"
)
```

This produces four output files. The merged `.sav` file preserves all
variable labels, value labels, and SPSS-style missing values from the
original data. The remaining files support reproducibility:

### The JSONL audit log

Each line is a JSON object recording one rule application:

``` json
{"rule_id":"H01_sentinel_recode","wave_id":"ch07a","variable":"42 cols",
 "action":"recode_to_na","rows_affected":3847,"values_changed":3847,
 "timestamp":"2026-02-18T14:23:07.442","duration_ms":12.3}
```

You can read this into R for programmatic inspection:

``` r

log <- jsonlite::stream_in(file("output/ch_merge_log.jsonl"), verbose = FALSE)

# total transformations
nrow(log)
#> [1] 287

# breakdown by action type
table(log$action)
#> add_era_flag      drop   note_only recode_to_na  strip_prefix
#>            4         3          12          136            17

# total values recoded to NA across all waves
sum(log$values_changed, na.rm = TRUE)
#> [1] 48293
```

### The JSON summary

A machine-readable snapshot of the merge output — useful for automated
checks in CI:

``` r

summary <- jsonlite::read_json("output/ch_merge_summary.json")
summary$total_rows
#> [1] 92847
summary$total_cols
#> [1] 265
summary$total_waves
#> [1] 17
```

### The text report

Human-readable, suitable for inclusion in a paper’s Supplementary
Materials section. It lists validation results and comparability
contracts.

## Step 3 — golden-reference regression testing

When you update a recipe (e.g. to add a new wave or fix a rule), you
want to verify that existing outputs did not change unexpectedly. The
golden-reference pattern captures key invariants from a known-good run
and compares future runs against them.

### Create golden references (first time)

``` r

# run after the first successful merge
merged <- haven::read_sav("output/ch_merged.sav")

golden <- list(
  row_count          = nrow(merged),
  col_count          = ncol(merged),
  wave_count         = length(unique(merged$wave_id)),
  unique_respondents = length(unique(merged$nomem_encr)),
  column_names       = sort(names(merged)),
  wave_ids           = sort(unique(merged$wave_id)),
  wave_row_counts    = as.list(table(merged$wave_id)),
  na_rates           = lapply(merged, \(x) round(mean(is.na(x)), 4)),
  column_types       = vapply(merged, \(x) class(x)[1], character(1))
)

jsonlite::write_json(golden, "golden/ch_golden.json",
                     pretty = TRUE, auto_unbox = TRUE)
```

### Run regression tests (after recipe changes)

``` r

# tests/test_merge_regression.R
library(testthat)
library(jsonlite)

test_that("CH merge output matches golden reference", {
  merged <- haven::read_sav("output/ch_merged.sav")
  golden <- jsonlite::read_json("golden/ch_golden.json")

  expect_equal(nrow(merged), golden$row_count)
  expect_equal(ncol(merged), golden$col_count)
  expect_equal(length(unique(merged$wave_id)), golden$wave_count)
  expect_equal(sort(names(merged)), golden$column_names)

  # check per-wave row counts
  actual_wave_counts <- as.list(table(merged$wave_id))
  for (w in names(golden$wave_row_counts)) {
    expect_equal(
      actual_wave_counts[[w]],
      golden$wave_row_counts[[w]],
      label = paste("wave", w, "row count")
    )
  }

  # check NA rate drift (warn at 5 percentage points)
  for (col in names(golden$na_rates)) {
    if (col %in% names(merged)) {
      actual_na <- round(mean(is.na(merged[[col]])), 4)
      expected_na <- golden$na_rates[[col]]
      expect_lt(
        abs(actual_na - expected_na), 0.05,
        label = paste("NA rate drift in", col)
      )
    }
  }
})
```

## Step 4 — validate recipes in CI

If your project uses GitHub Actions or similar CI, validate all recipes
on every push:

``` yaml
# .github/workflows/validate-recipes.yml
name: Validate merge recipes
on: [push, pull_request]
jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: r-lib/actions/setup-r@v2
      - name: Install lissr
        run: |
          install.packages("remotes")
          remotes::install_local(".")
        shell: Rscript {0}
      - name: Validate recipes
        run: |
          library(lissr)
          recipes <- list.files("recipes", pattern = "\\.yml$", full.names = TRUE)
          for (path in recipes) {
            r <- yaml::yaml.load_file(path)
            validate_recipe(r, path)
          }
          cat("All recipes passed validation.\n")
        shell: Rscript {0}
```

## Step 5 — new-wave onboarding

When LISS releases a new annual wave,
[`onboard_new_wave()`](https://siardv.github.io/lissr/reference/onboard_new_wave.md)
automates the detective work:

``` r

report <- onboard_new_wave(
  recipe_path  = "recipes/ch_custom.yml",
  new_file     = "data/ch/ch25r_EN_1.0p.sav",
  prev_wave_id = "ch24q"
)
```

The report tells you:

- which variables were added or removed relative to the previous wave,
- whether all critical variables (from `expected_presence`) are present,
- whether unusual column types or sentinel-value patterns appeared,
- a candidate `wave_index` entry you can paste into the recipe.

After reviewing the report, you add the new wave to the recipe, re-run
the merge, and update the golden reference.

## Step 6 — documenting data-cleaning decisions

The recipe itself *is* the documentation. Instead of writing a prose
description of your cleaning steps in the appendix, point reviewers to
the YAML file. Every rule has a `description` field, and boundary rules
have `comparability` contracts with explicit rationales.

For a paper’s methods section, a minimal reference looks like:

> Data were merged using the lissr R package (v1.0.0) with recipe
> `ch_custom.yml` (recipe version 1.0.0, schema version 1.0.0). Sentinel
> codes were recoded to NA per rules H01–H03. Self-rated health items
> (suffix 001) were pooled across all 17 waves; e-cigarette items
> (suffixes 265–267) were restricted to waves ch15h–ch24q per
> comparability contract B03 (method = no_pool). The merge log and
> recipe file are included in the replication package.

## Assembling a replication package

A complete replication package for a LISS-based paper should include:

| File | Purpose |
|----|----|
| `recipes/*.yml` | exact cleaning specification |
| `golden/*.json` | expected output invariants |
| `R/01_download.R` | data acquisition script (reviewers run this with their own LISS credentials) |
| `R/02_merge.R` | merge script referencing the recipe |
| `R/03_analysis.R` | statistical analysis |
| `output/ch_merge_log.jsonl` | audit trail (can be shared — contains no microdata) |
| `output/ch_merge_report.txt` | human-readable validation summary |
| `tests/` | regression tests against golden references |

The `data/` directory is *not* included (LISS data access requires an
individual agreement), but any reviewer with archive access can
regenerate the exact same merged output by running the scripts in order.

## Comparing recipe versions

If you change a recipe between paper revisions, you can diff the YAML
files to see exactly what changed. Because the recipe is plain text with
structured rule IDs, standard `diff` tools work well:

``` bash
diff recipes/ch_custom_v1.yml recipes/ch_custom_v2.yml
```

The structured `rule_id` fields make it easy to trace which rules were
added, modified, or removed.
