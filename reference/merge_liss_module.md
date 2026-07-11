# run the full merge pipeline for a single module

loads wave files, applies all rules (variable, harmonization, boundary,
drop/retain), derives variables, runs validation checks, and writes
outputs (merged SAV, JSONL log, JSON summary, text report).

## Usage

``` r
merge_liss_module(recipe, data_dir, output_dir = ".", strict = FALSE,
  overwrite = TRUE)
```

## Arguments

- recipe:

  a parsed recipe list (from
  [`load_recipe()`](https://siardv.github.io/lissr/reference/load_recipe.md)),
  or a path to a recipe file.

- data_dir:

  character. directory containing wave data files.

- output_dir:

  character. directory for output files.

- strict:

  logical. if `TRUE`, abort before writing any outputs when a validation
  check with `severity: error` fails or cannot be evaluated, or when a
  wave's selected file violates its `expected_release` pin; the default
  `FALSE` preserves the historical report-and-continue behavior.

- overwrite:

  logical. if `FALSE`, abort instead of overwriting an existing merged
  output file. default `TRUE` preserves prior behavior.

## Value

a list (invisibly) with elements `data`, `log`, `validation`, `summary`,
`recipe`, `provenance` (package and recipe versions, input file md5
hashes, release decisions, strictness, timestamp), and
`valid_for_analysis` (`TRUE` when no error-severity check failed or was
unevaluable and all release pins matched).
