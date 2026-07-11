# run the full merge pipeline for a single module

loads wave files, applies all rules (variable, harmonization, boundary,
drop/retain), derives variables, runs validation checks, and writes
outputs (merged SAV, JSONL log, JSON summary, text report).

## Usage

``` r
merge_liss_module(recipe, data_dir, output_dir = ".", strict = FALSE)
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
  check with `severity: error` fails; the default `FALSE` preserves the
  historical report-and-continue behavior.

## Value

a list (invisibly) with elements `data`, `log`, `validation`, `summary`,
and `recipe`.
