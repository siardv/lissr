# run the full merge pipeline for a single module

loads wave files, applies all rules (variable, harmonization, boundary,
drop/retain), derives variables, runs validation checks, and writes
outputs (merged SAV, JSONL log, JSON summary, text report).

## Usage

``` r
merge_liss_module(
  recipe,
  data_dir,
  output_dir = ".",
  strict = FALSE,
  overwrite = TRUE,
  source_nonresponse = FALSE
)
```

## Arguments

- recipe:

  a parsed recipe list (from
  [`load_recipe()`](https://siardv.github.io/lissr/reference/load_recipe.md)),
  or a path to a recipe file.

- data_dir:

  character. directory containing wave data files.

- output_dir:

  character. directory for output files, or `NULL` to return the
  complete result in memory without writing files.

- strict:

  logical. if `TRUE`, abort before writing any outputs when a validation
  check with `severity: error` fails or cannot be evaluated, or when a
  wave's selected file violates its `expected_release` pin; the default
  `FALSE` preserves the historical report-and-continue behavior.

- overwrite:

  logical. if `FALSE`, abort instead of overwriting an existing merged
  output file. default `TRUE` preserves prior behavior.

- source_nonresponse:

  logical. opt in to the explicitly selected reviewed scalar source
  classifier before harmonization. The default CI recipe selects
  CI23p339; schema 2.0.0 explicitly selects either CI23p337 or CI23p338
  with two exact codes under one field receipt. Taxable carriers cannot
  enter the household income cleaner. Package-registry approval is
  required. Schema 3.0.0 explicitly selects the complete
  CI23p337/CI23p338 pair, validates both original fields before
  normalizing either, and returns a tagged collection with two complete
  scalar receipts. Schema 4.0.0 explicitly selects all three
  CI23p337/338/339 fields through a separately pinned collection
  adapter. All three original checks precede normalization, and the
  tagged version-4 result retains complete native v2/v2/v1 receipts.
  Registry-2 authority, defaults and scalar/pair behavior stay
  unchanged. Separate pinned policies permit explicit household `s339`
  flag simulation or applied `correct`/`na_only` results on a complete
  version-4 result. Applied results require `background = NULL`; taxable
  consumption remains unsupported. Requires `output_dir = NULL`; guarded
  SAV export and automatic panel joining are not yet supported. Retain
  the complete result (or serialize it with
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html)) to preserve its
  audit.

## Value

a list (invisibly) with elements `data`, `log`, `validation`, `summary`,
`recipe`, `provenance` (package and recipe versions, input file md5
hashes, release decisions, strictness, timestamp), and
`valid_for_analysis` (`TRUE` when no error-severity check failed or was
unevaluable and all release pins matched).
