# merge multiple modules sequentially

by default, detects modules with local wave data and uses their built-in
recipes, in module-code order. detection accepts `.sav`, `.zsav`, `.dta`
and `.csv` files named with a module-wave prefix (e.g. `ch07a_`). it
uses the contents of `data_dir`, including earlier downloads, rather
than the most recent download selection. recipe wave coverage remains
unchanged.

## Usage

``` r
merge_liss_modules(
  recipe_paths = NULL,
  data_dir,
  output_dir = ".",
  strict = FALSE
)
```

## Arguments

- recipe_paths:

  optional character vector of paths to YAML recipe files. default
  `NULL` selects built-in recipes for modules with local wave data.

- data_dir:

  character. root data directory. per-module subdirectories are used
  when present (e.g. `data_dir/ch/`); otherwise scans `data_dir`.

- output_dir:

  character. directory for output files.

- strict:

  logical. forwarded to
  [`merge_liss_module()`](https://siardv.github.io/lissr/reference/merge_liss_module.md).

## Value

a named list of per-module results (invisibly).

## Details

validates the selected recipes before running any module merge. when
`recipe_paths` is supplied, uses those recipes in the supplied order and
reports modules with no data files as skipped. recipe and data warnings
remain visible in both modes.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- merge_liss_modules(data_dir = "liss", output_dir = "output")
} # }
```
