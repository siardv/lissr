# merge multiple modules sequentially

validates all recipes first, then runs each module merge. modules with
no data files in `data_dir` are silently skipped.

## Usage

``` r
merge_liss_modules(recipe_paths, data_dir, output_dir = ".", strict = FALSE)
```

## Arguments

- recipe_paths:

  character vector of paths to YAML recipe files.

- data_dir:

  character. root data directory. per-module subdirectories are tried
  first (e.g. `data_dir/ch/`), falling back to `data_dir`.

- output_dir:

  character. directory for output files.

- strict:

  logical. forwarded to
  [`merge_liss_module()`](https://siardv.github.io/lissr/reference/merge_liss_module.md).

## Value

a named list of per-module results (invisibly).
