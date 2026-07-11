# merge multiple LISS modules into a single panel dataset

takes the output of
[`merge_liss_modules()`](https://siardv.github.io/lissr/reference/merge_liss_modules.md)
(or a list of per-module data frames) and joins them into one wide
dataset keyed by respondent and wave year. module-specific columns are
prefixed with the module code (e.g. `ch_s004`, `cv_s004`) to avoid name
collisions. every module must be one row per join key; the joins are
performed with `relationship = "one-to-one"` and abort otherwise.

## Usage

``` r
merge_liss_panel(
  results,
  join_by = c("nomem_encr", "wave_year"),
  shared_cols = c("nohouse_encr"),
  join_type = c("full", "inner", "left"),
  write_to = NULL
)
```

## Arguments

- results:

  either the named list returned by
  [`merge_liss_modules()`](https://siardv.github.io/lissr/reference/merge_liss_modules.md)
  (where each element has a `$data` tibble), or a named list of data
  frames directly. names should be module codes (e.g. `"ch"`, `"cv"`).

- join_by:

  character vector of columns to join on. defaults to
  `c("nomem_encr", "wave_year")`.

- shared_cols:

  character vector of additional columns to keep unprefixed, coalesced
  across modules in list order (first non-NA wins). defaults to
  `c("nohouse_encr")`.

- join_type:

  character. type of join: `"full"` (default), `"inner"`, or `"left"`
  (keeps all rows from the first module).

- write_to:

  optional file path. if provided, the merged panel is written as SAV
  (SPSS format, preserving labels). should end in `.sav`.

## Value

a tibble with all modules joined side by side.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- merge_liss_modules(recipe_paths, data_dir = "~/Downloads/liss")
panel <- merge_liss_panel(results)
panel <- merge_liss_panel(results, write_to = "./output/liss_panel.sav")
} # }
```
