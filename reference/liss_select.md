# interactively select modules, waves, and file types

presents a series of interactive menus to choose which modules, waves,
and file types to include in a download. the result can be passed
directly to
[`liss_download()`](https://siardv.github.io/lissr/reference/liss_download.md).
Background Variables months (YYYYMM) are selected separately from core
wave numbers. Its undated documents remain available at the file-type
step. ZIP archives are explicit choices; all listed languages and
release versions remain available, without preferring a version or
choosing a month automatically.

## Usage

``` r
liss_select()
```

## Value

a tibble suitable for
[`liss_download()`](https://siardv.github.io/lissr/reference/liss_download.md),
or `NULL` if the user cancels at any step.

## Examples

``` r
if (FALSE) { # \dontrun{
selection <- liss_select()
liss_download(selection)
} # }
```
