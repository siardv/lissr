# interactively select modules, waves, and file types

presents a series of interactive menus to choose which modules, waves,
and file types to include in a download. the result can be passed
directly to
[`liss_download()`](https://siardv.github.io/lissr/reference/liss_download.md).

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
