# download files from the LISS Data Archive

downloads the files described in a selection tibble (typically produced
by
[`liss_select()`](https://siardv.github.io/lissr/reference/liss_select.md))
to a local directory. requires an active session established by
[`liss_login()`](https://siardv.github.io/lissr/reference/liss_login.md).

## Usage

``` r
liss_download(
  .hosted = NULL,
  .dir = "liss",
  .modules = NULL,
  .waves = NULL,
  .unzip = TRUE
)
```

## Arguments

- .hosted:

  a tibble with at least columns `module`, `wave`, `file`, and `path`,
  as returned by
  [`liss_select()`](https://siardv.github.io/lissr/reference/liss_select.md)
  or `get_hosted_files()`. if `NULL`, calls `get_hosted_files()`
  internally.

- .dir:

  character. local directory to save files to. created automatically if
  it does not exist.

- .modules:

  optional character or integer vector to filter by module.

- .waves:

  optional integer vector to filter by wave number.

- .unzip:

  logical. if `TRUE` (the default), ZIP files are extracted and the
  archive is removed.

## Value

a tibble of download results with columns `file` and `status`
(invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
liss_login()
selection <- liss_select()
liss_download(selection)
} # }
```
