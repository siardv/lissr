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
  .unzip = TRUE,
  .skip_existing = FALSE,
  .timeout = 300,
  .retries = 2
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
  archive is removed once extraction is verified; a failed extraction
  keeps the archive and reports `unzip_failed_archive_kept`.

- .skip_existing:

  logical. if `TRUE`, files whose listed name is already present in
  `.dir` are skipped without a network request (status
  `skipped_existing`). the check uses the archive's listed file name,
  not the name a content-disposition header may assign.

- .timeout:

  numeric. per-file download timeout in seconds.

- .retries:

  integer. how many times a failed request (curl error or HTTP 5xx) is
  retried before giving up on that file.

## Value

a tibble of download results with columns `file` and `status`
(invisibly). `status` is one of `"ok"`, `"skipped_existing"`,
`"session_expired"`, `"skipped_batch_aborted"`, `"http_<code>"`,
`"unzip_failed_archive_kept"`, or `"error: <message>"`.

## Details

downloads stream to disk through the authenticated session with a
per-file timeout, and transient failures (network errors, HTTP 5xx) are
retried. HTTP error responses are never written to disk as data files,
ZIP archives are only removed after their extraction has been verified,
and the first session expiry aborts the remaining batch (each remaining
file is reported as `skipped_batch_aborted` instead of failing one by
one).

## Examples

``` r
if (FALSE) { # \dontrun{
liss_login()
selection <- liss_select()
liss_download(selection)
} # }
```
