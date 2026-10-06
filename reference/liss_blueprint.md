# build a complete file inventory of the LISS Data Archive

scrapes every module page to build a data frame listing all downloadable
files (SPSS, Stata, codebooks, and monthly Background Variables ZIP
archives) across every wave. the result is cached in memory so
subsequent calls return instantly.

## Usage

``` r
liss_blueprint(refresh = FALSE)
```

## Arguments

- refresh:

  logical. if `TRUE`, re-scrapes the archive even when a cached
  blueprint exists.

## Value

a tibble with columns `module`, `module_id`, `wave`, `wave_id`, `type`,
`name`, `file`, and `path`. For Background Variables (module 322),
`wave` is the filename's YYYYMM month, `wave_id` is 322, and ZIP files
have `type = "archive"`. Its documents have `wave = NA` because they are
not monthly releases. Filenames and published descriptions are
preserved; archive contents and their observed periods are not verified
by the catalogue.

## Examples

``` r
if (FALSE) { # \dontrun{
bp <- liss_blueprint()
bp
} # }
```
