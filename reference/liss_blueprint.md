# build a complete file inventory of the LISS Data Archive

scrapes every module page to build a data frame listing all downloadable
files (SPSS, Stata, codebooks) across every wave. the result is cached
in memory so subsequent calls return instantly.

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
`name`, `file`, and `path`.

## Examples

``` r
if (FALSE) { # \dontrun{
bp <- liss_blueprint()
bp
} # }
```
