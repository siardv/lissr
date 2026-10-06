# list available LISS panel modules

returns a data frame of modules available in the LISS Data Archive. if a
blueprint has already been cached (via
[`liss_blueprint()`](https://siardv.github.io/lissr/reference/liss_blueprint.md)),
the module list is derived from the cache; otherwise the archive index
page is scraped directly.

## Usage

``` r
liss_modules(.details = FALSE)
```

## Arguments

- .details:

  logical. if `TRUE`, includes file counts per type (requires a cached
  blueprint). Cached details include an `archives` count for ZIP
  releases.

## Value

a tibble with columns `module`, `module_id`, and `waves`. Without a
cache and with `.details = FALSE`, only the first two columns are
returned. Background Variables counts distinct monthly releases as
waves; its undated documents do not add a wave.

## Examples

``` r
if (FALSE) { # \dontrun{
liss_modules()
liss_modules(.details = TRUE)
} # }
```
