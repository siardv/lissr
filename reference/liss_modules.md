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
  blueprint).

## Value

a tibble with columns `module`, `module_id`, and `waves`.

## Examples

``` r
if (FALSE) { # \dontrun{
liss_modules()
liss_modules(.details = TRUE)
} # }
```
