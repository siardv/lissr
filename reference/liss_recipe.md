# load a built-in merge recipe by module code

convenience function to load one of the bundled YAML recipes shipped
with the package (in `inst/recipes/`).

## Usage

``` r
liss_recipe(module)
```

## Arguments

- module:

  character. two-letter module code (e.g. `"ch"`, `"cv"`).

## Value

a parsed recipe list (validated against the canonical schema).

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- liss_recipe("ch")
} # }
```
