# load and validate a canonical YAML merge recipe

reads a YAML recipe from disk, runs the pre-flight schema validator, and
returns the parsed recipe list.

## Usage

``` r
load_recipe(path)
```

## Arguments

- path:

  character. path to a YAML recipe file.

## Value

a named list representing the parsed recipe.
