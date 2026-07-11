# validate a merge recipe against the canonical schema

checks required sections, field presence, action vocabulary, anomaly_ref
format, and rule_id uniqueness. aborts on any violation.

## Usage

``` r
validate_recipe(recipe, path = "<unknown>")
```

## Arguments

- recipe:

  a named list (parsed YAML recipe).

- path:

  character. file path used in error messages.

## Value

invisible `TRUE` on success (aborts otherwise).
