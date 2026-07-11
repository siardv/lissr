# Load an income-cleaning ruleset

Reads and validates a declarative income-cleaning ruleset. With
`path = NULL` the ruleset shipped with the package
(`inst/cleaning/income_cleaning_rules.yml`) is used. A custom ruleset
lets researchers re-parameterise, disable, or extend individual decision
rules;
[`validate_cleaning_ruleset()`](https://siardv.github.io/lissr/reference/validate_cleaning_ruleset.md)
enforces the schema.

## Usage

``` r
liss_cleaning_ruleset(path = NULL)
```

## Arguments

- path:

  path to a ruleset YAML file, or `NULL` for the packaged default.

## Value

a validated ruleset object of class `liss_cleaning_ruleset`.

## See also

[`liss_clean_income()`](https://siardv.github.io/lissr/reference/liss_clean_income.md),
[`validate_cleaning_ruleset()`](https://siardv.github.io/lissr/reference/validate_cleaning_ruleset.md)
