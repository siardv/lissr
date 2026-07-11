# Validate an income-cleaning ruleset

Checks a parsed ruleset against the income-cleaning schema v1.0.0:
required metadata and variable mappings, sane global constraints, unique
rule ids, actions drawn from the controlled vocabulary, non-empty
descriptions, resolvable reference keys, and numeric parameter sanity.
Unrecognized rule keys draw a warning only, mirroring the merge engine's
authoring check.

## Usage

``` r
validate_cleaning_ruleset(ruleset, quiet = FALSE)
```

## Arguments

- ruleset:

  a parsed ruleset (from
  [`liss_cleaning_ruleset()`](https://siardv.github.io/lissr/reference/liss_cleaning_ruleset.md)
  or
  [`yaml::read_yaml()`](https://yaml.r-lib.org/reference/read_yaml.html)).

- quiet:

  suppress cli output and only return the result.

## Value

invisibly, a list with `valid`, `errors`, `warnings`, and `n_rules`.
