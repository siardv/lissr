# Equivalise household income

Converts household income to a per-equivalent-adult scale for
cross-household comparison. The default `"weighted_sqrt"` scale divides
by `(adults + child_weight * children)^elasticity`, the scale used by
the source analysis pipelines; `"oecd_modified"` divides by
`1 + 0.5 * (adults - 1) + 0.3 * children`; `"sqrt"` divides by the
square root of household size. Rows with an invalid composition (size
below one, negative children, or more children than members) yield NA
and are counted in a warning.

## Usage

``` r
liss_equivalise_income(
  income,
  household_size,
  n_children = 0,
  scale = c("weighted_sqrt", "oecd_modified", "sqrt"),
  child_weight = 0.8,
  elasticity = 0.5,
  verbose = TRUE
)
```

## Arguments

- income:

  numeric household income.

- household_size:

  total household members (LISS `aantalhh`).

- n_children:

  number of children (LISS `aantalki`).

- scale:

  equivalence scale, see details.

- child_weight:

  weight per child under `"weighted_sqrt"`.

- elasticity:

  size elasticity under `"weighted_sqrt"`.

- verbose:

  warn about invalid compositions.

## Value

numeric vector of equivalised income.

## Examples

``` r
liss_equivalise_income(30000, household_size = 3, n_children = 1)
#> [1] 17928.43
liss_equivalise_income(30000, 3, 1, scale = "oecd_modified")
#> [1] 16666.67
```
