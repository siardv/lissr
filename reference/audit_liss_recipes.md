# audit merge recipes for engine conformance

scans recipes (by default all ten bundled ones) and reports, per module,
whether every rule payload conforms to its action's specification, how
the declared validation checks classify (executable, documentary, skip),
and whether wave metadata is internally consistent. this turns the
scattered authoring-time warnings into one corpus-level conformance
report, suitable for CI gating.

## Usage

``` r
audit_liss_recipes(paths = NULL, quiet = FALSE)
```

## Arguments

- paths:

  character vector of recipe file paths. default `NULL` audits all
  bundled recipes.

- quiet:

  logical. `TRUE` suppresses the printed report.

## Value

invisibly, a list with one entry per recipe (fields `module`,
`schema_version`, `recipe_version`, `n_waves`, `covered_waves_match`,
`noncanonical_patterns`, `nonconforming_rules`, `checks`) plus a
`totals` entry.

## Details

for each recipe the audit reports: schema and recipe versions; wave
count; whether `meta$covered_waves` equals the `wave_index` ids; whether
every `file_pattern` is the canonical glob built from the wave id, in
the `ch07a_*` style (documented pins are listed, not flagged); rules
whose payload carries keys their action does not read (per the payload
registry in `action_vocabulary.yml`); and the validation-check
classification counts.

## Examples

``` r
if (FALSE) { # \dontrun{
audit <- audit_liss_recipes()
audit$totals
} # }
```
