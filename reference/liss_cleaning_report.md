# Write the income-cleaning report and audit artifacts

Renders a markdown report with the run configuration, the full
methodology generated from the ruleset (every rule with its description,
rationale, parameters, and references), result summaries with
observed-versus-cleaned distributions, and a decision appendix.
Alongside the report, the complete decision ledger is written as CSV and
the engine-shaped audit log as JSONL.

## Usage

``` r
liss_cleaning_report(result, output_dir, verbose = TRUE)
```

## Arguments

- result:

  a `liss_clean_result` from
  [`liss_clean_income()`](https://siardv.github.io/lissr/reference/liss_clean_income.md).

- output_dir:

  directory for the artifacts (created if needed). Required, so three
  files are never written into the working directory by accident.

- verbose:

  print the written paths.

## Value

invisibly, a list with the `report`, `decisions`, and `log` paths.
