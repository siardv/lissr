# Detect and correct implausible household-income values

Applies a declarative income-cleaning ruleset to merged LISS data:
preparation (background attachment, sentinel guarding, sign
rectification, bracket-code expansion), global voiding of unrecoverable
values, household-level detection of scale errors, bound violations, cap
exceedances and robust statistical outliers, constrained candidate-based
correction, a hard plausibility cap, and dataset-level flagging. Every
decision is recorded in a ledger with the responsible rule, the
evidence, the admissible candidate set, and a plain-language
justification.

## Usage

``` r
liss_clean_income(
  data,
  background = NULL,
  ruleset = NULL,
  mode = c("correct", "flag", "na_only"),
  income_cap = NULL,
  min_income = NULL,
  disable = NULL,
  enable_only = NULL,
  params = NULL,
  variables = NULL,
  output_dir = NULL,
  verbose = TRUE
)
```

## Arguments

- data:

  a data frame of merged LISS income data, or a
  [`merge_liss_module()`](https://siardv.github.io/lissr/reference/merge_liss_module.md)
  result (its `$data` element is used).

- background:

  optional background/demographics frame attached by rule P01 before
  cleaning (joined on the person id and, when the background is
  wave-stamped, the aligned annual wave index).

- ruleset:

  a ruleset object from
  [`liss_cleaning_ruleset()`](https://siardv.github.io/lissr/reference/liss_cleaning_ruleset.md),
  a path to a ruleset YAML file, or `NULL` for the packaged default.

- mode:

  `"correct"` (apply corrections), `"flag"` (dry run, propose only), or
  `"na_only"` (void detected cells without imputation).

- income_cap, min_income:

  optional overrides for the global plausibility constraints.

- disable, enable_only:

  optional character vectors of rule ids to switch off, or to run
  exclusively. `enable_only` is scoped per ruleset section: only
  sections that contain a named rule are restricted to the named set, so
  `enable_only = "D06"` isolates one detection rule while the
  preparation, correction, and finalization machinery keeps running.

- params:

  optional named list of per-rule parameter overrides, e.g.
  `list(D06 = list(volatility_min = 0.7))`.

- variables:

  optional named list overriding entries of the ruleset's variable
  mapping, e.g. `list(target = "ci00a339")`.

- output_dir:

  optional directory; when given, the report, decision ledger, and JSONL
  log are written via
  [`liss_cleaning_report()`](https://siardv.github.io/lissr/reference/liss_cleaning_report.md).

- verbose:

  print progress with cli.

## Value

invisibly, a `liss_clean_result` list with elements `data`, `decisions`
(the ledger), `log`, `summary`, `ruleset`, `variables`, and `mode`.

## Details

The returned data always carries the untouched input values in
`<target>_observed`. In `"correct"` mode the target column holds the
cleaned values and `<target>_clean_status` marks each modified cell with
its final action and rule. In `"flag"` mode the target column is left
untouched and the fully simulated result is returned in
`<target>_proposed` (with `<target>_proposed_status`), so the entire
procedure can be inspected as a dry run. In `"na_only"` mode detected
cells are voided instead of imputed. Dataset-level flags (rule D11) are
annotations in `<target>_dataset_flag` and never modify values.

Calling the function on already-cleaned data (recognizable by the
`<target>_observed` column) is an error, which prevents accidental
double cleaning.

## See also

[`liss_cleaning_ruleset()`](https://siardv.github.io/lissr/reference/liss_cleaning_ruleset.md),
[`liss_cleaning_report()`](https://siardv.github.io/lissr/reference/liss_cleaning_report.md),
[`liss_equivalise_income()`](https://siardv.github.io/lissr/reference/liss_equivalise_income.md)
