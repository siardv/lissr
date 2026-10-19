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
  wave-stamped, the aligned annual wave index). P01 rejects
  source-nonresponse results or claims before converting the background.
  For ordinary income input, disabling P01 leaves the background unused.

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

The returned data include the numeric input values in
`<target>_observed`. For normalized source-bound input, that audit
column recovers the verified original source codes for classified cells.
In `"correct"` mode the target column holds the cleaned values and
`<target>_clean_status` marks each modified cell with its final action
and rule. In `"flag"` mode the target column is left untouched and the
fully simulated result is returned in `<target>_proposed` (with
`<target>_proposed_status`), so the entire procedure can be inspected as
a dry run. In `"na_only"` mode detected cells are voided instead of
imputed. Dataset-level flags (rule D11) are annotations in
`<target>_dataset_flag` and never modify values.

Calling the function on already-cleaned data (recognizable by the
`<target>_observed` column) is an error, which prevents accidental
double cleaning.

When supplied a result from
[`liss_classify_source_nonresponse()`](https://siardv.github.io/lissr/reference/liss_classify_source_nonresponse.md)
or an opt-in guarded module merge, source-classified amounts are
excluded from numerical proposals in every mode, independently of rule
overrides. The source audit is retained in `$source_nonresponse`;
finite-count and distribution summaries exclude the classified amount
codes. In `"flag"` mode the target remains exactly as supplied,
including normalized missing amounts; recovered source codes remain in
the observed audit column and carrier. A source-bound household-income
target cannot also be assigned as `category_code`. Readiness covers the
specified source exclusion and does not certify every returned audit
column for analysis. Guarded inputs require separate package-registry
approval for the household net-income construct, EUR, the reviewed
measurement period and target binding before numerical preparation.
Classification readiness alone does not approve another consumer or
establish the origin of a marker-stripped bare frame. Source-result
classes or source/readiness attributes without a usable carrier are
rejected before household preparation; a wrapper readiness slot alone
may still report a legacy `not_evaluated` result. Explicit taxable
CI23p337/338 version-2 carriers are rejected before target resolution,
Background/P01 or numeric preparation, under every mode and override.
Schema-3 taxable collections remain unsupported. An intact schema-4
three-field collection has a separate package-pinned household FLAG
capability: explicitly select `variables = list(target = "s339")` with
`mode = "flag"`. This retains all original columns and the complete
collection, excludes the sealed taxable amounts from every secondary
cleaner role, and rejects guarded Background data. Guarded exports
remain unsupported. Complete returned wrappers carry a separate consumer
receipt binding the data, decisions, effective rules, resolved variables
and summary except duration. Retain the complete result as RDS for this
proof. Its bare `$data` carries source lineage only; it does not certify
proposals or decisions as a complete result. A consumer receipt
establishes source exclusion for household flag simulation, not
statistical validity, taxable-field approval or source ownership
elsewhere.

With an explicit schema-4 `s339` target, `mode = "correct"` or
`"na_only"` uses a distinct applied-result capability, requiring
`background = NULL` and `output_dir = NULL`. The complete original input
remains in `$source_input`; `$data` contains the derived household
amount and audit columns. Both tax fields and all other original columns
remain unchanged. Native source metadata, bound input metadata and
derived target metadata remain distinct. The original source carrier is
never rewritten to attest corrected values. `$applied_lineage` binds
source, execution, options and output, with scoped source readiness
separate from analysis execution status. `na_only` preserves the
existing method, including sign rectification and an explicit F01
winsorisation override; it is not universally a finite-to-missing
operation. Retain the whole applied result as RDS and use
[`liss_validate_applied_income()`](https://siardv.github.io/lissr/reference/liss_validate_applied_income.md)
for structural checks or optional exact-runtime replay. Ordinary digests
do not authenticate execution or establish statistical validity.
Extracted frames, manual subsets/reorders, repeat cleaning, background
attachment, guarded panel assembly and report exports are unsupported
for these results.

## See also

[`liss_cleaning_ruleset()`](https://siardv.github.io/lissr/reference/liss_cleaning_ruleset.md),
[`liss_cleaning_report()`](https://siardv.github.io/lissr/reference/liss_cleaning_report.md),
[`liss_equivalise_income()`](https://siardv.github.io/lissr/reference/liss_equivalise_income.md)
