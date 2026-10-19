# Validate a complete applied household-income result

Validate the complete analytical result returned by
[`liss_clean_income()`](https://siardv.github.io/lissr/reference/liss_clean_income.md)
for an explicitly selected schema-4 household `s339` input in `correct`
or `na_only` mode. The original native source collection is retained
separately in `source_input`; the changed analysis frame is not a native
source carrier. Whole-result RDS preserves this distinction. Extracted
frames, arbitrary subsets/reorderings and column projections do not
carry a complete result.

## Usage

``` r
liss_validate_applied_income(result, replay = FALSE)
```

## Arguments

- result:

  complete applied collection result, including `source_input`, saved
  `invocation` and `applied_lineage`.

- replay:

  one plain logical value. If `TRUE`, require and rerun the exact
  recorded computation after structural validation.

## Value

invisibly, a list containing `valid`, `validation`, scoped
`source_readiness`, `analysis_execution_status` and `replay_status`.

## Details

The default structural check verifies source bindings, full row/column
scope, immutable non-target columns and tax leaves, metadata, observed
recovery, protected missing cells, effective configuration and receipt
consistency. Constructor-owned decision, optional dataset-flag, summary
and log shapes are checked, including plain count and timing types.
Timing fields must have supported types before their nonsemantic values
are excluded from replay. Ordinary digests are not signatures or proof
that cleaning executed. An unlocked proposal and its receipts can be
changed together by a caller; structural success does not establish
numerical or statistical validity.

With `replay = TRUE`, require the recorded computation/runtime identity
and rerun the saved invocation through the collection consumer. Compare
complete semantic output against that execution, excluding only
duration, log timestamps and ruleset source paths. Replay does not read
original source files, install dependencies, export output or fit a
different default method. It relies on the current trusted
package/runtime and retained source evidence; it does not authenticate
caller-controlled code or fabricated source receipts. If that exact
computation is unavailable, structural validation may still be
supported, but replay fails explicitly. Guarded reports, panels,
exports, repeated consumption, background attachment and taxable
cleaning are unsupported. Removing all lineage claims deliberately
leaves ordinary unguarded input; names or values alone cannot recover
source ownership.
