# Create or validate a view of an applied household-income result

Select and reorder already-computed rows from a valid whole `s339`
applied result in `correct` or `na_only` mode. The exact complete
parent, including original native schema-4 `source_input`, stays in
`parent_result`. Creation and structural validation project existing
output without cleaning or fitting. The separately pinned view policy
and lineage are version 1.0.0; the native carrier remains version 4.0.0
and whole-parent lineage remains version 1.0.0.

## Usage

``` r
liss_create_applied_income_view(parent_result, row_keys)

liss_validate_applied_income_view(view, replay = FALSE)
```

## Arguments

- parent_result:

  complete result accepted by
  [`liss_validate_applied_income()`](https://siardv.github.io/lissr/reference/liss_validate_applied_income.md),
  with native schema-4 source input.

- row_keys:

  required plain ordered unique character vector of parent row keys,
  without names, missing values, empty keys or other attributes.

- view:

  complete applied income view, including its exact parent.

- replay:

  one plain nonmissing logical value; if `TRUE`, replay the complete
  parent after structural view validation.

## Value

Creation returns a closed list with `parent_result`, `selection`,
`data`, `decisions`, `summary` and `applied_lineage`, with class
`liss_clean_collection_applied_result` (without `liss_clean_result`).
Validation invisibly returns `valid`, `validation`, `view_status`,
`source_readiness`, `parent_source_readiness`,
`parent_analysis_execution_status` and `replay_status`.

## Details

Supply explicit ordered unique plain `row_keys` from
`parent_result$applied_lineage$row_keys`. `character(0)` selects an
empty view. All data columns remain in parent order. Row-dependent names
and dimensions follow the natural subset; other column metadata stay
exact. The frame keeps its existing guarded class with a distinct
versioned applied-view marker.

Decisions retain every parent ledger column, original root `row`, opaque
`decision_id` and subset row names. Integer `root_decision_row` is the
ordinal in the complete parent ledger; integer `view_row` is the
selected data-row position. Entries are ordered by `view_row`, then
`root_decision_row`. Stable references use the complete parent digest
and root ledger ordinal, without requiring unique decision IDs. Counts
refer to selected rows or ledger entries; multiple decisions can refer
to one selected data row.

Scoped source readiness is checked against the selected ORIGINAL source
frame with its unchanged native receipt, never the corrected output.
View projection status, scoped readiness, parent readiness and parent
execution status are separate. Empty and other-wave-only selections can
be `not_evaluated` for source scope while parent execution is
`completed`.

Validation reconstructs the closed projection from its complete retained
parent and exact selection. With `replay = TRUE`, it additionally
validates and replays the complete parent under
[`liss_validate_applied_income()`](https://siardv.github.io/lissr/reference/liss_validate_applied_income.md)
and its recorded computation/runtime gate. It never refits the selected
rows. Whole-view RDS preserves the contract; extracted frames and
partial wrappers do not. Whole-parent validation rejects view wrappers.
Nested views, repeated cleaning, source rebinding, background
attachment, panel/report consumers and SAV/CSV exports are unsupported.

Ordinary SHA-256 hashes (serialization version 2) are not
authentication. Coordinated resealing can describe another consistent
parent and view. Permitted nonsemantic parent timing changes can pass
structure and replay after resealing; no external authenticated parent
anchor exists. Replay checks execution outputs under trusted
code/runtime, not source authenticity or statistical validity. Removing
every recognized claim leaves unknown ownership; aliases cannot recover
it.

Serialized object hashes use profile `1.0.0`: SHA-256 over XDR version-2
R serialization with only the writer-version header fixed to R 4.6.1.
All payload bytes remain significant; raw file hashes are unchanged.
Compatibility covers the R 4.6.1 reference profile, not arbitrary
historical writers or future serialization bodies. Other legacy profiles
require their original package/runtime. Reference identity checks remain
separate from digests.
