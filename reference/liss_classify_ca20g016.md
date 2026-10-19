# Classify or validate exact CA20g016 source nonresponse

Read only the complete reviewed `ca20g_EN_1.0p.sav` source and classify
the exact `ca20g016` codes `-9999999999` and `-9999999998`. A separately
pinned authority and whole-result carrier are version 1.0.0. This field
is a signed reported investment value at 2019-12-31 in whole EUR.
Ownership depends on source household-role and ownership preloads; no
uniform household total or income interpretation is asserted. Negative
balances can be legitimate (for example, written options).

## Usage

``` r
liss_classify_ca20g016(source)

liss_validate_ca20g016(result, source = NULL)
```

## Arguments

- source:

  path to the exact reviewed local SAV file. Required for
  classification; optional for additional source consistency validation.

- result:

  complete result returned by `liss_classify_ca20g016()`.

## Value

Classification returns a guarded closed list with `original_data`,
`analysis`, `data`, `source_nonresponse` and
`source_nonresponse_readiness`. `analysis` contains `data` and `ledger`.
Validation invisibly returns `valid`, `validation`,
`source_consistency`, `classified_cells`, `source_readiness`,
`source_authentication` and `statistical_validity`.

## Details

Creation checks filename, byte size and SHA-256 before and after import.
Haven parses an in-memory byte buffer whose exact size and hash were
checked, with `user_na = TRUE` and minimal name repair. The complete
imported tibble, original row order, all columns and metadata remain in
`original_data`. Only the two codes become NA in the separate
`analysis$data` target. Its original source labels and missing
declarations remain descriptive metadata; they do not attest changed
analysis values. `data` duplicates that marked analysis frame for
existing consumer guards. The typed `analysis$ledger` retains original
code, root ordinal, key, case, unknown subtype, missing-amount
disposition and no-numeric-proposal flag. Source-cell references use the
source hash, original row ordinal and raw field.

SAV positive ten-digit missing declarations differ from its positive
eleven-digit labels and codebook forms. The negative codes are
undeclared. No sign/digit repair or authenticated DK/refusal subtype
follows. If `ca20g016` contains positive `9999999998`, `9999999999`,
`99999999998` or `99999999999`, classification and validation reject the
whole call with `liss_ca20g016_blocked`; no partial result is returned.
Ordinary signed values, including -9 and -8, and nonfinite nonmatches
stay untouched. Readiness covers only the two source encodings; overall
amount validity is `not_evaluated`. No amount or sign is reconstructed
from the following `ca20g017` bracket. Coded rows require the exact
nonmissing `ca20g006` ownership gate equal to one. Noncoded rows are not
required to satisfy this gate. All other fields, including `ca20g012`,
remain exact; this contract covers only `ca20g016` in this release. The
separate
[`liss_classify_ca20g012()`](https://siardv.github.io/lissr/reference/liss_classify_ca20g012.md)
contract is unchanged; results are independent and cannot be combined
into a multi-field carrier.

Validation requires the complete closed result and reconstructs it from
the retained complete original. With `source = NULL`, this checks
structural consistency only. Supplying `source` additionally rereads
verified source bytes and compares the entire imported original,
including all other columns and metadata. Result payloads and reread
originals must match in version-2 serialization, including signed zero,
missing-value payloads and attribute order. Guard-attribute placement on
analysis frames is ignored, while their values are checked separately.
Whole-result RDS retains the object. Validation still requires the
reconstructed result and any reread source to reproduce the required
serialized representation. Validation across R or package versions is
not guaranteed. Ordinary serialized hashes (version 2) are not
authentication: a consistently substituted original and result may pass
structure, while source rereading detects a mismatch against the trusted
file. Neither mode establishes statistical validity.

Existing collection claims are guards, not CI carrier or
numerical-consumer approval. Rebinding, income/wealth cleaning, applied
views, background, panel, report and SAV/CSV export are unsupported. In
explicit `correct` or `na_only` mode,
[`liss_clean_income()`](https://siardv.github.io/lissr/reference/liss_clean_income.md)
can evaluate `background` to check its required `NULL` value before
rejecting the unsupported CA parent. Rejection does not guarantee that
caller-supplied argument expressions remain unevaluated. Extracting
unmarked original data or stripping every recognized claim leaves
unknown ownership; aliases cannot recover it. The default CA merge
recipe and whole-wave B03 flag are unchanged.
