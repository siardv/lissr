# Classify or validate exact CA20g023 source nonresponse

Read only the complete reviewed `ca20g_EN_1.0p.sav` source and classify
the exact `ca20g023` codes `-9` and `-8`. A separately pinned authority
and whole-result carrier are version 1.0.0. The question estimates the
sales value of cars, motorcycles, boats and (static) caravans owned on
2019-12-31, in whole EUR. The date identifies the assets owned then; no
additional valuation-time interpretation is asserted. Ownership depends
on source household-role and own/joint ownership context, not a uniform
household total or income construct.

## Usage

``` r
liss_classify_ca20g023(source)

liss_validate_ca20g023(result, source = NULL)
```

## Arguments

- source:

  path to the exact reviewed local SAV file. Required for
  classification; optional for additional source consistency validation.

- result:

  complete result returned by `liss_classify_ca20g023()`.

## Value

Classification returns a guarded closed list with `original_data`,
`analysis`, `data`, `source_nonresponse` and
`source_nonresponse_readiness`. `analysis` contains `data` and `ledger`.
Validation invisibly returns `valid`, `validation`,
`source_consistency`, `classified_cells`, `source_readiness`,
`source_authentication` and `statistical_validity`.

## Details

Creation checks filename, byte size and SHA-256 before and after import.
Haven parses the in-memory byte buffer whose exact size and hash were
checked, with `user_na = TRUE` and minimal name repair. The complete
imported tibble, original row order, all columns and metadata remain in
`original_data`. Only the two codes become NA in the separate
`analysis$data` target. Its retained source labels and missing
declarations are descriptive metadata; they do not attest changed
analysis values. `data` duplicates that marked analysis frame for
existing consumer guards. The typed `analysis$ledger` retains original
code, root ordinal, key, case, unknown subtype, missing-amount
disposition and no-numeric-proposal flag. Source-cell references use the
source hash, original row ordinal and raw field.

SAV positive ten-digit missing declarations AND value labels differ from
the codebook's positive eleven-digit forms. The negative codes are
undeclared. No sign/digit repair or authenticated don't-know/refusal
mapping follows. If `ca20g023` contains positive `9999999998`,
`9999999999`, `99999999998` or `99999999999`, classification and
validation reject the whole call with `liss_ca20g023_blocked`; no
partial result is returned. Positive `8` and `9` are ordinary amounts,
not aliases for the negative codes. Every nonmatch remains untouched and
uncertified, including other negatives, either zero sign, fractional and
out-of-range values and nonfinite values. The documented amount domain
`1..999999999` is descriptive, not enforced by this classifier.
Readiness covers only the two source encodings; overall amount validity
is `not_evaluated`. No amount, midpoint, sign or authenticated subtype
is inferred from the `ca20g024` follow-up bracket. Coded rows require
the exact finite, nonmissing `ca20g008` ownership gate equal to one.
Noncoded rows need not satisfy that gate; eligible source-missing
answers remain missing. All 65 other columns, including `ca20g012`,
`ca20g016` and `ca20g024`, remain exact. The separate
[`liss_classify_ca20g012()`](https://siardv.github.io/lissr/reference/liss_classify_ca20g012.md)
and
[`liss_classify_ca20g016()`](https://siardv.github.io/lissr/reference/liss_classify_ca20g016.md)
contracts are unchanged; no combined multi-field carrier is defined.

Validation requires the complete closed result and reconstructs it from
the retained complete original. With `source = NULL`, this checks
structural consistency only. Supplying `source` additionally rereads
verified source bytes and compares the entire imported original,
including every non-target column and metadata. Result payloads and
reread originals must match both R identity and version-2 serialization,
including signed zero, missing-value payloads and attribute order.
Guard-attribute placement on analysis frames is ignored while marker and
readiness values are checked separately. Whole-result RDS retains the
object; validation across R or package versions is not guaranteed.
Ordinary serialized hashes are not authentication: a consistently
substituted original and result may pass structure, while source
rereading detects a mismatch against the trusted file. Neither mode
establishes statistical validity.

Existing collection claims are guards, not CI carrier or
numerical-consumer approval. Rebinding, income/wealth cleaning, applied
views, background, panel, report and SAV/CSV export are unsupported. In
explicit `correct` or `na_only` mode,
[`liss_clean_income()`](https://siardv.github.io/lissr/reference/liss_clean_income.md)
can evaluate `background` to check its required `NULL` value before
rejecting the unsupported CA parent. Rejection does not guarantee
caller-supplied argument expressions remain unevaluated. Extracting
unmarked originals or stripping every recognized claim leaves unknown
ownership; aliases cannot recover it. The default CA recipe and
whole-wave B03 flag are unchanged. No other field or release is covered.

Serialized object hashes use profile `1.0.0`: SHA-256 over XDR version-2
R serialization with only the writer-version header fixed to R 4.6.1.
All payload bytes remain significant; raw file hashes are unchanged.
Compatibility covers the R 4.6.1 reference profile, not arbitrary
historical writers or future serialization bodies. Other legacy profiles
require their original package/runtime. Reference identity checks remain
separate from digests.
