# Classify or validate exact CA20g014 source nonresponse

Read only the complete reviewed `ca20g_EN_1.0p.sav` source and classify
the exact `ca20g014` codes `-9` and `-8`. A separately pinned authority
and whole-result carrier are version 1.0.0. The question asks the
guaranteed minimum payout of single-premium or life annuity insurance,
or the savings amount of endowment insurance on 2019-12-31, in whole
EUR. The amount reference date is distinct from September and October
2020 collection. Household and financial-role instructions distinguish
own assets from own plus joint assets; no uniform household total,
market or surrender value, or income is asserted.

## Usage

``` r
liss_classify_ca20g014(source)

liss_validate_ca20g014(result, source = NULL)
```

## Arguments

- source:

  path to the exact reviewed local SAV file. Required for
  classification; optional for additional complete-source consistency
  validation.

- result:

  complete result returned by `liss_classify_ca20g014()`.

## Value

Classification returns a guarded closed list with `original_data`,
`analysis`, `data`, `source_nonresponse` and
`source_nonresponse_readiness`. `analysis` contains `data` and `ledger`.
Validation invisibly returns `valid`, `validation`,
`source_consistency`, `classified_cells`, `source_readiness`,
`source_authentication` and `statistical_validity`; the last two are
FALSE.

## Details

Creation checks filename, byte size and SHA-256 before and after import.
Haven parses the in-memory byte buffer whose exact size and hash were
checked, with `user_na = TRUE` and minimal name repair. The complete
imported tibble, all columns and metadata, and original row order remain
in `original_data`. Only the exact negative pair becomes ordinary NA in
separate `analysis$data`. Retained source labels and missing
declarations describe the original source; they do not attest changed
analysis values. Top-level `data` duplicates the marked analysis frame.
The typed `analysis$ledger` binds source hash, original row ordinal and
raw field, preserving key, original code, case, unknown subtype,
missing-amount disposition and no-numeric-proposal flag. Coverage lists
every source row. Readiness concerns the two encodings, not overall
amount validity.

SAV positive ten-digit missing declarations AND value labels differ from
the codebook's positive eleven-digit forms. The negative pair is
undeclared. No sign/digit repair or authenticated don't-know/refusal
mapping follows. Target `9999999998`, `9999999999`, `99999999998` or
`99999999999` makes classification and validation reject the whole call
with `liss_ca20g014_blocked`; no partial result is returned. Positive
`8` and `9` are ordinary preserved amounts. Every nonmatch stays exact
and uncertified, including other negatives, either zero sign, fractions,
out-of-range and nonfinite payloads. The documented `1..999999999`
domain is descriptive, not enforced. Unknown subtype remains
`nonresponse_unknown` with `unresolved` evidence; no numerical proposal
or overall amount-validity claim is made.

Coded rows require finite, nonmissing possession gate `ca20g005 = 1`.
Noncoded rows need not satisfy that gate; eligible source-missing
answers remain missing. Exact ordered ID/014/005 metadata are admission
requirements. The `ca20g015` column is required by the complete schema,
but its metadata reference hash is contextual only. The record carries
reference and actual attribute digests separately with
`required_for_admission = FALSE`; equality between those hashes is not
an admission condition. Missing follow-up values, including on coded
rows, are allowed. No amount, midpoint, sign, donor or authenticated
subtype is inferred from its categories. All 65 other columns, including
`ca20g012`, `ca20g016`, `ca20g023` and `ca20g015`, remain exact. The
three older standalone authorities remain separate and unchanged.

Validation requires the complete closed result and reconstructs it
against the complete retained original. With `source = NULL`, this
checks structural consistency only. Supplying `source` additionally
rereads verified bytes and compares the entire imported original,
including every non-target column and metadata. R identity AND version-2
serialized payload equality are required, including numeric payloads,
reference identity and attribute order. Analysis guard placement is
normalized while marker/readiness values remain checked. Whole-result
RDS preserves the object; validation across R or package versions is not
guaranteed. Ordinary hashes do not authenticate origin: a consistently
substituted original/result can pass structure. Source mode detects a
mismatch with the trusted file; neither mode establishes statistical
validity.

Existing collection claims support denial only. They grant no CI
carrier, rebinding, income/wealth cleaning, applied-view, background,
panel, report or SAV/CSV export capability. Explicit `correct` or
`na_only` cleaner rejection can evaluate `background` while checking its
required `NULL` value. Rejection does not promise every caller
expression remains unevaluated. Complete claim stripping or extracting
unmarked originals leaves unknown ownership; aliases cannot recover it.
No combined CA carrier is defined. Default CA recipe/B03, numerical
methods and complete CI parent/view lineage are unchanged. No other
field or release is covered.
