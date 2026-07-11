# lissr: Empirical Verification Report

Companion to `lissr-review.md`. Every claim below was computed from the
uploaded bundle (`lissr_verification_bundle.zip`, 35 data files).
Integrity: all 35 md5 checksums match the bundle’s `MANIFEST.tsv`
**and** the original `files_liss-siardv_20260701_214334.csv` inventory,
so the analyzed bytes are exactly the archive copies.

**Methodological note that is itself a finding (corrected).** The `.sav`
files declare most DK/refusal codes as SPSS *user-defined missing
values*. A default `pyreadstat` read masks them to NA, and a first-pass
scan under those defaults found almost no sentinels; re-reading with
`user_missing = TRUE` revealed tens of thousands of them. An earlier
revision of this note claimed that preserving these codes “is what
[`haven::read_sav()`](https://haven.tidyverse.org/reference/read_spss.html)
(and therefore lissr) does”; direct measurement during patch development
showed that is wrong. haven’s default is `user_na = FALSE`, which also
converts declared user-missing codes to NA at read, and the 1.0.0 engine
used that default. Three corrected implications follow. First, under the
shipped 1.0.0 engine the recipes’ sentinel recodes largely never saw the
declared codes, because the read step had already masked them; the
recodes were doubly inert (mis-keyed rules, and pre-masked inputs). The
masking was silent, unlogged, collapsed the DK-vs-refusal distinction,
and missed any sentinel that a wave failed to declare (the undeclared
ones are exactly the values that did reach outputs). Second, the scans
in this report, made with `user_missing = TRUE`, describe what the data
actually contain and what an engine reading with
`haven::read_sav(user_na = TRUE)` sees; the 1.1.0 engine reads that way
deliberately, so the recipes’ recodes receive the codes and handle them
auditably. Third, lissr’s `to_numeric` policy plus
[`haven::write_sav()`](https://haven.tidyverse.org/reference/read_spss.html)
discarded the user-missing *declarations* entirely; the 1.1.0 label
round-trip restores them where provably safe and sweeps the remainder
per wave. Preserving the DK-vs-refusal distinction beyond that (for
example via
[`haven::tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html))
remains follow-up work (review C9).

------------------------------------------------------------------------

## 1. cd10c version pair: the recipe’s premise is false, and the current behavior is confirmed corruption

|                     | `cd10c_EN_1.0p.sav` | `cd10c_EN_1.1p.sav` |
|---------------------|---------------------|---------------------|
| rows                | 3,626               | 3,626               |
| columns             | 85                  | 84                  |
| unique `nomem_encr` | 3,626               | 3,626               |

Respondent overlap between the two files: **3,626 of 3,626 (100
percent)**; the id sets are identical, union 3,626. All 83 shared
substantive columns are value-identical row by row. The only difference
is that 1.1p **removes** `cd10c059`, the open-text item “Where is this
dwelling located (place and country)?”, a classic disclosure-control
redaction (the 1.1 codebook still documents the variable, consistent
with a post-release data-only removal).

So `cd10c_EN_1.1p` is a superseding re-release, not a supplemental
sample. The recipe’s `aux_note` (“stack both; zero respondent overlap
(anomaly 9)”) is empirically wrong for these files, and the engine’s
current fallback behavior, which loads and `bind_rows()`s both,
produces:

1.  **7,252 rows for wave cd10c** instead of 3,626: every respondent
    duplicated, propagating multiplicatively into
    [`merge_liss_panel()`](https://siardv.github.io/lissr/reference/merge_liss_panel.md)
    joins.
2.  **Resurrection of the redacted location field** on the 1.0p half of
    the rows, which is a privacy regression relative to what the data
    provider intended the current release to contain.

Recommended recipe change (replaces the stacking design):

``` yaml
# cd_merge_recipe.yml, wave_index entry for cd10c
- id: cd10c
  year: 2010
  file_pattern: "cd10c_EN_1.1p*"   # 1.1p supersedes 1.0p (059 redacted); do not stack
  notes: >
    1.0p and 1.1p contain the identical 3626 respondents; 1.1p removes the
    open-text dwelling-location item cd10c059 (disclosure control). load 1.1p only.
```

Delete `aux_files`/`aux_note` and boundary rule `B01_cd10c_stack`, and
re-point `CHK06`/`CHK07` at the executable `uniqueness` check type so
the guarantee is enforced rather than asserted. The engine-side
version-disambiguation logic from review C8 remains the systemic fix;
this makes cd correct even before that lands. Users should also consider
deleting `cd10c_EN_1.0p.sav` from the data directory, since it is
superseded and contains the redacted field.

------------------------------------------------------------------------

## 2. cr religion crosswalks: the mapping tables are correct; the execution path corrupts them

**Mapping validation.** For each era I read the actual value labels from
the `.sav` (corroborated against the codebooks) and checked every code
assignment in `HR10`/`HR11`/`HR12` against the harmonized 1-13 scheme
documented in the recipe. Result: **all 40 code assignments across the
three eras are semantically correct**, including the judgment calls
(era-1 codes 5 “Dutch Reformed” and 6 “Gereformeerd” to 3 “Reformed
family”; era-1 14 to 13; era-2’s reassigned 5-12 block; era-3 codes 3-6
folding into 3). The era-2 bridge variable `cr14g133` exists with the
expected Dutch Reformed / Gereformeerd / No / DK labels. One
documentation nit: `HR12` describes a “16-value instrument”, but
`cr19l144` carries 14 substantive codes plus -9 DK.

**Execution defects quantified on real data.** I replayed the engine’s
actual sequential recode loop against the observed distributions
(sentinels pre-removed, mirroring the HR01-HR03 rule order) and compared
it with correct snapshot semantics:

| wave (era) | answered | misclassified by sequential engine | rate |
|----|----|----|----|
| cr08a (era 1, HR10) | 2,992 | 86 | 2.9% |
| cr14g (era 2, HR11) | 2,222 | 312 | 14.0% |
| cr19l (era 3, HR12) | 1,417 | 0 | map order coincidentally safe |

The misclassifications are not noise; they are systematic category
swaps:

- cr08a: all 73 Evangelical/Pentecostal respondents end up coded
  **Reformed family**; all 9 Eastern Orthodox end up **Reformed
  family**; 4 “other Eastern religion” end up **Judaism**.
- cr14g: 211 “other Christian” respondents cascade to **other
  non-Christian religion**, 64 Evangelicals and 14 Hindus and 9 Orthodox
  likewise cascade to 13; 6 Buddhists and 6 Jews end up coded **Islam**.

These are single-wave counts; era 1 spans six waves and era 2 spans
five, so the full-panel damage scales accordingly. And to restate the
current shipped state: because the rules use `suffix:` (unread) instead
of `suffixes:`, **neither the correct nor the corrupted recode runs
today**; the merged column mixes three incompatible coding schemes raw.
The fix order from the review is therefore mandatory: land the
snapshot-based recode in the engine first, then this recipe patch:

``` yaml
# cr_merge_recipe.yml: HR10, HR11, HR12, HR20, HR21
# change the scoping key on each rule
suffixes: ["013"]     # was: suffix: "013"   (HR12: suffixes: ["144"])
```

**Sentinel rules validated.** `HR01` (999) and `HR02` (99) have real
work in the early eras: cr08a carries 999 in 3 columns (32 cells) and 99
in 14 columns (673 cells), every one DK-labelled at that code; cr19l
carries -9 in 10 columns (454 cells), all DK-labelled, validating
`HR03`. One collateral hit: in cr14g, `HR02`’s all-numeric scope also
catches `cr14g120` “Duration in seconds”, where 99 occurs 12 times as a
legitimate duration. See section 3 for the general pattern.

------------------------------------------------------------------------

## 3. cp08a: the over-application mechanism confirmed, with an exact blast radius

With user-missing preserved, cp08a contains 237 cells equal to 999,
distributed as:

- **234 cells on the three intended items** `cp08a010/011/019` (each
  DK-labelled at 999), so the recipe’s *targeting intent* is exactly
  right, and
- **3 cells on `cp08a193` “Duration in seconds”**, which has no value
  labels at all: three respondents who took 999 seconds.

Because the engine ignores `items:` and falls back to `all_numeric`, the
rule as shipped targets all 237 cells; under the 1.0.0 default read the
234 declared cells were already NA at read, so the recode’s only real
effect was destroying the 3 legitimate (undeclared) duration values, and
under a code-preserving read the unscoped version would still destroy
them. Small in this wave, but it is categorical proof of the mechanism,
and the same pattern recurs independently in cr14g (12 duration cells at
99) and plausibly in every module wave that pairs unscoped sentinel
recodes with paradata. Two recommendations beyond the `items` to
`suffixes` key fix:

1.  As a defensive default, exclude duration/paradata columns from any
    `all_numeric` sentinel scope (a `scope_exclude_pattern` or a
    built-in exclusion for columns labelled “Duration in seconds”).
2.  The more principled scoping is code-anchored label matching: recode
    code X to NA only in columns whose (stashed) value labels define
    code X as DK/refusal. cp08a, cr08a, cr14g, cr19l, and cv08a all show
    a perfect separation under that rule (every DK-labelled occurrence
    is intended, every unlabelled one is collateral).

``` yaml
# cp_merge_recipe.yml: A1_dk_999_to_na and A1_dk_neg9_to_na
suffixes: ["010", "011", "019"]   # was: items: ['010', '011', 019]; note the quoted "019"
```

------------------------------------------------------------------------

## 4. cs08a: the explicit DK suffix list the inert rule needs

Exactly **3 variables** in cs08a carry a “don’t know” label at code 999:
suffixes **“001”, “002”, “283”**, with **211 observed 999 cells** among
them, which the inert `A1_dk_recode` never converts to the module’s
unified -9 convention (under the 1.0.0 default read these declared cells
were silently NA’d instead, collapsing DK into item nonresponse; under a
code-preserving read the rule performs the documented 999 to -9
conversion). No 999 occurs anywhere else in the file, so the enumerated
form is complete for this wave. An important caveat for anyone
implementing the label-regex design instead: 44 other cs08a variables
have a “don’t know” label at *substantive* codes (4, 6, 8, 9, 10, 14, as
a scale category), so matching on the label text alone would over-fire
badly; the match must be anchored on the sentinel code. Ready-to-paste
replacement (wave scoping unchanged):

``` yaml
# cs_merge_recipe.yml: A1_dk_recode
- rule_id: A1_dk_recode
  anomaly_ref: A-01
  description: >
    recode 999 -> -9 on the pre-cs20m items whose value labels define 999 as
    don't know (verified against cs08a: suffixes 001, 002, 283). post-cs20m
    waves already use -9. later pre-cs20m waves should be re-verified the
    same way before extending this list.
  action: value_recode
  waves: [cs08a, cs09b, cs10c, cs11d, cs12e, cs13f, cs14g, cs15h, cs16i, cs17j, cs18k, cs19l]
  suffixes: ["001", "002", "283"]
  from_value: 999
  to_value: -9
  log: true
```

I verified only cs08a; the other eleven pre-cs20m waves should get the
same code-anchored scan before the list is declared final (the script
pattern in section 8 does it in a few lines per wave).

------------------------------------------------------------------------

## 5. Sentinel regimes: the recipes’ wave-scoped claims are empirically confirmed

Observed sentinel cells per file, read with user-missing values
preserved (`pyreadstat user_missing = TRUE`, equivalent to
`haven::read_sav(user_na = TRUE)`; see the corrected methodological
note):

| file | observed sentinel codes (code: cells) |
|----|----|
| ci08a | 9999999999: 13,444; 9999999998: 6,729; 9999: 37; 999: 1,446; 99: 2,386 |
| ci20m | -9: 7,026; -8: 3,590; -9999999999: 5,778; -9999999998: 1,221; 999: 6; 99: 23 |
| cv08a | 999: 33,278; 99: 1,147; 9999: 1 |
| cv20l | -9: 39,344; -8: 138; 99: 335; 999: 1 |
| ca08a | 99999999999: 1,157; 99999999998: 1,757; 9999999999: 1,806; 9999999998: 1,452; 999: 1,284; 99: 28; 9999: 2 |
| ca22h | -9999999999: 1,409; -9999999998: 1,872; -9: 1,101; -8: 1,043; 9999: 5,714; 999: 838; 99: 34 |

Every regime claim checks out: ci’s early positive and late negative
regimes; ca’s meta note that the positive recode must cover **both** the
10-digit and 11-digit widths (all four present in ca08a); and ca’s A-01
“sentinel-encoding reversal” with large-negative amount codes appearing
exactly on the ca22h side. Residuals worth a follow-up when those
recipes are next touched: ci20m still carries 999 x6 and 99 x23, and
cv20l carries 99 x335, which should be checked against the respective
exclusion lists to confirm they are substantive (durations again, most
likely) rather than missed DK codes.

**cv HR01 target validation.** All six suffixes the rule names (008,
053, 102, 103, 104, 105) exist in cv08a, each is labelled “I dont know”
at 99, and their observed 99 counts (20 + 47 + 200 + 236 + 103 + 541 =
1,147) account for **every** 99 in the file. The rule’s targeting is
exactly right; only its payload key (`recode:` instead of
`mapping:`/`codes:`) keeps it from running. Correction on the shipped
consequence: because these 99s are declared user-missing and the 1.0.0
engine read with haven’s default, they were converted to NA at read
rather than shipped raw, silently and without a log entry. The rule’s
job under a code-preserving read (1.1.0) is to perform that conversion
explicitly and auditably, and the closure property means activating it
introduces zero collateral in this wave.

**ci naming convention.** ci08a columns are `ci08a001`-style; after
prefix stripping they become `s001`-style. The ci recipe’s `Q066`-style
targets resolve through `find_col()`’s q-strip fallback (`Q066` to bare
`066` to `s066`), so that resolution ladder is confirmed to work against
the real files.

------------------------------------------------------------------------

## 6. New-wave onboarding: measured diffs and drafted `wave_index` entries

Real structural diffs of each uncovered wave against its predecessor
(suffix sets after prefix stripping):

| new wave | rows x cols | vs predecessor | fieldwork (`_m`) | notes |
|----|----|----|----|----|
| ch25r | 4,671 x 244 | +29 suffixes (278-306), -0 | 2025-11 | one new contiguous question block |
| cp25q | 4,970 x 159 | +0, -0 | 2025-05 | structurally identical to cp24p; clean onboarding |
| cs25r | 4,540 x 450 | +0, -0 | 2025-10 | structurally identical to cs24q |
| cv26r | 5,100 x 213 | +5 (221, 256, 353, 354, 355), -5 (306, 341, 343, 344, 345) | no `_m` variable | needs boundary review before pooling |

All four lack `nohouse_encr`, consistent with the README’s household-id
note. Drafted entries (extension-agnostic patterns; module-specific
extras inherited from each recipe’s last entry, to be reviewed against
the new codebooks):

``` yaml
# ch_merge_recipe.yml
- id: ch25r
  year: 2025
  file_pattern: "ch25r_*"
  notes: "new item block, suffixes 278-306 (29 items); fieldwork 2025-11; no nohouse_encr"
  role_map: *ch_role_map_latest   # inherit ch24q role_map; re-verify 262/266 anchors

# cp_merge_recipe.yml
- id: cp25q
  year: 2025
  file_pattern: "cp25q_*"
  notes: "structurally identical to cp24p (159 vars); fieldwork 2025-05"
  order: 17
  label: "Wave 17"
  n_vars_expected: 159
  period: B

# cs_merge_recipe.yml
- id: cs25r
  year: 2025
  file_pattern: "cs25r_*"
  notes: "structurally identical to cs24q; fieldwork 2025-10"
  fieldwork_var: cs25r_m
  period: post_reorganization

# cv_merge_recipe.yml
- id: cv26r
  year: 2026
  file_pattern: "cv26r_*"
  notes: >
    suffix changes vs cv25q: added 221, 256, 353-355; removed 306, 341, 343-345.
    review against the cv26r codebook for renumbering vs new content before pooling.
    no _m fieldwork variable (absent in cv since at least cv20l).
  era: 5
  file_version: "1.0"
  admin_structure: three_part
  dk_scheme: negative
  party_scheme: 4          # confirm against cv26r codebook
  vote_actual_suffix: "307"
  vote_hypo_suffix: "308"
  has_nohouse: false
```

Onboarding is not only `wave_index`: `covered_waves` needs the four ids
appended, and the wave-scoped rule lists that end at the predecessor
need extending case by case. The complete candidate sets, from a
mechanical scan of each recipe:

- ch (rules/checks whose wave lists include ch24q): V09-V15, B01-B03,
  B07-B12, CHK01, CHK02, CHK05.
- cp: A1_dk_neg9_to_na, A9_paradata_anchor_periods,
  V04_structural_na_post_cp19k, V14_item135_period_b_range.
- cs: A2_hobby_freq_flag, A3_org_no_connection, A4a/A4b/A4d stem rules,
  A6b_stem002_dropped, DV01_instrument_period, V04, V08, V09.
- cv: HR02_dk_negative, HR04_ref_negative, BR01-BR03, BR06, DV01_wave.

Each is a judgment call (does the condition persist into the new wave?),
which is exactly what the codebooks in the bundle’s block 06 are for;
the diffs above say cp25q and cs25r are almost certainly “extend
everything”, ch25r is “extend everything plus decide how to handle
278-306”, and cv26r needs the removed-suffix review first.

------------------------------------------------------------------------

## 7. The `fieldwork_ym` auto-derive: convention confirmed, with one module exception

The `{wave_id}_m` variable exists as a numeric `yyyymm` in every sampled
file of cd, cr, cp, cs, ca, ci, and ch (for example `cd10c_m` = 201006,
`ch25r_m` = 202511). It is **absent from cv** in every sampled recent
wave (cv20l, cv25q, cv26r; cv08a still had it, 200712). Consequences:
the engine’s automatic `fieldwork_ym` derivation silently yields nothing
for recent cv waves, and the cv recipe’s `DV09_fieldwork_month` has an
empty `source`, so it currently materializes as an all-NA column. Since
the engine already supports the `wave_values` mechanism, the clean fix
is a per-wave constant map in DV09 (fieldwork months are known per wave
and could even be harvested from
[`liss_blueprint()`](https://siardv.github.io/lissr/reference/liss_blueprint.md)),
plus upgrading `derive_fieldwork_month` from copy-only to an integer
`yyyymm` parse as the action vocabulary already promises.

------------------------------------------------------------------------

## 8. Consolidated deltas to the original review

1.  **C8 upgraded from design fragility to confirmed active
    corruption**: with this archive, the shipped cd configuration
    duplicates all 3,626 wave-10 respondents and resurrects a redacted
    open-text location field. Section 1’s recipe patch fixes cd
    immediately; the engine-side version logic remains the systemic fix.
    This is now unambiguously priority 1 alongside the cr pair.
2.  **C1/C2 quantified**: 86 (2.9%) and 312 (14.0%) misclassified
    respondents per wave in eras 1 and 2 if the key is fixed without the
    snapshot fix; zero in era 3 by luck of map ordering. The mapping
    tables themselves are fully validated as correct, so the entire
    defect is mechanical and cheap to fix.
3.  **C4/C5 quantified and bounded**: cp08a collateral is exactly 3
    duration cells (with 234 intended); cs08a’s inert recode leaves its
    211 DK cells to silent read-time masking instead of the documented
    -9 convention; cv08a’s inert HR01 likewise leaves 1,147 DK cells to
    unlogged masking. A new cross-cutting recommendation emerges:
    sentinel scopes should exclude paradata durations or, better, anchor
    on the sentinel code’s own value label. See the corrected
    methodological note for the read-semantics nuance that revises the
    earlier “raw codes ship into output” phrasing.
4.  **New finding: user-missing declarations are destroyed** by the
    `to_numeric` plus `write_sav` path; fold their preservation into the
    C9 label round-trip.
5.  **Recipes’ domain knowledge holds up**: everywhere the checks could
    bite, the *declarative content* (mappings, wave scoping, sentinel
    regimes, target suffixes) proved correct against the data and
    codebooks; the failures are concentrated in the execution layer
    (unread keys, sequential recode, unimplemented check types,
    accidental file matching). That is encouraging: the expensive
    research work is sound, and the fixes are engineering.

Remaining unverified with this bundle: the other era-1/era-2 cr waves
(cr09b-cr13f, cr15h-cr18k) for per-wave misclassification counts, the
pre-cs20m waves beyond cs08a for the DK suffix list, cp’s -9 era on the
intended items, and the provenance of the ci20m/cv20l residual 99/999
cells. Each follows the same few-line scan pattern used here and can be
run over the full archive locally once the fixes land, ideally as a
`verify_recipe_against_data()` helper in the package itself, which would
institutionalize exactly this class of check.
