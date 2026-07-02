# lissr 1.1.0 patch notes

Companion documents: `lissr-review.md` (static review, findings C1-C9 and
H1-H11) and `lissr-verification-report.md` (empirical verification against
the real LISS archive; includes a corrected methodological note on read
semantics, see below). This file maps each shipped defect to its fix and to
the empirical evidence that the fix is right.

Verification status: 10 unit assertions (harness A) plus 28 real-data
assertions (harness B) pass, and the full package test suite passes in
installed-package context: 62 passed, 0 failed, 4 skipped (3 keyring
error-path skips, 1 CRAN skip). The new regression tests live in
`tests/testthat/test-engine-regressions.R`; the empirical block activates
when `LISSR_VERIFICATION_DIR` points at the verification bundle.

## Changed files

```
M  R/liss_merge_engine.R                     engine fixes (see below)
M  R/perform_twofactor_authentication.R      2FA failure handling
M  inst/recipes/cd_merge_recipe.yml          cd10c supersession, checks
M  inst/recipes/cp_merge_recipe.yml          A1 scoping keys
M  inst/recipes/cr_merge_recipe.yml          HR10-HR21 scoping keys
M  inst/recipes/cs_merge_recipe.yml          A1_dk_recode rewritten
M  inst/recipes/cv_merge_recipe.yml          comments, schema_version 1.1.0
M  DESCRIPTION                               version 1.1.0, RoxygenNote
M  inst/schema/CANONICAL_SCHEMA.md                       schema v1.1.0 (additive)
M  NEWS.md                                   1.1.0 changelog
M  TODO.md                                   resolved and new items
M  man/merge_liss_module.Rd                  strict parameter (roxygen)
M  man/merge_liss_modules.Rd                 strict parameter (roxygen)
A  tests/testthat/test-engine-regressions.R  regression suite
A  inst/scripts/verification/harness.R              standalone unit harness
A  inst/scripts/verification/harness_b.R            standalone real-data harness
```

`R/liss_executors.R` is unchanged. Running `roxygen2::roxygenise()` locally
will additionally reflow three unrelated Rd files (link-target formatting
under roxygen 7.3.1); that is cosmetic.

## Engine fixes, finding by finding

**C1, snapshot recode semantics.** `value_recode`, `recode_to_na`, and the
`recode` branch build every mask against the column as it stood when the
rule started, so overlapping maps cannot chain. Evidence: replaying the old
sequential loop on real distributions misclassifies 86/2,992 answered
respondents (2.9 percent) in cr08a and 312/2,222 (14.0 percent) in cr14g;
under the patched engine the end-to-end cr merge reproduces the verified
era-1 and era-2 target distributions exactly (harness B1, regression test
"cr three-era harmonization"). Fix ordering mattered: the engine snapshot
landed before the cr recipe keys, because fixing the keys alone would have
converted a silent omission into silent corruption.

**C8/H1, superseded releases and file discovery.** `discover_wave_files`
now separates recipe-declared `aux_files` from primary candidates, resolves
aux declarations independently of `file_pattern` (a narrowed pattern cannot
silently drop a declaration), ranks release versions when several primaries
match (keep highest, warn with the ignored files; abort when unrankable),
and `merge_liss_module` enforces that aux rows are disjoint from the
primary on the id variable, plus an unconditional duplicate-id gate per
wave. Evidence: with both cd10c releases on disk the shipped engine
produced 7,252 rows for wave cd10c and resurrected the redacted
`cd10c059`; the patched merge yields 3,626 unique respondents, no `s059`,
and a warning naming the ignored 1.0p file when the pattern matches both
(harness A5/A6, B2). The fallback pattern is restricted to data extensions
so codebook PDFs can never be swept in, and `read_wave_file` aborts on
unknown extensions instead of parsing them as CSV (H2).

**Read semantics (new finding during patch work).** haven's default
`user_na = FALSE` converts SPSS user-defined missing codes to NA at read;
the 1.0.0 engine used that default, so declared DK/refusal codes were
silently masked before recipes ran (unlogged, DK collapsed into item
nonresponse, undeclared sentinels missed). The 1.1.0 engine reads with
`user_na = TRUE` so recipes receive the codes and convert them auditably.
This corrects the framing in an earlier revision of the verification
report; the report now carries the corrected methodological note.

**C9, label and user-missing round-trip.** Under
`labelled_policy: to_numeric` the engine stashes value labels, na_values,
na_range, and the variable label per wave at read, and at write restores
`haven::labelled_spss` where provably safe: identical metadata across all
contributing waves and every observed value accounted for by a label, a
declared missing code, or NA. Non-restorable columns pass through a
residual sweep that converts codes to NA per wave (a code declared missing
only in wave A is never swept from wave B) and honors recipe `exclude`
blocks as a veto. Restoration coerces string-typed metadata losslessly and
is wrapped so no metadata quirk can abort phase 7 (a real cr string
variable with na_values "999" exercised this). Evidence: the cv
verification merge writes 75 labelled columns, `LABEL_RESTORE` and
`NA_SWEEP` appear in the JSONL log, cv20l's excluded suffix 243 keeps all
154 eligibility -9s while no -9 survives outside the carve-outs (harness
B5/B6).

**C6, validation honesty and the strict gate.** Unimplemented check types
report SKIP with `passed = NA` instead of PASS; summaries report n_pass,
n_fail, n_skip. The `uniqueness` family (aliases `assert_unique`,
`n_duplicates`) executes a key-within-group duplicate check.
`merge_liss_module(strict = TRUE)` aborts before phase 7 on any failed
severity-error check, so no outputs are written; `merge_liss_modules`
forwards the argument (harness A7/A8 and the corresponding regression
tests).

**C3, audit trace for empty rules.** A rule that resolves zero targets
writes a `NO_TARGETS` log entry, which is exactly how the cr/cp/cs class
of mis-keyed rules would have been caught (harness A3).

**C7, two-factor authentication.** The 2FA step selects the verification
form by its "code" field, returns NULL on failure, and `liss_login` no
longer caches a failed session as authenticated.

## Recipe fixes

**cr.** HR10/HR11/HR12/HR20/HR21 scoped with `suffixes` (the singular
`suffix` was unread; the entire three-era religion harmonization never
ran). All 40 crosswalk assignments were validated against wave value
labels and codebooks before activation; HR12's description corrected to
the 14-value instrument. **cd.** cd10c loads the superseding 1.1p release
only (`file_pattern: "cd10c_EN_1.1p*"`); the stacking rule is retired to
`note_only` and CHK06/CHK07 are executable `uniqueness` checks with
severity error. **cp.** A1_dk_999_to_na and A1_dk_neg9_to_na scope with
`suffixes: ["010","011","019"]` (unread `items:` had made the recode fall
back to all numeric columns, whose only real effect was destroying the 3
undeclared duration 999s in cp08a). **cs.** A1_dk_recode is an executable
`value_recode` 999 to -9 on the verified suffixes 001/002/283 for
cs08a-cs19l; exactly the 211 verified DK cells convert (harness B4).
Remaining pre-cs20m waves should get the same code-anchored scan before
the list is declared final (TODO.md). **cv.** No rule-body changes needed:
HR01 through HR04 execute via the engine's `recode` alias and `exclude`
support; comments updated and `schema_version: 1.1.0` declared. All five
revised recipes carry `recipe_version: 1.1.0`.

Advisory unknown-key noise on the five revised recipes drops from 28 to 20
flagged rules under the same engine (the remainder are deliberate
documentation keys such as `variable_suffix_range`); the untouched five
recipes drop from the review's baseline as well because genuinely consulted
and genuinely documentary keys were added to the engine's recognized and
sanctioned sets.

## Not included, by design

Onboarding of ch25r, cp25q, cs25r, and cv26r is drafted but not applied:
extending each module's wave-scoped rule lists is a content judgment per
rule (does the condition persist into the new wave?), and cv26r changes
five suffixes that need a codebook-backed boundary review. The measured
diffs, drafted `wave_index` entries, and per-module rule lists to extend
are in `lissr-verification-report.md` section 6; TODO.md tracks it.

## Applying

Applied to this repository with `update_lissr_repo.sh` (dry run first). After pulling:

```bash
R CMD INSTALL .
# full suite including the real-data regression block:
LISSR_VERIFICATION_DIR=/path/to/lissr_verification_bundle \
  Rscript -e 'testthat::test_dir("tests/testthat")'
```

The standalone harnesses under `inst/scripts/verification/` reproduce the
before/after evidence outside the package context; they read their paths
from `LISSR_ENGINE_DIR`, `LISSR_RECIPE_DIR`, `LISSR_VERIFICATION_DIR`, and
`LISSR_ORIG_RECIPE_DIR`.
