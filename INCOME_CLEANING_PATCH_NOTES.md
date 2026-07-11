# lissr 1.2.0 income-cleaning patch notes

Companion documents: `INCOME_CLEANING_DESIGN.md` (architecture, ruleset
schema, rule catalog, candidate selection, output contract, and the full
provenance mapping to the legacy analysis scripts) and the
`# lissr 1.2.0` section of `NEWS.md` (user-facing changelog). This file
maps the patch to the repository, lists every deliberate deviation from
the source cleaning logic together with its regression test, and
describes how to apply and verify the patch.

Verification status: the full package test suite passes in
installed-package context with the patch applied: 74 test blocks, 208
passing expectations, 0 failures, 6 skips (all pre-existing environment
gates: the empirical blocks behind `LISSR_VERIFICATION_DIR`, one CRAN
skip, one keyring path), and the one pre-existing intentional warning.
The 30 baseline 1.1.0 blocks pass unchanged next to the 44 new blocks in
`tests/testthat/test-clean-income.R`. A seeded end-to-end smoke run
(`inst/scripts/verification/income_cleaning_smoke.R`; 400 households,
2,168 person-waves, 158 planted errors of seven types) recovers 100
percent of every planted type, modifies 0 of 2,010 clean cells, and
completes in about half a second; flag-mode proposals are byte-identical
to correct-mode output, and the ledger covers exactly the changed cells.
`R CMD check` reports no code, documentation, example, or test problems;
the remaining warning and notes are container artifacts (locale, absent
suggested packages) documented in the design document.

## Changed files

    M  DESCRIPTION                                   version 1.2.0, Description sentence
    M  NAMESPACE                                     5 new exports, 3 S3 methods
    M  NEWS.md                                       1.2.0 changelog
    M  .Rbuildignore                                 ignore the income-cleaning repo docs
    A  R/liss_clean_executors.R                      numeric kernels (pure base R)
    A  R/liss_clean_income.R                         ruleset loader/validator, engine, ledger, report, equivalisation
    A  inst/cleaning/income_cleaning_rules.yml       declarative ruleset (schema 1.0.0, 24 rules)
    A  tests/testthat/test-clean-income.R            44-block regression suite
    A  man/liss_clean_income.Rd                      reference docs (roxygen-output style)
    A  man/liss_cleaning_ruleset.Rd                  reference docs
    A  man/validate_cleaning_ruleset.Rd              reference docs
    A  man/liss_cleaning_report.Rd                   reference docs
    A  man/liss_equivalise_income.Rd                 reference docs
    A  vignettes/income-cleaning.Rmd                 workflow vignette (chunks eval = FALSE)
    A  inst/scripts/verification/income_cleaning_smoke.R   seeded end-to-end verification
    A  INCOME_CLEANING_DESIGN.md                     architecture and provenance (build-ignored)
    A  INCOME_CLEANING_PATCH_NOTES.md                this file (build-ignored)

`R/liss_merge_engine.R`, `R/liss_executors.R`, and every recipe, schema,
and existing test file are byte-identical to 1.1.0; the patch is purely
additive apart from the four modified metadata files above. The five new
Rd files are hand-authored in roxygen output style and match the roxygen
comments in the sources, which remain the source of truth; running
`roxygen2::roxygenise()` regenerates them (and, as with 1.1.0, may
reflow unrelated Rd files cosmetically under roxygen 7.3.1).

## Deviations from the legacy cleaning logic, defect by defect

The framework consolidates the income-cleaning block of
`02-income-cleaning.R` and the inlined copy in the Big Five project’s
`script.R`. Eleven behaviors were deliberately changed; each is
documented in full in `INCOME_CLEANING_DESIGN.md` section 8 and pinned
by a test named below.

**1. Magnitude-vector misalignment.** `get_power10()` returned a
shortened vector that the caller wrote back over all non-NA positions,
so a single zero income silently misaligned every magnitude after it.
`power10_magnitude()` returns a full-length vector with NA at invalid
positions. Test: “power10_magnitude returns full-length output with NA
at invalid positions”.

**2. Donor self-contamination.** `similar_cases()` searched the full
frame including the row under correction, so a flagged value could
donate to itself. `donor_pool_value()` always excludes the target row.
Test: “donor_pool_value narrows hierarchically and never self-donates”.

**3. Regex over-matching.** Income columns were located with
`grep("net|brut|nethh", ...)`, which also matches unrelated names.
Resolution is now by explicit name and alias lists in the ruleset’s
`variables` mapping. Tests: “the target resolves through its alias and
names the output columns”; the mapping is overridable via the
`variables` argument.

**4. Silent sign coercion.** A blanket
[`abs()`](https://rdrr.io/r/base/MathFun.html) over income-like columns
left no trace and mutated context columns. Rule P03 rectifies the target
only and ledgers every flip; personal-income context is compared in
absolute value without being modified in the output. Test: “P03
rectifies a sign-entry error and ledgers the flip”.

**5. Selection contradicted its comment.** For bracket violations the
legacy code promised the largest deviation but took `err_idx[1]`;
`bound_deviation_ratio()` ranks violations as documented. Tests: “bound
deviation_ratio ranks violations”, “D07 pulls a bracket violation back
inside the reported bounds”.

**6. Row-order dependence.** Households were processed in file order;
the engine sorts each group by the wave variable and a shuffled-input
test asserts order independence. Test: “cleaning is deterministic and
row-order independent within households”.

**7. Sentinel bookkeeping ambiguity.** The legacy `outlier` column
stored the original value with 0 meaning untouched, so a genuinely zero
original was indistinguishable from no correction. The typed 18-column
ledger and the `<target>_clean_status` column remove sentinel semantics
entirely. Test: “the observed column preserves the input and the ledger
covers every change”.

**8. Dead code.** `hh_min_bound`/`hh_max_bound` were computed and capped
but never used; they are not reproduced (documented in the design
document; nothing to test).

**9. Dependency removal.** `imputeTS::na_ma(weighting = "linear")` is
reimplemented natively in `wma_impute_at()`, including window widening,
so the framework adds no dependency beyond what lissr already imports.
Test: “wma_impute_at matches linear weighting, widens, and needs
support”.

**10. Residual sentinel guard.** Data of other provenances than a lissr
1.1.0 merge (or reads with `user_na = TRUE`) can still carry
9999999998/9999999999 as values; P06 honors declared haven
`na_values`/`na_range` first and the configured codes as a fallback,
before any detector can mistake a code for an income. Test: “P06 sweeps
configured sentinel codes and declared SPSS user-missing values”.

**11. New: a scale guard on the extreme-z rule.** The legacy CRITERION 5
gated on nothing but `|z| > 3`. On the seeded panel the ungated rule
rewrote 12.09 percent of clean cells, because a tiny MAD in a tightly
clustered household lets ordinary variation exceed the threshold. D10
now additionally requires the flagged value to deviate from the
household median of the other waves by at least `min_relative_deviation`
(default 0.30); with the gate the same panel shows 0.00 percent false
positives and unchanged 100 percent recall. Tests: “D10 catches an
extreme modified z-score in a three-wave household”, “D10’s
relative-deviation gate spares tight households with mild dips”.

One behavior was kept although it may look surprising: whenever a
household offers at least one other observed wave, the household-median
candidate is at distance zero from the selection anchor and therefore
wins. The legacy selection had the same fixed point. The richer
candidate set matters exactly when bracket bounds exclude the median,
which is when midpoints, rescalings, smoothing, and donors decide the
outcome; the ledger records the full candidate set either way.

## How to apply

The bundle ships `apply_income_cleaning_1.2.0.sh` next to a `payload/`
directory that mirrors the repository layout. The script is a dry run by
default and writes nothing without `--execute`.

    ./apply_income_cleaning_1.2.0.sh --repo /path/to/lissr                    # plan only
    ./apply_income_cleaning_1.2.0.sh --repo /path/to/lissr --execute --check  # branch, copy, test, commit
    ./apply_income_cleaning_1.2.0.sh --repo /path/to/lissr --execute --check --push

Preflight checks (all must pass before anything is written): git
identity configured (`user.name` and `user.email`), repository on `main`
with a clean worktree, `origin/main` fetched and identical to local
`main`, and a complete payload. `--execute` creates the branch
`feature/income-cleaning-1.2.0`, copies the files, stages exactly the
paths in the changed-files block, and commits; `--check` installs the
patched tree into a temporary library and runs the full test suite
between staging and commit, aborting before the commit if anything
fails; `--push` pushes the branch and leaves merging to you. The script
never commits to `main`.

Manual alternative: copy the contents of `payload/` over the clone root,
then

    git checkout -b feature/income-cleaning-1.2.0
    git add DESCRIPTION NAMESPACE NEWS.md .Rbuildignore \
      R/liss_clean_executors.R R/liss_clean_income.R \
      inst/cleaning/income_cleaning_rules.yml \
      tests/testthat/test-clean-income.R man/liss_clean_income.Rd \
      man/liss_cleaning_ruleset.Rd man/validate_cleaning_ruleset.Rd \
      man/liss_cleaning_report.Rd man/liss_equivalise_income.Rd \
      vignettes/income-cleaning.Rmd \
      inst/scripts/verification/income_cleaning_smoke.R \
      INCOME_CLEANING_DESIGN.md INCOME_CLEANING_PATCH_NOTES.md

To verify after applying, from the repository root:

    R CMD INSTALL --no-docs .
    Rscript -e 'library(lissr); testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'
    Rscript inst/scripts/verification/income_cleaning_smoke.R   # artifacts to LISSR_SMOKE_DIR or a tempdir

Expected: 74 blocks, 0 failures, 6 environment skips; the smoke run
prints 158/158 recovered, 0 false positives, and writes the report,
decision ledger, and JSONL log.

## Compatibility

The merge pipeline is untouched:
[`merge_liss_module()`](https://siardv.github.io/lissr/reference/merge_liss_module.md),
[`merge_liss_modules()`](https://siardv.github.io/lissr/reference/merge_liss_modules.md),
[`merge_liss_panel()`](https://siardv.github.io/lissr/reference/merge_liss_panel.md),
the recipes, the schema, and all 1.1.0 behavior are byte-identical,
which `lissr_1.2.0_income_cleaning.diff` makes checkable at a glance. No
new package dependencies: temporal smoothing and all numeric kernels are
base R, and yaml, cli, dplyr, tibble, jsonlite, and haven were already
imports. The NAMESPACE changes are additive (five exports, three S3
methods). Cleaned output is guarded against accidental double cleaning:
a `<target>_observed` column aborts a second run, and a rerun after
deliberately stripping the audit columns reaches a steady state on
already-clean data (both pinned by “re-cleaning is guarded, and a
stripped re-run reaches a steady state”).
[`liss_clean_income()`](https://siardv.github.io/lissr/reference/liss_clean_income.md)
accepts a
[`merge_liss_module()`](https://siardv.github.io/lissr/reference/merge_liss_module.md)
result directly, reads haven-labelled columns, and preserves the
target’s variable label on the cleaned column.
