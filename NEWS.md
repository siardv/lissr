# lissr 1.2.1

Correctness patch: three guarantees the package documents are now enforced by
the engine. No recipe format or output format changes.

## Engine hardening

* Recipe `condition` expressions no longer pass through `eval(parse())` with
  `enclos = baseenv()`. Conditions are parsed, their syntax tree is checked
  against a whitelist grammar (column references, literals, comparison and
  logical operators, `%in%`, `is.na()`, `c()`, unary sign), and evaluation is
  enclosed in a sandbox whose parent is the empty environment. A condition
  such as `system("...")` is rejected before evaluation and the check reports
  "condition not evaluable"; recipes are declarative data again.
  `liss_select()` wave input is parsed by a regex-guarded expander accepting
  the `1:5` and `1,3,7` forms instead of being evaluated.
* Rule execution is atomic on failure. Every executor snapshots the frame and
  the log before its dispatch; on error the rule rolls back to the snapshot
  and contributes exactly one `ERROR:` entry, so the JSONL log now means a
  rule either fully applied or did nothing. The `<<-` accumulation in the
  error handlers is retired.
* `merge_liss_panel()` asserts per module that inputs are unique on the join
  keys (naming the module and the duplicate count), performs every join with
  `relationship = "one-to-one"`, attaches shared columns with a left join so
  metadata can never add rows, and coalesces `shared_cols` (by default
  `nohouse_encr`) across modules in list order instead of taking them from
  the first module only, warning when modules disagree on a key.

## Tests

* A synthetic end-to-end module merge now runs everywhere including CI: two
  generated `.sav` waves pass through a snapshot recode, a boundary flag, a
  derived variable, and uniqueness plus condition-gated na_rate checks, with
  assertions on the merged frame, the JSONL log, the text report, and the
  summary artifact.
* Kernel unit tests cover `crosswalk_map`, `crosswalk_map_scheme`,
  `crosswalk_coverage`, and `dv_aggregate`.
* The recipe-load test covers all ten bundled recipes (previously eight,
  omitting ca and cr) and fails on any warning outside the known
  unrecognized-rule-key class.
* Regression tests pin the restricted condition grammar, the wave-input
  parser, atomic rollback, the duplicate-key guard, and the shared-column
  coalescing.

## Dependencies

* `dplyr (>= 1.1.0)` is now the declared minimum (the `relationship`
  argument of the join functions).

# lissr 1.2.0

Feature release: a rule-driven income-cleaning framework for merged LISS
data. Every behavior below is exercised by the regression tests in
`tests/testthat/test-clean-income.R` and by a seeded end-to-end smoke run
(`inst/scripts/verification/income_cleaning_smoke.R`); the architecture,
rule catalog, and the mapping to the legacy analysis scripts live in the
repository's `INCOME_CLEANING_DESIGN.md`.

## Income cleaning

* New `liss_clean_income()` detects, evaluates, and corrects implausible
  household-income values under a declarative YAML ruleset
  (`inst/cleaning/income_cleaning_rules.yml`, schema 1.0.0) of 24 rules:
  six preparation rules (P01-P06), eleven detectors (D01-D11), six
  correction-candidate generators (C01-C06), and one finalizer (F01).
  Rules dispatch by action, evaluate in ruleset order, and every rule
  carries a description, rationale, parameters, and literature references
  that the generated report reproduces.
* Full decision transparency. Original values are preserved unchanged in
  `<target>_observed`; every modified cell is marked in
  `<target>_clean_status` with its final action and rule; and every
  decision, applied or proposed, lands in an 18-column typed ledger with
  the responsible rule, the evidence, the admissible candidate set with
  sources, the anchor, the valid range, and a plain-language
  justification. `liss_cleaning_report()` renders the methodology directly
  from the ruleset plus a decision appendix, and writes the ledger as CSV
  and an engine-shaped JSONL audit log.
* Three modes. `correct` applies corrections; `flag` is a true dry run
  whose `<target>_proposed` column is identical to what `correct` would
  write while the data remain untouched; `na_only` voids detected cells
  without imputing. Re-running on already-cleaned data aborts (the
  `*_observed` column acts as a double-cleaning guard).
* Researcher control. `income_cap`, `min_income`, `disable`,
  `enable_only`, per-rule `params`, a `variables` mapping, and custom
  ruleset files are honored and recorded in the run metadata and the
  report, so a reviewer can reproduce any configured run from its report
  alone.
* Seeded smoke evidence: on 400 synthetic households (2,168 rows) with
  158 planted errors across seven families, recovery is 100 percent in
  every family (65/65 decimal shifts, 33/33 extra zeros, 9/9 cap
  blowouts, 22/22 personal-income echoes, 11/11 tiny junk values, 11/11
  sign flips, 7/7 residual sentinels), 0 of 2,010 clean cells are
  falsely modified, 83 percent of scale corrections land within 10
  percent of the true value (median relative error 3.8 percent, at the
  simulation's noise floor), and the run completes in about half a
  second.
* The modified-z detector (D10) gained a `min_relative_deviation` gate
  (default 0.3) after the smoke run exposed that the ungated legacy
  criterion rewrote 12.09 percent of clean cells in tight households,
  where a tiny MAD inflates the z-score of ordinary variation. The gate
  removes every false positive with recall unchanged; the tight-household
  case is pinned in the regression suite.
* `liss_equivalise_income()` converts household income to a
  per-equivalent-adult scale (`weighted_sqrt`, matching the source
  pipelines' `stand_inc` formula, plus `oecd_modified` and `sqrt`).
* New exports: `liss_clean_income()`, `liss_cleaning_ruleset()`,
  `validate_cleaning_ruleset()`, `liss_cleaning_report()`, and
  `liss_equivalise_income()`, with print and summary methods for run
  results and rulesets. A new vignette, `income-cleaning`, walks the
  workflow.
* No new dependencies. Temporal smoothing uses a native weighted moving
  average (equivalent to `imputeTS::na_ma` with linear weighting,
  including window widening), and the numeric kernels are base R.

## Corrections to the source cleaning logic

The framework supersedes the income-cleaning blocks of the two analysis
scripts it was distilled from. Eleven behaviors were deliberately changed,
each documented in `INCOME_CLEANING_DESIGN.md` and pinned by a regression
test, among them: the donor pool no longer offers the flagged row as its
own donor; the power-of-ten kernel returns a full-length vector so zeros
and negatives cannot desynchronize magnitudes from rows; the target
variable resolves by explicit name and alias instead of a `net|brut`
pattern match that could capture personal-income columns; blanket `abs()`
on the target became the ledgered sign-rectification rule P03; bound
violations rank by deviation ratio rather than first index; households
process in wave order rather than file order; and residual SPSS
user-missing codes are swept by the declared metadata (P06) rather than
trusted to upstream reads.

## Tests

* 44 new test blocks in `tests/testthat/test-clean-income.R`: kernel
  units against hand-computed values, one fixture household per detector,
  ledger invariants, mode contracts, determinism and row-order
  independence, override paths, alias and fallback resolution, background
  attachment, report artifacts, and equivalisation.
* Full suite in installed-package context: 74 test blocks, 208 passing
  expectations, 0 failures, 6 skips (5 empirical gates behind
  `LISSR_VERIFICATION_DIR`, 1 CRAN skip).

# lissr 1.1.0

Correctness release. Every fix below was verified against real LISS Panel
files; the empirical evidence, per-wave counts, and methodology live in the
repository's `lissr-verification-report.md` and in the regression tests
under `tests/testthat/test-engine-regressions.R`.

## Merge engine

* Value recodes now use snapshot semantics. `value_recode`, `recode_to_na`,
  and `recode` masks are built against the column as it stood when the rule
  started, so overlapping maps can no longer chain. On real data the old
  sequential loop misclassified 86 of 2,992 answered respondents (2.9
  percent) in cr08a and 312 of 2,222 (14.0 percent) in cr14g once the
  religion crosswalks were made to run; the snapshot engine reproduces the
  verified target distributions exactly.
* `recode_to_na` accepts the `recode:` alias for its sentinel map and
  honors wave-scoped `exclude:` blocks (skip cells whose suffix and wave
  both match). This activates the cv module's HR01 through HR04 sentinel
  rules, which previously validated but never executed.
* Superseded-release protection. When several primary files match one
  wave's `file_pattern`, the engine ranks release versions parsed from the
  file names, keeps the highest, and warns; unrankable candidates abort.
  Auxiliary files declared via `aux_files` resolve independently of the
  primary pattern and must be disjoint from the primary file on the id
  variable; any shared respondent id aborts. A duplicate-id gate runs on
  every wave regardless. Previously all pattern matches were stacked,
  which duplicated all 3,626 cd10c respondents when both the 1.0p and the
  superseding 1.1p release were on disk, and resurrected a field the 1.1p
  release had redacted.
* `read_wave_file` reads by extension from a whitelist (.sav, .zsav, .dta,
  .csv) and aborts on anything else instead of parsing it as CSV. SPSS
  files are read with `user_na = TRUE`, so declared DK/refusal codes reach
  the recipes as values instead of being silently converted to NA at read.
* Value labels and user-missing declarations round-trip under
  `labelled_policy: to_numeric`. Metadata is stashed per wave at read time
  and restored at write time where provably safe (identical metadata
  across waves, every observed value accounted for); columns that cannot
  be restored pass through a per-wave residual sweep that converts codes
  their own wave declared user-missing to NA, honoring recipe `exclude`
  blocks as a veto. Outputs regain their value labels (75 labelled columns
  in the cv verification merge) and no DK or refusal code can leak into
  the output as a substantive value.
* Rules that resolve zero target columns write a `NO_TARGETS` entry to the
  JSONL log, so mis-keyed or mis-scoped rules are visible in the audit
  trail.
* Validation checks that name an unimplemented type report `SKIP` with
  `passed = NA` instead of PASS; summaries report `n_pass`, `n_fail`, and
  `n_skip`. New `uniqueness` check family (aliases `assert_unique`,
  `n_duplicates`).
* New `strict` argument on `merge_liss_module()` and
  `merge_liss_modules()`: failed checks of severity `error` abort before
  any output is written. Default `FALSE` preserves 1.0.0 behavior.
* Two-factor authentication returns `NULL` on failure instead of a
  half-initialized session, so `liss_login()` can no longer cache an
  unauthenticated session as logged in.

## Recipes

* cr: HR10, HR11, HR12, HR20, HR21 scoped with `suffixes` (the singular
  `suffix` key was not read, so the entire three-era religion
  harmonization silently never ran). All 40 crosswalk code assignments
  were verified against the wave value labels and codebooks. HR12's
  instrument description corrected to 14 values plus -9.
* cd: cd10c loads the superseding 1.1p release only; the stacking rule is
  retired and the overlap claim corrected (both releases contain the
  identical 3,626 respondents; 1.1p removes the redacted open-text
  dwelling-location item cd10c059). CHK06 and CHK07 re-pointed at the
  executable `uniqueness` check.
* cp: the A1 DK recodes scope with `suffixes: ["010", "011", "019"]`
  (the `items` key was not read, so the recode fell back to every numeric
  column, destroying legitimate duration paradata).
* cs: A1_dk_recode rewritten as an executable `value_recode` (999 to -9)
  on the verified DK suffixes 001, 002, and 283 for the pre-cs20m waves.
* cv: schema_version 1.1.0; HR01 through HR04 now execute through the
  engine's `recode` alias and `exclude` support (no rule changes needed
  beyond comments).
* Revised recipes carry `recipe_version: 1.1.0`.

## Documentation

* `CANONICAL_SCHEMA.md` updated to v1.1.0 (strictly additive; v1.0.0
  recipes remain valid): `recode_to_na` keys, `aux_files` contract,
  release-version disambiguation, check execution semantics, `strict`,
  and the label round-trip are specified.

# lissr 1.0.0

First public release.

* Recipe-driven merge engine. Longitudinal LISS waves are merged from
  declarative YAML recipes that conform to the canonical schema
  (`CANONICAL_SCHEMA.md`, schema version 1.0.0). A recipe captures every
  merge-relevant decision for a module: wave file patterns, variable
  harmonization, boundary handling, comparability contracts, and validation
  checks.
* Controlled action vocabulary with fail-fast validation. Recipes are
  validated before any merge runs via `validate_recipe()` (also called by
  `load_recipe()` and `merge_liss_module()`); unknown actions and malformed
  rules are rejected up front.
* Authoring-time check for unrecognized rule keys. `validate_recipe()` emits a
  non-fatal warning listing any rule-level key that the merge engine neither
  consults nor sanctions as documentation, so mis-named keys are surfaced at
  load time rather than ignored silently. The check is warning-only; every
  recipe still loads and merges unchanged. The recognized set and the
  documentation allow-list are both documented in `CANONICAL_SCHEMA.md`.
* Audit-grade JSONL logging, with a per-run summary artifact.
* Ten built-in module recipes: Assets (ca), Housing (cd), Family and
  Household (cf), Health (ch), Economic Integration (ci), Personality (cp),
  Religion and Ethnicity (cr), Culture and Sports (cs), Politics and Values
  (cv), and Work and Schooling (cw).
* Authentication against the LISS Data Archive with two-factor verification;
  credentials stored via the system keyring.
* Interactive browse, select, and download workflow (`liss_modules()`,
  `liss_wave_matrix()`, `liss_select()`, `liss_download()`).
* New-wave onboarding via `onboard_new_wave()` to extend an existing recipe
  to a newly released wave.
