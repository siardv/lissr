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
