# lissr 1.4.0

A nine-stage, evidence-driven overhaul of the merge engine, the bundled
recipes, the income cleaner, the download and authentication layer, and
the documentation. Every stage landed with its own regression tests and
a green `R CMD check`; the test suite grew from 448 to roughly 760
assertions. The stage-by-stage record follows the highlights.

Highlights:

* Merge engine correctness: `fieldwork_ym` is populated across the nine
  declaring modules (it was blanked by an ordering bug), cross-wave type
  harmonization is factor-safe, and every rule application leaves an
  audit trace.
* Recipe conformance, made permanent: per-action payload validation
  driven by the vocabulary registry, a check-type grammar under which
  all 99 bundled validation checks classify as 68 executable and 31
  documentary (none silently skipped), and an exported
  `audit_liss_recipes()` corpus report suitable for CI gating.
* End-to-end assurance: every bundled recipe merges a synthetic panel in
  the test suite with strict assertions; merged outputs carry provenance
  (versions, input md5 hashes, release decisions) and an explicit
  `valid_for_analysis` verdict; release-version pins and an overwrite
  guard protect outputs.
* Output-changing semantic repairs, each verified against archive
  evidence: the cf module rebuilt from a 17-wave scan (exact recode
  counts reproduced on real data), ca's `wave_year` standardized to the
  fieldwork year with the asset reference year preserved per respondent,
  cv's fieldwork months made executable, and cr's 2019 scale breaks
  expressed as era-scoped output columns.
* Income cleaning: eight policy defects repaired (anchor dispatch,
  range-fallback voiding, single-person and zero-gross gates, bracket
  mapping, household-id fallback with join diagnostics, scoped
  `enable_only`, configurable F01 disposition, equivalisation guards),
  ruleset 1.1.0, controlled-value validation, and an adversarial
  fixture suite.
* Download and authentication hardening: HTTP errors are never written
  as data, verified unzip protects sole copies, session expiry aborts
  batches, downloads stream with timeout and retries, credentials are
  never echoed, 2FA is retryable and guarded, the login probe is cheap
  and RNG-neutral, and keyring moved to Imports.
* Documentation rebuilt against reality, with drift tests that re-derive
  every factual claim from the code and recipes, and a canonical-schema
  article rendered from the packaged schema at build time.

## Stage 8: documentation overhaul

The documentation now describes the package the preceding stages built,
and a drift-test suite (`tests/testthat/test-stage8-docs.R`) re-derives
every factual claim from the code and the bundled recipes so the two
cannot diverge silently again.

* Corrected content: the Background Variables file is no longer called
  "module (CA)" anywhere (it is a separate monthly release; ca is
  Assets); every "eight core modules" claim now says ten and the batch
  examples include ca and cr; the multi-module download loop no longer
  derives directory names by truncating display names (a bug that wrote
  Health files into `data/he/`) and matches modules by name fragment
  instead of exact display strings; the longitudinal vignette's
  fabricated columns (`s002` as age, `s038` as BMI) and its undefined
  `treated` variable are replaced by real recipe-declared flag columns
  (`ch_ecig_dental_items_present`, `s020_wording_period`, and the
  cancer/alzheimer era flags) and an explicitly reader-supplied
  treatment definition; the reproducible-pipelines CI chunk installs
  lissr from GitHub instead of `install_local(".")` on a non-package
  research project.
* Settled facts: avars files carry their fieldwork period as a `wave`
  column (YYYYMM), so the merge-workflow example reads it directly
  instead of parsing filenames (the multi-module vignette already did);
  wave counts and year lists reflect the current recipes (ch has 18
  waves, ch07a-ch25r, skipping 2014; cv skips 2015 and reaches cv26r;
  cw has 15); the `str(recipe)` and `VALID_ACTIONS` listings show the
  real current output; the documented schema version is v1.1.0
  everywhere (the packaged schema's own version), with the additive
  relationship to v1.0.0 stated.
* Additions: a new "The Canonical Recipe Schema" article renders
  `inst/schema/CANONICAL_SCHEMA.md` at build time (zero duplication, no
  drift); the README gains the income-cleaning and schema articles in
  its vignette list, a directory-and-naming-contract section, the
  keyring-ships-with-lissr note, provenance/valid_for_analysis and
  `audit_liss_recipes()` coverage, and module labels matching the
  recipes; the pkgdown reference index now lists `audit_liss_recipes()`
  (its absence would have failed the site build) and the new article.
* Drift tests: no "(CA)" mislabel and no eight-module claim anywhere;
  batch lists must contain exactly the ten module codes; the action
  vocabulary and `str()` listings in custom-recipes must equal the
  installed package's reality; flag names quoted in vignettes must
  exist in the recipes; wave-count claims must match the recipes'
  wave_index lengths; the documented schema version must match the
  packaged schema. README assertions run in a source tree and skip
  under R CMD check.

## Stage 7: downloader and authentication hardening

The network layer no longer reports failure as success, no longer
destroys data on failure, and fails loudly and diagnosably when the
archive's pages drift. The I/O surface is reduced to three small shims,
and every error path is exercised offline by mocked tests
(`tests/testthat/test-stage7-network.R`).

* N1, download integrity: HTTP error responses are detected by status
  code and never written to disk as data files (a 404 page previously
  became `foo.sav` with status "ok"); ZIP archives are extracted with
  verification and the archive is deleted only after the extracted
  files exist (a corrupt zip previously triggered deletion of the only
  copy); the first session expiry aborts the remaining batch with
  status `skipped_batch_aborted` instead of failing file by file.
* N5, transfer robustness: downloads stream to disk through the
  session's cookie handle (no whole-file buffering in memory), carry a
  per-file `.timeout`, retry transient failures (curl errors and HTTP
  5xx, never deterministic 4xx) with `.retries`, and can skip files
  already present via `.skip_existing`. Partial transfers land in a
  temporary name and are renamed only after the status check.
* N2, confirmation logic: the "download everything?" prompt now
  defaults to NO, is skipped when an explicit selection tibble was
  passed, and a non-interactive full-archive request aborts with
  guidance instead of silently auto-confirming.
* N3, credential hygiene: the failed-login message no longer echoes a
  masked password (first character plus stars); an empty or unreadable
  keyring password aborts the login with a re-store hint instead of
  crashing on `strrep()`.
* N4, form discovery: a login page without the expected form or
  username/password fields aborts with a diagnosis naming what was
  found, instead of a subscript-out-of-bounds error.
* N6, content-disposition: header parsing supports the quoted,
  unquoted, and RFC 5987 `filename*=` forms (the unquoted form
  previously yielded the whole header as the file name), and every
  resulting name is sanitized (path components stripped, traversal and
  reserved characters rejected).
* N7, blueprint cache: failed module and wave pages are tallied and
  reported; a partial scrape caches with an explicit incompleteness
  warning, an empty scrape aborts and caches nothing, and an
  unreachable module index aborts immediately.
* N8, two-factor entry: the code is whitespace-normalized (email
  copy-paste), a rejected code can be retried up to three times
  without restarting the login, a kicked-back session aborts with
  guidance, and non-interactive sessions abort before prompting
  (previously an empty string was submitted silently).
* N9, `liss_is_logged_in()`: the probe is now cheap and side-effect
  free. With a cached blueprint it HEADs the first known protected
  file path (no body transfer, deterministic choice); without one it
  inspects the login page. It no longer triggers a full archive
  scrape, downloads a random data file, or mutates the RNG state.
* N11, keyring placement: `keyring` moves from Suggests to Imports; it
  was already unconditionally required for credentialed use, and the
  install-time guards pretending otherwise are gone (with them the
  three permanently-skipping guard tests).

## Stage 6: income-cleaning policy repairs (ruleset 1.1.0)

Eight policy defects in `liss_clean_income()` that could destroy or
fabricate values on real data are repaired, the ruleset validator now
enforces controlled values, and an adversarial fixture suite pins every
repair (`tests/testthat/test-stage6-cleaning.R`).

* C1: `selection.anchor` is now executed, not merely reported. The
  engine dispatches the candidate-selection statistic
  (`household_median` or `household_mean`) from the ruleset's
  `selection` block, so the generated report can no longer claim a
  method the code did not use; unknown anchors are rejected at load
  time.
* C2: the `range_midpoint` correction fallback (C06) additionally
  requires the anchor itself to lie inside the admissible range. A
  below-minimum household (e.g. 5000/5200/50000/5100 against
  `min_income` 8000) previously had its 10x entry error "corrected" to
  the range midpoint 79000, fifteen times the truth, ledgered as a
  confident correction; such cells are now voided with an explicit
  justification.
* C3: D04 (personal-income echo) fires only in households known to
  hold at least `min_household_size` (default 2) members, resolved
  from the new `variables.household_size` mapping (`aantalhh`); in a
  single-person household the echo is the expected truth, and roughly
  40 percent of Dutch households are single-person. D03 (contextual
  floor) requires a positive gross personal income instead of merely a
  finite one, so "household income near zero while gross personal is
  0" no longer voids.
* C4: bracket-code expansion (P04) classifies the category column by
  the share of nonzero finite values in 1..max_code (new `code_share`
  param, default 0.9) and maps per value. One stray sentinel no longer
  silently disables mapping for the whole column (after which raw
  codes were misread as euro bounds and D07 was disarmed); stray codes
  are warned about and treated as missing brackets, and ambiguous
  mixtures are declined loudly.
* C5: rows with a missing household id fall back to person-id grouping
  for the household stage instead of silently receiving no cleaning;
  skipped and fallback row counts appear in the summary, log, and
  report. The background join (P01) detects the background wave scale
  (yyyymm, calendar year, or annual wavenr) instead of assuming
  yyyymm, reports its match rate, and warns when nothing matches (a
  year-keyed background previously produced an all-NA join logged as
  success).
* C6: `enable_only` is scoped per ruleset section: only sections
  containing a named rule are restricted, so `enable_only = "D06"`
  isolates one detection rule while preparation, correction, and
  finalization machinery keep running (previously the detected cell
  was voided because its correction rules were disabled too).
* C7: the F01 hard cap gains a `disposition`: `void` (default, the old
  behavior), `winsorise` (over-cap values clamp to the cap;
  non-positive values still void), or `flag` (ledger only, values
  retained). Genuine top incomes exist, and unconditional voiding
  biases the right tail downward; the choice is now explicit,
  per-ruleset or per-call.
* C9: equivalisation guards: zero-adult compositions yield NA (the
  OECD-modified divisor could fall below 1), composition vectors of
  intermediate length are an error instead of silently recycling, and
  the documentation states that `aantalki` (children at home of any
  age) only approximates the OECD under-14 child definition.
* A8: `validate_cleaning_ruleset()` enforces controlled values for
  detection-rule `disposition`/`scope`/`stage`, the finalization
  disposition, the selection anchors, consensus-detector `methods`
  (with `consensus` bounded by the method count), and nested
  per-method `thresholds`.
* Audit consistency: D01's ledger `variable` field no longer renders
  as the literal "NA/..." when a bounds column is unresolved; P03, D01,
  and the F01 dispositions emit proper JSONL log entries; per-rule
  `log: false` now suppresses that rule's per-rule decision aggregation
  in the JSONL trace (the decision ledger itself is never suppressed);
  `liss_cleaning_report()` requires `output_dir` instead of writing
  three files into the working directory by default.
* Tests: three cli-rendered assertions were width-fragile (multi-word
  regexes break when cli wraps between words) and now match across
  wrap points.

## Stage 5b: output-changing recipe semantics (ca, cv, cr)

* ca `wave_index` years now mean the same thing as in every other module:
  the FIELDWORK year. They previously held the asset REFERENCE year (one
  year earlier), so `wave_year` in merged ca output shifts by +1 for all
  ten waves (ca08a 2007 -> 2008 ... ca25j 2024 -> 2025). The reference
  year is not lost: a new executable derived variable
  `asset_reference_year` (DV00, `wave_values`) carries it per respondent,
  and the recipe header documents the new semantics. ca recipe 1.2.0.
* cv DV06/DV07/DV08 deleted: they materialized all-NA shadow duplicates of
  the BR01/BR02/BR03 flag columns. DV09 (`fieldwork_month`) is
  re-expressed as an executable derivation (`fieldwork_ym` mod 100), so
  it carries real calendar months wherever a wave has a `_m` variable
  instead of being all-NA everywhere; waves without `_m` stay NA pending
  the archive-metadata backfill tracked in TODO.md. cv recipe 1.2.0.
* cr BR20/BR21/BR30 (the 2019 scale breaks in religious attendance,
  prayer frequency, and afterlife belief) are re-expressed as executable
  `split_variable` rules with explicit `output_vars`: each produces
  pre-2019 and post-2019 columns (`attendance_pre2019_8pt` /
  `attendance_post2019_6pt`, the `prayer_*` pair, and
  `afterlife_pre2019_4pt` / `afterlife_post2019_3pt`) so era-specific
  analysis never has to pool the raw stacked column across the break.
  cr recipe 1.3.0.
* The audit's nonconforming set drops from 10 to 7 (all tracked TODO
  items), and the pinned snapshot test now expects exactly those seven.

## Stage 5a: the cf module repair, verified against the full archive

The Family and Household recipe was the one module whose execution layer
failed in both directions (core missing-code rules inert, sentinel recodes
scoped to every numeric column, string-map recodes that would null items if
naively re-keyed, and a labelled policy that factorized monetary amounts).
It is rebuilt on evidence: a new scanner
(`inst/scripts/verification/cf_scan.R`) read all 17 archive waves with SPSS
user-missing values preserved, and every rule scope below is verified
against that scan (v1.4/cf_scan_results.json, 2026-07-11).

* `labelled_policy` switched from `to_factor` to `to_numeric`, aligning cf
  with the other nine modules. Under `to_factor`, partially labelled
  variables (18 to 49 per wave, including euro amounts and paradata) became
  factors: numeric recodes skipped them and `write_sav()` re-coded levels
  1..k, detaching output codes from the codebook.
* HARM-005 (999) re-keyed to its three verified DK items 166/180/181: the
  label holds in all 17 waves and NO other column is 999-labelled anywhere,
  so the legitimate 999s on durations and euro amounts are provably
  untouched. HARM-006 (9999, cf11d onward) and HARM-007 (99999,
  cf08a-cf10c) re-keyed to the ten verified monetary items; cf12e lacks six
  of them (handled by if_absent), and cf08a's suffix 008 carries no 99999
  label (its Dutch label appears in cf09b/cf10c), exactly as the recipe's
  defensive note said.
* HARM-001 (-9) rewritten from an inert module-wide sweep to
  evidence-scoped recode blocks: the imputation-flag trio 398/399/400 from
  cf10c (whose user-missing declarations are inconsistent across waves, so
  the recode is load-bearing), 535 from cf22o, and 554/555/556 in cf24q.
  HARM-004 (-8) scoped to its single verified item 535 (cf22o onward).
* HARM-002 and HARM-003 re-expressed as documentation: the evaluation-item
  label eras (boundaries verified at cf13f/cf14g and cf21n/cf22o) are
  translation artifacts on stable codes, and the school-denomination
  schemes keep their codebook codes (cf12e's "Islamitic" spelling variant
  recorded). The old string-map value_recode forms would have coerced the
  items to NA had they ever executed.
* VAR-002 (fieldwork month) and VAR-004 (completion time; the F8-to-F10
  widening lands exactly at cf20m) re-keyed and executing; VAR-003 retired,
  its cf11d type-inconsistency claim does not reproduce in any wave.
* End-to-end verification on real waves (cf08a, cf11d, cf24q): per-rule
  recode counts match the scan ground truth exactly (472 / 1,046 / 1,522 /
  143 / 112 cells), the seven duration 999s and one 999-euro amount
  survive, zero sentinels remain on targets, fieldwork_ym carries the real
  months, and the output is flagged valid_for_analysis. The audit's
  nonconforming set drops from 20 to 10, none of them cf.

## Stage 4: per-module fixtures, provenance, and release pins

* Every bundled recipe now merges a synthetic multi-wave panel end to end
  in the test suite (`tests/testthat/test-stage4-fixtures.R`). The fixture
  generator derives each module's columns from its own recipe (rule and
  check suffixes, value sets implied by value_in_set/value_range checks,
  planted values for value_present checks, and structural-absence
  declarations honored as NAs), and the assertions are strict: no rule
  rolls back, no unimplemented action is hit, every declared flag column is
  non-degenerate in the OUTPUT, no check skips, and no error-severity check
  fails.
* The fixtures immediately caught three real defects, now fixed: cp `V07`'s
  `max_waves: 16` predated the cp25q onboarding (a full-panel respondent
  can legitimately appear in 17 waves, so the check would have failed on
  real data); `value_absence` coerced forbidden values to numeric, so
  character codes (ch `CHK10`'s forbidden wave id) collapsed to NA and
  mis-matched; and the uniqueness executor's `$` lookups could be hijacked
  by partial matching (cd `CHK11`'s `scope_wave` key matched `$scope` and
  became the key column). Check-payload lookups now use exact indexing,
  and `uniqueness` accepts the singular `variable` key.
* `merge_liss_module()` gains provenance and an explicit quality verdict:
  the returned object carries `provenance` (package, recipe, and schema
  versions, recipe file md5, per-input md5 hashes, release decisions,
  strictness, timestamp) and `valid_for_analysis` (TRUE only when no
  error-severity check failed or was unevaluable and all release pins
  matched); both are written into the text report.
* Release-selection transparency: when several files match one wave, the
  ranking decision (selected, ignored, rule) is recorded in provenance and
  the report. A `wave_index` entry may pin `expected_release: "1.1p"`;
  a violated pin warns, clears `valid_for_analysis`, and aborts under
  `strict = TRUE`.
* New `overwrite` argument on `merge_liss_module()`: `FALSE` refuses to
  clobber an existing merged output (default `TRUE` preserves behavior).

## Stage 3b: per-action payload validation and the corpus audit

`action_vocabulary.yml` now carries a machine-readable payload specification
per action (`reads`: the keys the executor consults; `annotations`:
action-specific documentation keys), and `validate_recipe()` checks each
rule's keys against its own action instead of one global recognized-key
set. A key read by one action no longer passes silently on an action that
ignores it; documentary and pending actions (note_no_op, pending_spec,
stub, and the note_only/flag_only/flag_absence no-ops) are exempt by
design. The registry loads in `.onLoad()` alongside the vocabulary.

The sharper scan surfaced and fixed another round of silently inert or
misfiring rules: cr `BR70`-`BR79` presence flags never materialized
(`flag_name` on `structural_na`, which reads `flag_column`); cd `D01`'s
nohouse_encr drop was inert (singular `variable`); cs `DR01_open_text`
dropped nothing (a `stems` list the drop executor does not read; now an
executable pattern); cs checks `V02`/`V03`/`V04`/`V08` passed vacuously
(scopes written as `stem_NNN`, which double-prefixes in `find_col`); cr
`HR30` lowercased every labelled column instead of its nine targets
(`suffixes` instead of `scope`); cv `VR03`/`VR07` were re-expressed as
documentation (stacking already covers both); cv `VR04`/`VR08` and ch
`V06` were re-keyed and now execute (V06's Dutch label fix is live).

New export `audit_liss_recipes()`: a corpus conformance report (per-module
rule conformance, check classification, wave-metadata consistency,
pattern canonicality) suitable for CI gating. The regression suite pins
the audit snapshot: exactly 20 known nonconforming rules remain, every
one mapped to the planned cf repair stage, the cv party pipeline, or a
TODO item; any new nonconforming rule fails the suite. cd's five free-text
check types were re-typed to executable primitives and two to documentary
types; corpus totals are now 68 executable, 31 documentary, 0 skipped.

## Stage 3a: executable validation checks

The recipe validation layer now evaluates most of what the recipes declare.
Before this change, 70 of the 99 checks across the ten bundled recipes named
types the runner did not implement and silently reported SKIP; five recipes
had no executable recipe-level checks at all.

* Check-type aliases normalize recipe-specific names onto the canonical
  executors: the uniqueness family (`unique_key`, `no_duplicate_ids`,
  `unique_per_wave`, `assert_identifier`), the value-absence family
  (`none_equal`, `sentinel_absence`, `no_residual_sentinels`,
  `assert_no_values`, `value_absence_check`, `value_restriction` as the
  allowed-waves complement), the range family (`value_in_range`,
  `assert_range`), the NA-rate family (`na_rate_above`, `na_rate_below`,
  `not_missing`), and the structural-missingness family
  (`structural_absence`, `all_na`, `structural_na_count`,
  `missingness_check` including the expected-present-plus-NA-elsewhere
  shape).
* New primitives: `value_in_set` (shared or per-variable allowed sets,
  `allow_na`), `value_present` (a code must occur, per wave), `row_count`
  (per-wave or total bounds), `per_wave_mean` (mean bounds per wave, the
  DK-spike detector), and `wave_count` gains an `expected` exact form.
  `value_absence` accepts block payloads (`targets`/`checks` lists with
  per-block waves, columns, and codes) and exclusion lists.
* Declared diagnostics that need distributional tests or inputs unavailable
  at merge time (distribution comparisons, panel consistency, presence
  matrices, and similar) are classified as documentary: they report `DOC`
  instead of `SKIP` and are counted separately (`n_doc`).
* A `severity: error` check whose type cannot be evaluated is escalated: a
  loud warning always, and under `strict = TRUE` the merge aborts before
  writing outputs (an error-level check must be executable). The validation
  summary and return value now carry `error_skips`.
* The action vocabulary is refreshed from the installed
  `action_vocabulary.yml` in `.onLoad()`; the install-time probe that could
  resolve a previously installed version is gone. Stale vocabulary statuses
  corrected (crosswalk, conditional_label_swap, label_to_string,
  derive_combined_party, and transform are implemented; fix_label's
  canonical keys are `old_fragment`/`new_fragment`).
* Recipe corrections required to activate checks: cs `V06` re-typed from an
  empty string to `value_absence` (its block payload now executes), and cs
  `V07`'s expected wave count corrected from the stale 17 to 18 (cs25r was
  onboarded in 1.3.0).

## Stage 2: recipe re-keys and pattern normalization

Every bundled recipe now says what the engine executes. Rule intents are
unchanged; payload keys the engine never read are re-expressed in canonical
form, so seven flag/period columns that used to materialize all-NA (or under
a wrong fallback name) are now populated, one inert recode executes, and one
unfulfilled promise is documented instead of implied. Regression tests in
`tests/testthat/test-stage2-recipes.R` sweep every boundary flag rule in
every bundled recipe and assert non-degeneracy against synthetic data.

* cr: `BR01` (redesign_2019, now 0/1 via waves_pre/waves_post), `BR02`
  (instrument_phase), `BR10` (religion_coding_era), `BR60`
  (eval_wording_period), and `BR80` (fieldwork_season) re-keyed to the
  executable `eras`/`phases`/`flag_column` forms. `fieldwork_ym` is declared
  under expected_presence again now that the stage-1 engine fix makes the
  declaration safe.
* ch: `B12` re-keyed (s020_wording_period now materializes under its own
  name instead of an all-NA `B12_period`); `B06` crosswalk_rename realigned
  to `old_suffix`/`new_suffix` with `harmonized_name: h_premium_period`, so
  the ch08b premium-period item really coalesces with suffix 261; the `D02`
  drop of the historical stray `h_` column is retired to documentation.
* ci: `A-09` realigned (nine VUT/prepensioen pairs) with explicit
  harmonized columns `h_q363` through `h_q371`; `DR03` retired likewise.
* cp: `A9_paradata_anchor_periods` re-keyed; `paradata_anchor_regime` is now
  populated with its three anchor regimes.
* ca: `B02` (ca25j_redesign) and `B03` (ca20g_largeneg_review) re-keyed to
  executable forms; `H04`'s `preserve_original` promise is removed from the
  description because no copy mechanism exists yet (tracked in TODO.md).
* cw: `HR03_pension_dates` re-expressed as a wave-scoped `value_recode`
  (cw25r: 1 to 2023, 2 to 2024); previously the rule never executed and
  cw25r categorical codes pooled raw against calendar years.
* All `file_pattern`s normalized to the extension-agnostic `{wave_id}_*`
  form (85 entries across cd, cf, ci, cp, cs were `.csv`-suffixed or bare
  and only loaded through the fallback matcher); the cd10c supersession pin
  (`cd10c_EN_1.1p*`) is preserved.
* `recipe_version` bumped on all nine touched recipes;
  `CANONICAL_SCHEMA.md`'s crosswalk-alias known-limitation section replaced
  with the resolution note. Merged output gains new additive columns
  (populated flags and harmonized series); no existing column changes
  meaning.

## Stage 1: engine correctness fixes

No recipe, schema, or API changes; four behaviors
of the merge engine are corrected and pinned by regression tests in
`tests/testthat/test-stage1-fixes.R`.

* `fieldwork_ym` is now derived from the `{wave_id}_m` convention BEFORE the
  expected-presence check runs. Previously, declaring `fieldwork_ym` under
  `global.expected_presence` (as nine of the ten bundled recipes do) created
  an all-NA placeholder first, which suppressed the derivation entirely, so
  merged outputs carried an all-NA `fieldwork_ym` and a misleading
  "created as NA" warning for every wave that actually had `_m` data. The cr
  recipe's workaround note is retired in the stage-2 recipe pass.
* Cross-wave type harmonization is factor-safe. When the same column is a
  factor in one wave and numeric in another (registry archetype TD-02, and
  any module under `labelled_policy: to_factor`), the column is now coerced
  through `as.character()` with a warning naming the affected columns.
  Previously the factor side was passed to `as.numeric()`, which stacks
  level indices (1..k) against real codes in the same column.
* The residual user-missing sweep honors recipe `exclude` vetoes for
  q/Q-prefixed column names, mirroring `find_col()`'s full candidate ladder.
  Previously a veto written against a q-form name was silently ignored and
  the swept cells could lose documented substantive codes.
* `value_recode` leaves an audit-log entry when a target suffix does not
  resolve (`:TARGET_ABSENT`, honoring `if_absent`) or resolves to a
  non-numeric column (`:SKIPPED_non_numeric`). Previously such targets
  vanished from the JSONL log entirely.
* Internal: dead variable removed from the `wave_count` validation check.

# lissr 1.3.2

Completes the party-scheme taxonomy: all five schemes catalogued from
archive value labels, three of the four vacant registry slots named, and
the taxonomy schema generalized to carry the pre-registry schemes.

## Full catalogue (taxonomy 2.0.0)

* Schemes 1 (cv08a054/058), 2 (cv13f207/209), and 3 (cv18j307/308) join
  schemes 4 and 5, catalogued verbatim from SPSS value labels with zero
  data rows read. The schema generalizes accordingly: suffix-agnostic
  field names (`code_actual`/`code_hypo`), per-scheme `hypo_offset` and
  special codes, `verified_waves` per scheme, and separate actual and
  hypothetical party tables for scheme 1, whose two items carry different
  party sets (LPF and Een NL only in the actual vote, Trots op Nederland
  only in the hypothetical).
* The registry finding is now scoped precisely: schemes 1 and 2 renumber
  wholesale between schemes (CDA is 1, then 5, then 3) and must never be
  pooled across schemes; the persistent registry begins at scheme 3, whose
  2017 list is carried code-for-code into schemes 4 and 5.
* Three vacant scheme-4 slots are named with in-archive evidence:
  5 GroenLinks, 7 PvdA, 10 50PLUS (reoccupied in scheme 5). Code 19
  remains pending, expected to surface in a cv22n harvest together with
  the late scheme-3 code sets (the 2021 election falls inside the
  scheme-3 span), both recorded as open items in the taxonomy.
* Tests extended: per-scheme offsets, scheme-1 set divergence, the
  renumbering anchors, registry stability from scheme 3 onward, and
  named retired codes cross-checked against their evidence scheme.

# lissr 1.3.1

First cut of the party-scheme taxonomy, and a factual correction to the
cv26r scheme note.

## Party-scheme taxonomy

* New reference file `inst/recipes/taxonomies/cv_party_scheme.yml` (the
  externalization the cv recipe's meta has pointed at since the schema was
  written). Schemes 4 (cv24p, cv25q) and 5 (cv26r) are fully catalogued from
  the SPSS value labels of the 307/308 vote items, verbatim, including the
  special codes and the 307-to-308 offset relation; schemes 1 to 3 are
  declared with their waves and marked pending. Engine consumption remains
  future work.
* New tests pin the taxonomy to the recipe's per-wave scheme declarations
  and turn the catalogued registry invariants into regressions: unique codes
  per scheme, `code_308 = code_307 + 1` for every entry, and identity of
  every code shared between schemes 4 and 5.

## Correction

* The 1.3.0 note on cv26r said the post-election mapping was "not
  code-comparable with scheme 4". Cataloguing the value labels shows the
  opposite mechanism: codes form a stable party registry. Every code shared
  by schemes 4 and 5 denotes the same party; the delta is exactly 50PLUS
  entering at its historical slot (307:10, 308:11) and NSC leaving (307:21,
  308:22), with retired codes left unassigned rather than reused. The wave
  entry and the meta note now state this; the scheme-5 declaration itself
  stands, since the id names the code set a wave can contain.

# lissr 1.3.0

Wave onboarding release: the four 2025/2026 waves enter the recipes, and the
onboarding diff is repaired so future onboardings rest on real comparisons.

## New waves onboarded

* `ch25r`, `cp25q`, `cs25r`, and `cv26r` are added to their module recipes:
  `wave_index` entries, `covered_waves`, and every wave-scoped rule whose
  condition persists into the new wave (46 recipe edits in total). The
  extensions were decided by a structural and label-level review against the
  predecessor wave of each module.
* `ch25r`: 29 new suffixes 278-306 (a two-week symptom-frequency battery and
  a stress-domain block), no removals; reference-year labels updated.
* `cp25q` and `cs25r`: structurally and label-identical to their
  predecessors.
* `cv26r`: political-item churn after the 29 October 2025 parliamentary
  elections (added 221/256 for the returning 50PLUS and 353-355 for new
  politician items; removed 306, 341, 343-345). The wave declares
  `party_scheme: 5` because the post-election ballot mapping is not
  code-comparable with scheme 4; the value-label catalogue for the pending
  party-scheme taxonomy is noted in the wave entry.

## Onboarding diff repaired

* `onboard_new_wave()` step 3 previously reconstructed the previous wave's
  variable names from the new wave's own names, so it always reported zero
  additions and hardcoded zero removals. It now locates the actual previous
  wave file through the recipe's `file_pattern` (with the engine's fallback
  and release-version disambiguation), reads it, and reports the real
  bidirectional suffix diff. A new `prev_file` argument overrides the
  automatic resolution; when no file can be found the report carries
  `diff_skipped = TRUE` instead of a silent empty diff.
* The new-wave reader now goes through the engine's `read_wave_file()`, so
  SPSS user-defined missing codes stay visible to the step-6 sentinel scan.

## Tests

* New regression tests cover the bidirectional diff, the skipped-diff path,
  and the `prev_file` override, plus an invariant test pinning
  `covered_waves` to `wave_index` across all ten bundled recipes.

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
