# lissr income-cleaning framework: design and rule catalog

Companion to `inst/cleaning/income_cleaning_rules.yml` (the executable
ruleset), `R/liss_clean_income.R` (the engine), and
`R/liss_clean_executors.R` (the pure numeric kernels). This document
records the architecture, the full rule catalog, the mapping from the
two legacy analysis-project implementations the framework consolidates,
every deliberate deviation from that legacy logic, and the validation
evidence.

## 1. Purpose and scope

lissr merges LISS waves faithfully; it has, until now, left data quality
untouched. Household income (`nethh`, codebook `ci00a339`) is the
variable where that neutrality costs the most: self-reports carry
misplaced decimals, monthly-for-annual entries, personal income keyed
into the household field, placeholder junk, sign errors, and residual
SPSS missing codes. Two downstream analysis projects each maintained
their own copy of a cleaning procedure; this framework consolidates that
logic into the package as a declarative, individually switchable, fully
audited pipeline stage.

Three commitments shape the design:

1.  Transparency. Every modification is ledgered with the responsible
    rule, the numeric evidence, the admissible candidate set, and a
    plain-language justification. Original values remain in the returned
    data, and the generated report reproduces the methodology from the
    ruleset that actually ran.
2.  Contestability. Researchers can disable any rule, change any
    parameter, or substitute a whole ruleset, per run or via a custom
    YAML file, and the report records what they changed.
3.  Reviewability. Rules live in one YAML file with descriptions,
    rationales, and literature references, so the methodology can evolve
    through ordinary pull-request review while old runs stay
    reproducible against their recorded ruleset version.

## 2. Architecture

    income_cleaning_rules.yml       declarative decision rules (single
            |                       source of truth; validated schema)
            v
    liss_cleaning_ruleset()  ---->  validate_cleaning_ruleset()
            |
            v
    liss_clean_income()             orchestrator: resolves columns, walks
            |                       households, dispatches rules by action,
            |                       writes the decision ledger
            |
            +--> liss_clean_executors.R   pure kernels: magnitude and
            |                             volatility signatures, robust
            |                             detectors, candidate generators,
            |                             selection, equivalisation
            |
            +--> decision ledger          one typed row per decision
            |
            v
    liss_cleaning_report()          markdown report + CSV ledger + JSONL
                                    audit log (engine-shaped entries)

The split mirrors the merge engine: kernels are base-R,
data-frame-agnostic, and unit-testable without haven or real data; the
orchestrator owns column resolution, grouping, dispatch, and logging.
Rules are dispatched by their `action` field, never by `rule_id`, so a
custom ruleset may renumber or reorder rules freely; evaluation order
within a stage follows ruleset order.

## 3. Ruleset schema v1.0.0

Top-level sections:

| Section | Content |
|----|----|
| `meta` | ruleset name, version, schema_version, provenance |
| `references` | citation keys resolved in reports |
| `variables` | column mapping (target, aliases, ids, wave, context) |
| `constraints` | income_cap, min_income, wavenr_origin, brackets, sentinel codes |
| `preparation_rules` | actions run before detection |
| `detection_rules` | actions that identify implausible cells |
| `correction_rules` | candidate generators for cells routed to correction |
| `selection` | anchor policy for choosing among candidates |
| `finalization_rules` | post-correction guarantees |
| `logging` | artifact file names, appendix row cap |

Every rule carries `rule_id` (unique), `action` (from the controlled
vocabulary below), `description` (required), plus optional `rationale`,
`references`, `enabled` (default true), `log` (default true), `params`,
and the routing fields `disposition`, `scope`, and `stage`. Unknown rule
keys draw a warning-only notice at validation, mirroring the merge
engine’s mis-named-key check.

Controlled action vocabulary (the `CLEANING_ACTIONS` constant in
`liss_clean_income.R` is the source of truth):

| Section | Actions |
|----|----|
| preparation_rules | attach_background, resolve_target_variable, rectify_sign, map_category_bounds, backfill_age, guard_residual_sentinels |
| detection_rules | invalid_category_bounds, absolute_floor, contextual_floor, personal_income_echo, low_magnitude_scale, scale_error, category_bound_violation, exceeds_cap, robust_consensus, extreme_robust_z, dataset_consensus |
| correction_rules | household_center, category_midpoint, scale_rectification, temporal_smoothing, donor_pool, range_midpoint |
| finalization_rules | hard_cap_to_na |

Detection dispositions route what happens on a hit: `void_bounds`
(invalidate the row’s category bounds), `set_na` (void the target
value), `correct` (route to the candidate stage), `flag` (annotate
only). Scopes: `global` (vectorized over all rows), `household` (inside
the grouped, wave-ordered series), `dataset` (on the full cleaned
distribution). `stage: preliminary` marks the one household rule that
runs before the iterative loop.

## 4. Decision flow

1.  Guard: refuse data that already carries `<target>_observed`.
2.  P01 attach background (only when a frame is supplied): align the
    background month key to the annual scale, keep the latest month per
    person-year, join on the person id (never the household id).
3.  Resolve variables; snapshot the numeric view of the target as
    `observed`.
4.  P06 sweep declared SPSS user-missing values and configured sentinel
    codes; P03 rectify negative signs; P04 expand bracket codes to euro
    bounds; D01 void corrupt euro bounds.
5.  Global voids in ruleset order: D02 absolute floor, D03 contextual
    floor, D04 personal-income echo.
6.  Household stage, per group ordered by wave, requiring at least two
    finite values: D05 preliminary low-magnitude scaling, then the
    iterative loop. Each iteration recomputes volatility and magnitude
    signatures, evaluates D06 through D10 in ruleset order, takes the
    first rule that fires, selects that rule’s single worst cell, and
    applies one correction; corrected cells are never revisited, and the
    loop is capped at the household’s row count.
7.  F01 hard cap: any value still non-positive or above `income_cap`
    becomes NA.
8.  D11 dataset-level consensus flags on the cleaned distribution
    (annotation only, never a modification).

## 5. Rule catalog

| Rule | Action | Scope/disposition | Key defaults | Intent |
|----|----|----|----|----|
| P01 | attach_background | preparation | wavenr_origin 2007 | annual-aligned demographics join |
| P02 | resolve_target_variable | preparation | aliases \[ci00a339\] | explicit name resolution, no regex |
| P03 | rectify_sign | preparation | target only | ledgered absolute value for negatives |
| P04 | map_category_bounds | preparation | max_code 7 | bracket codes to euro bounds |
| P05 | backfill_age | preparation | internal only | age for donor matching from birth year |
| P06 | guard_residual_sentinels | preparation | 9999999998/9 | sweep declared and configured missing codes |
| D01 | invalid_category_bounds | global/void_bounds | \[0, 120000\] | discard corrupt bracket metadata |
| D02 | absolute_floor | global/set_na | threshold 10 | void placeholder junk |
| D03 | contextual_floor | global/set_na | threshold 100 | void household income contradicted by personal gross |
| D04 | personal_income_echo | global/set_na | ceiling 10000, tol 100 | void personal-in-household entries |
| D05 | low_magnitude_scale | household/correct, preliminary | ceiling 1000, factor 10 | whole-household monthly-for-annual |
| D06 | scale_error | household/correct | volatility_min 0.6 | decimal-point break in the magnitude series |
| D07 | category_bound_violation | household/correct | factors 1.5/0.5 | euro answer contradicting the bracket answer |
| D08 | exceeds_cap | household/correct | income_cap | absolute implausibility |
| D09 | robust_consensus | household/correct | iqr+mad, consensus 1, vol 0.4, min_obs 4 | Tukey/MAD agreement with volatility confirmation |
| D10 | extreme_robust_z | household/correct | z 3, min_obs 3, min_relative_deviation 0.3 | extreme modified z with a scale guard |
| D11 | dataset_consensus | dataset/flag | iqr+mad, consensus 1 | annotate distribution extremes |
| C01 | household_center | candidates | median, mean | other-wave central tendency |
| C02 | category_midpoint | candidates |  | bracket midpoint |
| C03 | scale_rectification | candidates | /10, /100, x10 below 1000 | preserve the respondent’s digits |
| C04 | temporal_smoothing | candidates | k 2, linear | native na_ma-equivalent moving average |
| C05 | donor_pool | candidates | median, min_donors 1 | hierarchically matched similar cases |
| C06 | range_midpoint | candidates | fallback only | guaranteed bounded correction |
| F01 | hard_cap_to_na | finalization | income_cap | void what correction could not repair |

References carried by the ruleset: Tukey (1977) for IQR fences, Leys et
al. (2013) for MAD over mean/SD, Iglewicz and Hoaglin (1993) for the
modified z-score, Moritz and Bartz-Beielstein (2017) for the moving
average semantics C04 reproduces natively.

## 6. Candidate generation and selection

For each cell routed to correction, the enabled correction rules
generate candidates in ruleset order; candidates are filtered to the
cell’s valid range
(`[category lower or min_income, min(category upper, income_cap)]`),
de-duplicated with the earliest-generated source winning, and the
candidate closest to the anchor is applied. The anchor is the household
median of the other observed values, falling back to the valid-range
midpoint when the household offers nothing else.

One structural property deserves explicit mention because it also holds
in the legacy code: whenever the household has at least one other
observed value, the household-median candidate sits at distance zero
from the anchor and therefore wins. The richer candidates (bracket
midpoint, rescaled originals, smoothing, donors) decide the outcome
exactly when bounds exclude the household median, and they are always
recorded in the ledger so a reviewer can see what the selection
considered. `na_only` mode bypasses this stage entirely and voids the
flagged cell instead.

## 7. Modes and output contract

| Mode | Target column | Extra columns | Ledger `applied` |
|----|----|----|----|
| correct | cleaned values | `<t>_observed`, `<t>_clean_status`, `<t>_dataset_flag` | TRUE for mutations |
| flag | untouched | `<t>_observed`, `<t>_proposed`, `<t>_proposed_status`, `<t>_dataset_flag` | FALSE for mutations |
| na_only | detected cells NA | as correct mode | TRUE for mutations |

`<t>_clean_status` holds the final action and rule per modified cell
(`corrected:D06`, `voided:D02`, `rectified:P03`, `capped:F01`); a cell
touched twice keeps the last status while the ledger keeps the full
history. Flag-mode proposals are byte-identical to the correct-mode
result on the same input, which the tests assert.

Decision ledger columns:

| Column | Meaning |
|----|----|
| decision_id, rule_id, action, applied | identity and effect |
| row, person_id, household_id, wave, variable | location |
| observed | cell value immediately before this action |
| corrected | applied value (NA for voids, flags, caps) |
| valid_min, valid_max, anchor | the constraint window and target |
| candidates, candidate_source | admissible set and the chosen source |
| evidence | the numeric trigger, e.g. volatility and magnitudes |
| justification | one reviewable sentence per decision |

Artifacts written by
[`liss_cleaning_report()`](https://siardv.github.io/lissr/reference/liss_cleaning_report.md):
a markdown report whose methodology section is generated from the
ruleset (so documentation cannot drift from behavior), the full ledger
as CSV, and a JSONL log whose entries share the merge engine’s
`make_log` field set, so merge logs and cleaning logs read alike.

## 8. Provenance: mapping from the legacy implementations

The framework consolidates `02-income-cleaning.R` (dedicated module of
the health-dynamics project) and the inlined copy in the Big Five
project’s `script.R`; the equivalisation helper reproduces the
`stand_inc` construction of the downstream analysis code.

| Legacy construct | Framework counterpart |
|----|----|
| `prepare_income()` background merge with annual alignment | P01 attach_background |
| `ci00a339` grep fallback | P02 alias resolution |
| blanket [`abs()`](https://rdrr.io/r/base/MathFun.html) on income-like columns | P03 (target only, ledgered); detectors compare personal context in absolute value without mutating it |
| category code to `income_bounds` mapping | P04 + constraints.category_bounds |
| `gebjaar` carry and `leeftijd` backfill | P05 (internal donor frame only) |
| invalid `nethh_min` handling via `dummy_na()` | D01 void_bounds |
| `detect_income_outliers()` three void conditions | D02, D03, D04 |
| DECISION POINT 3 low-value scaling | D05 |
| CRITERION 1 scale error | D06 |
| CRITERION 2 bound violations | D07 |
| CRITERION 3 cap exceedance | D08 |
| CRITERION 4 IQR+MAD with lag-diff confirmation | D09 |
| CRITERION 5 robust z | D10 (with the new deviation gate, see below) |
| `detect_dataset_outliers()` | D11 (flag only, unchanged) |
| candidate list 1 through 5 and the midpoint fallback | C01 through C06 |
| closest-to-household-median selection | selection.anchor |
| post-pass hard cap to NA | F01 |
| `is_na`, `user_na`, `outlier`, `power10`, `diff` bookkeeping columns | typed decision ledger + `<t>_clean_status` |
| `stand_inc = nethh / ((aantalhh - aantalki + 0.8 * aantalki)^0.5)` | `liss_equivalise_income(scale = "weighted_sqrt")` |

### Corrections of latent defects found while porting

Each deviation from the legacy logic below is deliberate, and each is
covered by a unit or regression test.

1.  Magnitude-vector misalignment. `get_power10()` dropped non-finite
    and non-positive entries and returned a shorter vector; the caller
    assigned it back over all non-NA positions, so any household
    containing a zero income silently misaligned every magnitude after
    it. `power10_magnitude()` returns a full-length vector with NA at
    invalid positions.
2.  Donor self-contamination. `similar_cases()` searched the full frame
    including the row under correction, so a flagged value could donate
    to itself. `donor_pool_value()` always excludes the target row.
3.  Regex over-matching. Income columns were located with
    `grep("net|brut|nethh", ...)`, which also matches unrelated names
    (any internet-usage variable, for instance). Resolution is now by
    explicit name and alias lists in `variables`.
4.  Silent sign coercion. The blanket
    [`abs()`](https://rdrr.io/r/base/MathFun.html) left no trace; P03
    ledgers every rectified cell, and the personal-income context is
    compared in absolute value without being modified in the output.
5.  Selection contradicted its comment. For bound violations the code
    promised the largest deviation but took `err_idx[1]`;
    `bound_deviation_ratio()` now ranks violations as documented.
6.  Row order dependence. Households were processed in file order; the
    engine sorts each group by the wave variable (input order only when
    no wave column exists, with a logged note), and a shuffled-input
    regression test asserts order independence.
7.  Sentinel ambiguity in bookkeeping. `outlier` stored the original
    value with 0 meaning untouched, so a genuinely zero original was
    indistinguishable from no correction; the typed ledger and status
    column remove sentinel semantics entirely.
8.  Dead code. `hh_min_bound`/`hh_max_bound` were computed and capped
    but never used downstream; they are not reproduced.
9.  Dependency removal. `imputeTS::na_ma(weighting = "linear", k = 2)`
    is reimplemented natively (`wma_impute_at()`), including the
    window-widening behavior, and tested against hand-computed values,
    so the framework adds no dependency beyond what lissr already
    imports.
10. Residual sentinel guard. Merged lissr 1.1.0 output sweeps declared
    user-missing codes, but data of other provenances (or reads with
    `user_na = TRUE`) can still carry 9999999998/9999999999 as values;
    P06 honours declared haven `na_values`/`na_range` first and the
    configured codes as a fallback, per cell, before any detector can
    mistake a code for an income.
11. New: a scale guard on the extreme-z rule. The legacy CRITERION 5
    gated on nothing but `|z| > 3`. On a seeded synthetic panel (400
    households, 2168 person-waves, 158 planted errors of seven types)
    the ungated rule “corrected” 12.09 percent of clean cells, because a
    tiny MAD in a tightly clustered household lets an ordinary
    fluctuation exceed the threshold. D10 therefore additionally
    requires the flagged value to deviate from the household median of
    the other waves by at least `min_relative_deviation` (default 0.30).
    With the gate, the same panel shows 0.00 percent false positives and
    unchanged 100 percent recall on every planted type. The parameter is
    overridable like any other, and a regression test pins both sides of
    the behavior.

## 9. Validation evidence

Unit and regression tests: `tests/testthat/test-clean-income.R` adds 44
test blocks covering every kernel against hand-computed values, ruleset
loading, schema validation and override mechanics, one engine scenario
per rule (a fixture household per error signature plus a stable control
and a tight-household D10 regression), ledger invariants (the observed
column preserves the input byte-for-byte; the set of changed cells
equals the set of applied mutating ledger rows; statuses mark exactly
the changed cells), mode contracts (flag-mode proposals identical to
correct-mode output; na_only never imputes), determinism and row-order
independence, the double-cleaning guard and a stripped-rerun steady
state, configurability (disable, params, cap overrides, custom ruleset
files), input handling (merge-result lists, tibbles, alias resolution,
missing household or wave columns), background attachment on both
monthly and snapshot files, report artifacts, and the equivalisation
scale against the analysis-script formula.

Full-suite status in installed-package context: 74 test blocks pass, 0
fail, 6 skips (all pre-existing environment skips: 4 empirical blocks
gated behind `LISSR_VERIFICATION_DIR`, 1 CRAN skip, 1 keyring path),
with the single pre-existing intentional warning. `R CMD check`
(vignette building skipped for a container without knitr) reports no
code, documentation, example, or test problems; the one warning and two
notes are container artifacts (locale, absent suggested packages).

Seeded synthetic verification
(`inst/scripts/verification/ income_cleaning_smoke.R`): 400 households,
2168 person-waves, natural per-wave noise around 6 percent, 158 planted
errors on distinct rows.

| Planted type  | Planted | Recovered |
|---------------|---------|-----------|
| decimal_shift | 65      | 65 (100%) |
| extra_zero    | 33      | 33 (100%) |
| cap_blowout   | 9       | 9 (100%)  |
| personal_echo | 22      | 22 (100%) |
| tiny_junk     | 11      | 11 (100%) |
| sign_flip     | 11      | 11 (100%) |
| sentinel      | 7       | 7 (100%)  |

False positives: 0 of 2010 clean cells. Scale-type corrections land
within 10 percent of the pre-error truth in 83 percent of cases (median
relative error 3.8 percent, essentially the panel’s natural noise floor,
since the household median cannot recover idiosyncratic fluctuation).
The changed-cell set equals the planted-row set exactly; flag-mode
proposals are identical to correct-mode output; runtime is about half a
second for the full panel.

## 10. Extending the framework

Adding a decision rule end to end:

1.  Declare it in the ruleset with a fresh `rule_id`, the section’s
    `action` (or a new action), `description`, `rationale`, `params`,
    and `references` where the method has literature.
2.  If the action is new, add it to `CLEANING_ACTIONS` in
    `liss_clean_income.R` and implement the dispatch arm (global voids,
    the household loop, or a new candidate generator in
    `generate_candidates()`); put any numeric logic in
    `liss_clean_executors.R` as a pure kernel.
3.  Extend the numeric-parameter list in
    [`validate_cleaning_ruleset()`](https://siardv.github.io/lissr/reference/validate_cleaning_ruleset.md)
    for any new parameter names.
4.  Add a fixture household reproducing the error signature to
    `test-clean-income.R`, plus a control asserting the rule leaves
    clean data alone, and re-run the seeded smoke script to check the
    false-positive rate.
5.  Nothing else: the report’s methodology section, the ledger, and the
    per-rule log entries pick the rule up automatically.

Pull requests changing rules or defaults should carry: the ruleset diff,
the rationale text in the YAML itself, the new or adjusted tests, and
smoke-script numbers before and after. Version the ruleset
(`meta.ruleset_version`) on any behavioral change so old reports remain
attributable.

## 11. Roadmap

Multivariate detection (local outlier factor per Breunig et al. 2000) is
the natural D12 once a reference implementation without new hard
dependencies is settled; the dataset-consensus flag column already gives
it a place to land as annotation before any correction authority. Other
candidates: an opt-in rule imputing the cells D02 through D04 void
(currently they stay NA by design, matching the legacy pipelines),
per-module ruleset variants for income variables outside CI, and
propagating the cleaning ledger into
[`merge_liss_panel()`](https://siardv.github.io/lissr/reference/merge_liss_panel.md)
metadata so linked panels carry their cleaning provenance.
