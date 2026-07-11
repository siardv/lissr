# Reconciliation of the two independent lissr assessments

Compares my assessment (`lissr-1.4-assessment.md`, 2026-07-11) with the ChatGPT 5.6 Sol Pro assessment supplied on the same materials. Every ChatGPT-specific factual claim referenced below was re-verified against the v1.3.2 code before being accepted or corrected; verification method is stated per item. The original assessment file is left unchanged for traceability; section 5 here contains the merged v1.4 priority list that supersedes its section 10.

## 1. Provenance differences that matter for weighing the two

The two reviews had different capabilities, and their blind spots line up with those capabilities almost exactly.

- The ChatGPT review was static only: it states R was not installed in its environment, so nothing was executed, and no live-site probing is evidenced. It could, however, read `local_lissr_project.csv` and `liss_data.csv`, which I could not (the project stores them as unreadable blobs).
- My review executed the engine and executors against synthetic inputs, ran the income-cleaning test suite and smoke script with targeted probes, and probed the live archive; the two CSV inventories were unavailable to me.

Consequently: where the ChatGPT review is strongest (schema-level and recipe-corpus observations, inventory facts), it is confirmatory or additive; where it is silent (income cleaner, downloader integrity, concrete recipe execution defects), the silence reflects its inability to run anything, not an all-clear.

## 2. Convergent findings (independent replication, highest confidence)

1. **The central diagnosis is identical.** Both reviews conclude that v1.4 should be a conformance-and-reliability release, not a feature release: the recipes and documentation currently promise more enforcement than the engine executes. ChatGPT's three closing conditions (every operational rule executes; every error-level check is evaluated; every output carries provenance) are the same target as my A1/A2/A3/A4 block.
2. **The validation-check numbers match exactly.** Both reviews independently counted 99 declared checks with 29 executable and 70 skipping, with the identical per-module distribution (ca 0/7, cd 3/11, cf 0/8, ch 7/10, ci 0/8, cp 13/14, cr 0/10, cs 0/9, cv 3/10, cw 3/12). Treat this table as settled fact.
3. **Same remedy for the checks**: a small normalized check grammar plus alias mapping (my A2; ChatGPT Priority 1, whose ten-primitive list is a good concrete draft), with error-severity checks never allowed to skip silently.
4. **Per-action payload enforcement**: my A1 (vocabulary-driven per-action required/accepted keys) and ChatGPT's Priorities 3 and 9 (machine-readable schema; handler registries deriving the validator vocabulary) are the same move at different layers. Recommendation on mechanism in section 4.3.
5. **Per-recipe synthetic end-to-end fixtures** (my A4; ChatGPT Priority 11), **corpus audit as a CI gate** (my A3 plus static analyzer; ChatGPT Priority 4's `audit_liss_recipes()` is a good exported-function packaging of the same thing), **engine split** (my S9; ChatGPT Priority 8), **two-tier empirical verification with hashed manifests** (my S2 adjacent; ChatGPT Priority 12), **.dta decide-or-drop** (both), **multi-module timing semantics documented and diagnosed** (my M6/D5; ChatGPT Priority 13 generalizes it correctly: equal calendar year does not mean equal observation occasion), and **background variables as a constrained attach-helper rather than a second harmonization system** (my S1 tier b; ChatGPT Priority 14, essentially the same function sketch) all appear in both reviews.

## 3. ChatGPT-unique findings, now verified

### 3.1 fieldwork_ym derivation is suppressed in nine of ten modules. Confirmed by execution; upgraded to P1. (new item M14)

ChatGPT's Priority 6 flagged the design smell; verification shows it is a live output defect. Nine recipes (all except cr) declare `fieldwork_ym` under `expected_presence.critical` with `on_absence: warn`. In the engine, `check_expected_presence()` runs before the rules and creates the missing column as all-NA; the `_m`-based auto-derivation later runs only `if (!("fieldwork_ym" %in% names(df)))`, so the NA placeholder blocks it. Executed probe: with the declaration, a wave whose `s_m` carries 200811/200812 ends with `fieldwork_ym = NA,NA,NA`; without the declaration it ends with the real values. The cr recipe even documents the mechanism in an `optional_note` ("pre-declaring it under expected_presence currently suppresses that derivation; revisit once the engine-side fix lands"), which means the workaround was applied to cr only and the other nine recipes were left affected.

Consequences: merged outputs for ca/cd/cf/ch/ci/cp/cs/cv/cw carry an all-NA `fieldwork_ym` (cv separately writes `fieldwork_month` for some waves via its own rule), every merge emits a misleading "fieldwork_ym absent, created as NA" warning per wave, the merge-workflow vignette's claim that outputs include `fieldwork_ym` is false in practice, and the documented background-variables workflow ("match the avars file to the fieldwork month") asks users to consult a column that is empty. It also blocks the planned attach-helper (S1), which needs per-row fieldwork months.

Fix (S engine + S recipes): derive `fieldwork_ym` before `check_expected_presence()` (or make expected-presence skip placeholder creation for engine-generated variables), remove the cr workaround note, re-declare fieldwork_ym in cr, and add a regression test that `fieldwork_ym` is non-NA when a wave carries `_m`. ChatGPT's cleaner long-term split of `expected_input` vs `generated_output` sections is worth adopting when the schema is next revised, but the ordering fix should not wait for it.

### 3.2 Schema-version statement mismatch. Confirmed. (new item D12, S)

README ("conforming to CANONICAL_SCHEMA.md (v1.0.0)") and DESCRIPTION ("canonical schema (v1.0.0)") both say v1.0.0, while the shipped schema document is v1.1.0 (additive) and the cv recipe declares 1.1.0. The website mirrors the README. Small but symbolically relevant, exactly as ChatGPT says. Fix wording to "v1.1.0 (v1.0.0 recipes remain valid)".

### 3.3 Release-selection provenance and pinning. Adopted as P2. (new item M15, S-M)

The engine already ranks releases and warns; ChatGPT's addition is to print the decision in the merge report (selected file, ignored files, rule) and support an optional `expected_release:` pin per wave that warns or fails under strict mode when a different release is chosen. Cheap, and it hardens exactly the cd10c-class situation that the user's archive actually contains (per the inventory ChatGPT read: both 1.0p and 1.1p are on disk).

### 3.4 Structured condition classes. Adopted as P3. (new item, S-M)

Package-specific error/warning classes (`lissr_recipe_error`, `lissr_validation_error`, and so on) so pipelines can catch failures without parsing message text. Complements my M10/A-items; cheap to add incrementally as functions are touched.

### 3.5 Inventory facts (provisional; I could not read the CSVs)

From ChatGPT's reading of `liss_data.csv`: every wave ID declared in the ten recipes has at least one matching local file; the cd10c 1.0p/1.1p multi-release pair is present; background variables comprise hundreds of monthly `.sav`/`.dta` files. These are plausible and consistent with everything else, but I could not verify them, and they answer only the declared-to-file direction. My two open coverage questions run the other way and remain open: whether cf25r exists locally but is not yet onboarded, and whether cw08a-cw10c exist locally while the cw recipe deliberately starts at cw11d. The metadata-catalogue idea (ChatGPT Priority 15) and my `verify_recipe_against_data()` (S2) are complementary halves of the same infrastructure: catalogue what exists, then check recipes against it.

## 4. ChatGPT claims that needed correction or reframing

### 4.1 "crosswalk in CW" as accepted-but-inert: refuted

cw `HR02_edu_harmonize` carries an engine-conformant payload (`crosswalk`, `scheme_column`, `output_variable`, `output_scheme_flag`, `variables`), and the crosswalk executor is implemented (kernels in `liss_executors.R`, dispatch in the harmonization switch, scheme resolution from the wave entry). The likely source of the error is instructive: `action_vocabulary.yml` still marks `crosswalk` (and three other implemented actions) as `stub`, so a static reader that trusts the vocabulary is misled. That stale-status file is my D11; fixing it prevents exactly this misreading. The neighboring ChatGPT examples are, however, correct: ch/ci `crosswalk_rename` alias keys (`from`/`to`, `old`/`new`) are confirmed in the recipes (1 rule in ch, 9 entries in ci) and are the documented deferred limitation.

### 4.2 "Remove tracked .DS_Store" and archive hygiene: mostly moot

`git ls-files` shows no `.DS_Store` or macOS artifacts tracked; they exist only as untracked files in the local zip. `inst/doc` and `build/vignette.rds` are tracked deliberately (pre-rendered vignettes are a documented decision, given that building them requires LISS credentials). Remaining kernel of the point: generate review/release archives with `git archive`, and state the pre-rendered-vignettes policy in CONTRIBUTING or the README build note. Downgraded to an S hygiene note.

### 4.3 Machine-readable schema (JSON Schema) vs vocabulary-driven validation: same goal, different mechanism

I recommend implementing A1 by extending `action_vocabulary.yml` (already declared the single source of truth, already loaded by the validator) with per-action `required`/`accepted` payload keys, rather than introducing a parallel JSON Schema artifact that can itself drift. ChatGPT's handler-registry idea (Priority 9) is the right end state and pairs naturally with the engine split (S9); doing registries first would front-load a refactor before the correctness fixes land. Sequence: vocabulary-driven per-action validation in 1.4; registry refactor with the engine split in 1.5.

### 4.4 `strict = TRUE` as the default: right direction, wrong moment

Flipping the default before A2 lands would be a no-op (skipped checks cannot fail), and flipping it immediately after A2 would make previously-skipped error-level checks abort many existing workflows in the same release that first executes them. Recommended sequence: 1.4 implements the checks, reports pass/fail/skip prominently, adds ChatGPT's `valid_for_analysis` flag to the returned object, and does not write final data files on error-level failure in strict mode; 1.5 flips the default to strict with an explicit `exploratory` escape hatch, once the recipes' checks are known to pass on real data. ChatGPT's provenance additions (package version, recipe hash, schema version, input hashes, strictness recorded in every audit record) are adopted as part of M10.

### 4.5 `compatibility = "1.3"` engine mode: over-engineered for the current stage

Reproducing historical outputs is better served by recipe versioning discipline (D11), NEWS documentation, and pinning the package version than by maintaining a second semantics inside the engine. Fix the ch/ci crosswalk aliases (and the M1/M2 re-keys) as ordinary versioned recipe changes with a clear NEWS entry describing exactly which output columns change.

### 4.6 What the ChatGPT review did not see

For calibration rather than criticism (it could not execute anything): it contains none of the concrete execution defects that my review confirmed by running the code. Specifically absent: the entire cf module failure (core -9/-8 rules inert plus module-wide 999/9999/99999 over-application, M1), the degenerate comparability flags in cr/ch/cp/ca (M2), the inert cw pension-date recode (M3), ca's lost detail column (M4), the factor-coercion corruption (M5), all nine income-cleaning findings (C1-C9; its only comment on that subsystem is that the tests are extensive), and all download/auth integrity findings (N1-N3, including error bodies saved as `.sav` with status "ok" and the partial password echo). Its Priority 2 is the right category but its concrete instances are limited to what the schema itself confesses. The practical implication: a v1.4 plan built from the ChatGPT list alone would leave the most severe user-facing corruption paths (cf, cleaning, downloader) unfixed while making the correct architectural improvements around them.

## 5. Merged v1.4 priority list (supersedes section 10 of my assessment)

**Core, in order:**

1. M14 fieldwork_ym ordering fix plus regression test (new, from ChatGPT P6; executed confirmation)
2. M2 + M3 + M4 + M5 recipe re-keys and small engine fixes, now explicitly including the ch/ci crosswalk_rename alias fix (ChatGPT P2) as part of the same pass
3. C1 + C2 + C3 + C4 + C5 income-cleaner policy fixes
4. N1 + N2 + N3 download integrity and credential hygiene
5. A1 per-action payload validation (vocabulary-driven) + A2 check grammar and aliases; error-level checks can no longer skip silently
6. M1 cf repair (schedule with data verification, or ship 1.4 with a loud known-issue note and make cf the headline of 1.4.1)
7. D1 + D2 + D4 + D7 + D12 documentation P1s (D12 = schema-version statement, new)
8. A3 zero-warning recipes + the corpus audit surfaced as an exported `audit_liss_recipes()` (ChatGPT P4 packaging), wired into CI

**Strongly recommended (P2):** A4 per-module synthetic fixtures, A5 cleaning fixtures, A8 ruleset-validator depth, M6 ca year semantics, M7 cv fieldwork month (largely subsumed by M14 plus a `wave_values` backfill for cv waves without `_m`), M8 cr splits, M9 onLoad fix, M15 release provenance and pinning (new), N4-N9, C6 + C7 + C9, D3 + D5 + D6 + D8-D11, M10 output control including `valid_for_analysis` and provenance fields, S1 tier (b) background attach-helper if capacity allows (both reviews independently converged on the same helper design, which raises confidence it is the right scope).

**Deferred with decisions recorded (P3):** strict-by-default flip (1.5, per 4.4), handler registries + engine split (1.5, per 4.3), S2 verify_recipe_against_data plus the generated metadata catalogue, structured condition classes (incremental), S3/S4/S6/S7/S8, M11-M13, N10-N11, archive-hygiene note.

## 6. Bottom line

The two assessments were produced independently and disagree on almost nothing structural: both identify the declared-versus-executed gap as the defining risk and propose materially the same enforcement machinery, and their one exactly-overlapping quantitative claim (29 of 99 checks executable) matches to the digit. ChatGPT contributed one significant confirmed defect my review missed (fieldwork_ym suppression, now P1 item M14), one small confirmed doc defect (D12), and two adoptable design ideas (release pinning, structured conditions), while three of its claims did not survive verification (cw crosswalk inert; tracked .DS_Store; and, as a matter of sequencing rather than fact, strict-by-default and the compatibility mode). My review contributes the executed correctness findings (cf, flags, cleaning, downloader) that a static review could not reach. The merged list in section 5 is, I believe, the complete and correctly ordered v1.4 scope given everything both reviews know; the remaining unknowns are exactly the ones that need your local data (cf verification waves, the two coverage questions, and one avars file).
