# LISS Panel Merge Recipe: Canonical Schema v1.1.0

Version note: v1.1.0 is strictly additive to v1.0.0. Every v1.0.0 recipe
remains valid and merges unchanged; the additions are the `recode` alias and
`exclude` blocks on `recode_to_na` rules, the `aux_files` resolution and
zero-overlap contract on `wave_index` entries, release-version disambiguation
when several primary files match one wave, the `uniqueness` validation check
family, the SKIP outcome for unimplemented check types, the `strict` merge
gate, and the value-label / user-missing round-trip under
`labelled_policy: to_numeric`. Recipes may declare either schema version in
`meta.schema_version`; the engine accepts both.

## Purpose

This document defines the **single authoritative YAML structure** that every
LISS module merge recipe must follow. A unified R engine
(`liss_merge_engine.R`) reads any recipe that conforms to this schema. The
schema is enforced by `validate_recipe()`, which runs as a pre-flight check
before any merge work begins.

## Top-level sections

| Section              | Key              | Type    | Required |
|----------------------|------------------|---------|----------|
| Metadata             | `meta`           | mapping | yes      |
| Global settings      | `global`         | mapping | yes      |
| Wave index           | `wave_index`     | list    | yes      |
| Variable rules       | `variable_rules` | list    | no       |
| Harmonization rules  | `harmonization_rules` | list | no  |
| Boundary rules       | `boundary_rules` | list    | no       |
| Drop / retain rules  | `drop_retain_rules`  | list | no  |
| Derived variables    | `derived_variables`  | list | no  |
| Validation checks    | `validation_checks`  | list | no  |
| Logging              | `logging`        | mapping | yes      |

---

## Controlled action vocabulary

**INVARIANT**: Every rule must have a non-empty `action` drawn from this list.
The engine rejects unknown or empty actions unless the action is `note_only`.

### Variable rules

| Action                  | Description                             |
|-------------------------|-----------------------------------------|
| `strip_prefix`          | remove wave prefix from column names    |
| `type_coerce`           | cast column to `target_type`            |
| `rename`                | rename columns via mapping              |
| `set_label`             | override variable metadata label        |
| `apply_labelled_policy` | apply haven labelled conversion         |
| `strip_value_labels`    | strip whitespace from value labels      |
| `note_only`             | documentary; no data transformation     |

### Harmonization rules

| Action                | Description                              |
|-----------------------|------------------------------------------|
| `recode_to_na`        | map sentinel codes to NA                 |
| `value_recode`        | map old values to new values             |
| `fix_label`           | correct typo in value label              |
| `crosswalk`           | multi-scheme value harmonization         |
| `strip_question_stem` | remove embedded stems from labels        |
| `lowercase_labels`    | normalize label case                     |
| `flag_only`           | mark anomaly without transforming data   |
| `note_only`           | documentary; no data transformation      |

#### `recode_to_na` keys (v1.1.0)

The sentinel map may be given as `mapping:`, `codes:`, or the alias
`recode:` (the three are equivalent; `recode` accepts the
`code: reason-tag` form used by several recipes, where the tag is
documentary). Scoping follows the common rules (`suffixes`, `variables`,
or `scope: all_numeric`). A rule may additionally carry `exclude:` blocks;
each block names `suffixes` and `waves`, and a cell is skipped when its
suffix and its wave both match a block. An exclude block is a documented
decision that the code is substantive for those cells, and the write-phase
user-missing sweep honors the same blocks (see Implementation notes).

```yaml
- rule_id: HR02_dk_negative
  action: recode_to_na
  scope: all_numeric
  recode: { -9: ".dk" }
  waves: [cv20l, cv21n]
  exclude:
    - suffixes: ["243"]
      waves: [cv20l, cv21n]
```

### Boundary rules

| Action              | Description                                |
|---------------------|--------------------------------------------|
| `add_era_flag`      | assign era/period indicator                |
| `add_flag`          | add binary flag at structural break        |
| `add_period_flag`   | add multi-level period indicator           |
| `split_variable`    | split suffix into pre/post derived vars    |
| `structural_na`     | insert NA for absent module/instrument     |
| `filter_rows`       | subset rows in specific wave               |
| `crosswalk_rename`  | suffix renumbering at boundary             |
| `stack_aux_files`   | vertically bind auxiliary files             |
| `note_only`         | documentary; no data transformation        |

#### `crosswalk_rename` keys

A `crosswalk_rename` rule carries a `crosswalk:` list. Per entry, the engine
reads `old_suffix` and `new_suffix` (each resolved to a column) plus an optional
`harmonized_name` (default `h_<old_suffix>`), and coalesces the old and new
columns into the harmonized column. An optional `post_recode` block applies a
scoped value remap to the harmonized column(s) after the coalesce.

```yaml
boundary_rules:
  - rule_id: BR01
    action: crosswalk_rename
    crosswalk:
      - old_suffix: "244"
        new_suffix: "261"
        harmonized_name: h_health_index
```

Resolved in 1.3.2.9000 (v1.4 stage 2): the `ch` and `ci` recipes previously
carried alias keys the engine does not read (`from`/`to` and `old`/`new`), so
their renames never ran and a stray all-NA `h_` column was produced and then
dropped. Both recipes now use `old_suffix`/`new_suffix` with explicit
`harmonized_name` entries (`h_premium_period`; `h_q363` through `h_q371`), the
harmonized columns are really produced, and the old `h_` drop rules are
retired as documentation. This intentionally changes merged output relative
to 1.3.x: the harmonized columns are new, additive columns.

### Drop / retain rules

| Action              | Description                                |
|---------------------|--------------------------------------------|
| `drop`              | remove column from output                  |
| `retain`            | force-keep column                          |
| `retain_if_present` | keep where available, NA elsewhere         |
| `retain_as_metadata_only` | keep as metadata, not analysis var   |
| `note_only`         | documentary; no data transformation        |

---

## Section specifications

### `meta`

Required fields: `module`, `module_label`, `schema_version`, `recipe_version`,
`created`, `source_spec`, `covered_waves`.

```yaml
meta:
  module: "ch"
  module_label: "Health"
  schema_version: "1.0.0"
  recipe_version: "1.0.0"
  created: "2026-02-11"
  source_spec: "reference.md"
  covered_waves: [ch07a, ch08b]
  notes: "optional free text"
```

### `global`

Required fields: `id_variable`, `wave_variable`, `year_variable`,
`labelled_policy`, `missing_variable_policy`, `strip_label_whitespace`.

```yaml
global:
  id_variable: "nomem_encr"
  wave_variable: "wave_id"
  year_variable: "wave_year"
  labelled_policy: "to_numeric"              # to_numeric | to_factor | keep_labelled
  missing_variable_policy: "warn_and_create_na"  # error | warn_and_skip | warn_and_create_na
  strip_label_whitespace: true
  na_sentinel_codes: [-9, -8]                # optional

  expected_presence:                          # v1.0.0
    critical:
      - variable: "nomem_encr"
        waves: "all"
        on_absence: "error"
    optional_note: "add module-specific variables"

  taxonomy_refs:                              # v1.0.0
    party_scheme:
      source: "taxonomies/cv_party_scheme.yml"
```

**Labelled policy values**: `to_numeric`, `to_factor`, `keep_labelled`.

**Missing-variable policy values**: `error`, `warn_and_skip`, `warn_and_create_na`.

### `wave_index`

Required per-entry fields: `id`, `year`, `file_pattern`.

```yaml
wave_index:
  - id: "ch07a"
    year: 2007
    file_pattern: "ch07a_*"
    role_map:                    # v1.0.0: semantic role → local suffix
      satisfaction_health: "001"
      satisfaction_life: "002"
    # module-specific extra fields preserved
    era: 1
```

Optional per-entry field `aux_files` (v1.1.0 contract): a list of
supplemental data files whose rows are appended to the wave after loading.
Each entry resolves independently of `file_pattern` (matched in the module
data directory by name, extension-agnostic), so narrowing the primary
pattern cannot silently drop a declaration; an entry that resolves to no
file warns. Auxiliary rows must be disjoint from the primary file on the
id variable: any shared respondent id aborts the merge, because an
overlapping "auxiliary" is in practice a superseded release of the same
wave and stacking it would duplicate respondents.

File resolution and reading (v1.1.0): when more than one primary file
matches a wave's pattern, the engine ranks release versions parsed from
the file names (for example `1.0p` vs `1.1p`), keeps the highest, and
warns with the ignored files listed; unrankable candidates abort with a
request to narrow `file_pattern`. Files are read by extension from a
whitelist (`.sav`, `.zsav`, `.dta`, `.csv`); unknown extensions abort
rather than being parsed as CSV. SPSS files are read with user-defined
missing values preserved (`haven::read_sav(user_na = TRUE)`), so declared
DK/refusal codes reach the recipes as values; see Implementation notes for
what happens to them at write time.

### Rule sections (common structure)

Every rule **must** have:

| Field        | Type   | Required | Constraint                   |
|--------------|--------|----------|------------------------------|
| `rule_id`    | string | yes      | non-empty, unique in section |
| `action`     | string | yes      | from controlled vocabulary   |
| `description`| string | yes      | non-empty                    |
| `anomaly_ref`| string | no       | null or `A-NN` format        |
| `log`        | bool   | no       | default true                 |
| `waves`      | list   | no       | wave ids the rule runs on; all waves if absent |

`waves` is the only key that restricts a rule to a subset of waves. The engine
resolves `rule$waves` (absent or null means all waves) and reads no other scoping
key.

Unrecognized rule keys: `validate_recipe` emits a non-fatal
warning for any rule-level key that the engine neither consults nor sanctions as
documentation. The check is warning-only; every recipe still loads and merges
unchanged, and the validation outcome, control flow, return value, and merge
output are untouched. Its purpose is to surface a mis-named key (the
`applies_to_waves` class) at authoring time instead of having the engine ignore
it silently. The recognized set is the global union of keys consulted across the
four rule sections; it is section-agnostic, so a key valid for one action family
does not warn when it appears on another (section-appropriateness is a separate,
later check). The two sets are reproduced from the `RECOGNIZED_RULE_KEYS` and
`SANCTIONED_RULE_KEYS` constants in `liss_merge_engine.R`, which are the source
of truth:

```
RECOGNIZED_RULE_KEYS (consulted; global union):
  action, anomaly_ref, assignments, codes, column, columns, combined_label,
  comparability, corrected_label, crosswalk, default, derived_suffix,
  description, early_label, eras, exclude, flag_column, flag_name,
  flag_true_waves, flag_value_post, flag_value_pre, flag_variable, from_value,
  if_absent, keep, keep_values, label_map, late_label, mapping, new_fragment,
  offset, old_fragment, output_scheme_flag, output_variable, output_vars,
  parties_to_pool, party_names_to_pool, pattern, phases, post_recode, prefix,
  present_in_waves, recode, recodes, retain, retain_in, rule_id, scheme_column,
  scope, sentinel_values, set_label, source, source_column, source_variable,
  sources, stem, stems, suffixes, suffixes_range, swap, target, target_column,
  target_type, target_variable, target_variables, to_value, transforms, value,
  variable, variable_pattern, variables, variables_pattern, wave, waves,
  waves_early, waves_late, waves_post, waves_pre

SANCTIONED_RULE_KEYS (documentation/provenance; never warn):
  description, log, note, notes, reason, guidance,
  absent_cw19l_only, absent_from_waves, absent_in, absent_waves, boundary,
  conservative_pooling, cw19l_only_variables, deleted_without_replacement,
  discontinued_blocks, dropped_variables, fill_missing_waves, introductions,
  label, later_drops_outside_the_93, missing_reason, new_variables_pattern,
  non_comparable_replacements, nonexistent_ids_matched_by_pattern, pool,
  pooling_allowed, post_check, present_waves, reintroduced_in_cw25r, required,
  restored_wave, review_required, sentinel_label, structurally_missing_waves,
  waves_absent, waves_absent_from, waves_available, waves_new, waves_old,
  waves_present
```

Authoring guidance: a rule is scoped only by its `waves` key. A mis-named
scoping key (for example `applies_to_waves`) is silently ignored by the
engine, which would cause the rule to run on every wave, so the warning
above flags it at load time. The warning does not change behavior; authors
must scope with `waves`.

### Comparability contract (v1.0.0)

Boundary rules introducing structural breaks should include:

```yaml
comparability:
  status: "non_comparable"       # comparable | non_comparable | partial
  method: "no_pool"              # pool_ok | pool_with_flags | no_pool
  rationale: "instrument redesign between 18k and 19l"
```

The engine generates comparability flag columns and emits warnings when
`method` is `no_pool`.

### `derive_fieldwork_month`: recorded dates in variable rules

In `variable_rules`, `derive_fieldwork_month` parses a designated recorded
year-month into a numeric `YYYYMM` target. `source_column` selects one column;
`source_suffix: true` resolves its post-prefix suffix. Alternatively, a
`group_column` and `source_by_group` map select the designated source for each
row's group. A group map cannot also declare `source_column` or `sources`.

```yaml
- rule_id: VR06c_fieldwork_ym_cv16h
  description: recorded date for the normal questionnaire group
  anomaly_ref: A-11
  action: derive_fieldwork_month
  waves: [cv16h]
  group_column: groep
  source_by_group:
    '0': maandnr
    '1': maandnr_lang
    '2': maandnr_lang
  source_suffix: true
  target_column: fieldwork_ym
  if_absent: warn_and_create_na
```

This map combines parallel questionnaire administrations. It uses only the
source designated for that group; a missing designated date never falls back
to another source. Conflicting valid alternate-source dates are audited while
the designated source wins. Missing or unmapped groups produce unknown dates.
For an ordinary `sources` list, first-existing column selection is retained;
it is not rowwise coalescing. An explicit single source is preferable when
the date has a specific meaning.

Supported recorded values are numeric integers or six-digit numeric text,
with calendar months 01–12 and years 1900–2100. These year bounds are a broad
parsing envelope, not a wave-specific plausibility check. Ordinary, blank,
tagged and declared SPSS missing values remain `NA`. Fractions, invalid
calendar values, dates outside the envelope and unsupported source types
produce `NA` with counted diagnostics. No year is inferred from `wave_year`.
When the global labelled policy converts a labelled text source to numeric,
the executor rejects that coerced source because the converted numbers cannot
verify the original six-digit text grammar. Plain text sources remain
parseable. Original source type and subsequent name/coercion lineage are
carried internally for this check, without adding attributes to output source
columns. When a rename fills a target from mixed origins, any contamination
by text whose grammar was lost causes conservative rejection of the entire
date source column.
Source columns and their metadata remain unchanged. A written target owns its
date metadata, so previously harvested target value labels and user-missing
declarations cannot be reapplied to it.

The CV recipe designates `_m` for eight single-part waves, group-specific
`maandnr`/`maandnr_lang` for `cv16h`, and `_m1` for nine three-part waves.
Multipart `fieldwork_ym` represents part 1 only. It does not describe every
part or item, and never substitutes `_m2` or `_m3`. Unknown dates do not
establish a complete temporal record merely because other validation checks
pass. The `parse_time` action retains its existing character-copy behavior;
the harmonization-phase `derive_fieldwork_month` handler remains skipped.

### `derived_variables`: aggregation and transforms

Derived columns run after all rule phases. Each entry has a `rule_id` and a name
(`name`, or the deprecated `var_name`), and a `sources` list of blocks, each
naming the `waves` it covers and the `variable` (or `variables`) to read in
those waves:

```yaml
derived_variables:
  - rule_id: DV-02
    name: received_disability_benefit
    method: max          # row-wise max over the resolved sources
    sources:
      - waves: [ci08a, ci09b, ci10c, ci11d, ci12e]
        variables: [Q096, Q097]
      - waves: [ci13f, ci14g, ci15h]
        variable: Q096
```

Aggregation (`method`): `sum` (default) sums across the resolved sources; `max`
takes the row-wise maximum, which for 0/1 indicators reads as "received any".
The DV loop reads the rule-level `method` only; a block-level `aggregation:` key
is not consulted, so the knob must sit on the rule (as on ci DV-02 and DV-03).

Transform vs reference: a numeric offset is applied only from an exact
`transform` key. The engine reads it with exact matching, so a documentary
`transform_ref` never partial-matches `transform`. A derived variable carrying
only `transform_ref` (the ci ladder, anomaly A-02) therefore passes through
unshifted; this is correct because ci15h is observed on 0-10 with no off-by-one,
so the source already uses the target coding.

`harvested_metadata` is an optional derived-variable policy: `keep` (default)
preserves the existing restoration behavior, while `drop` withdraws the
recomputed target's previously harvested label and user-missing metadata.
Only the recomputed target's metadata is withdrawn; other columns retain
theirs. Omission or `NULL` defaults to `keep`. Every non-`NULL` value must be
a plain scalar string, exactly `keep` or `drop`, and the policy belongs only
on a `derived_variables` entry.
CV's DV09 uses `drop` because `fieldwork_month` is owned by its derivation;
an old target declaration such as "11 is missing" must not erase a newly
derived November value. This withdrawal is logged as
`TARGET_METADATA_DROPPED` only when harvested target metadata exists and is
actually withdrawn.

### `logging` (v1.0.0)

```yaml
logging:
  log_file: "merge_log.jsonl"
  report_file: "merge_report.txt"
  log_format: "jsonl"
  per_rule_fields:
    - rule_id
    - wave_id
    - variable
    - action
    - rows_affected
    - values_changed
    - distinct_before
    - distinct_after
    - na_count_before
    - na_count_after
    - timestamp
    - duration_ms
  summary_artifact:
    enabled: true
    include:
      - total_na_created
      - total_values_recoded
      - total_rows_dropped
      - total_vars_dropped
      - wave_row_counts
      - per_variable_na_rates
```

---

## Anomaly registry (`anomaly_registry.yml`)

Maps anomaly archetype codes to canonical handling templates:

```yaml
archetypes:
  WS-01:
    name: "label_whitespace"
    template: "strip_value_labels"
  SC-01:
    name: "sentinel_positive_dk"
    template: "recode_to_na"
    parameters: { codes: [999, 9999999999] }
```

---

## Implementation notes

These notes record engine behavior that the schema fields above do
not fully capture. They are documentary.

- `derive_combined_party` is dispatched in two phases. The harmonization-phase
  handler coalesces several string party sources into one field and is currently
  unused. The active handler runs in the boundary phase as a passthrough and
  collapse: rows whose `source_variable` value is in `party_names_to_pool` become
  `combined_label`, all other rows carry the source value through unchanged, and
  NA stays NA.

- Religion harmonization (cr) maps three coding eras onto one coarse 1-13 scheme.
  Era-1 code 14 maps to 13, and the Reformed family (era-1 codes 5 and 6) maps to
  3. The per-era crosswalks and target labels are documented inline in the cr
  recipe.

- Value recodes use snapshot semantics (v1.1.0). `value_recode`,
  `recode_to_na`, and the `recode` branch build every mask against the
  column's values as they stood when the rule started, so overlapping maps
  such as `{1: 2, 2: 3}` cannot chain (a 1 becomes 2 and stops; only
  original 2s become 3). `post_recode` blocks already used snapshot
  semantics in v1.0.0.

- Label and user-missing round-trip under `labelled_policy: to_numeric`
  (v1.1.0). At read time each labelled column's value labels, user-missing
  declarations (`na_values`, `na_range`), and variable label are stashed
  per wave before the column is converted to plain numeric. At write time
  a column is restored to `haven::labelled_spss` only when every
  contributing wave carried identical metadata and every observed value is
  NA, a labelled code, or a declared missing code, so cross-era recodings
  are never mislabelled. Columns that cannot be restored pass through a
  residual sweep: any cell still equal to a code its own wave declared
  user-missing becomes NA rather than leaking into the output as a
  substantive value. The sweep is per wave (a code declared missing only
  in wave A is never swept from wave B) and honors `exclude` blocks from
  recode rules, which act as a veto for the named suffix and wave cells.
  Recipes always get first claim: a recode that moves a code (for example
  cs 999 to -9) runs earlier, and the moved value is not swept.

- A rule that resolves zero target columns writes a `NO_TARGETS` entry to
  the JSONL log (v1.1.0), so a mis-keyed or mis-scoped rule is visible in
  the audit trail instead of vanishing.

---

## Validation enforcement

`validate_recipe()` performs these preflight checks. `load_recipe()` invokes it,
and `merge_liss_module()` uses that loader when given a recipe-file path.
Passing an already parsed recipe list bypasses this preflight call.

1. All required top-level sections present
2. All required `meta` fields non-empty
3. All required `global` fields present with valid enum values
4. Every `wave_index` entry has `id`, `year`, `file_pattern`
5. Every rule has non-empty `rule_id`, `action`, `description`
6. All `action` values in controlled vocabulary
7. All `anomaly_ref` match `A-NN` or null
8. All `severity` in {error, warning, info}
9. No duplicate `rule_id` within a section

Violations produce immediate errors (not warnings). The optional
`global.expected_presence` contract is not checked here; it is evaluated at merge
time, where the engine honors each entry's `on_absence` (`error` or `warn`).

### Check execution semantics (v1.1.0)

Canonical check types with phase-6 executors: `structural_missingness`,
`uniqueness`, `value_absence`, `value_in_set`, `value_present`,
`expected_presence`, `value_range`, `na_rate`, `wave_count`, `row_count`, and
`per_wave_mean`. Registered aliases, such as `assert_unique` and `range_check`,
dispatch to their canonical executor. Type recognition alone does not establish
that every requested target is meaningfully evaluated by every executor.

A failed check has `passed = FALSE`; an unknown or unevaluable check reports
`SKIP` with `passed = NA`. Error-level
failures increment `error_count`; error-level skips enter `error_skips`.
Documentary diagnostics have `passed = NA` and `documentary = TRUE` and are
counted separately (`n_doc`), rather than entering `error_skips`. Both the
execution console and text report label them `DOC`, including at error severity.
Documentary diagnostics do not assert a pass or affect strict-mode output
eligibility or `valid_for_analysis`.
The other counts are `n_pass`, `n_fail`, and `n_skip`.

With `merge_liss_module(..., strict = TRUE)`, reported error-level failures or
unevaluable checks abort before phase 7, so no output artifacts are written.
The default `strict = FALSE` writes outputs and sets `valid_for_analysis = FALSE`
when either occurs. `merge_liss_modules()` forwards strictness. This relies on
individual executors detecting failures and unresolved targets correctly.

#### `uniqueness` required keys

`uniqueness` counts rows belonging to repeated combinations of the selected
column and grouping keys. Every selected key must resolve before duplicates
are counted, including default keys. Missing keys, partial matches and malformed
active declarations are unevaluable (`passed = NA`). Duplicate observations
fail (`passed = FALSE`). Both retain the declared severity and strict/report
handling above; an unresolved subset is never treated as the complete key.

- Column precedence is `column`, `key`, `variable`, the first `key_columns`
  entry, the first `variables` entry, character `scope`, then `nomem_encr`.
  Grouping precedence is `within`, `group_by`, the second `key_columns` entry,
  the second `variables` entry, then `wave_id`. These are exact field names;
  a field such as `scope_wave` cannot supply `scope`.
- Top-level null values fall through to the next field. Selected declarations
  must contain nonblank character names, without NA values. Direct column and
  grouping fields accept nonempty vectors or flat lists of scalar names;
  `scope` accepts only a character vector. Empty, nested, dimensioned or
  noncharacter active requests are unevaluable. Fully overridden fields are
  ignored.
- Positional `key_columns` and `variables` declarations support one or two
  names. Their complete structure is validated when needed for either role,
  before selecting entries; null list elements cannot shift key positions.
  A singleton can supply the column while grouping falls through. Overridden
  entries in a valid positional declaration need not resolve. More than two
  entries are unevaluable; use `column` and `within` vectors/lists for compound
  keys instead of relying on discarded positional entries.
- Exact column names take precedence over shorthand aliases: `wave` maps to
  `wave_id`, `year` to `wave_year`, `person` and `respondent` to `nomem_encr`,
  and `household` to `nohouse_encr`. There is no suffix lookup, numeric-column
  selection or range expansion. Repeated or alias-equivalent resolved keys
  are used once. Missing-key diagnostics include the requested key scope.
- The duplicate count includes every row in a repeated group, rather than
  only excess rows. Groups of sizes three and two contribute five duplicates.
  NA and blank key values retain ordinary grouping behavior: distinct key
  combinations can pass, and repeated combinations fail. A valid zero-row
  dataset with all required columns passes with `duplicates: 0`.
- No wave filtering is applied. Fields such as `scope_wave`, `waves` and
  `wave_filter` do not restrict comparisons. The selected grouping columns
  must exist, but this check adds no separate wave-membership or nonmissing-key
  requirement.

Aliases are `assert_unique`, `n_duplicates`, `unique_key`, `no_duplicate_ids`,
`unique_per_wave`, and `assert_identifier`. All share this predicate;
`assert_identifier` does not additionally enforce nonmissing identifiers.

The bundled Income (`ci`) recipe keeps `V-05` for uniqueness within each wave
and uses `V-05_nonmissing` (`not_missing`, threshold 0, direction `below`) to
check `nomem_encr` across all rows. Both have warning severity. The missing-value
check uses `is.na()`, including actual NA, NaN and tagged NA; blank/whitespace
strings and literal `"NA"`/`"NaN"` strings are not missing values. Valid empty
data with the required columns pass both checks. A warning failure is reported
but does not independently block strict output or invalidate analysis eligibility.
The existing loader still aborts for a missing identifier column or duplicate
nonmissing identifiers within a source wave. Identifier values and the shared
`assert_identifier` predicate are unchanged; no identifier-format check is added.

#### `value_range` and `value_in_set` target and wave scopes

These checks require every requested target to resolve. An absent target,
including one missing name among otherwise resolved columns, is unevaluable
(`passed = NA`), as is a malformed or empty target declaration. Diagnostics name
unresolved targets. An observed value outside the range or allowed set fails
(`passed = FALSE`). Both outcomes preserve the declared severity and use the
strict/report-mode handling above.

- Range target keys, in precedence order, are `suffixes`, `variables`, `variable`,
  and `items`. `items` expands zero-padded ranges such as `"020-069"`.
- Shared-set target keys are `suffixes`, `variables`, `scope`, `applies_to`,
  `items`, `stems`, `variable`, and `column`. The first non-null key is used.
  Set `items` contains individual targets, without range expansion.
  `variables: [{name: "005", allowed: [1, 2]}]` supplies per-variable sets;
  `allowed_values` is also supported within each entry. Every entry must resolve.
- Exact column names take priority, followed by the existing suffix lookup
  (`s`, `stem_`, `q`, `Q`, including q/Q-prefixed aliases). Names can be supplied
  as vectors or flat lists; null entries, nested names and blank/missing names
  are unevaluable. Existing numeric suffix inputs remain supported. Shared-set
  selectors `"numeric"` and `"all_numeric"` select numeric columns; an empty
  selection is unevaluable.
- Wave keys, in precedence order, are `in_waves`, `waves`, `wave_filter`, and
  `must_be_na_in`. The first declared key is used, including an explicit `"all"`.
  Omitted scope and scalar/list `"all"` mean all rows and do not require
  `wave_id`. Otherwise supply a nonempty character vector or flat list of wave
  names. Empty/null scopes, nested lists, missing/blank names and mixing `"all"`
  with wave names are unevaluable.
- A specific wave scope requires `wave_id` with known, nonblank row membership.
  Missing wave identification or any requested wave without rows is unevaluable.
  Values outside a valid requested scope are excluded from both checks.
- Numeric all-NA range targets still pass when their bounds are valid. Set checks
  retain `allow_na` behavior (default `TRUE`). Empty data with existing target
  columns can pass an all-row check; explicitly requested absent waves remain
  unevaluable.

This contract also applies to range aliases `range_check`, `value_in_range`,
`assert_range` and set aliases `value_set`, `assert_values`. It does not change
other validators or add support for additional range/set payloads. Range checks
continue to use numeric columns and `min`/`max`; `valid_range` and sentinel
exceptions are not implemented by this executor.

#### `value_range` bounds and numeric targets

After target and wave resolution, `value_range` and its aliases parse both bounds
before iterating over target values. Malformed bounds and reversed intervals are
unevaluable (`passed = NA`), including when the other bound is violated or no
values will be compared. Severity and strict/report handling are preserved.

- Exact `min` and `max` fields supply the bounds. Omitted/null values default
  to `-Inf` and `Inf`, respectively. Unrelated fields such as `min_note` or
  `max_note` cannot supply a bound.
- Plain integer, double and numeric-character scalars are supported, named or
  unnamed, including atomic one-cell arrays and flat one-element lists of
  undimensioned primitive scalars. All are normalized with R's numeric parser
  without integer truncation. Numeric text compares numerically; scalar arrays
  work regardless of how many target values are selected. Violation diagnostics
  show the parsed numeric endpoints.
- Classed bounds (including factors, dates and custom classes), logical, raw,
  complex, empty/multiple/nested values, list arrays, dimensioned list elements,
  NA/NaN and blank/nonnumeric text are unevaluable. These restrictions apply
  inside supported wrappers too.
- The interval must satisfy `min <= max`. Ordered fractional, negative and
  signed-infinite endpoints remain supported, including equal endpoints.
  Values equal to either endpoint are in range; observed infinities use the
  same comparisons. A reversed interval is invalid even on empty or all-NA data.
- Only targets for which `is.numeric()` is true are compared; other resolved
  targets are skipped, without data coercion. NA/NaN observations are excluded.
  With valid bounds, empty all-row data, all-NA numeric targets and nonnumeric
  targets retain their existing pass behavior. Such a pass does not establish
  that any value was compared. This repair adds no data-type validation or
  support for `valid_range`, sentinel exceptions or condition filtering.

The bundled Work and Schooling (`cw`) recipe uses `min`/`max` for
`V02_wage_numeric` (0–1000000, error severity) and
`V07_pension_dates_numeric` (1990–2030, warning severity). Both intervals are
inclusive and apply after harmonization. `HR01_sentinel_recode` converts numeric
`-9`/`-8` codes to NA before validation; the range checks exclude missing values,
and `V08_no_sentinels` separately rejects residual numeric sentinels. The pension
check covers all ten declared targets across all waves; `HR03_pension_dates`
recodes `1`/`2` to `2023`/`2024` only in `cw25r`. These are numeric interval checks,
not integer-year or target-type assertions; harmonization rules are unchanged.

#### `value_in_set` allowed values and missing-value handling

After target and wave resolution, `value_in_set` and its aliases parse the global
`allow_na` flag and every active allowed set before comparing any target. Invalid
payloads are unevaluable (`passed = NA`) with the declared severity, even on empty
or all-missing data or when an earlier target violates its set.

- Shared sets use the first non-null exact field among `allowed_values`,
  `allowed`, and `values`. Per-variable entries use `allowed`, then
  `allowed_values`. Empty non-null sets remain active; omitted/null fields
  ultimately default to an empty set. Per-variable mode ignores shared sets,
  and entries do not support `values` or their own `allow_na` overrides.
- Allowed values can be plain integer, double or numeric-character vectors,
  named or unnamed, including atomic arrays of any size. Flat lists of
  undimensioned primitive scalars are supported; null entries contribute no
  members. Numeric text uses R's numeric parser. Fractional, negative and
  signed-infinite values retain their membership semantics. List members are
  parsed separately, preserving numeric precision when numbers and text are
  mixed instead of first converting the numbers to text.
- Actual NA values (including character NA and logical NA from YAML `.na`),
  numeric NaN and text parsed as NaN are supported. Logical vectors containing
  only missing values are accepted as explicit NA declarations. Nonmissing
  logical values, raw/complex/classed payloads, nested or nonscalar list elements,
  list arrays and dimensioned list elements are unevaluable. Nonmissing text
  that parses as ordinary NA, including blank strings, `"NA"` and other
  nonnumeric text, is invalid and cannot accidentally admit missing observations.
- Exact global `allow_na` defaults to `TRUE` when omitted/null. It accepts a
  plain nonmissing scalar logical, named or unnamed, an atomic one-cell logical
  array, or a flat one-element list of an undimensioned logical scalar.
  In particular, `list(FALSE)` now applies `FALSE`. Classed, numeric, character,
  missing, empty, multiple and nested flag values are unevaluable.
- Target values retain their existing numeric coercion before membership
  testing; this check does not implement string-set matching. `allow_na = TRUE`
  exempts all converted NA/NaN values. With `FALSE`, missing values must match an
  explicitly allowed NA or NaN; these are distinct members. Thus `FALSE` removes
  the exemption rather than asserting that every observation is nonmissing.
  Nonnumeric target text can still become NA under the existing data coercion.
- With valid payloads, empty all-row data pass. An empty set rejects every
  compared nonmissing numeric value; the default missing-value exemption still
  applies. Empty numeric/character/logical vectors and lists are supported.
  Conditions are not evaluated by this executor. A pass on empty or exempted
  values does not establish that any nonmissing value was compared.

#### `value_absence` target and wave scopes

`value_absence` uses the required-target contract above: missing columns or
requested waves, malformed scopes and unavailable wave membership are
unevaluable (`passed = NA`). Observed forbidden values fail (`passed = FALSE`).
Severity is preserved, so error-level outcomes block strict output and invalidate
report-mode output. Diagnostics identify the block and its scope.

- A check can declare its payload directly or provide `targets`/`checks` as a
  nonempty list of named block mappings. The first declared block key is used.
  Empty/null containers and malformed entries are unevaluable. Every block's
  inputs are resolved before any values are tested, so a missing later block
  cannot be hidden by an earlier value violation.
- Target keys and numeric selectors follow shared-set checks above. `items`
  preserves exact-name/suffix matches, then expands unresolved zero-padded
  ranges. A block inherits parent targets only when it
  omits target declarations. Explicit empty, malformed or unresolved block
  targets never trigger parent fallback.
- Ordinary parent and block wave filters intersect, using the same wave-key
  precedence and scalar/list `"all"` behavior as range/set checks. Each explicit
  requested wave must exist, even if the eventual intersection is empty.
  A valid empty intersection, or an all-row check on empty data with resolved
  targets, may pass. All-NA absence targets also retain their meaning.
- Parent `waves_allowed`, when declared, replaces ordinary parent/block wave
  filters. Forbidden values are checked outside those allowed waves. Its names
  must resolve with known wave membership; null/empty/malformed declarations
  are unevaluable. Scalar/list `"all"` permits all rows, leaving an empty
  complement that needs no `wave_id` and can pass.
- Exclusion precedence is unchanged: first non-null parent `exclude_variables`,
  parent `exclude_suffixes`, then block `exclude_variables`. Lists are not
  combined. Unknown exclusion names are optional nonmatches; empty exclusions
  do nothing and malformed names are unevaluable. Every explicit target must
  resolve before exclusions apply. Excluding every resolved column is allowed.
- Forbidden-value aliases retain their exact first-non-null precedence. Block
  `forbidden_values`, `forbidden_value`, `sentinel_values`, `codes`, or `value`
  take precedence over inherited parent `forbidden_values`, `sentinel_values`,
  or `value`. Missing/empty forbidden values provide no value constraint, but
  target and wave resolution still runs. Active payloads follow the contract
  below; other validation executors are unaffected.

The same behavior applies to `assert_absent_values`, `none_equal`,
`sentinel_absence`, `no_residual_sentinels`, `assert_no_values`,
`value_absence_check`, and `value_restriction`.

The bundled `cv` check `VC01_no_raw_dk` uses two ordered `value_absence` target
blocks at error severity, both selecting all numeric columns:

- Block 1 rejects 99 and 999, preserving the existing 27 suffix exclusions and
  `cv17i_Total`, and adding the duration suffixes `301`, `302` and `303` in one
  31-name `exclude_variables` list. The inspected cv18j-cv26r releases define
  these three columns as duration in seconds for parts 1-3, with no value
  labels or user-missing declarations. Retained 99 and 999 are legitimate
  seconds on these columns.
- Block 2 rejects -9, with the original 28 exclusions plus `_m1`, `_m2`, `_m3` and
  `242`. This additional exemption is specific to the structural -9 on the
  part-fieldwork columns and the part-2 voting-version variable; 99 and 999 on
  those four columns still fail block 1. `160` is not exempt: HR00 recodes its
  -9 to -7 before validation, so a residual -9 remains a failure. The duration
  columns also remain checked for residual -9.

The bundled `cv` check `VC02_no_raw_ref` likewise uses two ordered
`value_absence` blocks at error severity, both selecting all numeric columns:

- Block 1 rejects 998, excluding only the duration suffixes `301`, `302` and
  `303`, where retained 998 is a legitimate duration in seconds.
- Block 2 rejects -8 without exclusions, including on the duration columns.

Neither parent declares exclusion keys or `forbidden_values`; each block
provides its own values and any exclusions. This preserves the first-non-null
precedence above without a parent list masking a block's exclusions. Bare
suffixes and exact names use the existing resolution mechanism. All blocks
deliberately apply to all waves: optional exclusion names can be absent from a
wave subset, whereas an explicit wave scope requires every named wave to be
present. The exemptions therefore do not make subset runs unevaluable merely
because other waves or excluded columns are absent.

The first violation follows block order, then column order within the block.
Its reported count covers that block's forbidden values in the first offending
column, not the union of values across both blocks. Thus a retained 99/999
violation is reported before a -9 violation in an earlier column for VC01;
VC02 checks 998 before -8. Retained substantive columns continue to reject
their respective forbidden codes. Other CV checks, severity and
output-eligibility rules are unchanged.

This is a recipe-specific validation change, not a change to the engine's
exclusion semantics, transformations or serialization rules. Existing early-wave
recodes still run before these checks; the duration exemptions apply to the
values that reach validation. Genuine failed or unevaluable error-severity
checks and release-pin violations still prevent `valid_for_analysis = TRUE`
and strict-mode output. Passing these two checks alone does not establish
overall analytical validity or strict-mode success.

#### `value_absence` forbidden-value payloads

Every block's targets, wave scopes and exclusions resolve before forbidden-value
parsing. All active payloads then validate before any observations are compared.
Malformed payloads are unevaluable (`passed = NA`) with the declared severity,
including when an earlier block would fail, selected rows are empty or every
column is excluded. Existing strict/report handling applies.

- Plain integer, double, logical and character vectors are supported, named or
  unnamed, including atomic arrays of any size. Flat lists can contain
  undimensioned primitive scalars and null entries; null entries contribute no
  members. Classed, raw, complex and other nonprimitive payloads are invalid,
  as are nested, nonscalar or empty list elements, list arrays and dimensioned
  list elements.
- Omitted/null payloads and empty primitive vectors/lists impose no value
  constraint. Empty non-null declarations block lower-priority aliases and
  parent fallback. Only selected payloads validate; overridden declarations and
  unsupported fields remain ignored. An empty or null-only payload still
  bypasses target value conversion after required scope resolution.
- Numeric and character matching both apply. Numeric text can match its numeric
  equivalent; arbitrary text, blank/whitespace strings and literal `"NA"` remain
  valid character codes. Character matching is exact and case-sensitive. Negative,
  fractional and signed-infinite numeric codes remain supported.
- Existing common-type promotion is retained for character forms and logical
  values. For example, `TRUE` forbids numeric `1` and literal `"TRUE"`;
  `list(TRUE, 99)` promotes to numeric and uses character `"1"`, while
  `list(TRUE, "refused")` promotes to text and does not forbid numeric `1`.
  These forms do not introduce a separate logical-only matching rule.
- Numeric members of mixed numeric/text lists retain their original precision
  for numeric matching instead of being rounded through text. For example,
  `list(1/3, "refused")` now forbids `"0.33333333333333331"`, as a bare numeric
  `1/3` does. Existing character matches, including rounded representations of
  numeric values, are preserved.
- Actual NA/NaN observations remain excluded from both matching paths. Actual
  NA payload members do not forbid missing observations or literal `"NA"`.
  Numeric NaN retains the character form `"NaN"`, so it can forbid that literal
  text but not a numeric NaN observation. A missing-only payload still enters
  target conversion; it is not treated as an empty payload.
- Target numeric/character conversion and the union of their match results,
  violation counts and first failing block/column remain unchanged. With valid
  payloads, empty scopes, all-excluded columns and all-missing targets retain
  their existing pass behavior. Conditions and `allow_na` are not used by this
  executor; this repair does not add target-type validation.

#### `structural_missingness` target and wave scopes

Every required target and wave scope must resolve before structural values are
tested. Missing targets (including partial matches), absent requested waves,
malformed scopes and unavailable required wave membership are unevaluable
(`passed = NA`). A non-NA value in an all-NA scope, or an entirely NA target in
an expected-present wave, fails (`passed = FALSE`). Both outcomes preserve
severity and the strict/report output handling above.

- Target keys, in precedence order, are `suffixes`, `variables`, `variable`,
  and `scope`; the first non-null key is used. Exact names, suffix aliases,
  numeric suffix inputs and flat lists follow the required-target rules above.
  Structural checks do not expand item ranges or numeric-column selectors.
  Quote zero-padded suffixes such as `"002"` to preserve them through YAML.
- All-NA wave keys, in precedence order, are `waves_must_be_all_na`,
  `must_be_na_in`, `expected_na_waves`, `wave_filter`, and `waves`. The first
  declared key is used. Scalar/list `"all"` checks every row, without requiring
  `wave_id`; an existing all-NA column or a zero-row target can pass.
- `waves_expected_present` requires at least one non-NA value in each requested
  wave for every target. Scalar/list `"all"` means every observed wave and needs
  at least one observed wave. Presence scopes always require valid `wave_id`.
- Specific wave requests in either scope require each named wave to have rows
  and all row memberships to be known and nonblank. Null/empty declarations,
  nested lists, noncharacter wave names and mixing `"all"` with wave names are
  unevaluable. Values outside a valid all-NA scope are not tested for absence.
- At least one all-NA or expected-present scope must be declared. A present-only
  check imposes no absence requirement elsewhere. When both scopes are supplied,
  both apply; overlapping all-NA and presence requirements can therefore fail.
- With `waves_expected_present`, `expect_elsewhere: all_na` (or fallback
  `expect: all_na`) replaces the ordinary all-NA scope with the complement of
  the validated presence scope. An empty complement may pass, but every
  requested presence wave must still exist and contain a non-NA target value.
  This shorthand enforces presence inside the declared waves. To permit all-NA
  values inside an allowed era, declare only its explicit all-NA complement.

These semantics also apply to `structural_absence`, `all_na`,
`structural_na_count`, and `missingness_check`. They do not change the distinct
`expected_presence` failure contract below or other validation executors.

#### `value_present` target and wave scopes

`value_present` and its alias `value_present_per_wave` require at least one
matching value in any selected target within each requested wave. Different
targets can supply the match in different waves. Every requested column must
resolve before matching begins, even when another column already has a match.

- Target keys, lookup and scalar/list numeric selectors follow the shared-set
  contract above. The first non-null target key is used; every explicit target
  and each numeric selection must resolve. `items` contains literal targets,
  without range expansion. Empty or malformed targets are unevaluable.
- Wave keys, in precedence order, are `wave_filter` and `waves`. The first
  declared key is used. Omitted scope and scalar/list `"all"` select all observed
  waves. Each specifically requested wave must have rows; missing names are
  never silently dropped. Null/empty scopes, nested lists, noncharacter names
  and mixing `"all"` with wave names are unevaluable.
- Every scope, including `"all"`, requires an atomic, undimensioned `wave_id`
  column with known, nonblank row membership. With no observed waves the check
  is unevaluable. A valid wave filter excludes other waves from value matching.
- Missing inputs and malformed scopes report `passed = NA`; an observed wave
  without a matching value reports `passed = FALSE`. Both retain severity and
  use the strict/report handling above. Diagnostics identify unresolved names
  or the wave and resolved columns lacking the value.
- An all-NA target cannot supply a nonmissing numeric value, but another
  selected column may supply it. Requested values follow the payload contract
  below; target numeric coercion and the per-wave matching predicate remain
  unchanged.

#### `value_present` requested-value payloads

After required targets, waves and observed-wave membership resolve,
`value_present` and `value_present_per_wave` parse the entire active requested
value payload before matching observations. Malformed payloads are unevaluable
(`passed = NA`) with the declared severity, even when an earlier requested value
would match. The existing strict/report handling applies.

- Exact `value`, then `values`, supply the first non-null payload. A valid
  primary payload ignores the secondary field; an invalid active primary does
  not fall back. Fields such as `value_note` and `values_note` cannot supply
  requested values. Both supported fields can contain multiple values: a match
  to any requested value in any selected target suffices within each wave.
- Plain integer, double and numeric-character vectors are supported, named or
  unnamed, including atomic arrays of any size and flat lists of undimensioned
  primitive scalars. Numeric text uses R's numeric parser. Fractional, negative
  and signed-infinite requests retain their numeric membership semantics.
  Numeric members of mixed numeric/text lists retain their full precision.
- Actual NA (including character NA and logical NA from YAML `.na`), numeric
  NaN and text parsed specifically as NaN remain supported. Logical vectors
  containing only missing values are accepted as explicit NA requests. NA and
  NaN match distinctly. Nonmissing text that parses as ordinary NA, including
  blank strings, `"NA"` and other nonnumeric text, is invalid and cannot
  accidentally match missing observations.
- Nonmissing logical, raw, complex and classed payloads, nested/nonscalar/empty
  list elements, list arrays and dimensioned list elements are unevaluable.
  Null list members also remain invalid, including YAML `[null]`; they are
  not dropped as in the `value_in_set` allowed-set parser.
- Omitted/all-null payload fields and empty numeric/character/logical vectors
  or lists retain empty-request semantics. An empty non-null primary remains
  active. No observation can match an empty request, so an otherwise evaluable
  check fails on the first observed wave. Zero-row data remain unevaluable
  because this executor requires an observed wave, regardless of the payload.
- Target values still undergo numeric coercion before matching. An explicit NA
  request can match nonnumeric target text converted to NA. This executor does
  not provide string matching, evaluate conditions or use `allow_na`. This
  repair validates requested values without adding target-type validation.

#### `na_rate` targets, wave scopes and rate payloads

`na_rate` requires every requested target and wave to resolve before a condition
is applied. Missing targets (including partial matches), absent requested waves,
malformed scopes, malformed active rate payloads, and unavailable required wave
membership are unevaluable (`passed = NA`). A measured rate outside the threshold
fails (`passed = FALSE`). Both outcomes preserve severity and the strict/report
handling above.

- Target keys, in precedence order, are `suffixes`, `variables`, `scope`, and
  `items`; the first non-null key is used. Names use the existing exact/suffix
  lookup, including numeric suffix inputs and flat lists. `items` expands
  zero-padded ranges before column lookup, even if a literal range-like column
  exists. Other target keys retain literal names. Numeric-column selectors are
  not expanded by this executor. Empty or malformed targets are unevaluable.
- Wave keys, in precedence order, are `waves` and `wave_filter`; the first
  declared key is used. Omitted scope and scalar/list `"all"` select all rows
  without requiring `wave_id`. Specific requests require every named wave to
  have rows and atomic, undimensioned `wave_id` with known, nonblank membership.
  Null/empty scopes, nested lists, noncharacter names and mixing `"all"` with
  wave names are unevaluable.
- Conditions use the existing restricted evaluator and intersect with the wave
  filter. False or NA condition results exclude rows. Invalid conditions,
  unresolved condition references or results of the wrong type/length remain
  unevaluable. An omitted, null or empty-string condition applies no filter.
- A valid condition selecting no rows, or empty all-row data with resolved
  targets, retains its pass result. The diagnostic explicitly says that no rows
  were eligible and no NA rate was calculated. All required targets and waves
  must resolve first, and the active threshold and direction must be valid;
  an empty selection cannot excuse missing inputs or malformed payloads.
- Rates are calculated separately for each target over the pooled selected rows,
  not separately per wave. Every target must meet the threshold. An all-NA target
  has rate 1; a nonmissing target has rate 0. `above` uses `>=` and `below` uses
  `<=`, including equality at the threshold.
- Threshold precedence is the first non-null exact field `threshold`, then
  `max_rate`, then the default: 0 for `not_missing`, 1 otherwise. A malformed
  active value is unevaluable without falling through; an overridden `max_rate`
  is ignored. Unrelated fields such as `threshold_note` cannot supply a limit.
- The active threshold is parsed numerically before condition filtering or rate
  comparisons. Plain integer, double and numeric-character scalars are supported,
  named or unnamed, including atomic one-cell arrays and flat one-element lists
  containing an undimensioned primitive scalar. Numeric text uses R's numeric
  parser, without integer truncation. Fractional, negative, outside-0..1 and
  signed-infinite thresholds retain their numeric predicates; no probability
  interval restriction is imposed. Violation details show the parsed threshold.
- Classed thresholds (including factors, dates and custom classes), logical,
  raw or complex values, empty/multiple/nested values, list arrays, dimensioned
  list elements, NA/NaN and blank/nonnumeric text are unevaluable. The same
  restrictions apply inside supported wrappers.
- The exact `direction` field must contain `above` or `below`, case-sensitive
  and without surrounding whitespace. Plain character scalars, named or
  unnamed, atomic one-cell character arrays and flat one-element lists of
  undimensioned character scalars are supported. Classed, noncharacter,
  missing, blank, unknown, multiple, nested and unsupported dimensioned payloads
  are unevaluable. Unknown directions no longer silently use `below`.
- Both selected payloads are validated after target/wave resolution and before
  condition filtering or any observed comparison. A failing rate, invalid
  condition or empty selection cannot bypass payload validation. Unevaluable
  declarations retain severity and the strict/report behavior above.

Aliases are `na_rate_check`, `na_rate_above`, `na_rate_below`, and `not_missing`.
`na_rate_above` defaults to direction `above`; the others default to `below`.
An explicit valid direction overrides that default; omitted/null direction
retains it. The condition evaluator and other validation payload families are
unchanged. Passing these checks does not establish analytical validity.

#### `wave_count` required inputs and counting modes

`wave_count` and its alias `n_distinct_wave` require the columns used by the
selected mode to exist, including on empty data. Missing inputs, malformed
active bounds or person-key requests and unsupported column shapes are unevaluable
(`passed = NA`). Observed count violations fail (`passed = FALSE`). Both retain
the declared severity and strict/report handling above.

- A non-null `expected` selects the global distinct-wave mode. Only `wave_id`
  is required; person-key fields and `max_waves` are ignored, even when malformed.
  The count uses `dplyr::n_distinct(wave_id)` and compares with the parsed bound
  converted by `as.integer()`. Fractional expected counts still truncate toward
  zero; negative counts are not separately prohibited. The conversion must yield
  a nonmissing integer, so infinite and out-of-range expected counts are
  unevaluable. A valid empty dataset has count 0.
- Otherwise, the check limits distinct waves per person. Person-key precedence
  is `column`, then `key`, then `nomem_encr`, using the first non-null value.
  The key must be one nonblank character column name. Empty, NA, list,
  dimensioned, noncharacter or multiple-name requests are unevaluable. Names
  attached to a scalar key cannot rename its grouping column.
- Person keys use exact column lookup, without shorthand aliases, suffix lookup,
  numeric selectors, range expansion or compound-key selection. Both `wave_id`
  and the selected key must exist. Only required columns are checked for atomic,
  undimensioned values; list and matrix columns are unevaluable.
- NA, blank and whitespace values retain their existing distinct-value and
  grouping behavior. NA waves are counted, and NA person keys form a group.
  Values are not converted to character before counting; factors and numeric
  identifiers retain their counting behavior. Repeated observations of the
  same person-wave combination do not increase the distinct-wave count.
- Per-person mode compares the largest count with `max_waves` using `<=`;
  its omitted/null default is `Inf`. Fractional and negative limits and both
  signed infinities remain supported. With no observed persons, the empty-set
  comparison uses `-Inf` without calculating a maximum or emitting a warning.
  The detail states `no observed persons; no per-person wave counts calculated`.
  This is not a measured zero count; required inputs must still resolve and the
  active bound must be valid.
- Both bounds accept a plain integer, double or numeric-character scalar,
  optionally in a one-element flat list. Names on a scalar do not affect its
  value. Character values use R's numeric parser before comparison, so `"10"`
  means ten rather than a text-ordering limit. `expected` additionally accepts
  an atomic one-cell array (including a matrix); `max_waves` does not accept
  arrays. A list's element must be an undimensioned scalar, not another container.
  Empty/multiple values, classed objects (including factors and dates), logical,
  raw and complex values, NA/NaN, blank strings and nonnumeric text are
  unevaluable. These restrictions also apply inside accepted wrappers.
- Payload fields use exact names. Fields such as `expected_wave` or
  `column_note` cannot supply `expected` or `column`. No wave filters are
  applied; `waves`, `wave_filter` and `scope_wave` do not restrict counts.
- Only the active bound is validated, with a diagnostic naming `expected` or
  `max_waves` on malformed input. Malformed `expected` never falls back to
  per-person mode. This bound contract does not change other check types or
  impose nonmissing-value requirements on the counted identifiers.

#### `row_count` required wave scope

`row_count` and its alias `assert_row_count_range` compare the number of rows
with inclusive `min_rows` and `max_rows` bounds (defaults 0 and `Inf`). An
observed count outside the bounds fails (`passed = FALSE`). Missing required
wave inputs, malformed scopes or invalid bounds are unevaluable (`passed = NA`).
Both retain the declared severity and strict/report handling above.

- An omitted or null `wave` counts all rows with `nrow(df)`. No columns are
  required, including `wave_id`; unrelated missing or malformed wave identifiers
  do not affect this global count. Empty data have a legitimate count of zero.
- A non-null `wave` selects one exact wave identifier. Supply one nonmissing,
  undimensioned atomic value with a nonblank character representation. Scalar
  character, numeric and factor values retain character conversion for matching;
  attached scalar names are ignored. Empty, NA/NaN, blank, list, dimensioned and
  multiple-value requests are unevaluable, not recycled comparisons or wave sets.
- Scoped counts require an exact `wave_id` column with atomic, undimensioned
  values. Every row must have known, nonblank membership; NA/NaN identifiers are
  unevaluable even when other rows match. Identifiers are converted to character
  for exact, case-sensitive matching; whitespace is not trimmed for matching.
  The requested wave must contain at least one row. An absent wave, including
  on empty data, is unevaluable rather than a measured zero.
- Rows outside a valid requested wave do not contribute to its count. The
  literal `wave: all` selects a wave actually named `all`; it is not a global
  selector. No plural/list wave filters are added. Fields such as `waves`,
  `wave_filter`, `in_waves` and `scope_wave` do not restrict counts.
- `wave`, `min_rows` and `max_rows` use exact field names; fields such as
  `wave_note` and `min_rows_note` cannot supply those values. Null bounds use
  their defaults. After wave resolution, both bounds are parsed before comparing
  the count, so a failed comparison cannot conceal a malformed other bound.
- Bounds accept a plain integer, double or numeric-character scalar, named or
  unnamed, including an atomic one-cell array (such as a matrix). A one-element
  flat list containing an undimensioned scalar is supported for either bound.
  Classed objects (including factors/dates), logical, raw or complex values,
  empty/multiple values, nested or dimensioned lists, list-wrapped arrays,
  NA/NaN and blank/nonnumeric strings are unevaluable. The restrictions also
  apply inside wrappers. Character values use R's numeric parser, so `"10"`
  means ten rather than a text-ordering limit.
- Fractional and negative bounds and both signed infinities retain numeric
  comparison without integer truncation. `min_rows > max_rows` is an invalid
  declaration, reporting unevaluable instead of an observed count failure.
  Otherwise an interval is valid even if no integer count can satisfy it;
  for example, `1.2..1.8`, `Inf..Inf` and `-Inf..-Inf` fail for finite counts.
  Bounds are validated on empty global data too. Malformed-input diagnostics
  name the affected field; reversed-bound diagnostics name both fields.
- Count diagnostics display the parsed numeric endpoints, including the sign
  of infinity. Scalar lists are normalized for both endpoints; unlike earlier
  versions, a maximum list does not fail during report formatting. This contract
  applies only to this executor, without changing wave selection or other checks.

The distinct documentary types `row_count_match` and `row_count_sum` remain
documentary diagnostics and do not dispatch to this executor.

#### `per_wave_mean` required targets and wave membership

`per_wave_mean` requires every selected target to resolve before comparing
means. Missing targets, including partial matches, malformed requests and
unavailable wave membership or invalid bounds are unevaluable (`passed = NA`).
An observed finite mean outside the bounds fails (`passed = FALSE`). Both outcomes retain the
declared severity and the strict/report handling above.

- Target keys, in precedence order, are `suffixes`, `variables`, `scope`,
  `applies_to`, `items`, `stems`, `variable`, and `column`; the first non-null
  key is used. Existing exact/suffix lookup accepts numeric suffix inputs and
  flat lists. Every target must resolve; empty or malformed requests are
  unevaluable. `items` uses literal names, without range expansion.
- Scalar character `"numeric"` and `"all_numeric"` select numeric columns and
  require at least one such column. List-wrapped forms retain their existing
  literal-name meaning, so `items: [numeric]` requests a column named `numeric`.
- Every row requires known, nonblank membership in an atomic, undimensioned
  `wave_id` column. Numeric and factor wave identifiers retain their character
  representation for grouping. This executor checks every observed wave and
  has no wave-filter support; `waves`, `wave_filter` and other wave-scope fields
  do not restrict its comparisons.
- Resolved targets and a valid zero-row `wave_id` column retain a pass result
  on empty data when the bounds are valid, with the diagnostic
  `no observed waves; no means calculated`. Empty data cannot excuse missing
  targets, a missing `wave_id` column or invalid bounds.
- Means use the existing numeric coercion and `na.rm = TRUE`, separately for
  every target in every observed wave. Every finite mean must be within the
  inclusive bounds, `min_mean` (default `-Inf`) and `max_mean` (default `Inf`).
  Values are not pooled across waves or targets.
- Bounds use exact field names; fields such as `min_mean_note` and
  `max_mean_note` do not supply limits. Omitted/null bounds use their defaults.
  After targets and wave membership resolve, both bounds are parsed before
  iterating over means, even when no finite means are available for comparison.
- Each bound accepts a plain integer, double or numeric-character scalar, named
  or unnamed, including an atomic one-cell array or a flat one-element list of
  an undimensioned scalar. Classed objects (including factors/dates), logical,
  raw or complex values, empty/multiple values, nested or dimensioned lists,
  list-wrapped arrays, NA/NaN and blank/nonnumeric strings are unevaluable.
  Restrictions also apply inside wrappers. Strings use R's numeric parser
  before comparison. Negative/fractional bounds and both signed infinities are
  supported; no integer truncation is applied.
- `min_mean > max_mean` is an invalid declaration, rather than an observed
  mean violation. Malformed-input diagnostics name the affected field;
  reversed-bound diagnostics name both. Violation details show the parsed
  numeric endpoints. A violation at one endpoint cannot conceal a malformed
  other endpoint.
- Non-finite means, including all-NA, NaN and infinite means, retain their
  existing exclusion from valid bound comparisons. If the check passes, its detail
  reports the number of these means and the first affected column and wave.
  A finite bound violation still fails even when other means are non-finite.

This check has no registered type aliases. Numeric coercion of the data and
non-finite-mean handling remain unchanged; these bound requirements apply only
to this executor.

#### `expected_presence` validation checks

This phase-6 check is separate from `global.expected_presence`; it does not
create columns or use `on_absence`.

```yaml
validation_checks:
  - check_id: "required_measure"
    type: "expected_presence"
    severity: "error"
    variable: "s005"
    waves: [ch07a, ch08b]  # or the scalar "all"
```

- `variable` must be one exact, nonblank character column name. There is no
  suffix lookup or alias for this check type.
- `waves` must be an explicit, nonempty vector or flat list of nonblank wave
  names, or `"all"` for all observed waves. Omitted/empty scopes, null entries,
  nested lists, noncharacter names and mixing `"all"` with wave names are
  unevaluable. These payload requirements are enforced during phase 6.
- A missing column, a requested wave with no rows, or a requested wave whose
  target values are all `NA` fails. Otherwise each requested wave must have at
  least one non-NA target value. `"all"` fails on an empty dataset.
- Wave identification uses `wave_id`. If it is absent, or any row has a missing
  or blank wave identifier, a check whose target column exists is unevaluable.
  Rows with unknown membership are not silently treated as outside the scope.
- Failures and unevaluable results include diagnostic details in returned
  validation results and text reports. Severity is preserved. `assert_presence`
  and `presence_matrix` remain separate documentary types.
## Scalar source nonresponse contracts, opt-in

The optional top-level `source_nonresponse` block has `schema_version: "1.0.0"`
and one `contracts` entry. Its exact reviewed CI rule is declared in
`ci_merge_recipe.yml`: `CI23P339_N01`, version `1.0.0`, binds module `ci`, wave
`ci23p`, language `EN`, `ci23p_EN_1.0p.sav` and its SHA256 to raw field
`ci23p339`, canonical field `s339`, exact double `-9999999999`, household net
income, euros, measurement year 2022. It separately records the codebook
filename and SHA256. Unknown keys, additional contracts, changed identity,
field/code scope, or policy fail validation. The first slice accepts only
`source_nonresponse_encoding`, `nonresponse_unknown`, `missing_amount`, and
`no_numeric_proposal: true`; its unresolved subtype is not inferred from the
last digit. This block is separate from the numerical action vocabulary.

The recipe block is a selector and mirror, not an approval mechanism.
`schema/source_nonresponse_registry.yml` is package-owned, versioned reviewed
data; trusted package code pins its SHA256 and compares the complete typed
recipe record with its approved contract. The installed schema/registry version 2.0.0 preserves the existing CI23p339
record under `records` and adds only the two approved taxable records under
`fields`. The installed CI recipe and `recipe = NULL` remain on schema 1.0.0
and legacy CI23p339. Missing or corrupt registry data, unsupported
versions, duplicate or overlapping records, and extra or changed recipe keys
block guarded work. A caller-supplied path, digest or reviewed flag cannot
approve a new contract. No working-directory lookup or network retrieval is
used. Registry approval, classification validity and named consumer approval
are separate checks. The digest constrains integrity and scope; it is not a
signature or an authentication claim.

`merge_liss_module(..., source_nonresponse = TRUE, output_dir = NULL)` captures
the reviewed source before binding, prefix stripping, metadata conversion or
recipe transformations. The default remains opt-out. The matched analysis
amounts become numeric missing values; originals, original field metadata,
source row keys and typed reasons remain in the result's separate carrier.
Each matched-cell ledger entry also records source release/language, raw
storage type and lossless code representation, the value before analysis and
its missing analysis disposition, prior system/source-declared missingness,
rule version, source metadata identity and codebook field reference. Original
field metadata are stored once and covered by the carrier's digest.
The final guard checks the source-to-target mapping after metadata restoration
and sweeping. Existing `valid_for_analysis` semantics remain unchanged;
`source_nonresponse_readiness` is scoped separately to this one reviewed field.

`liss_classify_source_nonresponse()` provides a strict explicit binding for a
raw frame or legacy merged frame. A raw binding requires `ci23p339`; a merged
binding requires one selected target, an exact wave-ID column, unique nominal
`nomem_encr` keys, and unchanged selected values against the actual source.
Years, suffixes, filenames alone and old aggregate provenance cannot establish
binding. Default cleaner aliases are unchanged. Sorting and subsets retain
source-cell identity only when column metadata and the complete carrier are
retained, as with supported tibble/dplyr operations. Base data-frame operations
that strip target attributes fail closed; metadata checks are not relaxed.
Changed values, keys, wave/target mapping, metadata or
carrier digest block quantitative use. Original values and metadata remain
available even when matched analysis values are already missing.
For a merged binding, every presented row needs a nonmissing, nonempty exact
wave identifier, including rows outside CI23p. A full-join panel with CI-absent rows lacking
CI wave identity is unsupported, including for the target `ci_s339`.
Retain the complete classifier wrapper when cleaning: the classifier does not
add a frame marker to its unchanged `$data`, so extracting and cleaning an
unmarked frame alone enters ordinary unguarded processing. Preserve the
original merge result separately when rebinding; its provenance, log and validation are not forwarded into the
classifier wrapper.
The sealed one-field input receipt covers every presented row's respondent
key, exact wave and target value, including rows outside CI23p. Validation
checks this complete receipt before selecting source scope, so moving a
classified or ordinary row to another wave cannot escape the guard. The
controlled merge finalization validates source-selected lineage and values,
then captures the trusted pipeline's final metadata and complete input keys;
ordinary cleaner validation never refreshes that receipt from changed data.
Readiness uses `passed`, `blocked`, and `not_evaluated`: absent scope is not a
pass, and a binding failure raises `liss_source_nonresponse_blocked` with the
blocked readiness record. The public binder can repeat an unchanged raw
binding. Already normalized missing values require their intact carrier;
rebinding them against the source without that carrier fails value agreement.

Flag mode preserves the actual supplied target, including normalized missing
values from a guarded merge. Verified source originals are recovered only in
the `_observed` audit column and source ledger; matched proposals remain
missing. Applied modes return missing matched analysis values while retaining
the source audit. Readiness covers this source/field contract, not every audit
column's suitability for quantitative analysis. A source-bound household
amount cannot also be assigned as the cleaner's `category_code`.
Before numerical preparation, the household cleaner separately checks that
the validated approved rule permits its consumer, household net income,
euros, measurement year 2022 and the selected target binding. Renaming a
column does not add consumer approval to an intact source contract. This
does not reconstruct field origin after all carrier markers have been
removed; such bare frames retain the documented legacy boundary.

The legacy one-field carrier remains schema version `1.0.0`, including its original
recipe rule object and key order. Registry metadata is resolved outside that
carrier. Valid R 4.6.1 reference-profile RDS carriers are validated without
migration or registry-driven resealing;
source, target, metadata, complete-row and digest checks remain in force.
Serialized object hashes now use the explicit identity profile `1.0.0`:
SHA-256 over XDR version-2 R serialization, with only the four writer-version
header bytes fixed to R 4.6.1. Format and minimum-reader words, every payload
byte, types, encodings, attributes and their order remain significant. Raw SAV,
codebook and policy-file hashes are unchanged. Direct R identity and serialized
payload equality checks remain separate; equal digests are not reference identity.
The profile preserves the verified R 4.6.1 reference representation. It does not
infer a writer for an arbitrary old carrier, normalize changing object bodies,
or promise compatibility with every R/package version. Other legacy profiles
require their original package/runtime; there is no permissive digest fallback.

New applied computation identities use schema `2.0.0`, include the serialization
profile, and fingerprint all identity helpers with the existing computation.
Reference-profile schema-1 computation records remain structurally readable.
They do not become the new computation identity: replay requires exact recorded
code, runtime, platform and dependency identity, so a changed computation rejects
legacy replay even within the same R version. Numerical methods are unchanged.

The carrier uses version-2 R serialization with SHA256 for integrity and
preserves typed reasons and exact doubles in an RDS round trip. The digest is
an integrity check, not an authentication claim. Automatic SAV/CSV lineage
round trips and automatic panel-lineage propagation are unsupported in this
slice. Guarded merge file exports are blocked; use the in-memory result/RDS,
or explicitly rebind an imported frame to the verified original source.
A schema 2.0.0 recipe uses exactly `source_nonresponse: {schema_version:
"2.0.0", field: <one complete reviewed field record>}`. The full typed mirror
selects either `CI23P337_FIELD` or `CI23P338_FIELD`, version 1.0.0, never both.
Mapping key order may vary and is preserved in the carrier; cases retain the
approved N01/N02 sequence. Each field owns exact codes `-9999999999.0` and
`-9999999998.0`, stable case IDs, source release 1.0p, EUR/2022, targets and
metadata policy. Aliases cannot select or approve a source field. Taxable
bindings allow only the selected raw name, canonical `s337`/`s338`, or
`ci_s337`/`ci_s338`; household aliases never belong to these fields.

Taxable carriers have `schema_version = "2.0.0"` and one `field`, source receipt,
coverage table, complete input-row receipt, cell ledger, metadata and digest.
Coverage counts selected rows once; each classified cell has one ledger row
and its actual case ID. Readiness includes field identity and named per-case
counts; `unresolved_cells` concerns failed binding/classification, not the
known-unknown answer subtype. All classified answers remain
`nonresponse_unknown`, with `missing_amount` and `no_numeric_proposal = TRUE`.
Original `na_values`/`na_range` evidence is evaluated separately for each code;
original attributes are retained without inventing missing declarations.
Taxable routing is null and 226/229 cannot resolve a subtype. No taxable
analytical consumer is approved.

Standalone classification retains unchanged values/metadata and
`normalized = FALSE`; it is an audit, not excluded analysis amounts. A scalar
guarded merge sets only the selected field's exact cases missing and returns
`normalized = TRUE`. Final validation rejects finite classified values,
unexpected mappings and metadata changes. Legitimate signed/zero amounts,
near codes, system missingness and unrelated brackets retain their existing
behavior. Separate 337/338 calls are required; their counts cannot be called
a respondent union or combined normalized result.

Before a tax binding unwraps any input, it rejects guarded wrappers, frame
markers, carriers, guarded classes and claimed source readiness, including
same-field rebinding. Every classifier binding, including legacy 339, rejects
inputs carrying or claiming version-2 lineage. Validation, metadata-preserving
subsets, reordering and RDS remain supported without a destructive rebind.
The household cleaner rejects taxable carriers before target resolution,
Background/P01 or numeric preparation under all modes and rule overrides.
Renaming to a household alias cannot grant consumer approval. These checks
do not recover origin after all lineage markers are stripped from a bare frame,
or authenticate manually forged/resealed R objects.

Other CI fields, CA20g, CH, taxable numerical cleaning, subtype resolution
and broader export sidecars remain outside this scope.


## Joint taxable collection, recipe and carrier schema 3.0.0

A schema-3 source_nonresponse block contains only `schema_version: "3.0.0"`
and `fields`, an unnamed list of exactly the complete approved CI23P337_FIELD
and CI23P338_FIELD mirrors, each once. Either selection order is accepted and
retained. Registry 2.0.0 and its approvals/pin stay unchanged; no 339 field record
is inferred or migrated. The installed CI recipe stays schema 1.0.0. The scalar
classifier explicitly rejects schema 3 before scalar lookup. Guarded module
merging is the sole new producer; guard-off/default behavior is unchanged.

Both original fields, complete values/metadata and common source/row/key bindings
must pass before any normalization. Both exact case sets are then made missing
before transformations. Expected metadata is pinned after restoration/sweep and
before sanitization for both canonical targets. Finalize and validate both leaves
and their consistency before attaching any collection. A second-field preflight
or finalizer failure returns no partial guard or guarded file. Four actual case
IDs retain their own logs; aggregate counts explicitly count field-cells.

The outer carrier has exactly schema_version, kind, source, fields, normalized,
readiness and digest. Schema is 3.0.0, kind is source_nonresponse_collection,
and normalized is TRUE. Fields is a named list of two complete sealed schema-2
states keyed by field IDs in selection order, retaining each approved record/key
order, source, bindings, coverage, complete input receipt, typed ledger, metadata,
readiness and digest. Common source and raw/canonical/id/wave bindings must agree.
The outer digest binds all nested bytes and order; it is not authentication.
A collection-specific result class inherits the source-result base; its frame
marker carries the identical complete outer state.

Collection readiness includes schema/kind, status, scoped description,
evaluated/classified/unresolved field-cell totals and named full leaf readiness.
Present valid fields pass even without exact matches. The shared wave entirely
absent has no carrier and not_evaluated readiness with two zero-count entries.
Missing a required field in a present source blocks. Unknown subtype differs
from unresolved binding/classification. Full collection validation supports
metadata-preserving subsets/reorder/RDS with returned-state resealing, retaining
the saved input. Unknown versions, naked/incomplete leaves and field replacement
or drop are unsupported. Scalar v1/v2 receipts are not implicitly migrated.

Every scalar rebind rejects existing/claimed collection lineage before unwrap,
including wrapper/frame disagreement, absent carrier, readiness and collection
class alone. The household cleaner rejects schema-3 collections before target/rules,
Background/P01 or numeric work under every mode/override. No 339 projection or
approval exists; unselected 339 is not covered. Report/panel/export restrictions
and the honest all-markers-stripped bare-frame boundary remain. Household
projection, CA20g/CH and wider lineage are separate later units.


## Three-field collection, recipe and carrier schema 4.0.0

Schema 4 selects exactly CI23P337_FIELD, CI23P338_FIELD and CI23P339_FIELD through
an unnamed `fields` list. Each selector has only `field_id` and `scalar`, the
complete existing scalar source_nonresponse block. Taxable members carry schema
2 plus their complete approved `field`; the household member carries schema 1
plus `contracts: [<complete CI23P339_N01 rule>]`. All six selection orders are
accepted and retained. Typed mirrors, IDs, native schemas and adapter membership
must agree. Changed, missing, additional, duplicate or ambiguous selectors block.
The public classifier remains scalar-only and rejects schema 4 before source reads.

A separate package-owned `source_nonresponse_collection_adapter.yml` has its own
trusted-code digest pin and same-buffer hash/parse path. It binds the collection
slot CI23P339_FIELD explicitly to the intact existing CI23P339_N01 authority;
337/338 bind to their existing field IDs. Its consumer list is empty. Registry
schema/version 2.0.0 and its bytes/pin remain unchanged and still own source,
bindings, metadata, construct, period, codes, routing and scalar approvals.
The new adapter is loaded only for schema 4; a missing/corrupt adapter blocks
that path without changing old scalar 1/2 or pair 3 authority. No caller hash,
path, flag, alias or suffix approves scope. No household scalar-v2 field or
additional household encoding case is introduced.

All three original receipts and every normalization lock pass against unchanged
source input before any selected raw value is replaced. All three canonical
metadata are pinned after restoration/sweep and before name sanitization.
Complete selected-wave source key sets, values and metadata must reconcile for
every member. All three finalizers and full outer validation pass before the
identical carrier is attached to wrapper/frame; failure returns no partial
collection, passed aggregate or guarded file. Household-first order cannot use
legacy scalar-v1 rebinding permission to discard existing guarded lineage.

The outer has exactly schema_version, kind, source, fields, normalized, readiness
and digest. Version is 4.0.0, kind source_nonresponse_collection, normalized TRUE.
Fields names are the three adapter IDs in caller order. Taxable leaves remain
complete sealed schema-2 states; the household leaf remains a complete sealed
schema-1 state with its native rule, ledger and readiness. Old states are not
migrated or supplemented to resemble another schema. Shared source, key, wave,
ID bindings and ID metadata agree. The digest binds every nested byte and order;
it checks integrity, not authentication. Outer validation explicitly dispatches
native leaves and verifies complete shapes/digests/receipt/ledger invariants.

At the new v4 boundary, household target metadata must equal the pinned restored
metadata before/after native-v1 finalization, and every normalized protected
household value must be NA during finalization and later validation. These
checks do not change scalar-v1's historical behavior. Household ledger candidates
retain original 229 producer evidence and valid candidate/evidence pairs. V1 has
no separate original 229 vector; coverage alone cannot reconstruct routing or
establish authority after manual resealing. The original producer and fresh raw
comparison establish the observed routing; all authoritative subtypes stay unknown.

Readiness retains three complete native leaf records, named by adapter ID, and
sums evaluated/classified/unresolved field-cell counts. V1 keeps its actual rule
ID; v2 keeps field identity and case counts. These are not respondent counts.
Present valid zero-match fields pass. An entirely absent shared wave returns no
carrier and three zero-count not_evaluated entries; any present missing member
blocks. Metadata-preserving subsets/reordering/RDS validate all leaves and reseal
returned state without changing saved inputs. Unknown versions, incomplete leaf
lists, replaced/dropped members, contradictory markers and metadata loss block.

Scalar approval and source readiness do not approve a collection consumer or
taxable predictor/donor roles. A separate pinned policy permits the closed
household flag operation below. The source collection is not a transformed-output
receipt for ordinary household corrections. Scalar rebinding rejects every
existing/claimed collection, including schema-only/class-only/readiness claims,
before unwrapping can discard it. Report/panel/export restrictions stay; guarded
output_dir remains NULL. All-markers-stripped bare frames retain the existing
unguarded boundary. Default/v1, taxable/v2 and exact-pair/v3 meanings remain
unchanged. Applied correction uses the separate result contract below; CA20g/CH
and broader lineage require separate contracts.

## Household flag consumer of the complete version-4 collection

`source_nonresponse_collection_consumers.yml` is a separately pinned, closed
package-owned policy. It references the unchanged registry2 and adapter1 hashes
and grants only CI23P339_V4_FLAG to `liss_clean_income()`. The adapter's empty
consumer list grants nothing by itself. The consumer resource is required only
for this new operation and its complete-result validation; old merges and scalar
consumers remain independent. Caller paths, hashes, flags and aliases cannot
approve new scope. Hashing and parsing use the same resource bytes.

The caller must explicitly select `mode = "flag"` and
`variables = list(target = "s339")` on a complete validated version-4 wrapper or
attributed frame. The sealed CI23P339_FIELD leaf supplies its native lock and
observed values internally; it never replaces the outer carrier. Complete v4
validation runs before rules/background preparation, after P01 and on output.
All three bound amount targets are denied in every non-target resolved role,
including identifiers, household grouping, personal income, household size,
category/bounds, donor keys, age and birth year, before and after P01. Correct
and na_only require the distinct applied policy below; tax targets and previously
produced consumer wrappers block.

Background must be NULL or an ordinary unguarded frame. Any guarded/claimed
background blocks before conversion, even when P01 is disabled. Reserved P01
helper columns and existing household observed/proposed/status/dataset audit
columns block before preparation; audit names are checked again after P01.
Every original column retains its position, values and attributes, with the same
rows and order. P01 may append context columns. Classified339 proposals stay
missing with source_nonresponse:missing_amount status; original codes appear
only in observed/audit values. Ordinary proposals retain existing flag rules.

The result and frame retain the complete native v2/v2/v1 carrier and full source
readiness. Supported source-input subsets/reorder/RDS include empty and
other-wave-only bound inputs: their native339 exclusion readiness is
not_evaluated with zero counts and locks. Ordinary other-wave flag simulation
does not promote source readiness. An absent-wave result without a bound carrier
is ineligible. No public scalar projection or declassification API is added.

The complete wrapper has class `liss_clean_collection_flag_result` and a separate
`source_nonresponse_consumer_readiness` record, schema1. It identifies the pinned
capability, full outer carrier digest and household339 source counts. Its scoped
status is passed only when household339 cells were evaluated. The frame has no
consumer record or consumer-result class; `$data` validation establishes source
lineage alone, not proposal or decision integrity.

The consumer result digest binds complete data and attributes, decisions,
effective ruleset, resolved variables, mode and summary except duration_s, using
R serialization version2. The entire log is outside this proof. A separate
record digest binds the closed typed consumer record. Consumer claims dispatch
before scalar/version validation, so replacing the complete carrier by a scalar
leaf cannot bypass validation. Private validation also reconstructs observed
values and checks every locked proposal/status. Complete consumer RDS roundtrips
validate; post-consumer subsets/reorder and repeated cleaning are not resealed.
Supply a supported source subset first and run a fresh flag simulation instead.

These digests check integrity, not authenticated origin, numerical validity or
manually resealed forgery. Copied/derived unmarked columns have no inferable
source ownership. All guarded report/panel/SAV/CSV exports and rebinding remain
denied, including new consumer class/record-only claims after source slots are
removed. Preserve the complete result with RDS. This FLAG policy adds no taxable
numerical consumer, applied correction, second household encoding case or CA20g/CH
scope.

## Applied household result with an immutable version-4 source input

`source_nonresponse_collection_applied.yml` is a distinct closed, digest-pinned
package-owned authority. It grants the explicit `CI23P339_V4_APPLIED` capability
to `liss_clean_income()` for `mode = "correct"` or `"na_only"`, an exact `s339`
target and an intact native version-4 collection. Both `background` and
`output_dir` must be NULL. The registry, adapter, FLAG policy and all native
carrier versions retain their previous bytes and meanings.

The complete result has class `liss_clean_collection_applied_result` followed by
`liss_clean_result`. It contains the ordinary data, decisions, log, summary,
ruleset, variables and mode plus `source_input`, captured `invocation` and
`applied_lineage`. The exact accepted native wrapper or attributed frame stays
unchanged in `source_input`, including all three leaves and original raw metadata.
Changed analysis data carry a distinct applied marker and frame class, never a
native source receipt. Both tax vectors and all other original non-target columns
retain values, types, attributes, column position, row identities and order.
Sealed amounts remain prohibited in every secondary role before numerical work.

Source339 locks and observed recovery derive from native parent validation.
Locked amounts stay NA with source_nonresponse:missing_amount status, including
under rule overrides. Checked scoped source readiness is recorded separately
from analysis execution status and any broader receipt retained in the original
parent. Nonempty other-wave analysis does not turn zero CI23p source evaluation
into passed source evidence. Empty analysis is not_evaluated.

Raw-source attributes in native leaves, bound normalized input target metadata
and derived target metadata are recorded separately. Applied target assembly
uses the existing numeric cleaner and retains the variable label; it does not
silently claim that all raw SPSS missing-value metadata are unchanged. The actual
correct/na_only behavior is retained, including sign rectification and explicitly
configured F01 winsorisation in na_only.

`liss_validate_applied_income()` checks the whole typed result, native source
relationship, permitted output changes, locks, metadata and structural digests.
Constructor-owned ledger columns, optional dataset flags, summary fields and
log entries have closed typed shapes. Count fields require plain nonnegative
whole numbers, without character or fractional coercion. Timing fields must have
valid scalar types even where their values are excluded from replay comparison.
Its default does not authenticate execution. Optional `replay = TRUE` requires
the matching recorded computation/helper and runtime identity, reruns the captured
invocation from retained source input, and compares deterministic output and
execution fields. Declared nonsemantic timing/path fields are excluded from that
comparison. Replay does not reread original sources or establish numerical or
statistical validity. A changed runtime can block replay while compatible
structural checks remain available.

Whole-result RDS is supported. Data-only/manual row or column projections are
not complete applied-result proof. The separate controlled view capability below
retains the complete parent. Repeated
consumption, source rebinding, panel assembly and report/SAV/CSV export reject
recognized applied claims before unwrapping or writing. This includes claims on
selected inner frames and incomplete wrappers. Deliberately stripping every
claim leaves unknown unguarded input; aliases cannot reconstruct ownership.
No taxable cleaner, new numerical method, background contract or CA20g/CH scope
is added by this applied-result capability.

## Controlled views of applied household results

`source_nonresponse_collection_applied_views.yml` grants the separate version-1
`CI23P339_V4_APPLIED_VIEW` capability. Its digest-pinned policy binds the existing
whole-parent applied policy without changing that policy or the numerical engine.
`liss_create_applied_income_view(parent_result, row_keys)` first validates a whole
`s339` correct/na_only applied parent from a native schema-4 collection. It retains
that exact complete object, including original `source_input`, in `parent_result`.
Nested views are unsupported; make another selection from the whole parent.

Selection requires explicit ordered unique plain character keys from the parent's
`applied_lineage$row_keys`, without names, missing/empty keys, coercion or other
attributes. `character(0)` is an explicit empty selection. The closed `selection`
contains those keys and their exact integer `root_rows`. All data columns remain
in parent order with selected values/types. Names and dimensions follow natural
row subsetting; all other column metadata stay exact. Root data row names remain.
No cleaning, outlier detection or fitting executes during projection.

Decisions preserve all original ledger columns, root `row`, opaque `decision_id`
and subset row names. They append integer `root_decision_row` (the ordinal in the
complete parent ledger) and `view_row` (selected data-row position), ordered by
`view_row` then `root_decision_row`. Reference identity is the full parent digest
plus root ledger ordinal. Decision IDs need not be unique and have no new meaning.
Multiple decisions per selected row are supported; empty ledgers retain types.

The closed `summary` contains integer `n_rows`, `n_decisions`, `n_finite` and
`n_source_locked`, plus `view_status`, `source_readiness`,
`parent_source_readiness` and `parent_analysis_execution_status`. Decisions count
ledger entries, which can exceed row counts. Scoped readiness comes from native
validation of selected ORIGINAL source rows with their unchanged receipt, never
from corrected amounts. `view_status` is `projected` for a nonempty selection and
`not_evaluated` for an empty selection. Empty and other-wave-only source scopes can
be `not_evaluated` while complete-parent execution is `completed`. Parent readiness
and execution status remain separately available, as do complete denominators.

The wrapper has exactly `parent_result`, `selection`, `data`, `decisions`,
`summary` and `applied_lineage`, in that order. Its exact existing guarded class is
`liss_clean_collection_applied_result`, without `liss_clean_result`. Frame classes
stay identical to the parent. The controlled frame-claim change is a distinct
version-1 `applied_income_view_frame` marker in `liss_source_nonresponse_applied`.
The version-1 `applied_income_view_lineage` records the view policy/capability,
parent lineage version, native carrier version, target/mode, full serialized parent
digest and selection/output/decisions/summary digests, then its own digest. SHA-256
uses R serialization version 2. These view records are distinct from whole-parent
lineage version 1.0.0 and native carrier version 4.0.0.

`liss_validate_applied_income_view()` validates the whole retained parent and
reconstructs the exact closed projection. Extra/partial fields, metadata changes,
incorrect mappings and projections that disagree with the parent fail even after
refreshing digests. `replay = TRUE` additionally uses the whole-parent validator's
matching computation/runtime gate and executes the complete saved invocation,
never a selected-row fit. Existing whole-parent structural/replay compatibility
is preserved, including its dynamic helper fingerprint. Whole-parent validation
rejects view wrappers; the separate view validator owns this shape and kind.
Whole-view RDS is supported. Existing guards reject recognized wrapper, frame and
partial claims before source/numerical work or writes. No panel/report consumer,
export, source rebind, background attachment, tax cleaning or new source scope is
granted. No new class requiring an unrecognized guard path is introduced.

Ordinary digests are not authentication. Coordinated resealing of a structurally
valid parent/view can describe another consistent object. Permitted finite parent
timing changes can pass structure and replay after consistent resealing; the full
parent hash changes, but there is no external authenticated anchor. Whole-parent
replay checks execution outputs under trusted code/runtime. It does not authenticate
source observations or establish statistical validity. Complete removal of every
recognized claim leaves unknown ownership; aliases cannot recover it.

## Missing source carriers at the household-cleaner boundary

A known source-result class or a non-null source/readiness attribute on the
outer input or its data frame requires a supplied carrier. The cleaner checks
these claims before unwrapping, target/rules resolution, background preparation
or numerical work. Missing carriers fail closed for scalar as well as collection
claims, including malformed readiness attributes. An outer wrapper's source
attribute cannot be discarded while its frame enters ordinary cleaning.

This marker check is separate from carrier validation and consumer approval.
Valid wrapper-slot or frame carriers follow their existing paths. Carrier-free
legacy wrapper readiness such as `list(status = "not_evaluated")`, actual
absent-wave scalar merge readiness and ordinary cleaner readiness remain
nonclaiming hints when no source class or attribute is present. They do not
certify source exclusion. Standalone classifier data extracted without any
marker still has the documented unguarded boundary. This missing-carrier guard
does not itself grant a consumer; the applied capability above has its own
complete-result contract. No taxable consumer is added.

The same explicit source-claim detector also protects report and panel
boundaries. A source class or source/readiness attribute on an outer object or
the selected inner frame cannot enter report writing or panel merging, including
when legacy partial data-name matching selects the frame. Existing scalar and
collection carrier denials remain in place. A readiness slot alone keeps its
legacy semantics, so an ordinary or actual absent-wave `not_evaluated` wrapper
is not promoted to a guarded source claim. This adds no export capability or
panel lineage format.

## Standalone exact-source CA20g012 classification

`source_nonresponse_ca20g012.yml` is a separate package-owned, digest-pinned
`CA20G012_SOURCE` authority, version 1.0.0, with a whole-result carrier schema
1.0.0. `liss_classify_ca20g012(source)` accepts only the complete reviewed
`ca20g_EN_1.0p.sav` file with its exact filename, byte size and SHA-256. It checks
the file before and after import and parses a verified in-memory byte buffer
with `haven::read_sav(user_na = TRUE, .name_repair = "minimal")`. This source
authority is independent of the CI registry, native carriers, FLAG/applied/view
policies and computation fingerprint.

The target `ca20g012` is a signed banking/savings balance on 2019-12-31 in whole
EUR. Only the exact negative encodings `-9999999999` and `-9999999998` become
NA in a separate analysis projection. Coded rows require the exact nonmissing
`ca20g078` gate equal to one. Their answer status is `nonresponse_unknown`,
analysis disposition is `missing_amount` and `no_numeric_proposal` is true.
The source's positive ten-digit SPSS missing declarations conflict with its
positive eleven-digit labels and codebook forms; the observed negative pair is
undeclared. No sign/digit repair or authenticated don't-know/refusal mapping
follows. Any of the four positive forms `9999999998`, `9999999999`,
`99999999998` or `99999999999` in `ca20g012` makes classification and validation
reject the whole call with `liss_ca20g012_blocked`; no partial result is returned.
Ordinary signed values,
including `-9` and `-8`, and nonfinite nonmatches remain untouched. Brackets
do not supply a reconstructed amount or sign. Readiness evaluates the two exact
source encodings only; `overall_amount_validity` remains `not_evaluated`.

The closed wrapper contains exactly `original_data`, `analysis`, `data`,
`source_nonresponse` and `source_nonresponse_readiness`. `original_data` is the
complete unchanged imported Haven tibble, preserving all columns, row order,
values, classes and metadata. `analysis` contains `data` and a typed `ledger`.
Only the two classified target values change in the analysis frame; source
labels and missing declarations are retained as descriptive metadata, not as
evidence for changed analysis values. Top-level `data` duplicates the marked
analysis frame for existing consumer guards. The ledger records original code,
source row ordinal, respondent key, case, unresolved subtype evidence and
missing-amount disposition. A source-cell key combines the source SHA-256,
original row ordinal and raw field, without inferring ownership from aliases.
The classification record binds complete coverage, original/analysis/ledger/
readiness digests and its own digest using R serialization version 2.

`liss_validate_ca20g012(result)` requires the complete result and reconstructs
it against its retained complete original. This default checks structural
consistency. Supplying `source` additionally rereads the exact verified file
and compares its complete imported original, including non-target columns and
metadata. Whole-result RDS retains the object. Validation still requires the
reconstructed result and any reread source to reproduce the required serialized
representation. Validation across R or package versions is not guaranteed.
Extracted frames, subsets, reordered or partial wrappers are unsupported. Ordinary hashes do not
authenticate origin. A consistently substituted original and result can pass
structural checks; source consistency compares against the trusted file.
Neither validation mode establishes statistical validity.

The wrapper and analysis frame use the existing guarded collection-result
class with distinct CA source/readiness record kinds. Those claims deny
unsupported consumers; they grant no CI native carrier or numerical-consumer
approval. Rebinding, income/wealth cleaning, applied views, background
attachment, panel assembly, reports and SAV/CSV export remain unsupported for
this result. In explicit `correct` or `na_only` mode, `liss_clean_income()` can
evaluate `background` to check its required `NULL` value before rejecting the
unsupported CA parent. Rejection does not guarantee that caller-supplied
argument expressions remain unevaluated. Deliberately extracting unmarked
originals or removing every
recognized claim leaves unknown ownership; aliases cannot recover it. This
contract covers no other CA field or release and changes no default CA merge
recipe or whole-wave B03 flag.

## Standalone exact-source CA20g016 classification

`source_nonresponse_ca20g016.yml` is a separate package-owned, digest-pinned
`CA20G016_SOURCE` authority, version 1.0.0, with a whole-result carrier schema
1.0.0. `liss_classify_ca20g016(source)` accepts only the complete reviewed
`ca20g_EN_1.0p.sav` file with its exact filename, byte size and SHA-256. It checks
the file before and after import and parses the same verified in-memory byte
buffer with `haven::read_sav(user_na = TRUE, .name_repair = "minimal")`. The
complete source schema, original row order, lossless unique respondent keys and
exact target, ID and routing metadata are required. This authority is separate
from CA20g012, the CI registry and native/FLAG/applied/view capabilities.

The target `ca20g016` is reported investment value on 2019-12-31 in whole EUR.
The documented range is signed, `-999999999..999999999`; written options may
represent debt, and legitimate negative balances must be preserved. Ownership
depends on the source household-role and ownership preloads. The amount is not
asserted to be a uniform household total or household income.

Only the exact negative encodings `-9999999999` and `-9999999998` become NA in a
separate analysis projection. Coded rows require a finite `ca20g006` investment
gate equal to one; this does not require every eligible row to have an answer.
Their answer status is `nonresponse_unknown`, subtype evidence is `unresolved`,
analysis disposition is `missing_amount` and `no_numeric_proposal` is true.
The source's positive ten-digit SPSS missing declarations conflict with its
positive eleven-digit labels and codebook forms; the observed negative pair is
undeclared. No sign/digit repair or authenticated don't-know/refusal mapping
follows. Any of `9999999998`, `9999999999`, `99999999998` or `99999999999` in
`ca20g016` makes classification and validation reject the whole call with
`liss_ca20g016_blocked`; no partial result is returned.

Every nonmatching target value remains unchanged and uncertified, including
ordinary `-9`/`-8`, legitimate negatives, either zero sign, fractional values,
finite values outside the documented range and nonfinite values. Readiness
evaluates only the two exact source encodings; `overall_amount_validity` remains
`not_evaluated`. The `ca20g017` follow-up is a category item, with substantive
codes 1..14 and declared 999, not a monetary replacement. It supplies no inferred
amount, midpoint, sign, donor or authenticated answer subtype.

The closed wrapper contains exactly `original_data`, `analysis`, `data`,
`source_nonresponse` and `source_nonresponse_readiness`. `original_data` retains
the complete unchanged imported Haven tibble, including every column, value,
type, class, row order and source attribute. `analysis` contains `data` and a
typed `ledger`. Only the two classified target values change in the analysis
frame. Its retained labels and missing declarations are descriptive source
metadata and do not attest the changed analysis values. All non-target columns,
including raw `ca20g012` and `ca20g017`, remain exact. Top-level `data` duplicates
the marked analysis frame for existing consumer guards.

The ledger records original code, source row ordinal, respondent key, fixed
case, unknown subtype, missing-amount disposition and no-numeric-proposal flag.
A source-cell key combines the exact source SHA-256, original row ordinal and
raw field. The classification record binds complete coverage, the authority
and source context, and original/analysis/ledger/readiness digests plus its own
digest using R serialization version 2.

`liss_validate_ca20g016(result)` requires the complete closed result and
reconstructs it against its retained complete original. This default checks
structural consistency. Supplying `source` additionally rereads verified source
bytes and compares the entire imported original, including all non-target
columns and metadata. Validation requires both R identity and the required
version-2 serialized representation of the reconstructed payload and any reread
original, including signed zeros, missing-value payloads and attribute order.
Guard marker and readiness values are checked separately. Whole-result RDS
retains the object; validation across R or package versions is not guaranteed.
Extracted frames, partial wrappers, subsets and reordered results are unsupported.
Ordinary hashes do not authenticate origin: a consistently substituted original
and result may pass structural checks. Source consistency compares against the
trusted file, and neither validation mode establishes statistical validity.

The wrapper and analysis frame use existing guarded collection-result claims
with distinct CA016 source/readiness record kinds. These claims grant no CI
native carrier, cleaner, FLAG, applied, view or export capability. Rebinding,
income/wealth cleaning, background attachment or consumption, panel assembly,
reports and SAV/CSV export remain unsupported. Rejection does not guarantee
that caller-supplied argument expressions remain unevaluated, including the
cleaner's required `background = NULL` check in explicit applied modes.
Extracting unmarked originals or stripping every recognized claim leaves
unknown ownership; aliases cannot recover it.

This separate contract changes no CA20g012 authority, result or behavior and
defines no combined CA012+CA016 carrier. It covers no other field or release and
changes no default CA merge recipe, whole-wave B03 flag, numerical method or
existing CI parent/view lineage.

## Standalone exact-source CA20g023 vehicle, boat and caravan value

`liss_classify_ca20g023(source)` and `liss_validate_ca20g023(result, source = NULL)`
use a separate `CA20G023_SOURCE` authority and whole-result carrier schema,
both version 1.0.0. The authority admits only the complete reviewed
`ca20g_EN_1.0p.sav`, with its exact filename, byte size and SHA-256. Creation
checks the source before and after import and parses the same verified
in-memory byte buffer with `haven::read_sav(user_na = TRUE,
.name_repair = "minimal")`. Complete source schema, original row order,
unique nonnegative finite lossless respondent keys and exact ID/023/008
metadata are required. This is independent of CA012/016 and every CI capability.

The question estimates total sales value of cars, motorcycles, boats and
(static) caravans owned on 2019-12-31, in whole EUR. The date identifies assets
owned then, without an additional valuation-time interpretation. Household-role
and own/joint ownership context do not imply a uniform household total or income.
The documented amount domain `1..999999999` is descriptive; the classifier does
not enforce that range or certify ordinary amounts.

Only exact raw `ca20g023 = -9` and `-8` become NA in a separate analysis
projection. Coded rows require finite, nonmissing ownership gate `ca20g008 = 1`;
this adds no answer requirement for other eligible rows. The undeclared negative
codes receive `nonresponse_unknown`, `unresolved` subtype evidence,
`missing_amount` disposition and `no_numeric_proposal = TRUE`.
CA023's SAV missing declarations AND value labels use positive ten-digit
`9999999998`/`9999999999`; the codebook prints positive eleven-digit
`99999999998`/`99999999999`. No sign/digit repair or authenticated don't-know/refusal
mapping follows. Encountering any of these four positive forms in the target
makes classification and validation reject the whole call with
`liss_ca20g023_blocked`, without a partial result.

Positive `8` and `9` are ordinary amounts, not aliases for the negative codes.
Every nonmatching target payload remains exact and uncertified, including
large negative codes used by other CA fields, other negatives, either zero
sign, fractions, out-of-range values and nonfinite values. Readiness concerns
only the two encodings; `overall_amount_validity` stays `not_evaluated`.
The `ca20g024` follow-up is a category item with substantive codes 1..14 and
declared 999. It supplies no recovered amount, midpoint, sign, donor or
authenticated subtype. No following bracket is used as a monetary replacement.

The closed result contains exactly `original_data`, `analysis`, `data`,
`source_nonresponse` and `source_nonresponse_readiness`. The complete unchanged
Haven import, all values, columns, types, classes, row order and attributes
remain in `original_data`. `analysis` contains a marked `data` frame and a typed
`ledger`; top-level `data` duplicates that frame. All 65 non-target columns,
including raw `ca20g012`, `ca20g016` and `ca20g024`, remain exact. Retained target
labels and missing declarations describe the original source and do not attest
changed analysis values.

The ledger binds source hash, original row ordinal and raw field in each cell
reference and retains the original code, respondent key, fixed case, unknown
subtype, missing-amount disposition and no-numeric-proposal flag. The record
binds complete coverage, authority/source/codebook context, reference date and
date scope, ownership context, descriptive amount domain and metadata-form
distinction, plus original/analysis/ledger/readiness digests and its own digest.

Default validation reconstructs the complete closed result against its retained
original and checks structural consistency only. Optional source validation
rereads verified source bytes and compares the entire original, including all
non-target columns and metadata. Both R identity and version-2 serialized payload
equality are required, including numeric payloads and attribute order. Equivalent
guard placement is normalized; marker/readiness values are checked separately.
Whole-result RDS retains the object; cross-R or cross-package-version validation
is not guaranteed. Hashes and reconstruction do not authenticate origin: a
consistently substituted original/result may pass structure. Source rereading
compares against the trusted file; neither mode establishes statistical validity.

Existing collection claims support denial only. They grant no CI native carrier,
scalar rebind, cleaner, FLAG/applied/view, background, panel, report or SAV/CSV
export capability. Rejection does not guarantee unevaluated caller expressions,
including the cleaner's required `background = NULL` check in explicit applied
modes. Unmarked originals or complete claim removal leave unknown ownership;
aliases cannot recover it. No combined CA012+CA016+CA023 carrier is defined.
Both older standalone authorities and results, numerical methods, existing CI
parent/view lineage, default CA merge recipe and whole-wave B03 flag are unchanged.
No other field or release is covered.

## Standalone exact-source CA20g014 insurance payout and endowment savings

`liss_classify_ca20g014(source)` and `liss_validate_ca20g014(result, source = NULL)`
use an independent `CA20G014_SOURCE` authority and standalone whole-result carrier,
both version 1.0.0. Only the complete reviewed `ca20g_EN_1.0p.sav` is admitted:
exact filename, byte size and SHA-256 before and after import, with Haven parsing
the same verified memory buffer using `user_na = TRUE` and minimal name repair.
Require the full schema, original row order, unique nonnegative finite lossless
integer-valued double keys and exact ordered ID/014/005 metadata. The complete
unchanged imported tibble remains in `original_data`.

The question asks guaranteed minimum payout of single-premium or life annuity
insurance, OR total savings amount of endowment insurance on 2019-12-31, in whole
EUR. This date differs from September/October 2020 collection. Household and
financial-role instructions distinguish own from own-plus-joint assets; there is
no uniform household-total, generic market/surrender-value or income meaning.
The printed amount domain `1..999999999` is descriptive and is not enforced.

Only exact raw `ca20g014 = -9/-8` becomes ordinary analysis NA. Those coded cells
alone require finite, nonmissing possession gate `ca20g005 = 1`. Noncoded rows
need no gate condition; eligible source-missing answers stay missing. Both codes
remain `nonresponse_unknown`, with `unresolved` subtype evidence, `missing_amount`,
`no_numeric_proposal = TRUE` and `overall_amount_validity = "not_evaluated"`.
SAV missing declarations AND labels use positive ten-digit `9999999998`/
`9999999999`; the codebook uses positive eleven-digit `99999999998`/`99999999999`.
The observed negative pair is undeclared; no sign/digit or authenticated subtype
repair follows. Any of those four positive forms in the target makes creation
and validation reject the whole call with `liss_ca20g014_blocked`, without a
partial result. Positive `8`/`9` and every other nonmatch remain exact and
uncertified, including other negatives, either zero sign, fractional/out-of-range
and nonfinite payloads. All 65 non-target columns/order/attributes remain exact.

The `ca20g015` column is mandatory under the complete schema. Its reference
metadata digest is contextual, separate from the three admission pins. The record
carries `contextual_metadata$ca20g015` with `role = "context_only"`,
`reference_sha256`, `actual_sha256` and `required_for_admission = FALSE`.
Reference/actual digest equality is not an admission condition. Missing follow-up
values, even on coded 014 rows, are allowed. Its categories supply no amount,
midpoint, sign, donor or authenticated subtype. Whole-original digests and optional
source comparison still bind the actual 015 values and metadata.

The closed result has exactly `original_data`, `analysis`, `data`,
`source_nonresponse` and `source_nonresponse_readiness`. Analysis contains the
separate marked `data` frame and typed 18-column `ledger`; top-level `data`
duplicates that frame. Full coverage lists every source row. Cell references bind
source hash, original ordinal and raw field, retaining key, original code, case,
unknown subtype and missing-amount/no-numeric-proposal disposition. Readiness
`unresolved_cells = 0` concerns cell binding, not subtype resolution. Retained
analysis labels/missing declarations describe the source, not changed NA values.

Default validation reconstructs the complete result against its retained original
and establishes structural consistency only. Optional source validation rereads
verified bytes and compares the entire original, including all non-targets and
015 metadata. Both R identity and version-2 payload bytes are required, covering
signed zero, missing payloads, reference identity and attribute order. Harmless
analysis guard placement is normalized; actual marker/readiness values remain
checked. Whole-result RDS retains the object, but cross-R/package-version
validation is not guaranteed. A consistently substituted original/result can
pass structure; source mode detects mismatch with the trusted file. Hashes do
not authenticate origin; `source_authentication` and `statistical_validity` are
always FALSE.

Existing collection claims provide denial only. No CI carrier, scalar rebind,
cleaner, FLAG/applied/view, background, panel, report or SAV/CSV export capability
is granted. Rejection does not guarantee unevaluated caller expressions, including
the cleaner's required `background = NULL` check in explicit applied modes.
Unmarked originals or complete claim stripping leave unknown ownership; aliases
cannot restore it. No combined CA carrier is defined. The separate CA012/016/023
authorities/results, all numerical methods, complete CI parent/view lineage and
default CA recipe/B03 behavior remain unchanged. No other field/release is covered.

## Standalone exact-source CA20g025 money loaned out

`liss_classify_ca20g025(source)` and `liss_validate_ca20g025(result, source = NULL)`
use an independent `CA20G025_SOURCE` authority and standalone whole-result carrier,
both version 1.0.0. Only the complete reviewed `ca20g_EN_1.0p.sav` is admitted:
exact filename, byte size and SHA-256 before and after import, with Haven parsing
the same verified memory buffer using `user_na = TRUE` and minimal name repair.
Require the full schema, original row order, unique nonnegative finite lossless
integer-valued double keys and exact ordered ID/025/010 metadata. The complete
unchanged imported tibble remains in `original_data`.

The question asks money loaned out to family, friends or acquaintances on
2019-12-31, in whole EUR. This date differs from September/October 2020 collection.
Household and financial-role instructions distinguish own from own-plus-joint
assets; no uniform household total, income or numerical wealth method follows.
The documented `1..999999999` amount domain is descriptive and is not enforced.

Only exact raw `ca20g025 = -9/-8` becomes ordinary analysis NA. Those coded cells
alone require finite, nonmissing `ca20g010 = 1`; this gate uses `1 = yes`, `2 = no`.
Noncoded rows need no gate condition; eligible source-missing answers stay missing.
Both codes remain `nonresponse_unknown`, with `unresolved` subtype evidence,
`missing_amount`, `no_numeric_proposal = TRUE` and amount validity `not_evaluated`.
SAV missing declarations AND labels use positive ten-digit `9999999998`/
`9999999999`; the codebook uses positive eleven-digit `99999999998`/`99999999999`.
The negative pair is undeclared; no sign/digit or authenticated subtype repair
follows. Any of those four positive forms in the target rejects whole creation
and validation with `liss_ca20g025_blocked`, without a partial result. Positive
`8`/`9` and every nonmatch remain exact and uncertified, including other negatives,
both zero signs, fractions, out-of-range and nonfinite payloads. All 65 non-target
columns, row order and metadata remain exact, including `ca20g012`, `ca20g014`,
`ca20g016`, `ca20g023` and `ca20g026`.

The `ca20g026` column is mandatory under the complete schema. Its reference
metadata digest is contextual, separate from the three admission pins. The record
carries `contextual_metadata$ca20g026` with `role = "context_only"`,
`reference_sha256`, `actual_sha256` and `required_for_admission = FALSE`.
Reference/actual digest equality is not an admission condition. Missing follow-up
values, even on coded rows, are allowed. Its categories supply no amount,
midpoint, sign, donor or authenticated subtype. Whole-original digests and optional
source comparison still bind all actual 026 values and metadata.

The closed result has exactly `original_data`, `analysis`, `data`,
`source_nonresponse` and `source_nonresponse_readiness`. Analysis contains the
separate marked `data` frame and typed 18-column `ledger`; top-level `data`
duplicates that frame. Full coverage lists every source row. Cell references bind
source hash, original ordinal and raw field, retaining key, original code, case,
unknown subtype and missing-amount/no-numeric-proposal disposition. Zero-code
complete sources retain full coverage and a typed empty ledger; zero-row or
miniature originals are unsupported. Readiness `unresolved_cells = 0` concerns cell
binding, not subtype resolution. Retained analysis labels/missing declarations
describe the source, not changed NA values. Frequencies are never policy constraints.

Default validation reconstructs the whole result against its retained original
and establishes structural consistency only. Optional source validation rereads
verified bytes and compares every original column and attribute, including 026.
Both R identity and version-2 payload bytes are required, covering signed zero,
missing payloads, reference identity and attribute order. Harmless analysis guard
placement is normalized; marker/readiness values remain checked. Whole-result RDS
retains the object, but cross-R/package-version validation is not guaranteed.
A consistently substituted original/result can pass structure; source mode detects
mismatch with the trusted file. Hashes do not authenticate origin;
`source_authentication` and `statistical_validity` are always FALSE.

Existing collection claims provide denial only. No CI carrier, scalar rebind,
cleaner, FLAG/applied/view, background, panel, report or SAV/CSV export capability
is granted. Rejection does not guarantee unevaluated caller expressions, including
the cleaner's required `background = NULL` check in explicit applied modes.
Unmarked originals or complete claim stripping leave unknown ownership; arbitrary
wrappers and aliases cannot restore it. No combined CA carrier is defined.
The four older standalone authorities/results, all numerical methods, complete
CI parent/view lineage and default CA recipe/B03 behavior remain unchanged.
No other field or release is covered.
