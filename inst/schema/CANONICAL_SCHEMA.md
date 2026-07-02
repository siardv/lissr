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

Known limitation: two recipes carry alias keys the engine does not
read, so their renames do not run. `ch` uses `from`/`to`, and `ci` uses
`old`/`new`. With no resolvable suffix, `harmonized_name` falls back to the
literal column name `h_`, created all-NA, which each recipe then removes with a
`drop` rule (`ch` D02, `ci` DR03). Realigning those recipes onto
`old_suffix`/`new_suffix` would change frozen outputs (the harmonized columns
would be produced and the `h_` drops would no longer apply), so it is deferred.

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

`validate_recipe()` (called by `load_recipe()` and `merge_liss_module()`) enforces before any merge:

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

Implemented check types, evaluated at merge time in phase 6:
`uniqueness` (aliases `assert_unique`, `n_duplicates`; key column within a
grouping variable, defaults `nomem_encr` within `wave_id`),
`value_absence` (alias `assert_absent_values`), `value_range` (alias
`range_check`), `na_rate` (alias `na_rate_check`), `expected_presence`,
`structural_missingness`, and `wave_count`. A check whose `type` is not in
this list reports `SKIP` with `passed = NA` and is counted separately from
passes and failures; it never reports PASS for logic that did not run, and
it never contributes to the error count. The per-run summary reports
`n_pass`, `n_fail`, and `n_skip`.

`merge_liss_module(..., strict = FALSE)`: with `strict = TRUE`, any failed
check of `severity: error` aborts the merge before phase 7, so no outputs
are written. The default keeps v1.0.0 behavior (failures are reported and
logged; outputs are still written). `merge_liss_modules()` forwards the
argument.
