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
counted separately (`n_doc`), rather than entering `error_skips`. The execution
console labels them `DOC`; the text report currently labels them `SKIP`.
The other counts are `n_pass`, `n_fail`, and `n_skip`.

With `merge_liss_module(..., strict = TRUE)`, reported error-level failures or
unevaluable checks abort before phase 7, so no output artifacts are written.
The default `strict = FALSE` writes outputs and sets `valid_for_analysis = FALSE`
when either occurs. `merge_liss_modules()` forwards strictness. This relies on
individual executors detecting failures and unresolved targets correctly.

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
- Numeric all-NA range targets still pass. Set checks retain `allow_na` behavior
  (default `TRUE`). Empty data with existing target columns can pass an all-row
  check; explicitly requested absent waves remain unevaluable.

This contract also applies to range aliases `range_check`, `value_in_range`,
`assert_range` and set aliases `value_set`, `assert_values`. It does not change
other validators or add support for additional range/set payloads. Range checks
continue to use numeric columns and `min`/`max`; `valid_range` and sentinel
exceptions are not implemented by this executor.

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
- Forbidden-value aliases and numeric/character matching are unchanged. Block
  `forbidden_values`, `forbidden_value`, `sentinel_values`, `codes`, or `value`
  take precedence over inherited parent `forbidden_values`, `sentinel_values`,
  or `value`. Missing/empty forbidden values provide no value constraint, but
  target and wave resolution still runs. This is not general validation of
  forbidden-value payloads or the remaining validation executors.

The same behavior applies to `assert_absent_values`, `none_equal`,
`sentinel_absence`, `no_residual_sentinels`, `assert_no_values`,
`value_absence_check`, and `value_restriction`.

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
- Existing `value`/`values` numeric coercion and matching are unchanged. An
  all-NA target cannot supply a nonmissing numeric value, but another selected
  column may supply it. This contract adds no general value-payload validation.

#### `na_rate` target and wave scopes

`na_rate` requires every requested target and wave to resolve before a condition
is applied. Missing targets (including partial matches), absent requested waves,
malformed scopes and unavailable required wave membership are unevaluable
(`passed = NA`). A measured rate outside the threshold fails (`passed = FALSE`).
Both outcomes preserve severity and the strict/report handling above.

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
  must resolve first; an empty selection cannot excuse missing inputs.
- Rates are calculated separately for each target over the pooled selected rows,
  not separately per wave. Every target must meet the threshold. An all-NA target
  has rate 1; a nonmissing target has rate 0. `above` uses `>=`, otherwise the
  existing `<=` comparison applies. Threshold precedence is `threshold`, then
  `max_rate`, then the default: 0 for `not_missing`, 1 otherwise.

Aliases are `na_rate_check`, `na_rate_above`, `na_rate_below`, and `not_missing`.
`na_rate_above` defaults to direction `above`; the others default to `below`.
An explicit direction overrides that default. This contract does not introduce
general threshold/direction validation or change the condition evaluator.

#### `per_wave_mean` required targets and wave membership

`per_wave_mean` requires every selected target to resolve before comparing
means. Missing targets, including partial matches, malformed requests and
unavailable wave membership are unevaluable (`passed = NA`). An observed finite
mean outside the bounds fails (`passed = FALSE`). Both outcomes retain the
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
  on empty data, with the diagnostic `no observed waves; no means calculated`.
  Empty data cannot excuse missing targets or a missing `wave_id` column.
- Means use the existing numeric coercion and `na.rm = TRUE`, separately for
  every target in every observed wave. Every finite mean must be within the
  inclusive bounds, `min_mean` (default `-Inf`) and `max_mean` (default `Inf`).
  Values are not pooled across waves or targets.
- Non-finite means, including all-NA, NaN and infinite means, retain their
  existing exclusion from bound comparisons. If the check passes, its detail
  reports the number of these means and the first affected column and wave.
  A finite bound violation still fails even when other means are non-finite.

This check has no registered type aliases. The contract does not add general
numeric-value or bounds-payload validation; coercion and non-finite handling
remain unchanged.

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
