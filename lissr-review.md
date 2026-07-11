# lissr: Improvement Review

Scope: full read of the package sources (engine, executors, downloader,
auth, onboarding, tests, docs), all ten bundled recipes,
`action_vocabulary.yml`, `anomaly_registry.yml`, and
`CANONICAL_SCHEMA.md`, cross-referenced against the file inventory in
`files_liss-siardv_20260701_214334.csv` (169 `.sav` files, 168 codebook
PDFs).

Severity legend: **C** = correctness (wrong or silently missing output),
**H** = high (robustness, misleading behavior, doc-vs-behavior gaps),
**M** = medium (design, API, packaging), **L** = low (polish).

------------------------------------------------------------------------

## 1. Critical correctness findings

### C1. `value_recode` applies mappings sequentially and can chain

In `exec_harmonization_rule()`, both the `value_recode` and the
`na_recode`/`recode` branches compute each mask against the live,
already-mutated column:

``` r

for (from_val in names(mapping)) {
  mask <- !is.na(df[[col]]) & df[[col]] == as.numeric(from_val)
  df[[col]][mask] <- as.numeric(to_val)
}
```

If any mapping target equals a later mapping source, values are recoded
twice. The cr religion crosswalks are exactly this shape. cr `HR10` maps
`{1:1, 2:2, 3:4, 4:5, 5:3, 6:3, 7:6, 8:8, 9:9, 10:11, 11:10, 12:7, 13:12, 14:13}`.
Traced through the sequential loop: original 3 becomes 4, is then caught
by the `4: 5` step and becomes 5, is then caught by `5: 3` and ends at
3. The intended 3 to 4 mapping never survives. `HR11` and `HR12` have
the same overlap structure (11 to 13 colliding targets each).

Notably, the engine author already solved this correctly elsewhere: the
`crosswalk_rename` `post_recode` block explicitly “matches against a
snapshot so multi-entry maps do not chain”, and the `crosswalk` action
goes through `crosswalk_map()`, which also reads from a frozen source.
The fix is to give `value_recode` and `recode` the same snapshot
semantics:

``` r

# snapshot-based recode: all masks computed against the original vector
orig <- df[[col]]
changed <- 0L
for (from_val in names(mapping)) {
  to_val <- mapping[[from_val]]
  mask <- !is.na(orig) & orig == as.numeric(from_val)
  n <- sum(mask)
  if (n > 0) {
    df[[col]][mask] <- if (is.null(to_val) || identical(to_val, ".na")) NA
                       else as.numeric(to_val)
    changed <- changed + n
  }
}
```

Add a unit test with an intentionally chaining map, for example
`{1: 2, 2: 3}` on `c(1, 2)`, asserting `c(2, 3)` rather than `c(3, 3)`.

### C2. The cr religion harmonization never runs at all: `suffix` vs `suffixes`

cr `HR10`, `HR11`, `HR12` (and `HR20`, `HR21`) use the singular key
`suffix:`. The engine reads
`rule$suffixes %||% rule$stems %||% rule$variables`; the singular
`suffix` is not in `RECOGNIZED_RULE_KEYS`, so the target list resolves
to [`list()`](https://rdrr.io/r/base/list.html) and the loop body
executes zero times. Two consequences:

1.  The three-era religion crosswalk, described in `CANONICAL_SCHEMA.md`
    as a working feature (“Religion harmonization (cr) maps three coding
    eras onto one coarse 1-13 scheme”), is entirely inert. Merged cr
    output carries three incompatible coding schemes in the same column
    with no recode and no era-safe pooling.
2.  No log entry is emitted for these rules, so the JSONL audit trail
    contains no trace that the rule was skipped. TODO.md documents cr
    `BR20/BR21/BR30` as pending but not `HR10-HR12`, so this appears to
    be an unknown silent failure rather than a known limitation.

Fix in two layers: rename `suffix:` to `suffixes:` in the cr recipe (and
`HR20/HR21`), and land C1 first, because with the key fixed the chaining
bug would corrupt the very crosswalk being repaired. Also consider
re-expressing these as `crosswalk` rules with `output_variable`, which
sidesteps chaining by construction and gains the unmapped-code coverage
check for free.

### C3. Rules that emit no log entry when their target list is empty break auditability

Generalizing C2: several executor branches (`value_recode`, `set_label`,
`fix_label`, and others) loop over resolved targets and only log inside
the loop. A rule whose scoping key is mis-named, or whose variables are
absent, produces neither a transformation nor a log line. For an engine
marketed as “audit-grade”, every rule should leave at least one entry
per wave it was scheduled on, including an explicit `NO_TARGETS` marker:

``` r

if (length(targets) == 0) {
  log_entries <- append(log_entries, list(
    make_log(rid, wave_id, "*", paste0(action, ":NO_TARGETS"), 0L)))
}
```

A cheap post-merge invariant is also worth adding: every `rule_id` in
the recipe (minus `note_only`) must appear at least once in the log,
otherwise warn.

### C4. cp sentinel recodes over-apply to every numeric column

cp `A1_dk_999_to_na` and `A1_dk_neg9_to_na` scope their recode with
`items: ['010', '011', 019]`, but the engine’s `recode` branch reads
`scope %||% suffixes %||% variables` and falls back to `"all_numeric"`
when none is present. `items` is unrecognized, so `999 -> NA` (waves
cp08a-cp18j) and `-9 -> NA` (cp20l-cp24p) are applied to **every numeric
column** in those waves rather than the three 0-10 scale items. Any
legitimate 999 elsewhere (paradata durations are the obvious candidate;
cp carries interview timestamps) is silently destroyed, and the intent
of the wave scoping (“true DK items only”) is violated. Fix: either add
`items` as a recognized scoping alias in the `recode` branch, or rewrite
the two rules to use `suffixes:`. Note also the YAML typing hazard here:
`019` unquoted parses as integer 19, so even with the alias fixed,
`find_col` would look for `s19` and miss `s019`. All suffixes should be
quoted strings, and
[`validate_recipe()`](https://siardv.github.io/lissr/reference/validate_recipe.md)
should flag bare-integer suffixes with leading-zero loss.

### C5. cs `A1_dk_recode` is a silent no-op, and its design is unsupported by the engine

cs `A1_dk_recode` (recode 999 to -9 pre-cs20m) targets variables via
`apply_to: all_with_value_label` plus `match_label_regex`. Neither key
is read; `suffixes/stems/variables` are absent; the rule does nothing,
is absent from the log, and is not listed in TODO.md’s pending set.
Beyond the key mismatch, label-regex targeting does not exist as an
engine capability, and under `labelled_policy: to_numeric` the value
labels needed to drive it are already gone by the time harmonization
rules run (they survive only in the `_original_labels` stash). Options:
(a) implement a `scope: labelled_matching` mechanism that consults
`_original_labels`, or (b) enumerate the affected suffixes explicitly in
the recipe (verifiable against the cs codebooks). Until then, pre-cs20m
999s flow into the merged output as if they were substantive values; the
guarding check `V01_no_999_remaining` also never runs (see C6).

### C6. Unimplemented validation checks report PASS

`run_validations()` handles a fixed set of check types; anything else
falls into the default branch:

``` r

list(check_id = cid, passed = TRUE, severity = "info",
     detail = paste0("type '", type, "' not auto-checked"))
```

`passed = TRUE` means the console and the merge report print `PASS` for
checks that never executed. Scanning all ten recipes, the large majority
of `validation_checks` use type names outside the implemented set (ca: 7
of 7, cd: 10, cf: 8, ci: 8, cr: 10, cs: 8, cv: 7, cw: 9, ch: 3, cp: 1).
Several are free-text sentences used as a `type` value (for example cd
`CHK06`: `type: 'no duplicate nomem_encr within any wave_id'`). The
uniqueness checks that would catch C8 below are among the never-run
ones.

Recommended fixes, in order of impact:

1.  Default branch returns `passed = NA` so status renders as SKIP, and
    the report gains a `skipped: N` summary line. Reporting PASS for
    unexecuted checks is worse than no check.
2.  Introduce a controlled check-type vocabulary (mirroring the action
    vocabulary) validated by
    [`validate_recipe()`](https://siardv.github.io/lissr/reference/validate_recipe.md),
    with alias normalization: `unique_key`, `no_duplicate_ids`,
    `unique_per_wave`, `assert_unique` all map to `uniqueness`;
    `value_in_range`/`assert_range` to `value_range`;
    `none_equal`/`sentinel_absence`/`no_residual_sentinels` to
    `value_absence`; `all_na`/`structural_absence` to
    `structural_missingness`; and so on. Most of the recipes’ intent is
    expressible in the existing executors once the names align.
3.  Make `severity: error` actually enforce. Today a failed
    error-severity check produces one warning and the outputs are still
    written. At minimum add a `strict` argument to
    [`merge_liss_module()`](https://siardv.github.io/lissr/reference/merge_liss_module.md)
    (abort on error-severity failure) and include pass/fail counts in
    the returned object so pipelines can gate on it.

### C7. A failed two-factor step still caches the session as authenticated

`perform_twofactor_authentication()` warns on failure but returns the
failed response object unconditionally.
[`liss_login()`](https://siardv.github.io/lissr/reference/liss_login.md)
then checks only `rvest::is.session(auth_session)`, which is true for
the failed session, and calls `.liss_set_session()`. Subsequent
[`liss_download()`](https://siardv.github.io/lissr/reference/liss_download.md)
calls will run against an unauthenticated session and fail one file at a
time with redirect-to-login statuses. Fix:

``` r

if (grepl("twofactor|login", authentication_response$url)) {
  cli::cli_alert_danger("verification failed")
  return(NULL)
}
authentication_response
```

Two adjacent nits in the same code path:
`rvest::html_form(session)[[2]]` hard-codes the form index (select by
field name `code` instead), and the failed-login branch prints the first
character of the password via `masked_password`; better to print only
the username and a generic failure hint.

### C8. Multi-file wave matches are silently stacked; the cd10c case works only by accident

`discover_wave_files()` keeps every path a pattern matches, and the
pipeline does `dplyr::bind_rows(dfs)` when there is more than one. Your
actual data directory (per the CSV) contains both `cd10c_EN_1.0p.sav`
and `cd10c_EN_1.1p.sav`. The recipe’s `file_pattern: cd10c_EN_1_0p.csv`
matches neither real file (see H1), so the prefix fallback `^cd10c[_.]`
matches both, and both are stacked.

For cd specifically, the recipe asserts this is intended: the
`aux_files` note says “stack both; zero respondent overlap (anomaly 9)”,
meaning 1.1p is a supplemental release with disjoint respondents. But
three things make the current behavior fragile:

1.  The mechanism that is supposed to express this intent,
    `stack_aux_files`, is a `pending_spec` no-op; the stacking happens
    only because the fallback glob accidentally over-matches. Anyone who
    “fixes” the file pattern would silently drop the 1.1p respondents;
    anyone whose archive copy of 1.1p is a superseding full re-release
    (the more common LISS pattern for version bumps) would get every
    wave-10 respondent duplicated.
2.  The safety checks written to guard it (`CHK06`, `CHK07`) never
    execute (C6).
3.  For every other module, a stray second match (an old version left in
    the folder, a `.dta` alongside the `.sav`, a partial download)
    duplicates respondents with no error, and the duplicates then
    propagate multiplicatively through
    [`merge_liss_panel()`](https://siardv.github.io/lissr/reference/merge_liss_panel.md)’s
    sequential joins.

Recommended design: make multi-file behavior explicit and defensive.

``` r

# inside discover_wave_files(), after matching
if (length(found) > 1) {
  aux <- w$aux_files %||% character(0)
  is_aux <- basename(found) %in% aux |
    tools::file_path_sans_ext(basename(found)) %in% tools::file_path_sans_ext(aux)
  primary <- found[!is_aux]
  if (length(primary) > 1) {
    # prefer the highest release version when several primaries match
    ver <- stringr::str_match(basename(primary), "_(\\d+(?:[._]\\d+)?)p?")[ , 2] %>%
      gsub("_", ".", .) %>% as.numeric()
    if (all(!is.na(ver))) {
      keep <- primary[which.max(ver)]
      cli::cli_warn("wave {w$id}: {length(primary)} candidate files; using {basename(keep)}")
      primary <- keep
    } else {
      cli::cli_abort("wave {w$id}: multiple files match and versions cannot be ranked; disambiguate on disk or in the recipe")
    }
  }
  found <- c(primary, found[is_aux])
}
```

Pair this with a first-class `aux_files` contract: after stacking,
assert zero `nomem_encr` overlap between primary and aux (erroring
otherwise), which turns the cd10c note into an executed guarantee. And
add an unconditional per-wave duplicate-id check in phase 1 regardless
of recipe content; duplicated respondent-wave rows are never valid for
these modules.

### C9. Labels do not survive the pipeline, and the docs claim they do

All ten recipes use `labelled_policy: to_numeric`.
`apply_labelled_policy()` implements it as
`as.numeric(haven::zap_labels(x))`, which strips **all** attributes;
value labels are stashed in `_original_labels`, but the variable label
(`label` attribute) is lost outright, and
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
will not reliably carry the nonstandard stash across waves.
[`haven::write_sav()`](https://haven.tidyverse.org/reference/read_spss.html)
then writes a file with no variable labels, no value labels, and no
user-missing declarations. Meanwhile the merge-workflow vignette states
“All merged output is written in SPSS `.sav` format to preserve variable
labels, value labels, and user-defined missing values”, and
`run_merge.R` repeats the claim. For a package whose output format was
chosen for metadata, this is the single most user-visible gap.

Suggested plan (the recipes’ `note`s already call it “the label
round-trip, Part VI”):

1.  Preserve the variable label immediately: in `to_numeric`, set
    `attr(vals, "label") <- attr(x, "label", exact = TRUE)` after the
    numeric cast. Cheap, safe, restores half the metadata today.
2.  On write, rebuild
    [`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html)
    columns from `_original_labels` where the stash survived and the
    observed values are still a subset of the labelled codes; log per
    column whether labels were restored, dropped-because-recoded, or
    absent.
3.  Because label-editing actions (`fix_label` value-label mode,
    `strip_question_stem`, `conditional_label_swap`, `lowercase_labels`,
    `label_to_string` input) are inert under `to_numeric`, either run
    them against `_original_labels` or reorder them before the policy
    application; today several are logged `INERT`, and others quietly
    edit an empty string.
4.  Until (2) lands, correct the vignette, README, and `run_merge.R`
    claims; researchers importing the output into SPSS will otherwise
    assume metadata that is not there.

Related destructive edge: ch `V06` (`fix_label` with
`label_find`/`label_replace`, keys the engine does not read) falls
through to the else-branch `attr(df[[col]], "label") <- new_frag` with
`new_frag = ""`, so under `keep_labelled` or `to_factor` it would blank
the labels of suffixes 001-003 in ch08b instead of fixing the Dutch
text. The action vocabulary compounds the confusion by declaring
`label_find`/`label_replace` the canonical keys and
`old_fragment`/`new_fragment` “legacy aliases”, which is the inverse of
what the engine implements. Pick one key pair, make the engine and
vocabulary agree, and guard the else-branch (`if (nzchar(new_frag))`).

------------------------------------------------------------------------

## 2. Recipe-vs-archive alignment (from the CSV cross-reference)

### H1. Bundled `file_pattern`s do not match the real archive files

Every cd, ci, and cp pattern ends in `.csv` with underscored versions
(`cd08a_EN_1_2p.csv`) while the archive delivers `.sav` with dotted
versions (`cd08a_EN_1.2p.sav`); cf patterns are `cfNNx_*p*.csv`; cs
patterns are bare wave ids with no wildcard (matching only because a
bare string is treated as an unanchored regex). The recipes were
evidently authored against a CSV-converted working copy. Consequences:
on a directory of freshly downloaded `.sav` files, **every wave of every
module** loads through the fallback prefix match, which (a) prints a
“matched via fallback pattern” line per wave, (b) is what enables the C8
over-matching, and (c) makes the documented pattern mechanism dead code
in practice. Recommend normalizing all patterns to the
extension-agnostic form the ca/cw recipes already use (`{wave_id}_*`),
and, if the CSV-based provenance matters, recording it in
`meta.source_spec` rather than in patterns.

Two archive naming variants worth encoding in tests, both present in
your folder: the early-wave order `{id}_{version}p_EN.sav`
(e.g. `ca08a_1.0p_EN.sav`, `cs12e_1.0p_EN.sav`) and the p-less version
`cs14g_EN_2.0.sav`.

### H2. `read_wave_file()` will parse anything as CSV

The extension dispatch falls through to
[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)
for any extension that is not sav/zsav/dta. Combined with the prefix
fallback, a stray `cd10c_EN_1.0p.pdf` codebook sitting next to the data
would be read as CSV and bound into the wave. Whitelist extensions and
abort on anything else:

``` r

ext <- tolower(tools::file_ext(path))
if (!ext %in% c("sav", "zsav", "dta", "csv")) {
  cli::cli_abort("unsupported data file extension {.val {ext}} for {.file {path}}")
}
```

Also constrain the fallback regex to data extensions:
`paste0("^", w$id, "[_.].*\\.(sav|zsav|dta|csv)$")`.

### H3. Recipes trail the archive by one wave in four modules

On disk but absent from the corresponding `wave_index`/`covered_waves`:
`ch25r`, `cp25q`, `cs25r`, `cv26r`. Users who download “all waves” will
see those files skipped with only a pre-scan message. Beyond onboarding
these four, two structural aids:
[`merge_liss_module()`](https://siardv.github.io/lissr/reference/merge_liss_module.md)
should report data files in `data_dir` that match the module prefix but
no wave entry (“1 file present but not covered by this recipe:
ch25r_EN_1.0p.sav”), and
[`onboard_new_wave()`](https://siardv.github.io/lissr/reference/onboard_new_wave.md)
needs its diff repaired; today it derives the previous wave’s suffixes
from the new file’s own names
(`gsub(new_wave_id, prev_wave_id, new_vars)` then strip), so `added` is
always empty by construction and `removed` is hard-coded empty. It
should locate and read the previous wave file via the recipe’s pattern
and diff actual suffix sets.

### H4. ca’s `year` is the asset reference year; the panel join mixes year semantics

The ca recipe deliberately sets `year` to the reference year (assets as
of 31 December), one less than the fieldwork year, and documents it in a
comment. But
[`merge_liss_panel()`](https://siardv.github.io/lissr/reference/merge_liss_panel.md)
joins modules on `wave_year`, so ca rows keyed 2007 align with ch/cv
rows whose 2007 means fieldwork year. That is a defensible analytic
choice for some designs and wrong for others, and nothing at the join
site surfaces it. Suggest: keep `wave_year` uniformly fieldwork-based
across recipes for joining, emit ca’s reference year as an additional
derived column (`asset_reference_year`), and have
[`merge_liss_panel()`](https://siardv.github.io/lissr/reference/merge_liss_panel.md)
document (and ideally message) the year semantics of each module it
joins. Independent of the choice, `wave_number` in ca could also be
exported, since biennial cadence makes `wave_year` gaps confusing
downstream.

### H5. The unknown-key warning fires on the package’s own recipes, about 95 rules across all ten

Loading any bundled recipe emits the “unrecognized rule-level key(s)”
warning; the aggregate across modules is roughly ninety-five flagged
rules. The mechanism is good (it caught C2, C4, C5), but when the
shipped recipes themselves trip it, users learn to ignore it, which
defeats its purpose. Triage the flagged keys into three buckets:

1.  Payload keys the engine should read: `items` (cp), `apply_to` (cs,
    cw), `suffix` (cr, ch, cv `VR08`), `target_suffixes` (cf, ca `B03`),
    `keep_exceptions` (ci `DR02`), `from`/`to` on cv `VR04` rename,
    `targets` on cv `VR05` ensure_column,
    `true_waves`/`levels`/`periods` variants (cr, ch, cp). Each is
    currently a silent behavior change (no-op or over-application).
2.  Pure annotation to add to `SANCTIONED_RULE_KEYS`: `label` (ci uses
    it on nine rules as a caption), `boundary` (ch, cd, cv), `pool`,
    `required`, `fill_missing_waves`, `sentinel_label`, and similar.
3.  Keys encoding features that need either implementation or explicit
    TODO entries: `preserve_original`/`original_suffix` (ca `H04`
    promises a preserved detail column `056_ca25j_detail` that is never
    created, undocumented), `create_flag_column`/`flag_suffix` (cw
    `HR01`, documented), `condition`-driven `conditional_label_swap` (ch
    `H01`), label-regex targeting (C5), `source_file`-driven
    `label_to_string` (cv `HR05` references an external crosswalk CSV
    the engine never loads).

The concrete goal: zero warnings when loading any bundled recipe,
enforced by a test that loads all ten and asserts no `cli` warnings.

### H6. Advertised subsystems that nothing consumes

`anomaly_registry.yml` and `global.taxonomy_refs` appear in the schema
and in the engine’s header comment (“anomaly registry integration (#7)”,
“taxonomy_refs support (#4)”), but no code reads either; the engine only
regex-validates `anomaly_ref` strings. Similarly,
`action_vocabulary.yml` statuses have drifted from reality: `crosswalk`,
`conditional_label_swap`, `label_to_string`, and `derive_combined_party`
are marked `stub` yet have working executors, while `fix_label` is
marked `implemented` with keys the engine does not read. Since the
vocabulary file claims to be “the single source of truth … from which
the CANONICAL_SCHEMA.md vocabulary tables are generated”, add the
generator script and a test that regenerates the schema tables and diffs
them; that closes the doc-vs-engine drift class permanently. Either wire
the anomaly registry in (resolve `anomaly_ref` to its archetype and
check the rule’s action/parameters against the template) or move it to
documentation and delete the integration claims.

------------------------------------------------------------------------

## 3. Engine robustness and design

### H7. Recipes can execute arbitrary code via `na_rate` conditions

`run_validations()` evaluates `chk$condition` with
`eval(parse(text = cond), envir = df, enclos = baseenv())`.
[`baseenv()`](https://rdrr.io/r/base/environment.html) still exposes
[`system()`](https://rdrr.io/r/base/system.html),
[`unlink()`](https://rdrr.io/r/base/unlink.html), and friends, so a
third-party recipe is a code-execution vector at merge time, which
undermines the “recipes are declarative data” premise. Replace with a
restricted evaluator: parse, walk the AST, allow only column names,
literals, comparison and logical operators, `%in%`, `is.na`, and reject
everything else. The same pattern fixes the second `eval(parse())` in
[`liss_select()`](https://siardv.github.io/lissr/reference/liss_select.md)
(see H10).

### H8. Error handling inside rule executors leaves partial mutations and misuses `<<-`

Each executor wraps its `switch` in `tryCatch`; on error it warns and
appends an `ERROR:` log entry via `log_entries <<- ...`. Two issues:
mutations made before the failure point persist, so a half-applied rule
flows downstream flagged only by a warning; and the `<<-` works only by
the accident of handler scoping, which is brittle under refactoring.
Cleaner: snapshot `df` before the switch, and in the handler restore the
snapshot and return it, so a failed rule is atomically a no-op with an
ERROR log line. (This also makes “re-run after fixing the recipe”
reasoning tractable.)

### H9. `merge_liss_panel()` has no duplicate-key guard before sequential joins

Any module result with duplicated `(nomem_encr, wave_year)` (the C8
scenario, or ca’s biennial reference years colliding with a future
annual wave) multiplies rows through the join chain. Add an explicit
pre-join assertion per module with a clear error naming the module and
the duplicate count, and pass `relationship = "one-to-one"` to the dplyr
joins so regressions surface loudly. Two smaller improvements in the
same function: `shared_cols` (`nohouse_encr`) is taken from the first
module only, leaving NA where later modules could have supplied it;
coalescing across modules would be strictly better. And `join_by`/id
handling hard-codes `nomem_encr` in a few places (`exec_drop_retain`’s
whitelist path likewise hard-codes `nomem_encr`, `wave_id`,
`wave_year`); route these through
`global$id_variable`/`wave_variable`/`year_variable` consistently.

### M1. `filter_rows` drops NA rows silently

The keep mask `!(wave == target & !(col %in% keep_vals))` removes rows
where the filter column is NA in the target wave, because
`NA %in% keep_vals` is FALSE. That may be intended (screening
variables), but it should be a documented, configurable choice
(`keep_na: true|false`) and the count of NA-driven drops should be
logged separately.

### M2. Regex-safety and name-resolution edges

`strip_wave_prefix()` and `strip_question_stem()` interpolate
`wave_id`/`stem` into regexes unescaped; use fixed matching
(`startsWith` plus `substring`) for prefixes and
`sub(..., fixed = TRUE)` for stems. `find_col()`’s candidate ladder
(`s`, `stem_`, `q`, `Q`) is sensible but undocumented in the schema;
document it, and add the bare-integer-suffix validator note from C4.
`resolve_check_cols()` and `expand_items()` are good utilities that the
schema never mentions; same remedy.

### M3. Package-load-time side effects

Top-level `local({ ... VALID_ACTIONS <<- ... })` and the
`source("liss_executors.R")` fallback execute at install/build time
inside a package; the vocabulary override also probes
[`getwd()`](https://rdrr.io/r/base/getwd.html) for a stray
`action_vocabulary.yml`, which could pick up an unrelated file during
build. Inside a package, drop the `exists("dv_aggregate")` dance
entirely (collation guarantees availability), and load the vocabulary
lazily at first
[`validate_recipe()`](https://siardv.github.io/lissr/reference/validate_recipe.md)
call from [`system.file()`](https://rdrr.io/r/base/system.file.html)
only, keeping the working-directory probes for the sourced-standalone
mode behind an explicit flag.

### M4. Misc engine nits

`wave_count`’s `counts` variable is dead code. `%||%` is defined twice
(package file and executors). The engine mixes `|>` and `%>%`; pick one
(the package imports magrittr). `generate_summary()` omits
`total_rows_dropped`/`total_vars_dropped`/`per_variable_na_rates` even
though the logging schema advertises them; either compute them (drop
rules already log enough to reconstruct) or trim the schema.
`derive_fieldwork_month`/`parse_time` are `copy_only` but log
`rows_affected = nrow(df)` even when no source resolved; log the
resolved source (or `NO_SOURCE`). Output filenames are fixed
(`{mod}_merged.sav`) and silently overwrite; add an `overwrite` argument
and optionally a format switch (`sav`/`parquet`/`rds`), since the label
round-trip (C9) is the only reason sav is privileged.

------------------------------------------------------------------------

## 4. Downloader, auth, and scraping

### H10. `liss_select()` evaluates raw wave input

`eval(parse(text = paste0("c(", wave_input, ")")))` executes whatever
the user types at the prompt. Interactive-only, but trivially replaced
by a safe parser:

``` r

parse_waves <- function(x) {
  x <- gsub("\\s", "", x)
  if (!grepl("^[0-9]+([:,-][0-9]+)*$", x)) return(NULL)
  parts <- strsplit(x, ",", fixed = TRUE)[[1]]
  out <- lapply(parts, function(p) {
    if (grepl("[:-]", p)) {
      bounds <- as.integer(strsplit(p, "[:-]")[[1]])
      seq.int(bounds[1], bounds[2])
    } else as.integer(p)
  })
  sort(unique(unlist(out)))
}
```

### H11. Download loop: no skip, no resume, no re-auth, memory-buffered writes

[`liss_download()`](https://siardv.github.io/lissr/reference/liss_download.md)
re-downloads files that already exist (the CSV shows 873 MB on disk; a
full re-run repeats all of it), buffers each file wholly in RAM via
`httr::content(as = "raw")`, and on session expiry keeps iterating so
every remaining file fails individually. Improvements, roughly in value
order: skip when the destination exists with matching size (offer
`overwrite = FALSE` default); on the first `session_expired`, stop the
loop and either prompt re-login or abort with a resume hint; stream to
disk (`httr::GET(url, httr::write_disk(path))` with the session’s
cookies, or migrate to httr2, which is the maintained successor); add
one retry with backoff for transient errors; and harden the
`content-disposition` parse for unquoted filenames. A `manifest.csv`
(file, md5, size, downloaded_at) written into `.dir` would also make
later integrity checks and the C8 disambiguation deterministic; the md5s
in your inventory CSV show exactly the shape this should take.

### M5. `liss_is_logged_in()` downloads a random data file

Verifying auth by `session_jump_to` on a randomly sampled `.sav`
(potentially many MB, and nondeterministic) is expensive and can itself
build the full blueprint as a side effect. A HEAD request to a known
auth-gated URL, or a GET of the account page checked for the login
redirect, does the same job in milliseconds.

### M6. Blueprint scraping resilience

[`liss_blueprint()`](https://siardv.github.io/lissr/reference/liss_blueprint.md)
fetches sequentially with no delay, retries nothing, and returns
silently partial results when a module or wave page errors (the per-page
`tryCatch(..., NULL)` swallows failures). Add: a small polite delay, one
retry, a tally of failed pages surfaced in the completion message (“2
wave pages failed; blueprint may be incomplete”), and an optional
on-disk cache (`~/.cache/lissr/blueprint.rds` with a timestamp and
`refresh` honoring it) so the scrape is not repeated every session. The
hard-coded selectors (`#id1 > .card-body`, `#id_mes`, `#id_dd`) will
break silently on a site redesign; assert non-empty results at each
level and abort with “the archive layout may have changed; please file
an issue” instead of returning an empty tibble.

------------------------------------------------------------------------

## 5. Testing

Current coverage is a handful of helper tests plus recipe-validation
smoke tests, and the recipe test loops over eight modules, omitting ca
and cr (cr being the module with the C1/C2 defects). The executors file
explicitly advertises that the kernels are “deliberately base-R … so
they are unit testable”, yet none of `crosswalk_map`,
`crosswalk_map_scheme`, `crosswalk_coverage`, `dv_aggregate`,
`transform_apply` has a test. Highest-value additions:

1.  Kernel unit tests, including the C1 chaining regression case and
    `dv_aggregate` edge cases (all-NA rows under both `missing_as_zero`
    settings, `presence`, `coalesce`).
2.  A synthetic end-to-end fixture: a tiny two-module recipe pair plus
    generated `.sav` files (haven can write them in the test),
    exercising strip-prefix, one recode, one boundary flag, one derived
    variable, one uniqueness check, and asserting on the merged frame,
    the JSONL log contents, and the report text. This is the single test
    that would have caught C2, C6, and C9.
3.  Recipe-integrity tests: all ten recipes load with zero warnings
    (post-H5); every `rule_id` referenced in TODO.md exists; every wave
    id referenced inside rules exists in `wave_index` (my scan found
    none today; the test keeps it that way); no `value_recode` mapping
    has source/target overlap unless the executor is snapshot-based.
4.  `discover_wave_files` tests over a temp directory covering: both
    archive naming orders, the p-less version,
    `.csv`-pattern-vs-`.sav`-file fallback, multi-version
    disambiguation, and the aux-file path.
5.  `merge_liss_panel` duplicate-key guard test.

------------------------------------------------------------------------

## 6. Documentation and packaging

The README/vignette label-preservation claims need the C9 correction.
The batch examples disagree on module count (README lists ten, the
merge-workflow vignette and the validation test list eight).
`Suggests: rlang` appears unused; drop it or use it. `httr` is
superseded upstream; plan a move to httr2 when touching the downloader.
`liss_recipe("nope")` surfaces the raw `system.file(mustWork=TRUE)`
error; wrap it to list the valid module codes.
[`liss_wave_matrix()`](https://siardv.github.io/lissr/reference/liss_wave_matrix.md)
prints inside the function; returning an object with a `print` method
(or at least `format`) is more idiomatic and testable. Finally, consider
adding `Language: en-US` and a `Config/Needs/website` if pkgdown is
planned; the vignette set is genuinely strong and deserves a site.

------------------------------------------------------------------------

## 7. Suggested priority order

| \# | Item | Sections |
|----|----|----|
| 1 | Snapshot-based recode + cr `suffix:` fix + chaining regression test | C1, C2 |
| 2 | Multi-file wave policy: version disambiguation, first-class `aux_files` with overlap assertion, unconditional per-wave duplicate check | C8 |
| 3 | Validation runner: SKIP for unknown types, controlled check-type vocabulary with aliases, `strict` mode | C6 |
| 4 | Scoping-key repairs across recipes (cp `items`, cs/cw `apply_to`, cf/ca `target_suffixes`, cv `recode`, ch `label_find`) plus sanctioning true annotations; zero-warning recipe test | C4, C5, H5 |
| 5 | Label round-trip: keep variable labels through `to_numeric`, restore value labels on write, fix doc claims | C9 |
| 6 | 2FA failure returns NULL; login hardening | C7 |
| 7 | `file_pattern` normalization + read extension whitelist + stale-wave onboarding (ch25r, cp25q, cs25r, cv26r) and `onboard_new_wave` diff repair | H1, H2, H3 |
| 8 | `NO_TARGETS` logging + rule-coverage invariant | C3 |
| 9 | Downloader: skip-existing, streaming, re-auth on expiry, manifest | H11 |
| 10 | Restricted expression evaluator for check conditions and wave input | H7, H10 |
| 11 | Panel join guards and shared-column coalescing | H9 |
| 12 | Vocabulary/schema generator + registry decision; year-semantics standardization | H6, H4 |
| 13 | End-to-end synthetic fixture and kernel tests | Section 5 |

------------------------------------------------------------------------

## 8. What the original `.sav` files and codebooks would let me verify

The CSV gets filename-level alignment; the following need file contents,
and I can check them mechanically if you upload the relevant sources:

1.  **cd10c overlap claim**: upload `cd10c_EN_1.0p.sav` and
    `cd10c_EN_1.1p.sav`; I will verify zero `nomem_encr` overlap (the
    premise of the stacking design) and report the union count against
    the recipe’s expectations.
2.  **cr religion crosswalks**: upload one wave per era (`cr08a`,
    `cr14g`, `cr19l`) plus their codebooks; I will read the value labels
    of suffixes 013/144 and confirm each era’s mapping table in
    `HR10-HR12` code by code, including the era-1 codes 5/6 to 3 and 14
    to 13 claims.
3.  **cp over-application blast radius**: upload `cp08a` (or any
    pre-cp18j wave); I will enumerate every numeric column containing
    the value 999 to quantify what the current all-numeric recode would
    destroy beyond items 010/011/019.
4.  **Sentinel regimes per wave**: with a small sample of early/late
    waves per module (for example `ci08a`, `ci20m`, `cv08a`, `cv20l`,
    `ca22h`) I can verify the recipes’ sentinel wave-scoping
    (999/9999999998/9999999999 vs -9/-8, and ca’s 11-digit and
    large-negative variants) against actual observed values.
5.  **role_map and suffix existence**: for any module you care most
    about, I can check that every suffix referenced by rules and
    `role_map`s exists in the corresponding wave file after prefix
    stripping, catching typos the engine currently converts into silent
    skips.
6.  **cs label-regex targets**: upload `cs08a` plus its codebook and I
    will produce the explicit suffix list for `A1_dk_recode` (variables
    whose value labels match the DK regex), turning the unimplementable
    rule into a concrete `suffixes:` list.
7.  **`fieldwork_ym` convention**: a couple of files would confirm
    whether the `_m` suffix convention the auto-derive relies on
    actually appears across modules and what its format is (yyyymm
    integer vs string), informing the `derive_fieldwork_month` upgrade
    from copy-only to a real parse.

The most informative minimal upload set: `cd10c` both versions, `cr08a`,
`cr14g`, `cr19l`, `cp08a`, `cs08a`, and the cr + cs codebook PDFs.
