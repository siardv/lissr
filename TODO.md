# TODO and known limitations

This file tracks known limitations and planned work for lissr. For bug
reports and feature requests, please use the GitHub issue tracker.

## Known limitations

- File formats. The package has been tested only with SPSS `.sav` files.
  Stata `.dta` download and a `.dta` read path exist but are untested
  and not guaranteed; `.csv` is also read but not formally validated.
  See the “File formats” section of the README.
- Background variables. Demographics from the LISS Background Variables
  (`avars`) file are not merged automatically; they must be downloaded
  and joined on `nomem_encr`. See the “Background variables” section of
  the README.
- Crosswalk alias keys. Two recipes carry alias keys the engine does not
  read; this is documented as a known limitation in
  `CANONICAL_SCHEMA.md` and is left as-is to keep merged outputs stable.
- Recipe rules pending execution. Some rules validate against the schema
  but do not yet execute (the validator emits non-fatal warnings for
  them). Known cases:
  - cr: BR20, BR21, BR30 (split_variable with no output_vars)
  - cf: VAR-002, VAR-003, VAR-004 (type_coerce via target_suffixes)
  - ca: H05, H07 (fix_label via find/replace)
  - cw: HR01 (missing flag column)

  Resolved in 1.1.0: cv HR01 through HR04 (engine now reads the `recode`
  alias and `exclude` blocks); cr HR10 through HR12 and HR20/HR21
  (recipe keys corrected to `suffixes`); cp A1 DK recodes (keys
  corrected to `suffixes`); cs A1_dk_recode (rewritten as an executable
  value_recode on the verified suffixes).

## Planned work

- Test and validate Stata `.dta` input end to end, or document and
  remove the `.dta` download option if it will not be supported.
- Implement the recipe rules listed above that currently validate but do
  not execute.
- Optional codebook cross-validation: check recipe assumptions (code
  lists, wave ranges, variable presence) against the LISS codebooks. A
  `verify_recipe_against_data()` helper would institutionalize the scans
  used for the 1.1.0 verification report.
- Onboard the archive waves not yet covered by recipes: ch25r, cp25q,
  cs25r, cv26r. Drafted `wave_index` entries, measured structural diffs
  against each predecessor, and the per-module lists of wave-scoped
  rules to extend are in `lissr-verification-report.md` (section 6).
  cv26r changes five suffixes and needs a boundary review; the other
  three are structurally clean.
- Extend the cs A1 DK verification (code-anchored label scan) from cs08a
  to the remaining pre-cs20m waves before declaring the suffix list
  final.
- cv fieldwork month: recent cv waves carry no `{wave_id}_m` variable,
  so DV09_fieldwork_month materializes as all-NA; populate it via
  `wave_values` and upgrade `derive_fieldwork_month` to an integer
  yyyymm parse.
- Consider
  [`haven::tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html)
  or re-declared user-missing ranges to keep the DK-vs-refusal
  distinction in outputs beyond what the 1.1.0 restore/sweep preserves.

## Tracking

The GitHub issue tracker is the canonical place for ongoing work; the
items above can be filed as issues and linked to commits and pull
requests.
