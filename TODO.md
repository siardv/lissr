# TODO and known limitations

This file tracks known limitations and planned work for lissr. For bug reports
and feature requests, please use the GitHub issue tracker.

## Known limitations

- File formats. The package has been tested only with SPSS `.sav` files. Stata
  `.dta` download and a `.dta` read path exist but are untested and not
  guaranteed; `.csv` is also read but not formally validated. See the "File
  formats" section of the README.
- Background variables. Demographics from the LISS Background Variables (`avars`)
  file are not merged automatically; they must be downloaded and joined on
  `nomem_encr`. See the "Background variables" section of the README.
- Crosswalk alias keys. Two recipes carry alias keys the engine does not read;
  this is documented as a known limitation in `CANONICAL_SCHEMA.md` and is left
  as-is to keep merged outputs stable.
- Recipe rules pending execution. Some rules validate against the schema but do
  not yet execute (the validator emits non-fatal warnings for them). Known
  cases:
  - cr: BR20, BR21, BR30 (split_variable with no output_vars)
  - cf: VAR-002, VAR-003, VAR-004 (type_coerce via target_suffixes)
  - ca: H05, H07 (fix_label via find/replace)
  - cv: HR01 through HR04 (recode_to_na via recode/sentinel_label)
  - cw: HR01 (missing flag column)

## Planned work

- Test and validate Stata `.dta` input end to end, or document and remove the
  `.dta` download option if it will not be supported.
- Implement the recipe rules listed above that currently validate but do not
  execute.
- Optional codebook cross-validation: check recipe assumptions (code lists, wave
  ranges, variable presence) against the LISS codebooks.

## Tracking

The GitHub issue tracker is the canonical place for ongoing work; the items
above can be filed as issues and linked to commits and pull requests.
