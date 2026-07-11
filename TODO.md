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

- ca H04 collapses the ca25j debt brackets without preserving the
  un-collapsed detail; the engine has no copy-before-recode mechanism.
  Either add a `copy_column` variable action or document the loss
  permanently. (The rule’s description now states the loss; resolved as
  a promise, open as a feature.)

- Recipe rules pending execution. Some rules validate against the schema
  but do not yet execute (the validator emits non-fatal warnings for
  them). Known cases:

  - (cr BR20/BR21/BR30 resolved in 1.3.2.9000 stage 5b: split_variable
    now ships executable output_vars producing pre/post-2019 columns for
    the attendance, prayer, and afterlife scale breaks)
  - (cf resolved in 1.3.2.9000 stage 5: labelled_policy switched to
    to_numeric, HARM-001 through HARM-007 re-scoped or re-expressed
    against a 17-wave archive scan, VAR-002/VAR-004 re-keyed and
    executing, VAR-003 retired with evidence; see
    inst/scripts/verification/cf_scan.R and NEWS)
  - ca: H05, H07 (fix_label via find/replace)
  - cw: HR01 (missing flag column)

  Resolved in 1.3.2.9000 (stage 2): cr BR01/BR02/BR10/BR60/BR80, ch B12,
  cp A9, ca B02/B03 (degenerate flag columns now populate); cw HR03
  (pension dates now recode in cw25r); ch B06 and ci A-09
  crosswalk_rename alias keys (harmonized columns now produced).

  Resolved in 1.1.0: cv HR01 through HR04 (engine now reads the `recode`
  alias and `exclude` blocks); cr HR10 through HR12 and HR20/HR21
  (recipe keys corrected to `suffixes`); cp A1 DK recodes (keys
  corrected to `suffixes`); cs A1_dk_recode (rewritten as an executable
  value_recode on the verified suffixes).

## Planned work

- Test and validate Stata `.dta` input end to end, or document and
  remove the `.dta` download option if it will not be supported.
- Offline network test infrastructure (assessment A6): the stage-7 shim
  mocks cover the download and probe error paths; a full fixture server
  or injectable base URL would extend coverage to the login and
  blueprint scrape flows.
- Implement the recipe rules listed above that currently validate but do
  not execute.
- Optional codebook cross-validation: check recipe assumptions (code
  lists, wave ranges, variable presence) against the LISS codebooks. A
  `verify_recipe_against_data()` helper would institutionalize the scans
  used for the 1.1.0 verification report.
- Extend the cs A1 DK verification (code-anchored label scan) from cs08a
  to the remaining pre-cs20m waves before declaring the suffix list
  final.
- cv fieldwork month: DV09_fieldwork_month executes since 1.3.2.9000
  stage 5b (fieldwork_ym mod 100) and carries real months wherever a
  wave has a `{wave_id}_m` variable; recent cv waves have none, so their
  months remain NA. Backfill those waves via `wave_values` once the
  fieldwork calendar is verified against archive metadata.
- Consider
  [`haven::tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html)
  or re-declared user-missing ranges to keep the DK-vs-refusal
  distinction in outputs beyond what the 1.1.0 restore/sweep preserves.
- Income cleaning, rule-conditional anchors (assessment C8): scale
  errors should anchor to the rescaled value rather than the household
  median, so “correction” is more than within-household median
  imputation on planted scale errors. Deferred from the 1.3.2.9000
  stage-6 pass (C1-C7, C9, A8 are resolved there; see NEWS).

## Tracking

The GitHub issue tracker is the canonical place for ongoing work; the
items above can be filed as issues and linked to commits and pull
requests.
