# onboard a new wave into a merge recipe

semi-automated workflow that reads the new wave file, locates and reads
the actual previous wave file (resolved through the recipe's
`file_pattern`, with the engine's release-version disambiguation), diffs
the real variable suffix sets in both directions, generates a candidate
`wave_index` entry, checks expected-presence constraints, flags
potential boundary breaks, and prints an onboarding checklist.

## Usage

``` r
onboard_new_wave(recipe_path, new_file, prev_wave_id = NULL, prev_file = NULL)
```

## Arguments

- recipe_path:

  character. path to the current canonical YAML recipe.

- new_file:

  character. path to the new wave data file (.sav, .dta, or .csv).

- prev_wave_id:

  character. wave id to diff against (e.g. `"ch24q"`). if `NULL`, the
  diff step is skipped.

- prev_file:

  character. optional explicit path to the previous wave file. if
  `NULL`, it is resolved via the recipe's `file_pattern` for
  `prev_wave_id` in the same directory as `new_file`.

## Value

an onboarding report list (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
onboard_new_wave(
  "ch_merge_recipe.yml",
  "ch25r_EN_1.0p.sav",
  prev_wave_id = "ch24q"
)
} # }
```
