# lissr

<!-- badges: start -->
[![R-CMD-check](https://github.com/siardv/lissr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/siardv/lissr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`lissr` is an R package for programmatic access to the
[LISS Data Archive](https://www.lissdata.nl/). It authenticates, browses, and
downloads survey files, then harmonizes, validates, and merges longitudinal
LISS panel waves with reproducible, recipe-driven YAML workflows that record
provenance. A rule-driven household-income cleaning framework records
decisions in audit ledgers and reports.

> **Repository history note:** The `v1.4.0` history was amended during release
> finalization while the GitHub workflow was being refined. For reproducibility,
> record the package version and Git commit used for an analysis.

## Installation

```r
# install from GitHub
# install.packages("remotes")  # if not already installed
remotes::install_github("siardv/lissr")
```

## Quick start

Credentials are stored in the operating system's keyring (macOS
Keychain, Windows Credential Store, or Linux Secret Service); the
`keyring` package is a dependency and installs with lissr.

```r
library(lissr)

# 1. store credentials (once, prompts for password)
liss_store_credentials("1234")

# 2. log in (credentials retrieved from keyring + 2FA prompt)
liss_login()

# 3. explore
liss_modules()
liss_wave_matrix()

# 4. interactively select modules, waves, file types
selection <- liss_select()

# 5. download
liss_download(selection)

# 6. merge downloaded core modules using their built-in recipes
results <- merge_liss_modules(
  data_dir = "liss",
  output_dir = "./output"
)
```

No module list or recipe paths are needed. The batch finds supported local
wave files (`.sav`, `.zsav`, `.dta`, `.csv`), then loads the built-in recipe
for each detected core module in alphabetical module-code order. For each
module, it uses `data_dir/ch/` (for example) when that subdirectory exists,
otherwise `data_dir` itself. Wave filenames must retain their module-wave
prefix, such as `ch07a_`; each recipe still determines which waves can be
processed.
Modules without local data are omitted from the automatic batch. Existing
recipe, comparability, and data warnings still apply to modules being merged.

Discovery uses the folder's contents, including earlier downloads; it does
not depend on the most recent `selection` or determine which variables your
analysis needs. Use a dedicated data folder to limit the batch to a particular
download. Explicit recipe paths and `merge_liss_module()` remain available
when you want to restrict processing or use a custom recipe.

## Vignettes

Worked examples ship with the package. After installing, list them with
`browseVignettes("lissr")` or open one by its name, for example
`vignette("getting-started", package = "lissr")`. You can also read the
rendered versions in your browser without installing:

- [Getting Started with lissr](https://siardv.github.io/lissr/articles/getting-started.html):
  a short orientation to the package and its workflow.
- [Merging LISS Panel Data](https://siardv.github.io/lissr/articles/merge-workflow.html):
  automatically merging downloaded modules, with optional recipe controls.
- [Longitudinal Panel Analysis](https://siardv.github.io/lissr/articles/longitudinal-panel-analysis.html):
  assembling and analyzing data across multiple waves.
- [Cross-Sectional Analysis with a Single Wave](https://siardv.github.io/lissr/articles/cross-sectional-analysis.html):
  working with one wave and attaching the Background Variables.
- [Multi-Module Linkage](https://siardv.github.io/lissr/articles/multi-module-linkage.html):
  joining several modules on the respondent id.
- [Custom Merge Recipes](https://siardv.github.io/lissr/articles/custom-recipes.html):
  writing or adapting a YAML recipe against the canonical schema.
- [Income Cleaning](https://siardv.github.io/lissr/articles/income-cleaning.html):
  rule-driven detection and constrained correction of implausible
  household-income values, with a full audit ledger.
- [Reproducible Research Pipelines](https://siardv.github.io/lissr/articles/reproducible-pipelines.html):
  structuring the workflow as a reproducible pipeline.
- [The Canonical Recipe Schema](https://siardv.github.io/lissr/articles/canonical-schema.html):
  the authoritative YAML structure every recipe conforms to, rendered
  from the packaged schema document.

## Merge system

The merge engine processes YAML recipes conforming to
`CANONICAL_SCHEMA.md`, schema version 1.1.0 (a strictly additive
extension of 1.0.0; recipes may declare either, and each recipe
additionally carries its own `recipe_version`). Each recipe encodes
every merge-relevant decision for a module: wave file patterns,
variable harmonization rules, boundary handling, comparability
contracts, and validation checks.

Every merged output carries provenance (package, recipe, and schema
versions, per-input md5 hashes, release-selection decisions) and an
explicit `valid_for_analysis` verdict, both in the returned object and
in the text report. `audit_liss_recipes()` produces a corpus-level
conformance report over the bundled recipes, suitable for CI gating.

Built-in recipes are included for all ten core LISS modules:
ch (Health), cv (Politics and Values), cd (Housing), cf (Family and
Household), cw (Work and Schooling), cp (Personality), cs (Culture and
Sports), ci (Economic Integration: Income), ca (Assets), and cr
(Religion and Ethnicity). The Background Variables file (`avars`) is a
separate monthly release, not one of these modules.

## Background variables

The merge engine covers the ten core study modules above. Monthly Background
Variables releases are available separately through the downloader; the merge
engine does not attach them. Demographics such as age, sex, education, income,
and household composition live in those separate files.

Refresh an existing catalogue after updating lissr. Background Variables has
`module_id = 322`, monthly ZIP releases have `type = "archive"` and a `wave`
code in YYYYMM, and its documents have `wave = NA`. The filenames, release
versions and descriptions are retained as published, without choosing a
latest month or preferring a version. `liss_select()` prompts for background
months separately from core waves, then offers ZIP archives and documents.
`liss_wave_matrix()` continues to show core study waves only.

```r
bp <- liss_blueprint(refresh = TRUE)
# illustrative month: choose it from the timing of the survey items
bg_files <- dplyr::filter(bp, module_id == 322L, type == "archive", wave == 202511L)
bg_files[c("wave", "name", "file")]
# inspect the available language and release version before downloading
stopifnot(nrow(bg_files) == 1L)
liss_download(bg_files, .dir = "data/avars")
```

The downloader requires `liss_login()` and verifies ZIP extraction. The
catalogue does not inspect archive contents or certify their data format.
Inspect the extracted files and verify their observed `wave` period before
using them. The default SPSS download selection still matches `.sav` files;
ZIP releases require an explicit selection.

Use `nomem_encr` as the respondent key, never `nohouse_encr`: household
assignments can change over time. For stacked monthly snapshots, join on
both the respondent and the selected month, reject duplicate background keys,
and never match missing keys. The survey month must be appropriate for the
items being analysed. In particular, three-part CV waves record part-1
`fieldwork_ym`, which is not an automatic month anchor for other parts.
The merge-workflow and cross-sectional-analysis guides show guarded joins
with verified local SPSS files; their file paths are illustrative.

`liss_clean_income()` can attach a background frame itself (its `P01`
rule aligns monthly `avars` waves to the annual scale and reports the
join match rate); see the income-cleaning vignette.

## File formats

The package has been developed and tested only with SPSS `.sav` files, which is
the default format throughout. The downloader can also fetch Stata `.dta` files,
and the engine includes a read path for them (`haven::read_dta`), but `.dta`
input has never been tested. Treat `.dta` support as experimental: there is no
guarantee the merge pipeline produces correct results from `.dta` sources, so
validate any `.dta`-based output yourself. The engine can also read `.csv` files
via `readr`.

A built-in fallback matches a wave file by its `wave_id` prefix when a recipe's
`file_pattern` extension does not match the file on disk (for example a recipe
written for `.sav` run against a downloaded `.dta`). This only locates the file;
it does not validate that a non-`.sav` format is handled correctly downstream.

### Directory and naming contract

The engine discovers each wave's file inside `data_dir` by the recipe's
`file_pattern`, which is the canonical `{wave_id}_*` glob for every
bundled recipe (matching archive names such as `ch07a_EN_1.0p.sav`).
The automatic batch accepts a common parent directory with module
subdirectories (for example `data/ch/`) or a flat directory with wave files
from several modules. The single-module helper also accepts that module's
own directory. When several files match
one wave, the engine ranks release versions, records the decision in
the provenance block, and a `wave_index` entry may pin
`expected_release`; a violated pin invalidates the output and aborts
under `strict = TRUE`.

## Validate recipes without merging

```r
recipe <- liss_recipe("ch")
validate_recipe(recipe, "ch_merge_recipe.yml")
```

`validate_recipe()` also emits a non-fatal warning listing any
rule-level key the engine neither consults nor sanctions as documentation, so a
mis-named key is surfaced at authoring time instead of being silently ignored.
The recognized and sanctioned key sets are documented in `CANONICAL_SCHEMA.md`.

## Onboard a new wave

```r
onboard_new_wave(
  recipe_path = system.file("recipes", "ch_merge_recipe.yml", package = "lissr"),
  new_file    = "ch25r_EN_1_0p.csv",
  prev_wave_id = "ch24q"
)
```

## A note on how this package was built

I started building `lissr` in June 2021, before AI coding assistants were a
realistic option, and it has been a constant companion project ever since.
The problem it addresses, the recipe grammar, the merge and harmonization
logic, and the design decisions grew out of five years of reading LISS
codebooks, breaking merges, and rebuilding them.

I also want to be open about the fact that AI language models (including
Anthropic's Claude) contributed to later versions. I used them as
assistants, not as authors: to review code, stress-test the merge engine,
cross-reference recipe rules against codebooks and real data files, propose
refactorings, draft tests and documentation, and speed up the grueling
parts of package development. Nothing was accepted on trust. Every
suggestion was read, questioned, run, and frequently rejected or rewritten.
Whatever ships has passed the full test suite and R CMD check, and
responsibility for every line, including the mistakes, is mine alone.

`lissr` exists to make merge and harmonization decisions in panel data
explicit instead of silent. It seems only consistent to be equally explicit
about how the package itself was made. If you have questions about any part
of that process, the issue tracker is open.

## License

MIT
