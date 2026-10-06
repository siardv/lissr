# Cross-Sectional Analysis with a Single Wave

## When to use this workflow

A cross-sectional design treats one wave as a population snapshot. This
is appropriate when you need a point-in-time estimate (prevalence,
association) and do not require within-person change. Common examples
include a Master’s thesis using a selected Health wave, or a methods
paper demonstrating a new estimator on a single data release.

Even for a single-wave analysis, lissr helps you select files and apply
the recipe’s declared harmonization and validation rules. Review the
module report, variable definitions, missing values and comparability
limits before deciding whether the result supports your analysis.

## Step 1 — download one wave of one module

``` r

library(lissr)
library(magrittr)

# authenticate (once per session)
liss_login()

# build the file inventory
bp <- liss_blueprint()

# pin the release used in this example
health_file <- "ch25r_EN_1.0p.sav"
selected_health <- bp %>%
  dplyr::filter(
    module == "Health",
    type == "spss",
    .data$file == .env$health_file
  )
stopifnot(nrow(selected_health) == 1L)

selected_health

# download just that one file
liss_download(selected_health, .dir = "data/ch")
```

The example is pinned to Health wave `ch25r`, release
`ch25r_EN_1.0p.sav`; it does not promise that this is the latest wave.
Keep the published filename unchanged. If you choose another wave,
update the selected file and recipe wave together. Use a dedicated data
folder and review release notes before merging competing versions.

## Step 2 — read and clean the data

You can read the file directly with `haven`, or use the merge engine for
a single wave. The recipe applies its declared recodes, including
specified missing-value codes; it does not replace checking the codebook
or guarantee that every value is suitable for modelling. The model below
uses the recipe result, not the unprocessed `raw` object.

``` r

# option A: read raw and clean yourself
raw <- haven::read_sav(file.path("data/ch", health_file))
dim(raw)

# option B: use the merge engine for a single wave
recipe <- liss_recipe("ch")

# temporarily trim the recipe to just the wave you need
recipe$wave_index <- purrr::keep(
  recipe$wave_index,
  ~ .x$id == "ch25r"
)

result <- merge_liss_module(recipe, data_dir = "data/ch", output_dir = "output")
health <- result$data
dim(health)
result$valid_for_analysis
```

The Health recipe renames suffixes such as `ch25r004` to `s004`, applies
its declared missing-code recodes, and records `wave_id`, `wave_year`
and `fieldwork_ym`. Inspect `output/ch_merge_report.txt`, including
warnings and documentary or unevaluated checks. A validation verdict is
limited to the checks actually run, not a general endorsement of an
analysis.

## Step 3 — attach background variables

The model below uses age, sex and education from Background Variables
(`avars`), a separate monthly snapshot. Survey preloads and background
covariates need not describe freshly answered items at the same time;
choose their sources and timing for your research question.

Use `nomem_encr` as the respondent identifier, never `nohouse_encr`:
household assignments change over time. Select the month for the survey
items being analysed. Join on both `nomem_encr` and the caller-selected
month when stacking monthly background files; reject duplicate
background keys and never match missing keys.

`fieldwork_ym` follows the recorded date selected by the module recipe.
For Politics and Values (`cv`), eight single-part waves use `_m`;
`cv16h` uses `maandnr` for group 0 and `maandnr_lang` for groups 1/2;
nine three-part waves use part 1 (`_m1`) only. A missing designated date
stays `NA`, without year inference or fallback to another part. Part-1
timing is not automatically the appropriate background month for part-2,
part-3 or particular CV items. Check their timing before choosing a
snapshot, even if a merged column’s label no longer conveys its
part-specific meaning.

[Health wave
`ch25r`](https://www.dataarchive.lissdata.nl/study-units/view/1661) was
collected in November and December 2025, including a repeat for November
noncompleters. Its `_m` field describes the year and month of the
fieldwork period; questionnaire start and end dates are separate fields.
Start and completion can cross a month boundary. The recipe copies `_m`
into `fieldwork_ym`, which is not automatically the appropriate month
for every item or covariate.

Inspect the recorded months before choosing an attachment month:

``` r

table(health$fieldwork_ym, useNA = "ifany")
```

The following illustration uses `fieldwork_ym` only after you have
decided that this anchor fits the items and covariates being analysed.
It retains the whole selected wave and shows two explicitly chosen
months. If your analysis intentionally uses one month or another
subsample, select and document those survey rows first; supplying one
background month does not define that subsample. Do not infer a
replacement month from a year, filename, another date column or the
nearest available snapshot.

Select monthly Background Variables ZIP releases with
`liss_blueprint(refresh = TRUE)` or
[`liss_select()`](https://siardv.github.io/lissr/reference/liss_select.md)
(`module_id = 322`, `type = "archive"`, `wave = YYYYMM`). Inspect
release descriptions and extracted contents. The catalogue does not
verify payload format or observed periods. The local SAV paths and
expected months below are illustrative; replace them together with the
files you explicitly selected. The helper does not download or extract
archives.

``` r

# caller-confirmed month column for this illustration
month_col <- "fieldwork_ym"
sources <- data.frame(
  sav_path = c(
    "data/avars/avars_202511_EN_1.0p.sav",
    "data/avars/avars_202512_EN_1.0p.sav"
  ),
  expected_month = c(202511L, 202512L)
)

attachment <- liss_attach_background(
  data = health,
  sources = sources,
  month_col = month_col,
  variables = c("leeftijd", "geslacht", "oplcat")
)

analysis_df <- attachment$data
stopifnot(nrow(analysis_df) == nrow(health))
attachment$audit
attachment$provenance
```

[`liss_attach_background()`](https://siardv.github.io/lissr/reference/liss_attach_background.html)
validates supplied SAV signatures, observed months, complete unique
background respondent-month keys, and compatible selected-variable
storage and metadata. Differences in labels or SPSS missing definitions
require an explicit harmonization decision before pooling sources. It
preserves survey rows, original columns and metadata, including repeated
observations. Missing survey keys never match.

The audit distinguishes unmatched survey rows from matched rows whose
covariates are missing; matching a key does not certify a usable model
row. Review coverage and the meanings of `leeftijd`, `geslacht` and
`oplcat` before modelling. The helper retains labels and missing
definitions on `avars_leeftijd`, `avars_geslacht` and `avars_oplcat`.
Provenance records file identities and metadata fingerprints, not raw
respondent keys or value-label contents. These identities do not certify
the release’s authenticity, item freshness or analytical timing. This
illustration does not change the separate annual income attachment
policy.

## Step 4 — run an analysis

Self-rated general health is `s004`, with categories 1 poor, 2 moderate,
3 good, 4 very good and 5 excellent. `s001` is a gender preload, not the
health outcome. Verify these definitions against the selected Health
codebook and review the attached covariates’ metadata.

The example preserves an illustrative numeric education slope: a
one-code increase in `oplcat` receives the same model step, which needs
substantive justification. Its `female` indicator compares sex code 2
with every other observed nonmissing code, rather than assuming there
are only two categories. Review coding, sample restrictions,
missing-data treatment and the proportional-odds assumption for your
actual analysis.

This model-only frame converts declared SPSS missing values to `NA`
before numeric conversion; the attached source columns remain unchanged.
Inspect missingness and choose your analysis sample before fitting. The
checks below deliberately stop on unexpected health codes or unresolved
missing model values instead of silently choosing a sample.

``` r

model_df <- analysis_df %>%
  dplyr::transmute(
    health_code = as.numeric(haven::zap_missing(s004)),
    age = as.numeric(haven::zap_missing(avars_leeftijd)),
    sex = as.numeric(haven::zap_missing(avars_geslacht)),
    edu_level = as.numeric(haven::zap_missing(avars_oplcat))
  )
stopifnot(all(is.na(model_df$health_code) | model_df$health_code %in% 1:5))

model_df <- model_df %>%
  dplyr::mutate(
    srh = factor(health_code, levels = 1:5, ordered = TRUE,
                 labels = c("poor", "moderate", "good", "very good", "excellent")),
    female = as.integer(sex == 2)
  )

colSums(is.na(model_df[c("srh", "edu_level", "age", "female")]))
stopifnot(all(stats::complete.cases(model_df[c("srh", "edu_level", "age", "female")])))
fit <- MASS::polr(srh ~ edu_level + age + female, data = model_df,
                 na.action = stats::na.fail)
summary(fit)
```

All examples in this guide are illustrative and are not executed while
rendering. Printed selections, dimensions, coverage and fitted estimates
must come from your own chosen inputs and checks; no panel-data model
fit or analytical month policy is established by this vignette.

## When *not* to use this approach

If your research question involves change (did self-rated health improve
after a policy?), or if you want to exploit the panel structure for
causal identification (fixed effects, difference-in-differences), you
should move to the longitudinal workflow described in [Longitudinal
Panel
Analysis](https://siardv.github.io/lissr/articles/longitudinal-panel-analysis.md)
*(or run
[`vignette("longitudinal-panel-analysis", package = "lissr")`](https://siardv.github.io/lissr/articles/longitudinal-panel-analysis.md)
in the console)*. A single wave cannot separate age, period, and cohort
effects, and it provides no within-person variation to control for
time-invariant confounders.

## Checklist

Before submitting results from a cross-sectional LISS analysis, verify:

You joined background variables on `nomem_encr` (not `nohouse_encr`).

Your month column fits the chosen items and covariates, and each
explicit snapshot has the expected observed period.

Whole-wave or intentional subsample scope is documented; unmatched rows,
missing covariates and model sample exclusions were reviewed.

Outcome `s004`, background coding, selected metadata and missing-value
definitions were checked against the exact releases and codebooks.

Recipe recodes and model-only missing-value conversions were reviewed;
no undeclared sentinel or automatic recode policy is assumed.

You report the wave identifier (e.g. ch25r) and the LISS data version
number in your methods section so results are reproducible.

If you selected a single wave from a module with instrument changes
(boundary rules), note that in your limitations.
