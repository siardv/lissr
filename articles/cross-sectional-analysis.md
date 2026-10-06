# Cross-Sectional Analysis with a Single Wave

## When to use this workflow

A cross-sectional design treats one wave as a population snapshot. This
is appropriate when you need a point-in-time estimate (prevalence,
association) and do not require within-person change. Common examples
include a Master’s thesis using the most recent Health wave, or a
methods paper demonstrating a new estimator on a single data release.

Even for a single-wave analysis, lissr adds value: it automates
authentication, lets you pick exactly the files you need, and gives you
access to the merge engine’s sentinel-code cleaning so you start from
analysis-ready data instead of raw SPSS.

## Step 1 — download one wave of one module

``` r

library(lissr)

# authenticate (once per session)
liss_login()

# build the file inventory
bp <- liss_blueprint()

# filter to the latest Health wave, SPSS format
latest_health <- bp |>
  dplyr::filter(
    module == "Health",
    type   == "spss"
  ) |>
  dplyr::filter(wave == max(wave))

latest_health
#> # A tibble: 1 × 8
#>   module module_id  wave wave_id type  name       file              path
#>   <chr>      <int> <int>   <int> <chr> <chr>      <chr>             <chr>
#> 1 Health        18    18    1102 spss  ch25r 1.0p ch25r_1_0p_EN.sav /down…

# download just that one file
liss_download(latest_health, .dir = "data/ch")
```

## Step 2 — read and clean the data

You can read the file directly with `haven`, or you can leverage the
merge engine to apply the recipe’s sentinel-code recoding even for a
single wave. The recipe knows which numeric codes are “don’t know” vs
“prefer not to say” vs genuine missing — cleaning you would otherwise do
by hand with a codebook open.

``` r

library(haven)
library(dplyr)

# option A: read raw and clean yourself
raw <- haven::read_sav("data/ch/ch25r_1_0p_EN.sav")
dim(raw)
#> [1] 4892  271

# option B: use the merge engine for a single wave
# (this applies prefix stripping, sentinel recoding, and labelled policy)
recipe <- liss_recipe("ch")

# temporarily trim the recipe to just the wave you need
recipe$wave_index <- purrr::keep(
  recipe$wave_index,
  ~ .x$id == "ch25r"
)

result <- merge_liss_module(recipe, data_dir = "data/ch", output_dir = "output")
health <- result$data
dim(health)
#> [1] 4892  265
```

The merge engine’s output has cleaner column names (no `ch24q` prefix),
sentinel values already recoded to `NA`, and a `wave_id` / `wave_year`
column appended.

## Step 3 — attach background variables

Most cross-sectional analyses need demographics: age, sex, education,
income. These live in the Background Variables file (`avars`, a separate
monthly release), not in the survey module itself.

Use `nomem_encr` as the respondent identifier, never `nohouse_encr`:
household assignments change over time. Select the month for the survey
items being analysed. Join on both `nomem_encr` and `fieldwork_ym` when
stacking monthly background files; reject duplicate background keys and
never match missing keys.

`fieldwork_ym` follows the recorded date selected by the module recipe.
For Politics and Values (`cv`), eight single-part waves use `_m`;
`cv16h` uses `maandnr` for group 0 and `maandnr_lang` for groups 1/2;
nine three-part waves use part 1 (`_m1`) only. A missing designated date
stays `NA`, without year inference or fallback to another part. Part-1
timing is not automatically the appropriate background month for part-2,
part-3 or particular CV items. Check their timing before choosing a
snapshot, even if a merged column’s label no longer conveys its
part-specific meaning.

This example uses Health data. Obtain the monthly Background Variables
release separately from the LISS Data Archive and extract its SPSS file
locally; the current blueprint file inventory does not provide these
background downloads. The November 2025 month and file path below are
illustrative. Inspect the actual Health fieldwork month and replace both
together; the checks verify observed periods before the join.

``` r

expected_month <- 202511L
survey_month <- unique(health$fieldwork_ym[!is.na(health$fieldwork_ym)])
stopifnot(length(survey_month) == 1L, survey_month == expected_month)
bg_raw <- haven::read_sav("data/avars/avars_202511_EN_1_0p.sav")
bg_month <- unique(as.integer(bg_raw$wave))
stopifnot(length(bg_month) == 1L, !anyNA(bg_month),
          bg_month == expected_month)

avars <- haven::zap_labels(bg_raw) %>%
  dplyr::mutate(fieldwork_ym = as.integer(wave)) %>%
  dplyr::select(
    nomem_encr, fieldwork_ym,
    age       = leeftijd,
    sex       = geslacht,
    edu_level = oplcat,
    hh_income = nettohh_f,
    urban     = sted
  )

bg_keys <- avars[c("nomem_encr", "fieldwork_ym")]
stopifnot(!anyNA(bg_keys), !anyDuplicated(bg_keys))
analysis_df <- dplyr::left_join(
  health, avars,
  by = c("nomem_encr", "fieldwork_ym"), na_matches = "never"
)
stopifnot(nrow(analysis_df) == nrow(health))

nrow(analysis_df)
#> [1] 4892
```

## Step 4 — run an analysis

With the merged, cleaned data you can proceed to standard modelling. For
example, estimating the association between education and self-rated
health (suffix `s001` in the harmonised output), adjusting for age and
sex:

``` r

analysis_df <- analysis_df |>
  dplyr::mutate(
    srh = factor(s001, levels = 1:5,
                 labels = c("poor", "moderate", "good", "very good", "excellent")),
    female = as.integer(sex == 2)
  )

fit <- MASS::polr(srh ~ edu_level + age + female, data = analysis_df)
summary(fit)
```

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

The background variables file matches the fieldwork month, not the
calendar year.

Sentinel codes (-9 = don’t know, -8 = prefer not to say, 999, etc.) have
been recoded to `NA` — the merge engine does this automatically, but
check if you loaded raw SPSS directly.

You report the wave identifier (e.g. ch25r) and the LISS data version
number in your methods section so results are reproducible.

If you selected a single wave from a module with instrument changes
(boundary rules), note that in your limitations.
