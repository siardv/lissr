## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)


## ----download-single----------------------------------------------------------
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


## ----clean-via-recipe---------------------------------------------------------
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


## ----inspect-months-----------------------------------------------------------
table(health$fieldwork_ym, useNA = "ifany")


## ----attach-demographics------------------------------------------------------
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


## ----model--------------------------------------------------------------------
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

