# ============================================================================
# stage 1 (v1.4 development) regressions: fieldwork_ym derivation order,
# factor-safe cross-wave type harmonization, sweep-veto name forms, and
# value_recode audit traces for unresolved or non-numeric targets.
# ============================================================================

# ---- fieldwork_ym derives before expected_presence (M14) --------------------

test_that("fieldwork_ym derives from _m even when expected_presence declares it", {
  skip_if_not_installed("haven")

  data_dir <- file.path(tempdir(), "lissr_s1_data")
  out_dir  <- file.path(tempdir(), "lissr_s1_out")
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(c(data_dir, out_dir), recursive = TRUE), add = TRUE)

  # wave 1 carries the {wave_id}_m fieldwork variable; wave 2 does not
  haven::write_sav(
    data.frame(nomem_encr = 1:2,
               xx01a005   = c(1, 2),
               xx01a_m    = c(200101, 200102)),
    file.path(data_dir, "xx01a_EN_1.0p.sav"))
  haven::write_sav(
    data.frame(nomem_encr = 1:2,
               xx02b005   = c(3, 4)),
    file.path(data_dir, "xx02b_EN_1.0p.sav"))

  recipe <- list(
    meta = list(
      module = "xx", module_label = "Fixture", schema_version = "1.0.0",
      recipe_version = "t", created = "t", source_spec = "t",
      covered_waves = list("xx01a", "xx02b")
    ),
    global = list(
      id_variable = "nomem_encr", wave_variable = "wave_id",
      year_variable = "wave_year", labelled_policy = "to_numeric",
      missing_variable_policy = "warn_and_create_na",
      strip_label_whitespace = TRUE,
      # the nine-recipe pattern that used to suppress the derivation:
      expected_presence = list(critical = list(
        list(variable = "nomem_encr", waves = "all", on_absence = "error"),
        list(variable = "fieldwork_ym", waves = "all", on_absence = "warn")
      ))
    ),
    wave_index = list(
      list(id = "xx01a", year = 2001, file_pattern = "xx01a_*"),
      list(id = "xx02b", year = 2002, file_pattern = "xx02b_*")
    ),
    logging = list(log_file = "xx_log.jsonl", report_file = "xx_report.txt",
                   summary_artifact = FALSE)
  )

  res <- suppressWarnings(suppressMessages(
    merge_liss_module(recipe, data_dir, out_dir)))
  d <- res$data

  # the declaring recipe no longer blanks the derived column
  expect_equal(as.numeric(d$fieldwork_ym[d$wave_id == "xx01a"]),
               c(200101, 200102))
  # a wave without _m still yields the NA placeholder (warn path unchanged)
  expect_true(all(is.na(d$fieldwork_ym[d$wave_id == "xx02b"])))
})

# ---- factor-safe cross-wave type harmonization (M5) -------------------------

test_that("factor/numeric type conflicts coerce via as.character, not level codes", {
  d1 <- data.frame(v = factor(c("no", "yes", "dk")))
  d2 <- data.frame(v = c(0, 1, 99))

  expect_warning(
    out <- lissr:::harmonize_column_types(list(a = d1, b = d2)),
    "factor"
  )
  # factor values survive as their labels, never as level indices 1..k
  expect_identical(out$a$v, c("no", "yes", "dk"))
  expect_identical(out$b$v, c("0", "1", "99"))
})

test_that("numeric-only type conflicts keep the numeric coercion path", {
  d1 <- data.frame(v = 1:3)              # integer
  d2 <- data.frame(v = c(1.5, 2, 3))     # double
  out <- suppressMessages(lissr:::harmonize_column_types(list(a = d1, b = d2)))
  expect_true(is.numeric(out$a$v))
  expect_true(is.numeric(out$b$v))
  expect_equal(out$a$v, c(1, 2, 3))
})

# ---- sweep veto honors the q/Q name forms (find_col parity) -----------------

test_that("sweep_user_missing honors exclude vetoes for q-prefixed columns", {
  registry <- new.env(parent = emptyenv())
  registry[["q005"]] <- list(list(labels = NULL, na_values = 999,
                                  na_range = NULL, wave = "xx01a",
                                  vlab = NULL))
  merged <- data.frame(wave_id = c("xx01a", "xx01a"),
                       q005    = c(999, 5))

  no_veto <- lissr:::sweep_user_missing(merged, registry, "q005")
  expect_equal(no_veto$swept, 1L)
  expect_true(is.na(no_veto$data$q005[1]))

  vetoed <- lissr:::sweep_user_missing(merged, registry, "q005",
                                       veto = list(`005` = "xx01a"))
  expect_equal(vetoed$swept, 0L)
  expect_equal(vetoed$data$q005, c(999, 5))
})

# ---- value_recode audit traces (auditability) -------------------------------

test_that("value_recode leaves audit entries for absent and non-numeric targets", {
  df <- data.frame(s001 = c("a", "b"), stringsAsFactors = FALSE)
  rule <- list(rule_id = "VRX", action = "value_recode", description = "t",
               suffixes = list("001", "999"), mapping = list(`1` = 2),
               if_absent = "warn_and_skip")

  out <- suppressWarnings(suppressMessages(
    lissr:::exec_harmonization_rule(df, rule, "xx01a", list(),
                                    c("xx01a"), list())))
  acts <- vapply(out$log, function(e) as.character(e$action), character(1))

  expect_true(any(grepl("SKIPPED_non_numeric", acts)))
  expect_true(any(grepl("TARGET_ABSENT", acts)))
  # the data itself is untouched
  expect_identical(out$df$s001, c("a", "b"))
})
