# ============================================================================
# 1.2.1 hardening regressions: restricted condition evaluation, atomic rule
# execution, panel join guards, and a synthetic end-to-end module merge that
# runs everywhere including CI (no LISS credentials or real data required).
# ============================================================================

# ---- restricted condition evaluator (T1.1) ---------------------------------

test_that("safe_eval_condition evaluates the documented grammar", {
  df <- data.frame(
    s001 = c(1, 9, -9, NA),
    s002 = c(NA, 2, 3, 4),
    grp  = c("a", "b", "a", "b"),
    stringsAsFactors = FALSE
  )

  expect_identical(
    lissr:::safe_eval_condition("s001 == 9 & !is.na(s002)", df),
    c(FALSE, TRUE, FALSE, NA)
  )
  expect_identical(
    lissr:::safe_eval_condition("s001 %in% c(1, 9)", df),
    c(TRUE, TRUE, FALSE, FALSE)
  )
  expect_identical(
    lissr:::safe_eval_condition("(s001 > 1) | s002 < 3", df),
    c(NA, TRUE, FALSE, NA)
  )
  expect_identical(
    lissr:::safe_eval_condition("grp == 'a'", df),
    c(TRUE, FALSE, TRUE, FALSE)
  )
  expect_identical(
    lissr:::safe_eval_condition("s001 == -9", df),
    c(FALSE, FALSE, TRUE, NA)
  )
})

test_that("safe_eval_condition rejects everything outside the grammar", {
  df <- data.frame(s001 = 1:3)

  expect_error(lissr:::safe_eval_condition("system('true')", df), "disallowed")
  expect_error(lissr:::safe_eval_condition("get('system')('true')", df),
               "disallowed")
  expect_error(lissr:::safe_eval_condition("x <- 1", df), "disallowed")
  expect_error(lissr:::safe_eval_condition("s001[1] > 0", df), "disallowed")
  expect_error(lissr:::safe_eval_condition("base::identity(s001)", df),
               "disallowed")
  expect_error(lissr:::safe_eval_condition("s001 == 1; s001 == 2", df),
               "single expression")
  # unknown symbols cannot fall through to the calling environment
  expect_error(lissr:::safe_eval_condition("no_such_column == 1", df))
})

test_that("a malicious condition has no side effects", {
  df <- data.frame(s001 = 1:3)
  # windows tempdir paths carry backslashes; keep the condition parseable so
  # the whitelist, not the parser, is what rejects the call on every platform
  sentinel <- gsub("\\\\", "/", tempfile(fileext = ".touched"))
  cond <- sprintf("file.create('%s')", sentinel)
  expect_error(lissr:::safe_eval_condition(cond, df), "disallowed")
  expect_false(file.exists(sentinel))
})

test_that("run_validations gates na_rate rows via the safe evaluator", {
  df <- data.frame(
    s001 = c(NA, NA, 1, 2),
    grp  = c("a", "a", "a", "b"),
    stringsAsFactors = FALSE
  )

  # evaluable condition: the na_rate is measured on grp == 'a' only (2/3)
  chk <- list(check_id = "C1", type = "na_rate", suffixes = list("001"),
              threshold = 0.7, direction = "below",
              condition = "grp == 'a'", severity = "error")
  v <- lissr:::run_validations(df, list(chk), list())
  expect_true(isTRUE(v$results[[1]]$passed))
  expect_equal(v$error_count, 0)

  # a condition outside the grammar is reported, not executed
  sentinel <- gsub("\\\\", "/", tempfile(fileext = ".touched"))
  bad <- chk
  bad$condition <- sprintf("file.create('%s')", sentinel)
  v2 <- lissr:::run_validations(df, list(bad), list())
  expect_true(is.na(v2$results[[1]]$passed))
  expect_match(v2$results[[1]]$detail, "condition not evaluable")
  expect_false(file.exists(sentinel))
  expect_equal(v2$error_count, 0)

  # a condition that does not even parse is reported the same way
  bad2 <- chk
  bad2$condition <- "s001 == 'C:\\Users'"
  v3 <- lissr:::run_validations(df, list(bad2), list())
  expect_true(is.na(v3$results[[1]]$passed))
  expect_match(v3$results[[1]]$detail, "condition not evaluable")
  expect_equal(v3$error_count, 0)
})

# ---- wave-input parser (T1.1, liss_select) ---------------------------------

test_that("parse_wave_input expands the documented forms and nothing else", {
  expect_identical(lissr:::parse_wave_input("1:5"), 1:5)
  expect_identical(lissr:::parse_wave_input("1,3,7"), c(1L, 3L, 7L))
  expect_identical(lissr:::parse_wave_input("2:4, 9"), c(2L, 3L, 4L, 9L))
  expect_identical(lissr:::parse_wave_input("5:3"), 3:5)
  expect_identical(lissr:::parse_wave_input(""), integer(0))

  expect_null(lissr:::parse_wave_input("system('true')"))
  expect_null(lissr:::parse_wave_input("1;2"))
  expect_null(lissr:::parse_wave_input("1:2:3"))
  expect_null(lissr:::parse_wave_input("a"))
})

# ---- atomic rule execution (T1.2) ------------------------------------------

test_that("a rule failing mid-execution rolls back to a logged no-op", {
  # the -7 collision guard fires on the second suffix after the first has
  # already been mutated; pre-1.2.1 the returned frame kept that mutation
  df <- data.frame(
    nomem_encr = 1:3,
    s005 = c(1, 99, 2),
    s006 = c(-7, 1, 2)
  )
  rule <- list(
    rule_id = "RB1", action = "value_recode", description = "t",
    suffixes = list("005", "006"),
    mapping = list(`99` = -7)
  )
  seed <- list(list(rule_id = "PRE", action = "seed"))

  r <- NULL
  expect_warning(
    r <- lissr:::exec_harmonization_rule(df, rule, "w1", list(),
                                         c("w1"), seed),
    "rolled back"
  )

  # frame identical to the input: the partial s005 mutation was reverted
  expect_identical(r$df$s005, c(1, 99, 2))
  expect_identical(r$df$s006, c(-7, 1, 2))

  # log = the pre-existing entries plus exactly one ERROR entry; the partial
  # per-suffix entry from the failed rule is discarded with the mutation
  actions <- vapply(r$log, function(e) e$action, character(1))
  expect_identical(actions, c("seed", "ERROR:value_recode"))
  expect_identical(r$log[[2]]$rule_id, "RB1")
})

# ---- panel join guards and shared-column coalescing (T1.3) ------------------

test_that("merge_liss_panel aborts on duplicated join keys, naming the module", {
  a <- data.frame(nomem_encr = c(1, 1), wave_year = c(2020, 2020), a1 = 1:2)
  b <- data.frame(nomem_encr = 1, wave_year = 2020, b1 = 1)

  err <- tryCatch(
    suppressMessages(merge_liss_panel(list(m1 = a, m2 = b))),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "duplicated join key")
  expect_match(err, "m1")
})

test_that("shared columns are coalesced across modules, not first-module-only", {
  a <- data.frame(nomem_encr = c(1, 2), wave_year = c(2020, 2020),
                  nohouse_encr = c(11, NA), a1 = c(1, 2))
  b <- data.frame(nomem_encr = c(1, 2), wave_year = c(2020, 2020),
                  nohouse_encr = c(NA, 22), b1 = c(3, 4))

  p <- suppressMessages(merge_liss_panel(list(a = a, b = b)))
  expect_equal(nrow(p), 2)
  expect_identical(as.numeric(p$nohouse_encr), c(11, 22))
})

test_that("conflicting shared-column values warn and keep the first module", {
  a <- data.frame(nomem_encr = 1, wave_year = 2020,
                  nohouse_encr = 11, a1 = 1)
  b <- data.frame(nomem_encr = 1, wave_year = 2020,
                  nohouse_encr = 99, b1 = 2)

  expect_warning(
    p <- suppressMessages(merge_liss_panel(list(a = a, b = b))),
    "disagree"
  )
  expect_identical(as.numeric(p$nohouse_encr), 11)
})

test_that("attaching shared columns never adds rows", {
  # inner join drops keys 1 and 3; the pooled shared frame still carries
  # them, and a full-join attach would have resurrected those rows
  a <- data.frame(nomem_encr = c(1, 2), wave_year = c(2020, 2020),
                  nohouse_encr = c(11, 12), a1 = c(1, 2))
  b <- data.frame(nomem_encr = c(2, 3), wave_year = c(2020, 2020),
                  nohouse_encr = c(NA, 33), b1 = c(3, 4))

  p <- suppressMessages(merge_liss_panel(list(a = a, b = b),
                                         join_type = "inner"))
  expect_equal(nrow(p), 1)
  expect_identical(as.numeric(p$nohouse_encr), 12)
})

# ---- synthetic end-to-end module merge (T2.2) -------------------------------

test_that("a synthetic module merges end to end with log, report, and summary", {
  skip_if_not_installed("haven")

  data_dir <- file.path(tempdir(), "lissr_e2e_data")
  out_dir  <- file.path(tempdir(), "lissr_e2e_out")
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(c(data_dir, out_dir), recursive = TRUE), add = TRUE)

  haven::write_sav(
    data.frame(nomem_encr = 1:3,
               xx01a005 = c(1, 2, 9),
               xx01a006 = c(10, 20, NA)),
    file.path(data_dir, "xx01a_EN_1.0p.sav"))
  haven::write_sav(
    data.frame(nomem_encr = 1:3,
               xx02b005 = c(2, 1, 3),
               xx02b006 = c(5, NA, NA)),
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
      strip_label_whitespace = TRUE
    ),
    wave_index = list(
      list(id = "xx01a", year = 2001, file_pattern = "xx01a_*"),
      list(id = "xx02b", year = 2002, file_pattern = "xx02b_*")
    ),
    harmonization_rules = list(
      list(rule_id = "HR1", action = "value_recode", description = "t",
           suffixes = list("005"), mapping = list(`1` = 2, `2` = 3))
    ),
    boundary_rules = list(
      list(rule_id = "BR1", action = "add_flag", description = "t",
           flag_name = "period",
           eras = list(early = list("xx01a"), late = list("xx02b")))
    ),
    derived_variables = list(
      list(rule_id = "DVT", name = "dv_total", method = "sum",
           sources = list("005", "006"))
    ),
    validation_checks = list(
      list(check_id = "V1", type = "uniqueness", column = "nomem_encr",
           within = "wave_id", severity = "error"),
      list(check_id = "V2", type = "na_rate", suffixes = list("006"),
           threshold = 0.5, direction = "below",
           condition = "period == 'early'", severity = "error")
    ),
    logging = list(log_file = "xx_log.jsonl", report_file = "xx_report.txt",
                   summary_artifact = TRUE)
  )

  res <- suppressWarnings(suppressMessages(
    merge_liss_module(recipe, data_dir, out_dir, strict = TRUE)))
  d <- res$data

  expect_equal(nrow(d), 6)
  # snapshot recode: {1: 2, 2: 3} must not chain within a wave
  expect_equal(as.numeric(d$s005), c(2, 3, 9, 3, 2, 3))
  expect_identical(as.character(d$period), rep(c("early", "late"), each = 3))
  expect_equal(as.numeric(d$dv_total), c(12, 23, 9, 8, 2, 3))

  # the condition-gated check evaluated (passed TRUE, not skipped as NA)
  v2 <- Filter(function(r) identical(r$check_id, "V2"), res$validation)[[1]]
  expect_true(isTRUE(v2$passed))

  # JSONL log parses and carries the rule ids of all three phases
  log_path <- file.path(out_dir, "xx_log.jsonl")
  expect_true(file.exists(log_path))
  entries <- lapply(readLines(log_path), jsonlite::fromJSON)
  ids <- vapply(entries, function(e) as.character(e$rule_id), character(1))
  expect_true(all(c("HR1", "BR1", "DVT") %in% ids))

  # report and summary artifacts
  rpt <- readLines(file.path(out_dir, "xx_report.txt"))
  expect_true(any(grepl("Merge Report", rpt)))
  expect_true(any(grepl("Rows: 6", rpt)))
  expect_true(any(grepl("V1", rpt)))

  smry <- jsonlite::fromJSON(file.path(out_dir, "xx_merge_summary.json"))
  expect_equal(as.numeric(smry$total_rows), 6)

  # the written .sav round-trips
  merged_back <- haven::read_sav(file.path(out_dir, "xx_merged.sav"))
  expect_equal(nrow(merged_back), 6)
})
