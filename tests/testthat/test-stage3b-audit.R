# ============================================================================
# stage 3b (v1.4 development) regressions: per-action payload validation,
# the corpus audit, and the re-typed cd checks. the audit snapshot pins the
# exact set of known nonconforming rules (all mapped to planned stage-5 work
# or TODO items); any NEW nonconforming rule fails here.
# ============================================================================

test_that("per-action payload validation flags keys the action does not read", {
  # value_recode does not read target_suffixes: must warn, naming the action
  recipe <- list(
    meta = list(module = "zz", module_label = "F", schema_version = "1.0.0",
                recipe_version = "t", created = "t", source_spec = "t",
                covered_waves = list("zz01a")),
    global = list(id_variable = "nomem_encr", wave_variable = "wave_id",
                  year_variable = "wave_year", labelled_policy = "to_numeric",
                  missing_variable_policy = "warn_and_create_na",
                  strip_label_whitespace = TRUE),
    wave_index = list(list(id = "zz01a", year = 2001, file_pattern = "zz01a_*")),
    harmonization_rules = list(
      list(rule_id = "R1", action = "value_recode", description = "t",
           target_suffixes = list("001"), mapping = list(`1` = 2))),
    logging = list(summary_artifact = FALSE))
  expect_warning(
    suppressMessages(validate_recipe(recipe, "zz")),
    "value_recode.*target_suffixes")

  # a key valid for one action is no longer silent on another that ignores
  # it: flag_true_waves is read by structural_na but not by add_flag
  recipe$harmonization_rules <- NULL
  recipe$boundary_rules <- list(
    list(rule_id = "B1", action = "add_flag", description = "t",
         flag_name = "f", flag_true_waves = list("zz01a")))
  expect_warning(
    suppressMessages(validate_recipe(recipe, "zz")),
    "add_flag.*flag_true_waves")

  # action-specific annotations stay silent (suffix on a boundary flag rule)
  recipe$boundary_rules <- list(
    list(rule_id = "B2", action = "add_flag", description = "t",
         flag_name = "f", suffix = "001",
         waves_pre = list(), waves_post = list("zz01a")))
  expect_no_warning(suppressMessages(validate_recipe(recipe, "zz")))

  # note_only payloads are documentation by definition: never checked
  recipe$boundary_rules <- list(
    list(rule_id = "B3", action = "note_only", description = "t",
         anything_at_all = "yes", made_up_key = 1))
  expect_no_warning(suppressMessages(validate_recipe(recipe, "zz")))
})

test_that("audit_liss_recipes returns per-module structure and totals", {
  audit <- suppressMessages(audit_liss_recipes(quiet = TRUE))
  mods <- setdiff(names(audit), "totals")
  expect_setequal(mods, c("ca", "cd", "cf", "ch", "ci", "cp", "cr", "cs",
                          "cv", "cw"))
  expect_equal(audit$totals$recipes, 10L)
  expect_equal(audit$totals$waves, 167L)
  # covered_waves consistent everywhere
  expect_true(all(vapply(mods, function(m) audit[[m]]$covered_waves_match,
                         logical(1))))
  # the only non-default pattern is the documented cd10c supersession pin
  noncanon <- unlist(lapply(mods, function(m) audit[[m]]$noncanonical_patterns))
  expect_identical(unname(noncanon), "cd10c 'cd10c_EN_1.1p*'")
  # check classification totals match the stage-3 grammar
  expect_equal(audit$totals$executable, 68L)
  expect_equal(audit$totals$documentary, 31L)
  expect_equal(audit$totals$skip, 0L)
})

test_that("the nonconforming-rule snapshot is pinned to the known set", {
  audit <- suppressMessages(audit_liss_recipes(quiet = TRUE))
  mods <- setdiff(names(audit), "totals")
  got <- sort(unlist(lapply(mods, function(m)
    vapply(audit[[m]]$nonconforming_rules,
           function(b) paste0(m, ":", b$rule_id), character(1)))))
  # every remaining entry maps to a tracked TODO item (ca H05/H07, cw HR01,
  # cp A8, ch H01/H02, cv HR05). stage 5a cleared all ten cf entries;
  # stage 5b cleared cr BR20/BR21/BR30 (split_variable now ships
  # executable output_vars)
  expected <- sort(c(
    "ca:H05_apostrophe_fix", "ca:H07_position003_retranslate",
    "cw:HR01_sentinel_recode",
    "cp:A8_coerce_double",
    "ch:H01", "ch:H02",
    "cv:HR05_party_crosswalk"
  ))
  expect_identical(got, expected)
})

test_that("re-typed cd checks execute through the canonical executors", {
  df <- data.frame(
    wave_id = c("cd08a", "cd08a", "cd13f", "cd13f", "cd15h"),
    nomem_encr = 1:5,
    h_rent_period = c(2, 5, 1, 2, 3),          # 5 in era1 -> CHK03 FAIL
    h_satisfaction_dwelling = c(0, 10, 5, 11, 3),  # 11 -> CHK02 FAIL
    s041 = c(NA, NA, 0, 1, NA),                # in-set in cd13f -> CHK08 pass
    h_financial_ref_year = c(NA, NA, NA, NA, 2014) # CHK10 pass
  )
  checks <- list(
    list(check_id = "CHK02", type = "value_range", severity = "error",
         variables = list("h_satisfaction_dwelling"), min = 0, max = 10),
    list(check_id = "CHK03", type = "value_absence", severity = "error",
         variables = list("h_rent_period"), forbidden_values = list(5),
         in_waves = list("cd08a")),
    list(check_id = "CHK08", type = "value_in_set", severity = "error",
         suffixes = list("041"), allowed_values = list(0, 1),
         in_waves = list("cd13f")),
    list(check_id = "CHK10", type = "value_in_set", severity = "info",
         variables = list("h_financial_ref_year"),
         allowed_values = list(2014), in_waves = list("cd15h"))
  )
  res <- suppressWarnings(suppressMessages(
    lissr:::run_validations(df, checks, list())))
  st <- vapply(res$results, function(r) isTRUE(r$passed), logical(1))
  expect_identical(unname(st), c(FALSE, FALSE, TRUE, TRUE))
})

test_that("re-keyed cv and ch micro-fixes execute", {
  # cv VR04: the rename now runs through mapping
  cv <- yaml::yaml.load_file(system.file("recipes", "cv_merge_recipe.yml",
                                         package = "lissr"))
  vr04 <- Filter(function(r) identical(r$rule_id, "VR04_total_col"),
                 cv$variable_rules)[[1]]
  df <- data.frame(Total = c(1, 2), nomem_encr = 1:2)
  out <- suppressMessages(lissr:::exec_variable_rule(
    df, vr04, "cv17i", list(), c("cv17i"), list()))
  expect_true("cv17i_Total" %in% names(out$df))
  expect_false("Total" %in% names(out$df))

  # ch V06: label fix now uses the canonical fragment keys
  ch <- yaml::yaml.load_file(system.file("recipes", "ch_merge_recipe.yml",
                                         package = "lissr"))
  v06 <- Filter(function(r) identical(r$rule_id, "V06"),
                ch$harmonization_rules)[[1]]
  dfl <- data.frame(s001 = 1:2)
  attr(dfl$s001, "label") <- "preloaded variabele: gender"
  out <- suppressMessages(lissr:::exec_harmonization_rule(
    dfl, v06, "ch08b", list(), c("ch08b"), list()))
  expect_identical(attr(out$df$s001, "label"), "preloaded variable: gender")
})
