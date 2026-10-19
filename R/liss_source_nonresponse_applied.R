# separate analytical output never replaces the native source observations
.liss_applied_hash <- function(value) {
  .liss_identity_hash(value)
}

.liss_applied_policy_path <- function() {
  system.file("schema", "source_nonresponse_collection_applied.yml", package = "lissr")
}

.liss_applied_policy <- function() {
  path <- .liss_applied_policy_path()
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path) || !file.exists(path) || dir.exists(path))
    .liss_nonresponse_abort("reviewed applied income policy is missing")
  bytes <- tryCatch(readBin(path, "raw", n = file.info(path)$size),
                    error = function(e) NULL)
  hash <- if (is.null(bytes)) "" else as.character(openssl::sha256(bytes))
  attributes(hash) <- NULL
  if (!identical(hash, "7c740496b5e7ac28a1b7018addc0c253258bf14c28b74a85506dd33dc65b6d2f"))
    .liss_nonresponse_abort("reviewed applied income policy integrity check failed")
  policy <- tryCatch(yaml::yaml.load(rawToChar(bytes), eval.expr = FALSE),
                     error = function(e) NULL)
  expected <- list(schema_version = "1.0.0",
    policy_id = "CI23P339_V4_APPLIED_CONSUMER", policy_version = "1.0.0",
    authority_registry = list(schema_version = "2.0.0", registry_version = "2.0.0",
      sha256 = "a2e6694acc5855d2df389df46d66e0c94b705637626309cc309ceee8d840ddc3"),
    collection_adapter = list(adapter_id = "CI23P_337_338_339_COLLECTION",
      adapter_version = "1.0.0",
      sha256 = "5e3235f1b4f78a589622ce27064eecf120c919e81fd95b84fe4a370126ac6c3f"),
    capability = list(capability_id = "CI23P339_V4_APPLIED", capability_version = "1.0.0",
      consumer = "liss_clean_income", carrier_schema_version = "4.0.0",
      field_id = "CI23P339_FIELD", authority_id = "CI23P339_N01", authority_version = "1.0.0",
      construct = "household_net_income", unit = "EUR", measurement_period = 2022L,
      canonical_column = "s339", modes = c("correct", "na_only"),
      input_scope = "complete_validated_collection", selection = "explicit_variables_target",
      background = "forbidden", output_scope = "complete_result_with_immutable_parent",
      transport = "whole_result_rds"))
  if (!identical(policy, expected))
    .liss_nonresponse_abort("reviewed applied income policy schema or authority is unsupported")
  .liss_nonresponse_registry()
  .liss_nonresponse_collection_adapter()
  policy
}

.liss_nonresponse_claims_applied <- function(data) {
  wrapper <- is.list(data) && !is.data.frame(data)
  frame <- if (wrapper) data[["data", exact = FALSE]] else data
  claim <- function(value) inherits(value, c("liss_clean_collection_applied_result",
      "liss_applied_income_frame")) ||
    !is.null(attr(value, "liss_source_nonresponse_applied", exact = TRUE))
  claim(data) || claim(frame) || (wrapper &&
    (!is.null(data[["applied_lineage", exact = TRUE]]) ||
     !is.null(data[["source_input", exact = TRUE]])))
}

.liss_applied_closed <- function(value, keys, classes = NULL) {
  is.list(value) && !is.data.frame(value) &&
    identical(names(value), keys) &&
    identical(sort(names(attributes(value))), if (is.null(classes)) "names" else c("class", "names")) &&
    (is.null(classes) || identical(class(value), classes))
}

.liss_applied_plain <- function(value) {
  if (is.null(value)) return(TRUE)
  attrs <- attributes(value)
  if (!is.null(attrs) && !identical(names(attrs), "names")) return(FALSE)
  nms <- names(value)
  if (!is.null(nms) && (anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms))) return(FALSE)
  if (is.list(value)) return(all(vapply(value, .liss_applied_plain, logical(1))))
  is.atomic(value) && typeof(value) %in% c("logical", "integer", "double", "character")
}

.liss_applied_scalar <- function(value, choices = NULL) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value) &&
    is.null(attributes(value)) && (is.null(choices) || value %in% choices)
}

.liss_applied_invocation_validate <- function(invocation) {
  invalid <- function() .liss_nonresponse_abort("unsupported applied income invocation")
  keys <- c("ruleset", "income_cap", "min_income", "disable", "enable_only", "params", "variables")
  if (!.liss_applied_closed(invocation, keys)) invalid()
  rs <- invocation$ruleset
  if (!is.list(rs) || !identical(class(rs), c("liss_cleaning_ruleset", "list")) ||
      !identical(sort(names(attributes(rs))), c("class", "names"))) invalid()
  attributes(rs) <- list(names = names(rs))
  valid_ruleset <- tryCatch(isTRUE(validate_cleaning_ruleset(rs, quiet = TRUE)$valid),
    error = function(e) FALSE)
  if (!.liss_applied_plain(rs) || !valid_ruleset) invalid()
  for (key in c("income_cap", "min_income")) {
    value <- invocation[[key]]
    if (!is.null(value) && (!is.numeric(value) || length(value) != 1L ||
        !is.null(attributes(value)) || !is.finite(value) || value <= 0)) invalid()
  }
  for (key in c("disable", "enable_only")) {
    value <- invocation[[key]]
    if (!is.null(value) && (!is.character(value) || !is.null(attributes(value)) ||
        anyNA(value) || any(!nzchar(value)) || anyDuplicated(value))) invalid()
  }
  for (key in c("params", "variables")) {
    value <- invocation[[key]]
    if (!is.null(value) && (!is.list(value) || is.null(names(value)) ||
        !.liss_applied_plain(value))) invalid()
  }
  if (!.liss_applied_scalar(invocation$variables[["target", exact = TRUE]], "s339")) invalid()
  if (!is.null(invocation$params) &&
      any(!vapply(invocation$params, function(x) is.list(x) &&
        (length(x) == 0L || !is.null(names(x))), logical(1)))) invalid()
  if (!is.null(invocation$params$F01$disposition) &&
      !.liss_applied_scalar(invocation$params$F01$disposition, c("void", "winsorise", "flag"))) invalid()
  invisible(TRUE)
}

.liss_applied_parent <- function(source_input) {
  if (.liss_nonresponse_claims_consumer(source_input))
    .liss_nonresponse_abort("applied income requires original native source input")
  wrapper <- is.list(source_input) && !is.data.frame(source_input)
  frame <- if (wrapper) source_input[["data", exact = TRUE]] else source_input
  state <- if (wrapper) source_input[["source_nonresponse", exact = TRUE]] else
    attr(frame, "liss_source_nonresponse", exact = TRUE)
  if (!is.data.frame(frame) || !is.list(state) ||
      !identical(state$schema_version, "4.0.0") ||
      is.null(attr(frame, "liss_source_nonresponse", exact = TRUE)))
    .liss_nonresponse_abort("applied income requires the complete intact schema-4 parent")
  .liss_nonresponse_consumer_columns(frame, "s339")
  checked <- .liss_nonresponse_validate_mixed(source_input, state)
  leaf <- checked$state$fields[["CI23P339_FIELD"]]
  rows <- .liss_nonresponse_input_rows(frame, leaf$target, leaf$id_col, leaf$wave_col, leaf$rule$wave)
  list(frame = frame, state = state, checked = checked,
    field = checked$fields[["CI23P339_FIELD"]], leaf = leaf, rows = rows)
}

.liss_applied_unmark <- function(data) {
  for (key in c("liss_source_nonresponse", "source_nonresponse_readiness",
      "liss_source_nonresponse_readiness", "source_nonresponse_consumer_readiness",
      "liss_source_nonresponse_consumer_readiness", "liss_source_nonresponse_applied"))
    attr(data, key) <- NULL
  class(data) <- setdiff(class(data), "liss_applied_income_frame")
  data
}

.liss_applied_semantic <- function(result) {
  payload <- result[c("data", "decisions", "log", "summary", "ruleset", "variables", "mode", "source_input", "invocation")]
  payload$data <- .liss_applied_unmark(payload$data)
  payload$summary$duration_s <- NULL
  payload$ruleset$meta$source_path <- NULL
  payload$invocation$ruleset$meta$source_path <- NULL
  payload$log <- lapply(payload$log, function(entry) {
    entry$timestamp <- NULL
    entry
  })
  payload
}

.liss_applied_engine <- function() {
  # deparsed computation is stable across installed and development namespaces
  functions <- c("liss_clean_income", "apply_ruleset_overrides", "validate_cleaning_ruleset",
    "resolve_cleaning_variables", "numeric_view", "declared_missing_codes", "fnum", "fmt_plain",
    ".liss_identity_profile", ".liss_identity_normalize", ".liss_identity_bytes", ".liss_identity_hash",
    "dist_stats", "wave_years_from", "first_rule", "rule_enabled", "rp", "new_ledger",
    "ledger_add", "ledger_frame", "clean_log_entry", "generate_candidates", "finite_vals",
    "stat_mode_num", "power10_magnitude", "local_log_volatility", "robust_zscore",
    "detect_univariate_outliers", "detect_outliers_consensus", "wma_impute_at", "donor_pool_value",
    "category_bounds_from_codes", "filter_candidates", "select_candidate", "bound_deviation_ratio",
    "source_nonresponse_validate", "validate_source_nonresponse_contracts", ".liss_nonresponse_validate_mixed",
    ".liss_nonresponse_consumer_roles", ".liss_nonresponse_input_rows", ".liss_nonresponse_ids",
    ".liss_nonresponse_values", ".liss_nonresponse_equal", ".liss_nonresponse_rows",
    ".liss_nonresponse_validate_field", ".liss_nonresponse_mixed_normalized",
    ".liss_nonresponse_mixed_receipts", ".liss_nonresponse_mixed_rows",
    ".liss_nonresponse_mixed_household_ledger", ".liss_nonresponse_collection_ledger",
    ".liss_applied_finish", ".liss_applied_record", ".liss_applied_semantic",
    ".liss_applied_invocation_validate", ".liss_applied_parent", "liss_validate_applied_income")
  ns <- asNamespace("lissr")
  helpers <- ls(ns, all.names = TRUE, pattern = "^\\.liss_(nonresponse|applied)_")
  loaders <- c(".liss_nonresponse_source_state", ".liss_nonresponse_read_source",
    ".liss_nonresponse_registry", ".liss_nonresponse_collection_adapter",
    ".liss_nonresponse_collection_consumers", ".liss_applied_policy")
  helpers <- helpers[!grepl("_path$", helpers) & !helpers %in% loaders]
  functions <- sort(unique(c(functions, helpers, "%||%")), method = "radix")
  hashes <- vapply(functions, function(name) {
    fn <- get(name, envir = ns, inherits = FALSE)
    .liss_applied_hash(list(formals = paste(deparse(formals(fn), width.cutoff = 500L), collapse = "\n"),
      body = paste(deparse(body(fn), width.cutoff = 500L), collapse = "\n")))
  }, character(1))
  dependencies <- c("cli", "dplyr", "haven", "openssl", "rlang", "tibble", "vctrs", "yaml")
  list(schema_version = "2.0.0", functions = hashes,
    constants = c(CLEANING_ACTIONS = .liss_applied_hash(get("CLEANING_ACTIONS", envir = ns)),
      CLEANING_RULE_KEYS = .liss_applied_hash(get("CLEANING_RULE_KEYS", envir = ns))),
    runtime = list(r = R.version$version.string, platform = R.version$platform,
      dependencies = vapply(dependencies, function(name) as.character(utils::packageVersion(name)), character(1))),
    serialization = .liss_identity_profile())
}

.liss_applied_engine_validate <- function(engine) {
  hashes <- function(value) is.character(value) && length(value) > 0L &&
    identical(names(attributes(value)), "names") && !is.null(names(value)) &&
    !anyNA(names(value)) && all(nzchar(names(value))) && !anyDuplicated(names(value)) &&
    !anyNA(value) && all(grepl("^[a-f0-9]{64}$", value))
  keys <- c("schema_version", "functions", "constants", "runtime")
  legacy <- .liss_applied_closed(engine, keys) && identical(engine$schema_version, "1.0.0")
  current <- .liss_applied_closed(engine, c(keys, "serialization")) &&
    identical(engine$schema_version, "2.0.0") && identical(engine$serialization, .liss_identity_profile())
  if ((!legacy && !current) || !hashes(engine$functions) || !hashes(engine$constants) ||
      !.liss_applied_closed(engine$runtime, c("r", "platform", "dependencies")) ||
      !.liss_applied_scalar(engine$runtime$r) || !.liss_applied_scalar(engine$runtime$platform) ||
      !is.character(engine$runtime$dependencies) || !.liss_applied_plain(engine$runtime$dependencies) ||
      is.null(names(engine$runtime$dependencies)) || anyNA(engine$runtime$dependencies) ||
      any(!nzchar(engine$runtime$dependencies)))
    .liss_nonresponse_abort("unsupported applied income computation identity")
  invisible(TRUE)
}

.liss_applied_numeric <- function(value, size = length(value)) {
  typeof(value) %in% c("integer", "double") && is.null(attributes(value)) &&
    length(value) == size
}

.liss_applied_count <- function(value, missing = FALSE) {
  if (missing && length(value) == 1L && is.null(attributes(value)) &&
      typeof(value) %in% c("logical", "integer", "double") &&
      is.na(value) && !is.nan(value)) return(TRUE)
  .liss_applied_numeric(value, 1L) && is.finite(value) &&
    value >= 0 && value == trunc(value)
}

.liss_applied_frame_shape <- function(value, template, numeric_columns = character()) {
  if (!is.data.frame(value) || !identical(class(value), "data.frame") ||
      !identical(sort(names(attributes(value))), sort(c("names", "row.names", "class"))) ||
      !identical(names(value), names(template))) return(FALSE)
  all(vapply(names(template), function(name) {
    column <- value[[name]]
    is.null(attributes(column)) && length(column) == nrow(value) &&
      if (name %in% numeric_columns) .liss_applied_numeric(column) else
        identical(typeof(column), typeof(template[[name]]))
  }, logical(1)))
}

.liss_applied_execution_shape <- function(result) {
  invalid <- function() .liss_nonresponse_abort("unsupported applied income execution, summary or audit shape")
  ledger <- result$decisions
  if (!.liss_applied_frame_shape(ledger, ledger_frame(new_ledger()))) invalid()
  summary <- result$summary
  keys <- c("mode", "target", "n_rows", "n_groups", "n_groups_processed",
    "n_finite_before", "n_finite_after", "n_decisions", "n_corrected", "n_voided",
    "n_rectified", "n_capped", "n_winsorised", "n_cap_flagged", "n_hid_fallback",
    "n_ungroupable", "n_bounds_voided", "n_flagged_dataset", "pct_corrected",
    "by_rule", "distribution_before", "distribution_after", "remaining_over_cap",
    "remaining_under_min", "overrides", "notes", "duration_s",
    "n_source_nonresponse", "n_raw_finite_before")
  if (!.liss_applied_closed(summary, keys)) invalid()
  counts <- keys[grepl("^(n_|remaining_)", keys)]
  if (!all(vapply(summary[counts], .liss_applied_count, logical(1))) ||
      summary$n_decisions != nrow(ledger) ||
      !.liss_applied_scalar(summary$mode) || !.liss_applied_scalar(summary$target) ||
      !.liss_applied_numeric(summary$pct_corrected, 1L) ||
      !.liss_applied_numeric(summary$duration_s, 1L) || !is.finite(summary$duration_s)) invalid()
  for (key in c("overrides", "notes")) {
    if (!is.character(summary[[key]]) || !is.null(attributes(summary[[key]]))) invalid()
  }
  by_rule <- summary$by_rule
  template <- data.frame(rule_id = character(), action = character(), n = integer())
  if (!.liss_applied_frame_shape(by_rule, template, numeric_columns = "n") ||
      any(!is.finite(by_rule$n) | by_rule$n < 0 | by_rule$n != trunc(by_rule$n))) invalid()
  for (key in c("distribution_before", "distribution_after")) {
    distribution <- summary[[key]]
    if (is.null(distribution)) next
    if (!.liss_applied_closed(distribution, c("n", "min", "q1", "median", "mean", "q3", "max", "sd", "mad")) ||
        !.liss_applied_count(distribution$n) || distribution$n == 0 ||
        !all(vapply(distribution[-1L], .liss_applied_numeric, logical(1), size = 1L))) invalid()
  }
  logs <- result$log
  log_keys <- c("rule_id", "wave_id", "variable", "action", "rows_affected",
    "values_changed", "distinct_before", "distinct_after", "na_count_before",
    "na_count_after", "timestamp", "duration_ms")
  if (!is.list(logs) || !is.null(attributes(logs)) || length(logs) == 0L) invalid()
  for (entry in logs) {
    # configured log identifiers retain their supported ruleset data without coercion
    if (!.liss_applied_closed(entry, log_keys) ||
        length(entry$rule_id) != 1L || !.liss_applied_plain(entry$rule_id) ||
        !all(vapply(entry[c("wave_id", "variable", "action", "timestamp")],
          .liss_applied_scalar, logical(1))) ||
        !.liss_applied_count(entry$rows_affected) ||
        !all(vapply(entry[c("values_changed", "distinct_before", "distinct_after",
          "na_count_before", "na_count_after")], .liss_applied_count, logical(1), missing = TRUE))) invalid()
    duration <- entry$duration_ms
    placeholder <- length(duration) == 1L && is.null(attributes(duration)) &&
      typeof(duration) %in% c("logical", "integer", "double") &&
      is.na(duration) && !is.nan(duration)
    if (!placeholder && (!.liss_applied_numeric(duration, 1L) || !is.finite(duration))) invalid()
  }
  if ("s339_dataset_flag" %in% names(result$data) &&
      (!is.character(result$data$s339_dataset_flag) ||
       !is.null(attributes(result$data$s339_dataset_flag)) ||
       length(result$data$s339_dataset_flag) != nrow(result$data))) invalid()
  invisible(TRUE)
}

.liss_applied_marker <- function(result, parent) {
  list(schema_version = "1.0.0", kind = "applied_income_analysis_frame",
    policy_id = "CI23P339_V4_APPLIED_CONSUMER", capability_id = "CI23P339_V4_APPLIED",
    parent_digest = parent$state$digest, mode = result$mode,
    output_digest = .liss_applied_hash(.liss_applied_unmark(result$data)))
}

.liss_applied_record <- function(result, policy, engine) {
  parent <- .liss_applied_parent(result$source_input)
  plain <- .liss_applied_semantic(result)
  capability <- policy$capability
  .liss_nonresponse_seal(list(schema_version = "1.0.0", kind = "applied_income_lineage",
    consumer = "liss_clean_income", policy_id = policy$policy_id, policy_version = policy$policy_version,
    policy_sha256 = "7c740496b5e7ac28a1b7018addc0c253258bf14c28b74a85506dd33dc65b6d2f",
    capability_id = capability$capability_id, capability_version = capability$capability_version,
    carrier_schema_version = "4.0.0", field_id = capability$field_id, rule_id = capability$authority_id,
    construct = capability$construct, unit = capability$unit, measurement_period = capability$measurement_period,
    target = "s339", mode = result$mode, input_rows = as.integer(nrow(parent$frame)),
    parent_digest = parent$state$digest, parent_frame_digest = .liss_applied_hash(parent$frame),
    row_keys = parent$rows$row_key, source_readiness = parent$checked$readiness,
    analysis_execution_status = if (nrow(parent$frame) > 0L) "completed" else "not_evaluated",
    target_metadata = list(raw = parent$leaf$metadata$attributes,
      input = attributes(parent$frame$s339), output = attributes(result$data$s339)),
    engine = engine, invocation_digest = .liss_applied_hash(plain$invocation),
    output_digest = .liss_applied_hash(plain$data), result_digest = .liss_applied_hash(plain)))
}

.liss_applied_reseal <- function(result) {
  parent <- .liss_applied_parent(result$source_input)
  attr(result$data, "liss_source_nonresponse_applied") <- .liss_applied_marker(result, parent)
  plain <- .liss_applied_semantic(result)
  result$applied_lineage$invocation_digest <- .liss_applied_hash(plain$invocation)
  result$applied_lineage$output_digest <- .liss_applied_hash(plain$data)
  result$applied_lineage$result_digest <- .liss_applied_hash(plain)
  result$applied_lineage <- .liss_nonresponse_seal(result$applied_lineage)
  result
}

.liss_applied_finish <- function(result, source_input, invocation) {
  if (!identical(names(result), c("data", "decisions", "log", "summary", "ruleset", "variables", "mode")))
    .liss_nonresponse_abort("applied income requires a complete household execution")
  .liss_applied_invocation_validate(invocation)
  policy <- .liss_applied_policy()
  parent <- .liss_applied_parent(source_input)
  result$data <- .liss_applied_unmark(result$data)
  class(result$data) <- c("liss_applied_income_frame", class(result$data))
  result$source_input <- source_input
  result$invocation <- invocation
  class(result) <- c("liss_clean_collection_applied_result", "liss_clean_result")
  attr(result$data, "liss_source_nonresponse_applied") <- .liss_applied_marker(result, parent)
  result$applied_lineage <- .liss_applied_record(result, policy, .liss_applied_engine())
  liss_validate_applied_income(result, replay = FALSE)
  result
}

#' Validate a complete applied household-income result
#'
#' Validate the complete analytical result returned by [liss_clean_income()] for
#' an explicitly selected schema-4 household `s339` input in `correct` or
#' `na_only` mode. The original native source collection is retained separately
#' in `source_input`; the changed analysis frame is not a native source carrier.
#' Whole-result RDS preserves this distinction. Extracted frames, arbitrary
#' subsets/reorderings and column projections do not carry a complete result.
#'
#' The default structural check verifies source bindings, full row/column scope,
#' immutable non-target columns and tax leaves, metadata, observed recovery,
#' protected missing cells, effective configuration and receipt consistency.
#' Constructor-owned decision, optional dataset-flag, summary and log shapes
#' are checked, including plain count and timing types. Timing fields must have
#' supported types before their nonsemantic values are excluded from replay.
#' Ordinary digests are not signatures or proof that cleaning executed. An
#' unlocked proposal and its receipts can be changed together by a caller;
#' structural success does not establish numerical or statistical validity.
#'
#' With `replay = TRUE`, require the recorded computation/runtime identity and
#' rerun the saved invocation through the collection consumer. Compare complete
#' semantic output against that execution, excluding only duration, log
#' timestamps and ruleset source paths. Replay does not read original source
#' files, install dependencies, export output or fit a different default method.
#' It relies on the current trusted package/runtime and retained source evidence;
#' it does not authenticate caller-controlled code or fabricated source receipts.
#' If that exact computation is unavailable, structural validation may still be
#' supported, but replay fails explicitly. Guarded reports, panels, exports,
#' repeated consumption, background attachment and taxable cleaning are unsupported.
#' Removing all lineage claims deliberately leaves ordinary unguarded input;
#' names or values alone cannot recover source ownership.
#'
#' Serialized object hashes use profile `1.0.0`: SHA-256 over XDR version-2
#' R serialization with only the writer-version header fixed to R 4.6.1.
#' All payload bytes remain significant; raw file hashes are unchanged.
#' Compatibility covers the R 4.6.1 reference profile, not arbitrary historical
#' writers or future serialization bodies. Other legacy profiles require their
#' original package/runtime. Reference identity checks remain separate from digests.
#'
#' New computation identities use schema `2.0.0` and record this profile.
#' Reference-profile schema-1 records remain structurally readable, but a changed
#' computation identity rejects replay even when the R runtime is unchanged.
#'
#' @param result complete applied collection result, including `source_input`,
#'   saved `invocation` and `applied_lineage`.
#' @param replay one plain logical value. If `TRUE`, require and rerun the exact
#'   recorded computation after structural validation.
#' @return invisibly, a list containing `valid`, `validation`, scoped
#'   `source_readiness`, `analysis_execution_status` and `replay_status`.
#' @export
liss_validate_applied_income <- function(result, replay = FALSE) {
  invalid <- function(message = "complete applied income result is missing, unsupported or tampered")
    .liss_nonresponse_abort(message)
  if (!is.logical(replay) || length(replay) != 1L || is.na(replay) || !is.null(attributes(replay)))
    invalid("replay must be one plain nonmissing logical value")
  keys <- c("data", "decisions", "log", "summary", "ruleset", "variables", "mode",
    "source_input", "invocation", "applied_lineage")
  if (!.liss_applied_closed(result, keys, c("liss_clean_collection_applied_result", "liss_clean_result"))) invalid()
  policy <- .liss_applied_policy()
  if (!.liss_applied_scalar(result$mode, policy$capability$modes)) invalid()
  .liss_applied_invocation_validate(result$invocation)
  parent <- .liss_applied_parent(result$source_input)
  data <- result$data
  if (!is.data.frame(data) ||
      !(identical(class(data), c("liss_applied_income_frame", "data.frame")) ||
        identical(class(data), c("liss_applied_income_frame", "tbl_df", "tbl", "data.frame"))) ||
      !identical(sort(names(attributes(data))), sort(c("names", "row.names", "class", "liss_source_nonresponse_applied"))) ||
      !identical(nrow(data), nrow(parent$frame)) || !identical(row.names(data), row.names(parent$frame)) ||
      anyNA(names(data)) || any(!nzchar(names(data))) || anyDuplicated(names(data))) invalid()
  n <- length(parent$frame)
  if (length(data) < n || !identical(names(data)[seq_len(n)], names(parent$frame))) invalid()
  original <- setdiff(names(parent$frame), "s339")
  if (!all(vapply(original, function(name) identical(data[[name]], parent$frame[[name]]), logical(1))))
    invalid("applied income changed an original non-target column, tax value or row identity")
  added <- names(data)[-seq_len(n)]
  if (!all(c("s339_observed", "s339_clean_status") %in% added) ||
      any(!added %in% c("s339_observed", "s339_clean_status", "s339_dataset_flag"))) invalid()
  bare <- .liss_applied_unmark(data)
  label <- attr(parent$frame$s339, "label", exact = TRUE)
  expected_metadata <- if (is.null(label)) NULL else list(label = label)
  if (!is.double(data$s339) || !identical(attributes(data$s339), expected_metadata) ||
      !identical(data$s339_observed, parent$field$observed) ||
      !is.character(data$s339_clean_status) || !is.null(attributes(data$s339_clean_status)) ||
      length(data$s339_clean_status) != nrow(data)) invalid()
  lock <- parent$field$lock
  if (any(!is.na(data$s339[lock])) || anyNA(data$s339_clean_status[lock]) ||
      any(data$s339_clean_status[lock] != "source_nonresponse:missing_amount"))
    invalid("applied income reintroduced a protected source nonresponse amount")
  if (!is.data.frame(result$decisions) || !is.list(result$summary) ||
      !is.list(result$log) || !is.list(result$variables) ||
      !inherits(result$ruleset, "liss_cleaning_ruleset")) invalid()
  .liss_applied_execution_shape(result)
  .liss_nonresponse_consumer_roles(result$variables, parent$checked$state)
  effective <- do.call(apply_ruleset_overrides,
    c(list(rs = result$invocation$ruleset), result$invocation[-1L]))
  if (!identical(effective, result$ruleset) ||
      !identical(resolve_cleaning_variables(parent$frame, effective$variables), result$variables))
    invalid("applied income effective configuration differs from its saved invocation")
  if (!identical(result$summary$mode, result$mode) || !identical(result$summary$target, "s339") ||
      result$summary$n_rows != nrow(data) ||
      result$summary$n_source_nonresponse != sum(lock) ||
      result$summary$n_raw_finite_before != sum(is.finite(parent$field$observed))) invalid()
  decisions <- result$decisions
  if (!identical(names(decisions), names(ledger_frame(new_ledger()))) ||
      anyNA(decisions$row) || any(decisions$row < 1L | decisions$row > nrow(data)) ||
      any(decisions$row != trunc(decisions$row)) ||
      any(is.finite(decisions$corrected[decisions$row %in% which(lock)]))) invalid()
  receipt <- result$applied_lineage
  if (!is.list(receipt) || is.null(receipt$engine)) invalid()
  .liss_applied_engine_validate(receipt$engine)
  expected <- .liss_applied_record(result, policy, receipt$engine)
  if (!identical(receipt, expected) ||
      !identical(attr(data, "liss_source_nonresponse_applied", exact = TRUE), .liss_applied_marker(result, parent))) invalid()
  if (replay) {
    if (!identical(receipt$engine, .liss_applied_engine()))
      invalid("recorded applied income computation/runtime is unavailable or different; replay is unsupported")
    rerun <- do.call(liss_clean_income, c(list(data = result$source_input,
      mode = result$mode, background = NULL, output_dir = NULL, verbose = FALSE), result$invocation))
    if (!identical(.liss_applied_semantic(result), .liss_applied_semantic(rerun)))
      invalid("applied income semantic replay differs from the retained result")
  }
  invisible(list(valid = TRUE, validation = if (replay) "replay" else "structural",
    source_readiness = receipt$source_readiness,
    analysis_execution_status = receipt$analysis_execution_status,
    replay_status = if (replay) "matched" else "not_requested"))
}
