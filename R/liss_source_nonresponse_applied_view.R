# view helpers stay outside the whole-parent computation fingerprint
.liss_view_abort <- function(message = "complete applied income view is missing, unsupported or tampered") {
  rlang::abort(message, class = "liss_applied_income_view_blocked")
}

.liss_view_policy_path <- function() {
  system.file("schema", "source_nonresponse_collection_applied_views.yml", package = "lissr")
}

.liss_view_policy_sha256 <- function() {
  "2907b71ea08338eb572a143d3f382109aac804dd73add6dbdb07bc68f01cd1de"
}

.liss_view_policy <- function() {
  path <- .liss_view_policy_path()
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path) || !file.exists(path) || dir.exists(path))
    .liss_view_abort("reviewed applied income view policy is missing")
  bytes <- tryCatch(readBin(path, "raw", n = file.info(path)$size),
    error = function(e) NULL)
  hash <- if (is.null(bytes)) "" else as.character(openssl::sha256(bytes))
  attributes(hash) <- NULL
  if (!identical(hash, .liss_view_policy_sha256()))
    .liss_view_abort("reviewed applied income view policy integrity check failed")
  policy <- tryCatch(yaml::yaml.load(rawToChar(bytes), eval.expr = FALSE),
    error = function(e) NULL)
  expected <- list(schema_version = "1.0.0", policy_id = "CI23P339_V4_APPLIED_VIEW",
    policy_version = "1.0.0", parent_policy = list(
      policy_id = "CI23P339_V4_APPLIED_CONSUMER", policy_version = "1.0.0",
      sha256 = "7c740496b5e7ac28a1b7018addc0c253258bf14c28b74a85506dd33dc65b6d2f"),
    capability = list(capability_id = "CI23P339_V4_APPLIED_VIEW", capability_version = "1.0.0",
      creator = "liss_create_applied_income_view", validator = "liss_validate_applied_income_view",
      parent_kind = "applied_income_lineage", parent_schema_version = "1.0.0",
      carrier_schema_version = "4.0.0", target = "s339", modes = c("correct", "na_only"),
      selection = "explicit_ordered_unique_row_keys", retained_scope = "exact_complete_parent",
      computation = "projection_only", ledger_identity = "full_parent_digest_and_root_decision_row",
      readiness_scope = "selected_original_source_input", transport = "whole_view_rds"))
  if (!identical(policy, expected))
    .liss_view_abort("reviewed applied income view policy schema is unsupported")
  .liss_applied_policy()
  policy
}

.liss_view_selection <- function(parent_result, row_keys) {
  if (!is.character(row_keys) || !is.null(attributes(row_keys)) ||
      anyNA(row_keys) || any(!nzchar(row_keys)) || anyDuplicated(row_keys))
    .liss_view_abort("row_keys must be an explicit plain unique nonmissing character vector")
  root_rows <- match(row_keys, parent_result$applied_lineage$row_keys)
  if (anyNA(root_rows))
    .liss_view_abort("row_keys contain an unknown parent row identity")
  list(row_keys = row_keys, root_rows = as.integer(root_rows))
}

.liss_view_slice <- function(frame, rows) {
  value <- frame[rows, , drop = FALSE]
  frame_attributes <- attributes(value)
  columns <- unclass(value)
  for (column in names(frame)) {
    metadata <- attributes(frame[[column]])
    if (is.null(metadata)) next
    # names and dimensions follow rows; descriptive metadata stays exact
    structural <- intersect(c("names", "dim", "dimnames"), names(metadata))
    selected <- attributes(value[[column]])
    if (length(structural)) {
      metadata[structural] <- NULL
      metadata <- c(metadata, selected[intersect(structural, names(selected))])
    }
    attributes(columns[[column]]) <- if (length(metadata)) metadata else NULL
  }
  # data.frame column replacement would strip selected vector names
  attributes(columns) <- frame_attributes
  columns
}

.liss_view_native_scope <- function(parent_result, selection) {
  source <- parent_result$source_input
  frame <- if (is.data.frame(source)) source else source[["data", exact = TRUE]]
  state <- attr(frame, "liss_source_nonresponse", exact = TRUE)
  selected <- .liss_view_slice(frame, selection$root_rows)
  checked <- .liss_nonresponse_validate_mixed(selected, state)
  list(source_readiness = checked$readiness,
    source_locked = checked$fields[["CI23P339_FIELD"]]$lock)
}

.liss_view_unmark <- function(data) {
  attr(data, "liss_source_nonresponse_applied") <- NULL
  data
}

.liss_view_marker <- function(parent_digest, selection, data, policy) {
  list(schema_version = "1.0.0", kind = "applied_income_view_frame",
    policy_id = policy$policy_id, policy_version = policy$policy_version,
    policy_sha256 = .liss_view_policy_sha256(),
    parent_digest = parent_digest, selection_digest = .liss_applied_hash(selection),
    output_digest = .liss_applied_hash(.liss_view_unmark(data)))
}

.liss_view_project <- function(parent_result, selection, policy) {
  data <- .liss_view_slice(parent_result$data, selection$root_rows)
  attr(data, "liss_source_nonresponse_applied") <- NULL
  parent_digest <- .liss_applied_hash(parent_result)
  attr(data, "liss_source_nonresponse_applied") <-
    .liss_view_marker(parent_digest, selection, data, policy)
  root_decision_rows <- which(parent_result$decisions$row %in% selection$root_rows)
  decisions <- parent_result$decisions[root_decision_rows, , drop = FALSE]
  decisions$root_decision_row <- as.integer(root_decision_rows)
  decisions$view_row <- as.integer(match(decisions$row, selection$root_rows))
  decisions <- decisions[order(decisions$view_row, decisions$root_decision_row), , drop = FALSE]
  native_scope <- .liss_view_native_scope(parent_result, selection)
  summary <- list(n_rows = as.integer(nrow(data)), n_decisions = as.integer(nrow(decisions)),
    n_finite = as.integer(sum(is.finite(data$s339))),
    n_source_locked = as.integer(sum(native_scope$source_locked)),
    view_status = if (nrow(data) > 0L) "projected" else "not_evaluated",
    source_readiness = native_scope$source_readiness,
    parent_source_readiness = parent_result$applied_lineage$source_readiness,
    parent_analysis_execution_status = parent_result$applied_lineage$analysis_execution_status)
  list(data = data, decisions = decisions, summary = summary)
}

.liss_view_record <- function(view, policy) {
  capability <- policy$capability
  record <- list(schema_version = "1.0.0", kind = "applied_income_view_lineage",
    policy_id = policy$policy_id, policy_version = policy$policy_version,
    policy_sha256 = .liss_view_policy_sha256(), capability_id = capability$capability_id,
    capability_version = capability$capability_version, parent_kind = capability$parent_kind,
    parent_schema_version = capability$parent_schema_version,
    carrier_schema_version = capability$carrier_schema_version,
    target = capability$target, mode = view$parent_result$mode,
    parent_digest = .liss_applied_hash(view$parent_result),
    selection_digest = .liss_applied_hash(view$selection),
    output_digest = .liss_applied_hash(.liss_view_unmark(view$data)),
    decisions_digest = .liss_applied_hash(view$decisions),
    summary_digest = .liss_applied_hash(view$summary))
  record$digest <- .liss_applied_hash(record)
  record
}

#' Create or validate a view of an applied household-income result
#'
#' Select and reorder already-computed rows from a valid whole `s339` applied
#' result in `correct` or `na_only` mode. The exact complete parent, including
#' original native schema-4 `source_input`, stays in `parent_result`. Creation
#' and structural validation project existing output without cleaning or fitting.
#' The separately pinned view policy and lineage are version 1.0.0; the native
#' carrier remains version 4.0.0 and whole-parent lineage remains version 1.0.0.
#'
#' Supply explicit ordered unique plain `row_keys` from
#' `parent_result$applied_lineage$row_keys`. `character(0)` selects an empty view.
#' All data columns remain in parent order. Row-dependent names and dimensions
#' follow the natural subset; other column metadata stay exact. The frame keeps
#' its existing guarded class with a distinct versioned applied-view marker.
#'
#' Decisions retain every parent ledger column, original root `row`, opaque
#' `decision_id` and subset row names. Integer `root_decision_row` is the ordinal
#' in the complete parent ledger; integer `view_row` is the selected data-row
#' position. Entries are ordered by `view_row`, then `root_decision_row`.
#' Stable references use the complete parent digest and root ledger ordinal,
#' without requiring unique decision IDs. Counts refer to selected rows or
#' ledger entries; multiple decisions can refer to one selected data row.
#'
#' Scoped source readiness is checked against the selected ORIGINAL source
#' frame with its unchanged native receipt, never the corrected output. View
#' projection status, scoped readiness, parent readiness and parent execution
#' status are separate. Empty and other-wave-only selections can be
#' `not_evaluated` for source scope while parent execution is `completed`.
#'
#' Validation reconstructs the closed projection from its complete retained
#' parent and exact selection. With `replay = TRUE`, it additionally validates
#' and replays the complete parent under [liss_validate_applied_income()] and
#' its recorded computation/runtime gate. It never refits the selected rows.
#' Whole-view RDS preserves the contract; extracted frames and partial wrappers
#' do not. Whole-parent validation rejects view wrappers. Nested views,
#' repeated cleaning, source rebinding, background attachment, panel/report
#' consumers and SAV/CSV exports are unsupported.
#'
#' Ordinary SHA-256 hashes (serialization version 2) are not authentication.
#' Coordinated resealing can describe another consistent parent and view.
#' Permitted nonsemantic parent timing changes can pass structure and replay
#' after resealing; no external authenticated parent anchor exists. Replay
#' checks execution outputs under trusted code/runtime, not source authenticity
#' or statistical validity. Removing every recognized claim leaves unknown
#' ownership; aliases cannot recover it.
#'
#' Serialized object hashes use profile `1.0.0`: SHA-256 over XDR version-2
#' R serialization with only the writer-version header fixed to R 4.6.1.
#' All payload bytes remain significant; raw file hashes are unchanged.
#' Compatibility covers the R 4.6.1 reference profile, not arbitrary historical
#' writers or future serialization bodies. Other legacy profiles require their
#' original package/runtime. Reference identity checks remain separate from digests.
#'
#' @param parent_result complete result accepted by
#'   [liss_validate_applied_income()], with native schema-4 source input.
#' @param row_keys required plain ordered unique character vector of parent
#'   row keys, without names, missing values, empty keys or other attributes.
#' @param view complete applied income view, including its exact parent.
#' @param replay one plain nonmissing logical value; if `TRUE`, replay the
#'   complete parent after structural view validation.
#' @return Creation returns a closed list with `parent_result`, `selection`,
#'   `data`, `decisions`, `summary` and `applied_lineage`, with class
#'   `liss_clean_collection_applied_result` (without `liss_clean_result`).
#'   Validation invisibly returns `valid`, `validation`, `view_status`,
#'   `source_readiness`, `parent_source_readiness`,
#'   `parent_analysis_execution_status` and `replay_status`.
#' @export
liss_create_applied_income_view <- function(parent_result, row_keys) {
  if (missing(row_keys)) .liss_view_abort("explicit row_keys are required")
  liss_validate_applied_income(parent_result, replay = FALSE)
  policy <- .liss_view_policy()
  selection <- .liss_view_selection(parent_result, row_keys)
  projected <- .liss_view_project(parent_result, selection, policy)
  view <- list(parent_result = parent_result, selection = selection,
    data = projected$data, decisions = projected$decisions, summary = projected$summary)
  view$applied_lineage <- .liss_view_record(view, policy)
  class(view) <- "liss_clean_collection_applied_result"
  view
}

#' @rdname liss_create_applied_income_view
#' @export
liss_validate_applied_income_view <- function(view, replay = FALSE) {
  if (!is.logical(replay) || length(replay) != 1L || is.na(replay) ||
      !is.null(attributes(replay)))
    .liss_view_abort("replay must be one plain nonmissing logical value")
  keys <- c("parent_result", "selection", "data", "decisions", "summary", "applied_lineage")
  if (!.liss_applied_closed(view, keys, "liss_clean_collection_applied_result"))
    .liss_view_abort()
  parent_check <- liss_validate_applied_income(view$parent_result, replay = FALSE)
  policy <- .liss_view_policy()
  if (!.liss_applied_closed(view$selection, c("row_keys", "root_rows")))
    .liss_view_abort("applied income view selection is incomplete or unsupported")
  selection <- .liss_view_selection(view$parent_result, view$selection$row_keys)
  if (!identical(view$selection, selection))
    .liss_view_abort("view root row mapping differs from its exact parent keys")
  expected <- .liss_view_project(view$parent_result, selection, policy)
  for (key in names(expected)) {
    if (!identical(view[[key]], expected[[key]]))
      .liss_view_abort(paste("applied income view", key, "differs from the retained parent projection"))
  }
  if (!identical(view$applied_lineage, .liss_view_record(view, policy)))
    .liss_view_abort("applied income view lineage differs from its closed projection")
  if (replay) parent_check <- liss_validate_applied_income(view$parent_result, replay = TRUE)
  invisible(list(valid = TRUE, validation = if (replay) "replay" else "structural",
    view_status = view$summary$view_status, source_readiness = view$summary$source_readiness,
    parent_source_readiness = view$summary$parent_source_readiness,
    parent_analysis_execution_status = view$summary$parent_analysis_execution_status,
    replay_status = parent_check$replay_status))
}
