# standalone source helpers stay outside the existing CI computation fingerprint
.liss_ca012_abort <- function(message = "complete CA20g012 source classification is missing, unsupported or inconsistent") {
  rlang::abort(message, class = "liss_ca20g012_blocked")
}

.liss_ca012_hash <- function(value) {
  .liss_identity_hash(value)
}

.liss_ca012_equal <- function(left, right) {
  identical(left, right) && identical(serialize(left, NULL, version = 2L),
    serialize(right, NULL, version = 2L))
}

.liss_ca012_result_equal <- function(left, right) {
  payload <- function(result) {
    frame <- function(data) list(data = .liss_ca012_unmark(data),
      marker = attr(data, "liss_source_nonresponse", exact = TRUE),
      readiness = attr(data, "source_nonresponse_readiness", exact = TRUE))
    list(original_data = result$original_data,
      analysis = frame(result$analysis$data), data = frame(result$data),
      ledger = result$analysis$ledger, source_nonresponse = result$source_nonresponse,
      readiness = result$source_nonresponse_readiness)
  }
  identical(left, right) && .liss_ca012_equal(payload(left), payload(right))
}

.liss_ca012_bytes_hash <- function(bytes) {
  hash <- as.character(openssl::sha256(bytes))
  attributes(hash) <- NULL
  hash
}

.liss_ca012_closed <- function(value, keys, classes = NULL) {
  is.list(value) && !is.data.frame(value) && identical(names(value), keys) &&
    identical(sort(names(attributes(value))), if (is.null(classes)) "names" else c("class", "names")) &&
    (is.null(classes) || identical(class(value), classes))
}

.liss_ca012_policy_path <- function() {
  system.file("schema", "source_nonresponse_ca20g012.yml", package = "lissr")
}

.liss_ca012_policy_sha256 <- function() {
  "ad4a39ea498faa5a1511a01d6dbc816a602e5d77c6f94b2af89c043eaf19dd28"
}

.liss_ca012_policy <- function() {
  path <- .liss_ca012_policy_path()
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path) || !file.exists(path) || dir.exists(path))
    .liss_ca012_abort("reviewed CA20g012 source authority is missing")
  bytes <- tryCatch(readBin(path, "raw", n = file.info(path)$size), error = function(e) NULL)
  if (is.null(bytes) || !identical(.liss_ca012_bytes_hash(bytes), .liss_ca012_policy_sha256()))
    .liss_ca012_abort("reviewed CA20g012 source authority integrity check failed")
  policy <- tryCatch(yaml::yaml.load(rawToChar(bytes), eval.expr = FALSE), error = function(e) NULL)
  if (!is.list(policy) || !identical(policy$schema_version, "1.0.0") ||
      !identical(policy$authority_id, "CA20G012_SOURCE") ||
      !identical(policy$authority_version, "1.0.0"))
    .liss_ca012_abort("reviewed CA20g012 source authority schema is unsupported")
  policy
}

.liss_ca012_receipt <- function(policy) {
  list(filename = policy$source$filename, sha256 = policy$source$sha256,
    size_bytes = as.double(policy$source$size_bytes))
}

.liss_ca012_path <- function(source, policy) {
  if (!is.character(source) || length(source) != 1L || is.na(source) ||
      !is.null(attributes(source)) || !nzchar(source) ||
      !identical(basename(source), policy$source$filename) ||
      !isTRUE(utils::file_test("-f", source)))
    .liss_ca012_abort("source must be a local file with the exact reviewed CA20g filename")
  source
}

.liss_ca012_source_state <- function(path) {
  before <- file.info(path)
  if (!isTRUE(utils::file_test("-f", path)) || is.na(before$size) ||
      before$size != .liss_ca012_policy()$source$size_bytes)
    .liss_ca012_abort("CA20g source must be a regular local file of the exact reviewed size")
  hash <- tryCatch({
    connection <- file(path, open = "rb", raw = TRUE)
    on.exit(close(connection), add = TRUE)
    value <- as.character(openssl::sha256(connection))
    attributes(value) <- NULL
    value
  }, error = function(e) .liss_ca012_abort("CA20g source could not be hashed"))
  after <- file.info(path)
  if (anyNA(c(before$size, after$size, as.numeric(before$mtime), as.numeric(after$mtime))) ||
      !identical(before$size, after$size) || !identical(before$mtime, after$mtime))
    .liss_ca012_abort("CA20g source changed while hashing")
  list(filename = basename(path), sha256 = hash, size_bytes = as.double(before$size))
}

.liss_ca012_read_source <- function(path) {
  source <- .liss_ca012_policy()$source
  bytes <- tryCatch(readBin(path, "raw", n = source$size_bytes + 1),
    error = function(e) .liss_ca012_abort("CA20g source bytes could not be read"))
  if (length(bytes) != source$size_bytes ||
      !identical(.liss_ca012_bytes_hash(bytes), source$sha256))
    .liss_ca012_abort("CA20g parsed bytes differ from the reviewed source")
  # parse the verified buffer, so a transient path replacement cannot change it
  tryCatch(haven::read_sav(bytes, user_na = TRUE, .name_repair = "minimal"),
    error = function(e) .liss_ca012_abort("reviewed CA20g source could not be imported"))
}

.liss_ca012_load <- function(source, policy) {
  path <- .liss_ca012_path(source, policy)
  receipt <- .liss_ca012_receipt(policy)
  before <- .liss_ca012_source_state(path)
  if (!identical(before, receipt)) .liss_ca012_abort("CA20g source identity differs from the reviewed authority")
  frame <- .liss_ca012_read_source(path)
  after <- .liss_ca012_source_state(path)
  if (!identical(before, after)) .liss_ca012_abort("CA20g source identity changed during import")
  frame
}

.liss_ca012_check_raw <- function(frame, policy) {
  if (!is.data.frame(frame) || !identical(class(frame), policy$frame_class) ||
      !identical(sort(names(attributes(frame))), sort(c("names", "row.names", "class"))) ||
      !identical(names(frame), policy$columns) || nrow(frame) != policy$rows ||
      !all(vapply(frame, function(column) is.null(dim(column)) && length(column) == policy$rows, logical(1))) ||
      !identical(unname(vapply(frame, typeof, character(1))), policy$column_types) ||
      !identical(row.names(frame), as.character(seq_len(nrow(frame)))))
    .liss_ca012_abort("the exact complete imported CA20g source schema and row order are required")
  for (column in names(policy$metadata_sha256)) {
    if (!identical(.liss_ca012_hash(attributes(frame[[column]])), policy$metadata_sha256[[column]]))
      .liss_ca012_abort("CA20g target, respondent or routing metadata differ from the reviewed source")
  }
  ids <- as.double(unclass(frame[[policy$id_column]]))
  if (any(!is.finite(ids)) || any(ids < 0) || any(ids != trunc(ids)) || any(ids > 2^53 - 1) || anyDuplicated(ids))
    .liss_ca012_abort("complete source keys must be unique nonnegative finite lossless integer-valued doubles")
  values <- as.double(unclass(frame[[policy$raw_column]]))
  if (any(values %in% policy$unsupported_positive_forms))
    .liss_ca012_abort("positive declared or documented nonresponse forms are outside this authority; the whole call is rejected")
  lock <- is.finite(values) & values %in% policy$codes
  route <- as.double(unclass(frame[[policy$route_column]]))
  if (any(!is.finite(route[lock]) | route[lock] != policy$required_route_for_coded_rows))
    .liss_ca012_abort("coded CA20g012 source rows require the exact nonmissing ca20g078 gate equal to one")
  list(policy = policy, ids = ids, keys = sprintf("%.0f", ids), values = values, lock = lock)
}

.liss_ca012_unmark <- function(frame) {
  for (key in c("liss_source_nonresponse", "source_nonresponse_readiness")) attr(frame, key) <- NULL
  class(frame) <- c("tbl_df", "tbl", "data.frame")
  frame
}

.liss_ca012_ledger <- function(checked) {
  policy <- checked$policy
  receipt <- .liss_ca012_receipt(policy)
  rows <- which(checked$lock)
  values <- checked$values[rows]
  data.frame(source_cell_key = if (length(rows)) paste(receipt$sha256, rows, policy$raw_column, sep = ":") else character(),
    source_row_index = as.integer(rows), nomem_key = checked$keys[rows],
    source_sha256 = rep(receipt$sha256, length(rows)), source_filename = rep(receipt$filename, length(rows)),
    raw_column = rep(policy$raw_column, length(rows)), case_id = policy$case_ids[match(values, policy$codes)],
    original_value = values, raw_value_repr = sprintf("%.0f", values),
    source_declared_missing = rep(FALSE, length(rows)), answer_status = rep(policy$answer_status, length(rows)),
    candidate_answer_status = rep(NA_character_, length(rows)), subtype_evidence = rep("unresolved", length(rows)),
    analysis_disposition = rep(policy$analysis_disposition, length(rows)),
    analysis_value_after = rep(NA_real_, length(rows)), no_numeric_proposal = rep(TRUE, length(rows)),
    route_column = rep(policy$route_column, length(rows)), route_value = rep(1, length(rows)), stringsAsFactors = FALSE)
}

.liss_ca012_readiness <- function(checked) {
  field <- list(raw_column = checked$policy$raw_column, status = "passed",
    scope = "two exact negative source encodings only; other values are untargeted and uncertified",
    evaluated_cells = as.integer(length(checked$values)), classified_cells = as.integer(sum(checked$lock)),
    unresolved_cells = 0L, untargeted_nonfinite_cells = as.integer(sum(!is.finite(checked$values))))
  list(schema_version = "1.0.0", kind = "ca20g012_readiness", status = field$status,
    overall_amount_validity = "not_evaluated", fields = list(CA20G012_FIELD = field))
}

.liss_ca012_record <- function(result, checked) {
  policy <- checked$policy
  record <- list(schema_version = "1.0.0", kind = "ca20g012_classification",
    authority_id = policy$authority_id, authority_version = policy$authority_version,
    authority_sha256 = .liss_ca012_policy_sha256(), source = .liss_ca012_receipt(policy), codebook = policy$codebook,
    source_context = policy$source_context, construct = policy$construct, unit = policy$unit,
    measurement_date = policy$measurement_date, documented_signed_range = policy$documented_signed_range,
    reader = list(package = "haven", user_na = TRUE, name_repair = "minimal", retained_scope = policy$retained_scope),
    fields = list(CA20G012_FIELD = list(raw_column = policy$raw_column, id_column = policy$id_column,
      route_column = policy$route_column, metadata_sha256 = policy$metadata_sha256)),
    coverage = data.frame(source_row_index = seq_along(checked$values), nomem_key = checked$keys,
      original_value = checked$values, classified = checked$lock, stringsAsFactors = FALSE),
    original_digest = .liss_ca012_hash(result$original_data),
    analysis_digest = .liss_ca012_hash(.liss_ca012_unmark(result$analysis$data)),
    ledger_digest = .liss_ca012_hash(result$analysis$ledger),
    readiness_digest = .liss_ca012_hash(result$source_nonresponse_readiness))
  record$digest <- .liss_ca012_hash(record)
  record
}

.liss_ca012_marker <- function(record) {
  list(schema_version = "1.0.0", kind = "ca20g012_analysis_frame", fields = record$fields,
    authority_sha256 = record$authority_sha256, source = record$source, parent_digest = record$original_digest,
    analysis_digest = record$analysis_digest, classification_digest = record$digest)
}

.liss_ca012_build <- function(input, checked) {
  data <- input
  target <- checked$values
  target[checked$lock] <- NA_real_
  attributes(target) <- attributes(input[[checked$policy$raw_column]])
  data[[checked$policy$raw_column]] <- target
  readiness <- .liss_ca012_readiness(checked)
  result <- list(original_data = input, analysis = list(data = data, ledger = .liss_ca012_ledger(checked)),
    data = data, source_nonresponse = NULL, source_nonresponse_readiness = readiness)
  record <- .liss_ca012_record(result, checked)
  class(data) <- c("liss_source_nonresponse_collection_result", class(input))
  attr(data, "liss_source_nonresponse") <- .liss_ca012_marker(record)
  attr(data, "source_nonresponse_readiness") <- readiness
  result$analysis$data <- data
  result$data <- data
  result$source_nonresponse <- record
  class(result) <- "liss_source_nonresponse_collection_result"
  result
}

#' Classify or validate exact CA20g012 source nonresponse
#'
#' Read only the complete reviewed `ca20g_EN_1.0p.sav` source and classify the
#' exact `ca20g012` codes `-9999999999` and `-9999999998`. A separately pinned
#' authority and whole-result carrier are version 1.0.0. This field is a signed
#' banking/savings balance at 2019-12-31 in whole EUR, not household income.
#'
#' Creation checks filename, byte size and SHA-256 before and after import.
#' Haven parses an in-memory byte buffer whose exact size and hash were checked,
#' with `user_na = TRUE` and minimal name repair. The complete imported tibble,
#' original row order, all columns and metadata remain in `original_data`.
#' Only the two codes become NA in the separate `analysis$data` target. Its
#' original source labels and missing declarations remain descriptive metadata;
#' they do not attest changed analysis values. `data` duplicates that marked
#' analysis frame for existing consumer guards. The typed `analysis$ledger`
#' retains original code, root ordinal, key, case, unknown subtype, missing-amount
#' disposition and no-numeric-proposal flag. Source-cell references use the
#' source hash, original row ordinal and raw field.
#'
#' SAV positive ten-digit missing declarations differ from its positive
#' eleven-digit labels and codebook forms. The negative codes are undeclared.
#' No sign/digit repair or authenticated DK/refusal subtype follows. If
#' `ca20g012` contains positive `9999999998`, `9999999999`, `99999999998` or
#' `99999999999`, classification and validation reject the whole call with
#' `liss_ca20g012_blocked`; no partial result is returned.
#' Ordinary signed values, including -9
#' and -8, and nonfinite nonmatches stay untouched. Readiness covers only the
#' two source encodings; overall amount validity is `not_evaluated`. No amount
#' or sign is reconstructed from a bracket. Other CA fields/releases are unassessed.
#'
#' Validation requires the complete closed result and reconstructs it from the
#' retained complete original. With `source = NULL`, this checks structural
#' consistency only. Supplying `source` additionally rereads verified source
#' bytes and compares the entire imported original, including all other columns
#' and metadata. Result payloads and reread originals must match in version-2
#' serialization, including signed zero, missing-value payloads and attribute
#' order. Guard-attribute placement on analysis frames is ignored, while their
#' values are checked separately. Whole-result RDS retains the object.
#' Validation still requires the reconstructed result and any reread source to
#' reproduce the required serialized representation. Validation across R or
#' package versions is not guaranteed. Ordinary serialized hashes
#' (version 2) are not authentication: a consistently substituted original and
#' result may pass structure, while source rereading detects a mismatch against
#' the trusted file. Neither mode establishes statistical validity.
#'
#' Existing collection claims are guards, not CI carrier or numerical-consumer
#' approval. Rebinding, income/wealth cleaning, applied views, background, panel,
#' report and SAV/CSV export are unsupported. In explicit `correct` or `na_only`
#' mode, [liss_clean_income()] can evaluate `background` to check its required
#' `NULL` value before rejecting the unsupported CA parent. Rejection does not
#' guarantee that caller-supplied argument expressions remain unevaluated.
#' Extracting unmarked original data
#' or stripping every recognized claim leaves unknown ownership; aliases cannot
#' recover it. The default CA merge recipe and whole-wave B03 flag are unchanged.
#'
#' Serialized object hashes use profile `1.0.0`: SHA-256 over XDR version-2
#' R serialization with only the writer-version header fixed to R 4.6.1.
#' All payload bytes remain significant; raw file hashes are unchanged.
#' Compatibility covers the R 4.6.1 reference profile, not arbitrary historical
#' writers or future serialization bodies. Other legacy profiles require their
#' original package/runtime. Reference identity checks remain separate from digests.
#'
#' @param source path to the exact reviewed local SAV file. Required for
#'   classification; optional for additional source consistency validation.
#' @param result complete result returned by [liss_classify_ca20g012()].
#' @return Classification returns a guarded closed list with `original_data`,
#'   `analysis`, `data`, `source_nonresponse` and `source_nonresponse_readiness`.
#'   `analysis` contains `data` and `ledger`. Validation invisibly returns
#'   `valid`, `validation`, `source_consistency`, `classified_cells`,
#'   `source_readiness`, `source_authentication` and `statistical_validity`.
#' @export
liss_classify_ca20g012 <- function(source) {
  if (missing(source)) .liss_ca012_abort("an explicit reviewed local source path is required")
  policy <- .liss_ca012_policy()
  original <- .liss_ca012_load(source, policy)
  .liss_ca012_build(original, .liss_ca012_check_raw(original, policy))
}

#' @rdname liss_classify_ca20g012
#' @export
liss_validate_ca20g012 <- function(result, source = NULL) {
  keys <- c("original_data", "analysis", "data", "source_nonresponse", "source_nonresponse_readiness")
  if (!.liss_ca012_closed(result, keys, "liss_source_nonresponse_collection_result") ||
      !.liss_ca012_closed(result$analysis, c("data", "ledger"))) .liss_ca012_abort()
  policy <- .liss_ca012_policy()
  checked <- .liss_ca012_check_raw(result$original_data, policy)
  expected <- .liss_ca012_build(result$original_data, checked)
  if (!.liss_ca012_result_equal(result, expected))
    .liss_ca012_abort("CA20g012 result differs from reconstruction against its complete retained original")
  if (!is.null(source) && !.liss_ca012_equal(result$original_data, .liss_ca012_load(source, policy)))
    .liss_ca012_abort("CA20g012 retained original differs from the freshly verified complete source")
  invisible(list(valid = TRUE, validation = if (is.null(source)) "structural" else "source_consistency",
    source_consistency = if (is.null(source)) "not_requested" else "matched",
    classified_cells = as.integer(sum(checked$lock)), source_readiness = expected$source_nonresponse_readiness,
    source_authentication = FALSE, statistical_validity = FALSE))
}
