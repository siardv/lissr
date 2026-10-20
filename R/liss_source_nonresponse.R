#' classify one reviewed CI23p source-income nonresponse field
#'
#' Bind one explicitly selected column to the exact, reviewed local SAV source.
#' The returned data retain their values and metadata. The separate carrier
#' records typed source nonresponse and prohibits numerical proposals for the
#' matched household cells when consumed by [liss_clean_income()]. The default
#' schema-1 recipe covers `ci23p339 = -9999999999`. An explicit schema-2 recipe
#' selects either `ci23p337` or `ci23p338`, with exact codes `-9999999999` and
#' `-9999999998` under one field receipt. Taxable fields have no household-cleaner
#' approval. One classifier call selects one field. An explicit schema-3 guarded
#' module merge can carry both taxable fields in one complete collection; this
#' scalar classifier rejects schemas 3 and 4 and every existing collection lineage.
#'
#' For household classification, pass the complete result to [liss_clean_income()].
#' The classifier
#' does not add a frame marker to its unchanged `$data`; extracting an unmarked
#' frame and cleaning it alone enters ordinary unguarded processing. Keep the
#' original merge result separately when rebinding, because its provenance, log and
#' validation fields are not copied into the classifier result.
#'
#' In flag mode the actual supplied target remains unchanged, including missing
#' analysis amounts from a guarded merge. Verified source originals are retained
#' in the cleaner's `_observed` audit column and source ledger; matched proposals
#' stay missing. Readiness covers only this source/field contract, not the
#' suitability of every audit column for analysis. A source-bound household
#' amount cannot also serve as the cleaner's `category_code`.
#'
#' Reordering and subsets require retained column metadata and the complete
#' carrier, as with supported tibble/dplyr operations. Base data-frame operations
#' that strip target attributes fail closed. Validation does not relax metadata
#' checks to accommodate such operations.
#'
#' Serialized object hashes use profile `1.0.0`: SHA-256 over XDR version-2
#' R serialization with only the writer-version header fixed to R 4.6.1.
#' All payload bytes remain significant; raw file hashes are unchanged.
#' Compatibility covers the R 4.6.1 reference profile, not arbitrary historical
#' writers or future serialization bodies. Other legacy profiles require their
#' original package/runtime. Reference identity checks remain separate from digests.
#'
#' @param data a data frame, or a merge result containing `data`.
#' @param source path to the reviewed `ci23p_EN_1.0p.sav` file. Its filename
#'   and SHA256 must match the contract; a caller-provided hash is insufficient.
#' @param target one explicitly selected column. Raw binding without a wave
#'   column requires the selected exact raw name. A merged binding requires
#'   an explicit wave column and unchanged values under unique respondent keys.
#'   Supported household targets are `ci23p339`, `s339`, `ci_s339`, `nethh`
#'   and `ci00a339`. Taxable targets are only the selected `ci23p337`/`s337`/
#'   `ci_s337` or `ci23p338`/`s338`/`ci_s338` names; aliases do not select a field
#'   or establish source ownership.
#' @param wave_col optional column containing exact wave identifiers. Only
#'   rows equal to `ci23p` are evaluated, but every presented row must have a
#'   nonmissing, nonempty exact wave identifier. Years alone are insufficient.
#'   Full-join panels with CI-absent rows lacking CI wave identity are unsupported.
#' @param id_col respondent-key column, default `nomem_encr`.
#' @param recipe optional parsed CI recipe or recipe path. Its contract must
#'   match the complete typed contract in the package-owned, digest-pinned
#'   reviewed registry. Recipes cannot approve new scope. The installed CI
#'   recipe is used by default and selects the existing household contract.
#'   Consumer approval is checked separately before numerical preparation.
#'   Schema 2.0.0 requires only `schema_version` and one complete reviewed
#'   `field` record. Existing version-1 carriers retain their original rule and
#'   scalar shape. Taxable bindings require unguarded inputs; rebinding any
#'   taxable carrier is unsupported, including a reverse household binding.
#'   Schema-3 taxable pairs and schema-4 mixed three-field collections are available
#'   only through an explicitly guarded module merge. A separate package-owned
#'   consumer policy permits explicit household339 flag simulation on an intact
#'   schema-4 collection while retaining all three fields and complete lineage.
#'   This scalar classifier provides no collection projection or declassification.
#' @return a `liss_source_nonresponse_result` with unchanged `data` and a
#'   `source_nonresponse` carrier containing the source receipt, original
#'   metadata, cell ledger, coverage, readiness and serialization digest.
#'   Retain this complete wrapper. RDS preserves the carrier. SAV/CSV alone do
#'   not preserve readiness; reimport requires a new explicit verified source
#'   binding.
#' @export
liss_classify_source_nonresponse <- function(data, source, target,
                                            wave_col = NULL,
                                            id_col = "nomem_encr",
                                            recipe = NULL) {
  rule <- .liss_nonresponse_rule(recipe)
  .liss_nonresponse_assert_rebind(data, rule)
  if (.liss_nonresponse_is_field(rule))
    return(.liss_nonresponse_classify_field(data, source, target, wave_col,
                                           id_col, rule))
  if (is.list(data) && !is.data.frame(data) && !is.null(data$data))
    data <- data$data
  if (!is.data.frame(data))
    .liss_nonresponse_abort("`data` must be a data frame or a merge result with $data")
  approved <- .liss_nonresponse_approved(rule)
  .liss_nonresponse_column(data, target, "target")
  .liss_nonresponse_target(target, rule)
  .liss_nonresponse_column(data, id_col, "id_col")
  if (is.null(wave_col) && !identical(target, rule$raw_column))
    .liss_nonresponse_abort("raw source binding requires the exact `ci23p339` target; merged binding requires an explicit `wave_col`")
  if (!is.null(wave_col)) .liss_nonresponse_column(data, wave_col, "wave_col")
  if (!is.character(source) || length(source) != 1L || is.na(source) ||
      !nzchar(source) || !file.exists(source) || dir.exists(source))
    .liss_nonresponse_abort("`source` must be one existing local SAV file")
  receipt <- .liss_nonresponse_source_state(source)
  if (!identical(receipt$filename, rule$source_filename) ||
      !identical(receipt$sha256, rule$source_sha256))
    .liss_nonresponse_abort("source nonresponse binding blocked: source filename or SHA256 does not match the reviewed CI23p source")
  raw <- .liss_nonresponse_read_source(source)
  after <- .liss_nonresponse_source_state(source)
  if (!identical(receipt, after))
    .liss_nonresponse_abort("source nonresponse binding blocked: source changed while being read")
  .liss_nonresponse_column(raw, rule$raw_column, "raw_column")
  .liss_nonresponse_column(raw, "nomem_encr", "source respondent key")
  source_ids <- .liss_nonresponse_ids(raw$nomem_encr)
  if (anyDuplicated(source_ids))
    .liss_nonresponse_abort("source nonresponse binding blocked: source respondent keys are duplicated")
  source_values <- .liss_nonresponse_values(raw[[rule$raw_column]])
  selected <- .liss_nonresponse_rows(data, wave_col, rule$wave)
  ids <- .liss_nonresponse_ids(data[[id_col]][selected])
  if (anyDuplicated(ids))
    .liss_nonresponse_abort("source nonresponse binding blocked: selected respondent keys are duplicated")
  pos <- match(ids, source_ids)
  if (anyNA(pos))
    .liss_nonresponse_abort("source nonresponse binding blocked: selected respondent keys are absent from the reviewed source")
  values <- .liss_nonresponse_values(data[[target]][selected])
  if (!all(.liss_nonresponse_equal(values, source_values[pos])))
    .liss_nonresponse_abort("source nonresponse binding blocked: selected values differ from the original source")
  coverage <- data.frame(nomem_encr = ids, source_row_index = pos,
                         original_value = source_values[pos],
                         stringsAsFactors = FALSE)
  matched <- is.finite(coverage$original_value) &
    coverage$original_value == rule$code
  ledger <- coverage[matched, , drop = FALSE]
  ledger$source_sha256 <- rep(rule$source_sha256, nrow(ledger))
  ledger$source_filename <- rep(rule$source_filename, nrow(ledger))
  ledger$source_release <- rep(approved$source_release, nrow(ledger))
  ledger$source_language <- rep(rule$language, nrow(ledger))
  ledger$raw_column <- rep(rule$raw_column, nrow(ledger))
  ledger$raw_storage_type <- rep(typeof(raw[[rule$raw_column]]), nrow(ledger))
  ledger$raw_value_repr <- sprintf("%.0f", ledger$original_value)
  ledger$current_value_before <- ledger$original_value
  ledger$analysis_value_after <- rep(NA_real_, nrow(ledger))
  ledger$system_missing_before <- rep(FALSE, nrow(ledger))
  original_metadata <- attributes(raw[[rule$raw_column]])
  declared_values <- original_metadata$na_values
  if (is.null(declared_values)) declared_values <- numeric()
  declared <- rule$code %in% declared_values
  missing_range <- original_metadata$na_range
  if (is.numeric(missing_range) && length(missing_range) == 2L &&
      all(is.finite(missing_range)))
    declared <- declared || (rule$code >= missing_range[1L] &&
                              rule$code <= missing_range[2L])
  ledger$source_declared_missing <- rep(declared, nrow(ledger))
  ledger$source_metadata_id <- rep(paste(rule$source_sha256,
                                         rule$raw_column, sep = ":"), nrow(ledger))
  ledger$evidence_ref <- rep(approved$evidence_ref, nrow(ledger))
  ledger$wave_id <- rep(rule$wave, nrow(ledger))
  ledger$current_column <- rep(target, nrow(ledger))
  ledger$source_key <- if (nrow(ledger))
    paste(rule$source_sha256, ledger$source_row_index, rule$raw_column, sep = ":")
  else character()
  ledger$rule_id <- rep(rule$rule_id, nrow(ledger))
  ledger$rule_version <- rep(rule$rule_version, nrow(ledger))
  ledger$response_validity <- rep(rule$response_validity, nrow(ledger))
  ledger$answer_status <- rep(rule$answer_status, nrow(ledger))
  ledger$analysis_disposition <- rep(rule$analysis_disposition, nrow(ledger))
  ledger$no_numeric_proposal <- rep(rule$no_numeric_proposal, nrow(ledger))
  ledger$measurement_period <- rep(rule$measurement_period, nrow(ledger))
  ledger$candidate_answer_status <- rep(NA_character_, nrow(ledger))
  ledger$subtype_evidence <- rep("unresolved", nrow(ledger))
  routing <- approved$routing
  if (routing$column %in% names(raw) && nrow(ledger)) {
    bracket <- .liss_nonresponse_values(raw[[routing$column]])[ledger$source_row_index]
    routed <- !is.na(bracket)
    ledger$candidate_answer_status[routed] <- routing$candidate_answer_status
    ledger$subtype_evidence[routed] <- routing$subtype_evidence
  }
  # materialize generated columns and names before R 4.5 serializes growth flags
  ledger <- ledger[seq_along(ledger)]
  state <- list(schema_version = "1.0.0", rule = rule, source = receipt,
                target = target, wave_col = wave_col, id_col = id_col,
                normalized = FALSE, coverage = coverage, ledger = ledger,
                input_rows = .liss_nonresponse_input_rows(
                  data, target, id_col, wave_col, rule$wave),
                metadata = list(raw_column = rule$raw_column,
                                attributes = attributes(raw[[rule$raw_column]]),
                                id_attributes = attributes(raw$nomem_encr),
                                codebook_filename = rule$codebook_filename,
                                codebook_sha256 = rule$codebook_sha256),
                target_metadata = attributes(data[[target]]),
                readiness = .liss_nonresponse_readiness(nrow(coverage), sum(matched), rule))
  state <- .liss_nonresponse_seal(state)
  structure(list(data = data, source_nonresponse = state,
                 source_nonresponse_readiness = state$readiness),
            class = "liss_source_nonresponse_result")
}

#' @noRd
.liss_nonresponse_source_state <- function(path) {
  before <- file.info(path)
  connection <- file(path, open = "rb", raw = TRUE)
  on.exit(close(connection), add = TRUE)
  hash <- as.character(openssl::sha256(connection))
  attributes(hash) <- NULL
  after <- file.info(path)
  if (anyNA(c(before$size, after$size, as.numeric(before$mtime),
              as.numeric(after$mtime))) || !identical(before$size, after$size) ||
      !identical(before$mtime, after$mtime))
    .liss_nonresponse_abort("source nonresponse binding blocked: source changed while hashing")
  list(filename = basename(path), sha256 = hash, size_bytes = before$size)
}

#' @noRd
.liss_nonresponse_read_source <- function(path) {
  haven::read_sav(path, user_na = TRUE)
}

#' @noRd
.liss_nonresponse_digest <- function(state) {
  state$digest <- NULL
  .liss_identity_hash(state)
}

#' @noRd
.liss_nonresponse_seal <- function(state) {
  state$digest <- .liss_nonresponse_digest(state)
  state
}

#' @noRd
.liss_nonresponse_readiness <- function(evaluated, classified, rule,
                                          case_counts = NULL) {
  approved <- .liss_nonresponse_approved(rule)
  if (.liss_nonresponse_is_field(rule)) {
    ids <- vapply(approved$cases, function(case) case$rule_id, character(1))
    if (is.null(case_counts)) case_counts <- stats::setNames(integer(length(ids)), ids)
    if (!is.numeric(case_counts) || !identical(names(case_counts), ids) ||
        anyNA(case_counts) || any(!is.finite(case_counts)) ||
        any(case_counts < 0 | case_counts != trunc(case_counts)) ||
        sum(case_counts) != classified)
      .liss_nonresponse_abort("source nonresponse binding blocked: invalid per-case readiness counts")
    return(list(status = if (evaluated > 0L) "passed" else "not_evaluated",
                field_id = rule$field_id, field_version = rule$field_version,
                evaluated_cells = evaluated, classified_cells = classified,
                unresolved_cells = 0L, case_counts = stats::setNames(as.integer(case_counts), ids),
                role = approved$readiness$role, scope = approved$readiness$scope))
  }
  list(status = if (evaluated > 0L) "passed" else "not_evaluated",
       rule_id = rule$rule_id, evaluated_cells = evaluated,
       classified_cells = classified, unresolved_cells = 0L,
       role = approved$readiness$role, scope = approved$readiness$scope)
}

#' @noRd
.liss_nonresponse_column <- function(data, column, argument) {
  if (!is.character(column) || length(column) != 1L || is.na(column) ||
      !nzchar(column) || sum(names(data) == column) != 1L)
    .liss_nonresponse_abort("source nonresponse binding requires one explicit existing `{argument}` column")
  invisible(TRUE)
}

#' @noRd
.liss_nonresponse_target <- function(target, rule) {
  approved <- .liss_nonresponse_approved(rule)
  if (!(target %in% unlist(approved$targets, use.names = FALSE)))
    .liss_nonresponse_abort(if (.liss_nonresponse_is_field(rule))
      "source nonresponse binding blocked: target is outside the reviewed field names"
      else "source nonresponse binding blocked: target is outside the reviewed household-income names")
  invisible(TRUE)
}

#' @noRd
.liss_nonresponse_values <- function(x) {
  if (!is.numeric(x) || is.factor(x))
    .liss_nonresponse_abort("source nonresponse binding blocked: target/source values must retain numeric storage")
  as.numeric(x)
}

#' @noRd
.liss_nonresponse_ids <- function(x) {
  if (is.numeric(x) && !is.factor(x)) {
    value <- as.numeric(x)
    if (any(!is.finite(value)) || any(value < 0 | value != trunc(value)) ||
        any(value > 2^53))
      .liss_nonresponse_abort("source nonresponse binding blocked: respondent keys must be complete lossless nominal identifiers")
    return(sprintf("%.0f", value))
  }
  if (is.character(x) && !anyNA(x) && all(grepl("^[0-9]+$", x)))
    return(x)
  .liss_nonresponse_abort("source nonresponse binding blocked: respondent keys must be complete numeric or digit-string nominal identifiers")
}

#' @noRd
.liss_nonresponse_rows <- function(data, wave_col, wave) {
  if (is.null(wave_col)) return(rep(TRUE, nrow(data)))
  .liss_nonresponse_column(data, wave_col, "wave_col")
  values <- data[[wave_col]]
  if (!is.character(values) && !is.factor(values))
    .liss_nonresponse_abort("source nonresponse binding requires exact wave identifiers, not numeric years")
  !is.na(values) & as.character(values) == wave
}

#' @noRd
.liss_nonresponse_equal <- function(x, y) {
  (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
}

#' @noRd
.liss_nonresponse_input_rows <- function(data, target, id_col, wave_col, wave) {
  ids <- .liss_nonresponse_ids(data[[id_col]])
  waves <- if (is.null(wave_col)) rep(wave, nrow(data)) else {
    .liss_nonresponse_column(data, wave_col, "wave_col")
    values <- data[[wave_col]]
    if ((!is.character(values) && !is.factor(values)) || anyNA(values) ||
        any(!nzchar(as.character(values))))
      .liss_nonresponse_abort("source nonresponse binding requires complete exact wave identifiers")
    as.character(values)
  }
  keys <- paste(ids, waves, sep = ":")
  if (anyDuplicated(keys))
    .liss_nonresponse_abort("source nonresponse binding blocked: complete input respondent/wave keys are duplicated")
  data.frame(nomem_encr = ids, wave_id = waves, row_key = keys,
             value = .liss_nonresponse_values(data[[target]]),
             stringsAsFactors = FALSE)
}

#' @noRd
.liss_nonresponse_rule <- function(recipe = NULL) {
  if (is.null(recipe)) recipe <- system.file("recipes", "ci_merge_recipe.yml",
                                            package = "lissr", mustWork = TRUE)
  if (is.character(recipe) && length(recipe) == 1L)
    recipe <- yaml::yaml.load_file(recipe)
  errors <- validate_source_nonresponse_contracts(recipe, "source binding")
  if (length(errors)) .liss_nonresponse_abort(c("invalid source nonresponse contract", errors))
  if (is.null(recipe$source_nonresponse))
    .liss_nonresponse_abort("recipe has no reviewed source nonresponse contract")
  if (recipe$source_nonresponse$schema_version %in% c("3.0.0", "4.0.0"))
    .liss_nonresponse_abort("source nonresponse binding blocked: the scalar classifier does not support schema 3 or 4 collections; use an explicitly guarded module merge")
  if (identical(recipe$source_nonresponse$schema_version, "2.0.0"))
    recipe$source_nonresponse$field
  else recipe$source_nonresponse$contracts[[1L]]
}

#' @noRd
.liss_nonresponse_registry_path <- function() {
  system.file("schema", "source_nonresponse_registry.yml", package = "lissr")
}

#' @noRd
.liss_nonresponse_registry <- function() {
  # resolve only the package resource; no caller path, cwd discovery or cache
  path <- .liss_nonresponse_registry_path()
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path) || !file.exists(path) || dir.exists(path))
    .liss_nonresponse_abort("reviewed source nonresponse registry is missing")
  bytes <- tryCatch(readBin(path, "raw", n = file.info(path)$size),
                    error = function(e) NULL)
  hash <- if (!is.null(bytes)) as.character(openssl::sha256(bytes)) else ""
  attributes(hash) <- NULL
  if (!identical(hash, "a2e6694acc5855d2df389df46d66e0c94b705637626309cc309ceee8d840ddc3"))
    .liss_nonresponse_abort("reviewed source nonresponse registry integrity check failed")
  registry <- tryCatch(yaml::yaml.load(rawToChar(bytes), eval.expr = FALSE),
                       error = function(e) NULL)
  .liss_nonresponse_registry_validate(registry)
  registry
}

#' @noRd
.liss_nonresponse_registry_validate <- function(registry) {
  keys <- function(x, required)
    is.list(x) && identical(names(attributes(x)), "names") &&
      identical(sort(names(x)), sort(required))
  text <- function(x)
    is.character(x) && is.null(attributes(x)) && length(x) == 1L &&
      !is.na(x) && nzchar(x)
  number <- function(x)
    is.numeric(x) && is.null(attributes(x)) && length(x) == 1L && is.finite(x)
  targets <- function(x) {
    values <- unlist(x, use.names = FALSE)
    is.character(x) && is.null(attributes(x)) && length(x) > 0L &&
      !anyNA(x) && all(nzchar(x)) && !anyDuplicated(values)
  }
  invalid <- function()
    .liss_nonresponse_abort("reviewed source nonresponse registry schema is unsupported or ambiguous")
  if (!keys(registry, c("records", "fields", "registry_version", "schema_version")) ||
      !identical(registry$schema_version, "2.0.0") ||
      !identical(registry$registry_version, "2.0.0") ||
      !is.list(registry$records) || !is.null(names(registry$records)) ||
      length(registry$records) != 1L) invalid()
  record <- registry$records[[1L]]
  if (!keys(record, c("contract", "source_release", "targets", "metadata_policy",
                      "evidence_ref", "routing", "readiness", "consumers"))) invalid()
  rule <- record$contract
  text_keys <- c("rule_id", "rule_version", "module", "wave", "language",
                 "source_filename", "source_sha256", "raw_column", "canonical_column",
                 "construct", "unit", "codebook_filename", "codebook_sha256",
                 "response_validity", "answer_status", "analysis_disposition")
  if (!keys(rule, c(text_keys, "code", "measurement_period", "no_numeric_proposal")) ||
      !all(vapply(rule[text_keys], text, logical(1))) ||
      !number(rule$code) || !number(rule$measurement_period) ||
      !isTRUE(rule$no_numeric_proposal) ||
      !identical(record$metadata_policy, "retain_original_attributes") ||
      !text(record$source_release) || !text(record$evidence_ref) ||
      !targets(record$targets) ||
      !keys(record$routing, c("column", "candidate_answer_status", "subtype_evidence")) ||
      !all(vapply(record$routing, text, logical(1))) ||
      !keys(record$readiness, c("role", "scope")) ||
      !all(vapply(record$readiness, text, logical(1))) ||
      !is.list(record$consumers) || !is.null(names(record$consumers)) ||
      length(record$consumers) != 1L) invalid()
  capability <- record$consumers[[1L]]
  if (!keys(capability, c("name", "construct", "unit", "measurement_period", "targets")) ||
      !all(vapply(capability[c("name", "construct", "unit")], text, logical(1))) ||
      !number(capability$measurement_period) || !targets(capability$targets) ||
      !all(unlist(capability$targets, use.names = FALSE) %in%
             unlist(record$targets, use.names = FALSE))) invalid()
  .liss_nonresponse_fields_validate(registry$fields, invalid)
  invisible(TRUE)
}

#' @noRd
.liss_nonresponse_rule_equal <- function(rule, exact) {
  if (!is.list(rule) || !identical(names(attributes(rule)), "names") ||
      !identical(sort(names(rule)), sort(names(exact))))
    return(FALSE)
  all(vapply(names(exact), function(key) {
    value <- rule[[key]]
    if (is.numeric(exact[[key]]))
      is.numeric(value) && is.null(attributes(value)) && length(value) == 1L &&
        is.finite(value) && value == exact[[key]]
    else identical(value, exact[[key]])
  }, logical(1)))
}

#' @noRd
.liss_nonresponse_approved <- function(rule) {
  registry <- .liss_nonresponse_registry()
  if (.liss_nonresponse_is_field(rule)) {
    matched <- vapply(registry$fields, function(field)
      .liss_nonresponse_field_equal(rule, field), logical(1))
    if (sum(matched) != 1L)
      .liss_nonresponse_abort("source nonresponse contract is outside the reviewed scope/policy")
    return(registry$fields[[which(matched)]])
  }
  matched <- vapply(registry$records, function(record)
    .liss_nonresponse_rule_equal(rule, record$contract), logical(1))
  if (sum(matched) != 1L)
    .liss_nonresponse_abort("source nonresponse contract is outside the reviewed scope/policy")
  registry$records[[which(matched)]]
}

#' @noRd
.liss_nonresponse_consumer <- function(rule, target, consumer = "liss_clean_income") {
  approved <- .liss_nonresponse_approved(rule)
  capabilities <- Filter(function(capability) identical(capability$name, consumer),
                         approved$consumers)
  if (length(capabilities) != 1L)
    .liss_nonresponse_abort("source nonresponse contract has no approval for this consumer")
  capability <- capabilities[[1L]]
  if (!.liss_nonresponse_rule_equal(
    rule[c("construct", "unit", "measurement_period")],
    capability[c("construct", "unit", "measurement_period")]) ||
      !(target %in% unlist(capability$targets, use.names = FALSE)))
    .liss_nonresponse_abort("source nonresponse construct/unit/period/target is not approved for this consumer")
  invisible(TRUE)
}

#' @noRd
validate_source_nonresponse_contracts <- function(recipe, path = "<unknown>") {
  block <- recipe$source_nonresponse
  if (is.null(block)) return(character())
  fail <- function(message) paste0("source_nonresponse in ", path, ": ", message)
  if (!is.list(block))
    return(fail("requires one supported scalar source nonresponse contract"))
  if (identical(block$schema_version, "1.0.0")) {
    if (!identical(sort(names(block)), c("contracts", "schema_version")) ||
        !is.list(block$contracts) || length(block$contracts) != 1L || !is.list(block$contracts[[1L]]))
      return(fail("requires schema_version 1.0.0 and exactly one reviewed contract"))
    rule <- block$contracts[[1L]]
  } else if (identical(block$schema_version, "2.0.0")) {
    if (!identical(names(attributes(block)), "names") ||
        !identical(sort(names(block)), c("field", "schema_version")) ||
        !is.list(block$field) || !.liss_nonresponse_is_field(block$field))
      return(fail("requires schema_version 2.0.0 and exactly one complete reviewed field"))
    rule <- block$field
  } else if (identical(block$schema_version, "3.0.0")) {
    if (!identical(names(attributes(block)), "names") ||
        !identical(sort(names(block)), c("fields", "schema_version")) ||
        !is.list(block$fields) || !is.null(attributes(block$fields)) ||
        length(block$fields) != 2L ||
        !all(vapply(block$fields, .liss_nonresponse_is_field, logical(1))))
      return(fail("requires schema_version 3.0.0 and exactly the complete reviewed taxable field pair"))
    approved <- tryCatch(lapply(block$fields, .liss_nonresponse_approved),
                         liss_source_nonresponse_blocked = function(e) e)
    if (inherits(approved, "liss_source_nonresponse_blocked"))
      return(fail(conditionMessage(approved)))
    ids <- vapply(approved, function(field) field$field_id, character(1))
    if (anyDuplicated(ids) ||
        !identical(sort(ids), c("CI23P337_FIELD", "CI23P338_FIELD")))
      return(fail("requires each reviewed taxable field exactly once"))
    if (!identical(recipe$meta$module, "ci"))
      return(fail("unreviewed recipe module"))
    return(character())
  } else if (identical(block$schema_version, "4.0.0")) {
    checked <- tryCatch(.liss_nonresponse_collection_selection(block),
      liss_source_nonresponse_blocked = function(e) e)
    if (inherits(checked, "liss_source_nonresponse_blocked"))
      return(fail(conditionMessage(checked)))
    if (!identical(recipe$meta$module, "ci"))
      return(fail("unreviewed recipe module"))
    return(character())
  } else return(fail("unsupported source nonresponse schema version"))
  approved <- tryCatch(.liss_nonresponse_approved(rule),
                       liss_source_nonresponse_blocked = function(e) e)
  if (inherits(approved, "liss_source_nonresponse_blocked"))
    return(fail(conditionMessage(approved)))
  module <- if (.liss_nonresponse_is_field(rule)) approved$module else approved$contract$module
  if (!identical(recipe$meta$module, module))
    return(fail("unreviewed recipe module"))
  character()
}

#' @noRd
source_nonresponse_validate <- function(data, state, target = NULL, wave_col = NULL) {
  if (.liss_nonresponse_claims_consumer(data)) {
    if (!is.null(target) || !is.null(wave_col))
      .liss_nonresponse_abort("complete consumer validation does not accept scalar target or wave overrides")
    return(.liss_nonresponse_validate_consumer_result(data, state))
  }
  if (is.list(state) && (identical(state$schema_version, "3.0.0") ||
                        identical(state$schema_version, "4.0.0"))) {
    if (!is.null(target))
      .liss_nonresponse_abort("source nonresponse collection validation requires the complete carrier without a scalar target")
    return(.liss_nonresponse_validate_collection(data, state, wave_col))
  }
  if (is.list(state) && identical(state$schema_version, "2.0.0"))
    return(.liss_nonresponse_validate_field(data, state, target, wave_col))
  if (!is.list(state) || !identical(state$schema_version, "1.0.0") ||
      !is.character(state$digest) || length(state$digest) != 1L ||
      !identical(state$digest, .liss_nonresponse_digest(state)))
    .liss_nonresponse_abort("source nonresponse binding blocked: missing or tampered carrier digest")
  if (!is.list(state$rule))
    .liss_nonresponse_abort("source nonresponse binding blocked: carrier rule is missing or malformed")
  contract_errors <- validate_source_nonresponse_contracts(
    list(meta = list(module = state$rule$module), source_nonresponse =
           list(schema_version = state$schema_version, contracts = list(state$rule))))
  if (length(contract_errors) || !identical(state$source$sha256, state$rule$source_sha256) ||
      !identical(state$source$filename, state$rule$source_filename))
    .liss_nonresponse_abort("source nonresponse binding blocked: carrier source/contract is unreviewed")
  if (!identical(target, state$target) || !identical(wave_col, state$wave_col))
    .liss_nonresponse_abort("source nonresponse binding blocked: target/wave mapping differs from the sealed carrier")
  .liss_nonresponse_target(target, state$rule)
  .liss_nonresponse_column(data, target, "target")
  .liss_nonresponse_column(data, state$id_col, "id_col")
  if (!identical(attributes(data[[target]]), state$target_metadata))
    .liss_nonresponse_abort("source nonresponse binding blocked: target metadata changed after binding")
  input <- .liss_nonresponse_input_rows(data, target, state$id_col,
                                        wave_col, state$rule$wave)
  if (!is.data.frame(state$input_rows) ||
      anyDuplicated(state$input_rows$row_key))
    .liss_nonresponse_abort("source nonresponse binding blocked: complete input receipt is missing or duplicated")
  input_pos <- match(input$row_key, state$input_rows$row_key)
  if (anyNA(input_pos))
    .liss_nonresponse_abort("source nonresponse binding blocked: complete input row/wave mapping changed after binding")
  input_expected <- state$input_rows$value[input_pos]
  input_equal <- .liss_nonresponse_equal(input$value, input_expected)
  if (isTRUE(state$normalized)) {
    source_pos <- match(input$nomem_encr, state$coverage$nomem_encr)
    source_value <- state$coverage$original_value[source_pos]
    protected <- input$wave_id == state$rule$wave & !is.na(source_pos) &
      is.finite(source_value) & source_value == state$rule$code
    input_equal <- input_equal | (protected &
                                    (is.na(input$value) | input$value == state$rule$code))
  }
  if (!all(input_equal))
    .liss_nonresponse_abort("source nonresponse binding blocked: complete input values changed after binding")
  selected <- .liss_nonresponse_rows(data, wave_col, state$rule$wave)
  ids <- .liss_nonresponse_ids(data[[state$id_col]][selected])
  if (anyDuplicated(ids) || anyDuplicated(state$coverage$nomem_encr))
    .liss_nonresponse_abort("source nonresponse binding blocked: respondent mapping is duplicated")
  pos <- match(ids, state$coverage$nomem_encr)
  if (anyNA(pos))
    .liss_nonresponse_abort("source nonresponse binding blocked: respondent mapping is absent from carrier")
  expected <- state$coverage$original_value[pos]
  matched <- is.finite(expected) & expected == state$rule$code
  values <- .liss_nonresponse_values(data[[target]][selected])
  equal <- .liss_nonresponse_equal(values, expected)
  if (isTRUE(state$normalized)) equal <- equal | (matched & is.na(values))
  if (!all(equal))
    .liss_nonresponse_abort("source nonresponse binding blocked: selected values changed after binding")
  lock <- rep(FALSE, nrow(data))
  lock[which(selected)[matched]] <- TRUE
  observed <- .liss_nonresponse_values(data[[target]])
  observed[which(selected)[matched]] <- expected[matched]
  state$coverage <- state$coverage[pos, , drop = FALSE]
  state$ledger <- state$ledger[state$ledger$nomem_encr %in% ids, , drop = FALSE]
  state$input_rows <- state$input_rows[input_pos, , drop = FALSE]
  state$readiness <- .liss_nonresponse_readiness(length(ids), sum(matched), state$rule)
  state <- .liss_nonresponse_seal(state)
  list(lock = lock, state = state, observed = observed,
       readiness = state$readiness)
}

#' @noRd
source_nonresponse_finalize <- function(data, state, target,
                                        wave_col = NULL, normalized = FALSE,
                                        expected_target_metadata = state$target_metadata) {
  if (is.list(state) && identical(state$schema_version, "2.0.0"))
    return(.liss_nonresponse_finalize_field(data, state, target, wave_col,
                                           normalized, expected_target_metadata))
  if (!is.list(state) || !identical(state$schema_version, "1.0.0") || !identical(state$digest, .liss_nonresponse_digest(state)))
    .liss_nonresponse_abort("source nonresponse binding blocked: cannot finalize a tampered carrier")
  state$target <- target
  state$wave_col <- wave_col
  state$normalized <- isTRUE(normalized)
  .liss_nonresponse_column(data, target, "target")
  state$target_metadata <- attributes(data[[target]])
  state$ledger$current_column <- rep(target, nrow(state$ledger))
  state <- .liss_nonresponse_seal(state)
  selected <- .liss_nonresponse_rows(data, wave_col, state$rule$wave)
  validated <- source_nonresponse_validate(data[selected, , drop = FALSE],
                                            state, target, wave_col)
  if (isTRUE(normalized) &&
      any(is.finite(.liss_nonresponse_values(data[[target]][selected])[validated$lock])))
    .liss_nonresponse_abort("source nonresponse binding blocked: normalized output reintroduced a source nonresponse amount")
  validated$state$input_rows <- .liss_nonresponse_input_rows(
    data, target, state$id_col, wave_col, state$rule$wave)
  .liss_nonresponse_seal(validated$state)
}

#' @noRd
.liss_nonresponse_is_field <- function(rule) {
  is.list(rule) && !is.null(rule$field_id)
}

#' @noRd
.liss_nonresponse_state_rule <- function(state) {
  if (!is.list(state))
    .liss_nonresponse_abort("source nonresponse binding blocked: carrier is missing or malformed")
  if (identical(state$schema_version, "1.0.0") && is.list(state$rule))
    return(state$rule)
  if (identical(state$schema_version, "2.0.0") && is.list(state$field))
    return(state$field)
  .liss_nonresponse_abort("source nonresponse binding blocked: carrier version or field is unsupported")
}

#' @noRd
.liss_nonresponse_cases <- function(values, rule) {
  cases <- if (.liss_nonresponse_is_field(rule)) rule$cases else list(rule)
  codes <- vapply(cases, function(case) case$code, numeric(1))
  index <- match(values, codes)
  index[!is.finite(values)] <- NA_integer_
  index
}

#' @noRd
.liss_nonresponse_match <- function(values, rule) {
  !is.na(.liss_nonresponse_cases(values, rule))
}

#' @noRd
.liss_nonresponse_case_counts <- function(values, field) {
  index <- .liss_nonresponse_cases(values, field)
  stats::setNames(tabulate(index, nbins = length(field$cases)),
                  vapply(field$cases, function(case) case$rule_id, character(1)))
}

#' @noRd
.liss_nonresponse_field_equal <- function(value, exact) {
  if (is.list(exact)) {
    if (!is.list(value) || length(value) != length(exact)) return(FALSE)
    if (is.null(names(exact))) {
      if (!is.null(attributes(value))) return(FALSE)
      return(all(vapply(seq_along(exact), function(i)
        .liss_nonresponse_field_equal(value[[i]], exact[[i]]), logical(1))))
    }
    if (!identical(names(attributes(value)), "names") ||
        !identical(sort(names(value)), sort(names(exact)))) return(FALSE)
    return(all(vapply(names(exact), function(key)
      .liss_nonresponse_field_equal(value[[key]], exact[[key]]), logical(1))))
  }
  if (is.numeric(exact))
    return(is.numeric(value) && is.null(attributes(value)) &&
             length(value) == length(exact) && !anyNA(value) &&
             all(is.finite(value)) && all(value == exact))
  identical(value, exact)
}

#' @noRd
.liss_nonresponse_fields_validate <- function(fields, invalid) {
  keys <- function(x, required)
    is.list(x) && identical(names(attributes(x)), "names") &&
      identical(sort(names(x)), sort(required))
  text <- function(x)
    is.character(x) && is.null(attributes(x)) && length(x) == 1L &&
      !is.na(x) && nzchar(x)
  text_keys <- c("field_id", "field_version", "module", "wave", "language",
                 "source_filename", "source_sha256", "source_release",
                 "raw_column", "canonical_column", "construct", "unit",
                 "metadata_policy", "codebook_filename", "codebook_sha256",
                 "evidence_ref")
  case_text <- c("rule_id", "rule_version", "response_validity", "answer_status",
                 "analysis_disposition")
  if (!is.list(fields) || !is.null(attributes(fields)) || length(fields) != 2L)
    invalid()
  ids <- character()
  for (i in seq_along(fields)) {
    field <- fields[[i]]
    number <- c("337", "338")[[i]]
    construct <- c("respondent_taxable_income", "respondent_estimated_taxable_income")[[i]]
    if (!keys(field, c(text_keys, "measurement_period", "targets", "cases",
                       "routing", "readiness", "consumers")) ||
        !all(vapply(field[text_keys], text, logical(1))) ||
        !identical(field$field_id, paste0("CI23P", number, "_FIELD")) ||
        !identical(field$field_version, "1.0.0") ||
        !identical(field$module, "ci") || !identical(field$wave, "ci23p") ||
        !identical(field$language, "EN") ||
        !identical(field$source_filename, "ci23p_EN_1.0p.sav") ||
        !identical(field$source_sha256, "2e4578cafb649cf4bb3e89d7bc27ea50f8ea1f42038e2ba957a055eff4871862") ||
        !identical(field$source_release, "1.0p") ||
        !identical(field$raw_column, paste0("ci23p", number)) ||
        !identical(field$canonical_column, paste0("s", number)) ||
        !identical(field$construct, construct) || !identical(field$unit, "EUR") ||
        !is.numeric(field$measurement_period) ||
        !is.null(attributes(field$measurement_period)) ||
        length(field$measurement_period) != 1L ||
        is.na(field$measurement_period) || field$measurement_period != 2022 ||
        !identical(field$targets, c(paste0("ci23p", number), paste0("s", number),
                                    paste0("ci_s", number))) ||
        !identical(field$metadata_policy, "retain_original_attributes") ||
        !identical(field$codebook_filename, "codebook_ci23p_EN_1.1.pdf") ||
        !identical(field$codebook_sha256, "408db5ddadf3e4ab4d3af269dc77a1e6edaf04935b9c2cc833d99904c2bb331f") ||
        !identical(field$evidence_ref, paste0("codebook_ci23p_EN_1.1.pdf:", number)) ||
        !is.list(field$cases) || !is.null(attributes(field$cases)) ||
        length(field$cases) != 2L || !is.null(field$routing) ||
        !keys(field$readiness, c("role", "scope")) ||
        !all(vapply(field$readiness, text, logical(1))) ||
        !identical(field$readiness$role, paste0(construct, "_amount")) ||
        !identical(field$readiness$scope,
                   paste0("ci23p", number, " exact reviewed encoding cases in the exact reviewed source")) ||
        !identical(field$consumers, list())) invalid()
    for (j in seq_along(field$cases)) {
      case <- field$cases[[j]]
      if (!keys(case, c(case_text, "code", "no_numeric_proposal")) ||
          !all(vapply(case[case_text], text, logical(1))) ||
          !identical(case$rule_id, paste0("CI23P", number, "_N0", j)) ||
          !identical(case$rule_version, "1.0.0") ||
          !is.double(case$code) || !is.null(attributes(case$code)) ||
          length(case$code) != 1L || !is.finite(case$code) ||
          case$code != c(-9999999999, -9999999998)[[j]] ||
          !identical(case$response_validity, "source_nonresponse_encoding") ||
          !identical(case$answer_status, "nonresponse_unknown") ||
          !identical(case$analysis_disposition, "missing_amount") ||
          !identical(case$no_numeric_proposal, TRUE)) invalid()
      ids <- c(ids, case$rule_id)
    }
  }
  if (anyDuplicated(ids)) invalid()
  invisible(TRUE)
}

#' @noRd
.liss_nonresponse_plan <- function(recipe = NULL) {
  if (is.null(recipe)) recipe <- system.file("recipes", "ci_merge_recipe.yml",
                                            package = "lissr", mustWork = TRUE)
  if (is.character(recipe) && length(recipe) == 1L)
    recipe <- yaml::yaml.load_file(recipe)
  errors <- validate_source_nonresponse_contracts(recipe, "source binding")
  if (length(errors)) .liss_nonresponse_abort(c("invalid source nonresponse contract", errors))
  block <- recipe$source_nonresponse
  if (is.null(block))
    .liss_nonresponse_abort("recipe has no reviewed source nonresponse contract")
  if (identical(block$schema_version, "4.0.0")) {
    rules <- .liss_nonresponse_collection_selection(block)
    return(list(schema_version = block$schema_version, collection = TRUE,
      rules = rules, wave = rules[[1L]]$wave, selections = block$fields))
  }
  collection <- identical(block$schema_version, "3.0.0")
  rules <- if (collection) stats::setNames(block$fields,
    vapply(block$fields, function(field) field$field_id, character(1)))
  else list(.liss_nonresponse_rule(recipe))
  list(schema_version = block$schema_version, collection = collection,
       rules = rules, wave = rules[[1L]]$wave)
}

#' @noRd
.liss_nonresponse_plan_readiness <- function(plan) {
  readiness <- lapply(plan$rules, function(rule)
    .liss_nonresponse_readiness(0L, 0L, rule))
  if (isTRUE(plan$collection))
    .liss_nonresponse_collection_readiness(readiness, plan$schema_version)
  else readiness[[1L]]
}

#' @noRd
.liss_nonresponse_claims_collection <- function(data) {
  if (.liss_nonresponse_claims_consumer(data)) return(TRUE)
  wrapper <- is.list(data) && !is.data.frame(data) && !is.null(data$data)
  frame <- if (wrapper) data$data else data
  collection_class <- function(value)
    inherits(value, "liss_source_nonresponse_collection_result")
  claim <- function(value)
    is.list(value) && (identical(value$schema_version, "3.0.0") ||
      identical(value$schema_version, "4.0.0") ||
      identical(value$kind, "source_nonresponse_collection") ||
      !is.null(value$fields))
  values <- list(attr(data, "liss_source_nonresponse", exact = TRUE),
                 attr(frame, "liss_source_nonresponse", exact = TRUE),
                 attr(data, "source_nonresponse_readiness", exact = TRUE),
                 attr(frame, "source_nonresponse_readiness", exact = TRUE),
                 attr(data, "liss_source_nonresponse_readiness", exact = TRUE),
                 attr(frame, "liss_source_nonresponse_readiness", exact = TRUE))
  if (wrapper) values <- c(values, list(data[["source_nonresponse"]],
                                        data[["source_nonresponse_readiness"]]))
  collection_class(data) || collection_class(frame) ||
    any(vapply(values, claim, logical(1)))
}

#' @noRd
.liss_nonresponse_preflight <- function(data, source, plan,
                                        id_col = "nomem_encr") {
  if (!isTRUE(plan$collection))
    .liss_nonresponse_abort("joint source nonresponse preflight requires a collection plan")
  if (identical(plan$schema_version, "4.0.0"))
    return(.liss_nonresponse_preflight_mixed(data, source, plan, id_col))
  errors <- validate_source_nonresponse_contracts(list(
    meta = list(module = "ci"), source_nonresponse = list(
      schema_version = "3.0.0", fields = unname(plan$rules))))
  if (length(errors)) .liss_nonresponse_abort(errors)
  if (!identical(names(plan$rules),
                 unname(vapply(plan$rules, function(field) field$field_id, character(1)))))
    .liss_nonresponse_abort("source nonresponse binding blocked: collection plan field identities disagree")
  .liss_nonresponse_assert_rebind(data, plan$rules[[1L]])
  if (!is.data.frame(data))
    .liss_nonresponse_abort("joint source nonresponse preflight requires the original source data frame")
  .liss_nonresponse_column(data, id_col, "id_col")
  for (field in plan$rules)
    .liss_nonresponse_column(data, field$raw_column, "raw_column")
  # every receipt is produced and checked before the caller changes either field
  states <- lapply(plan$rules, function(field) {
    classified <- .liss_nonresponse_classify_field(
      data, source, field$raw_column, NULL, id_col, field)
    state <- classified$source_nonresponse
    if (!identical(state$target_metadata, state$metadata$attributes) ||
        !identical(attributes(data[[id_col]]), state$metadata$id_attributes))
      .liss_nonresponse_abort("source nonresponse binding blocked: original source metadata differs from the reviewed read")
    source_nonresponse_validate(data, state, field$raw_column)$state
  })
  reference <- states[[1L]]
  for (state in states) {
    if (!identical(state$source, reference$source) ||
        !identical(state$id_col, reference$id_col) ||
        !identical(state$coverage[c("nomem_encr", "source_row_index")],
                   reference$coverage[c("nomem_encr", "source_row_index")]) ||
        !identical(state$input_rows[c("nomem_encr", "wave_id", "row_key")],
                   reference$input_rows[c("nomem_encr", "wave_id", "row_key")]) ||
        !identical(state$metadata$id_attributes, reference$metadata$id_attributes))
      .liss_nonresponse_abort("source nonresponse binding blocked: selected field source or row/key receipts disagree")
  }
  states
}

#' @noRd
.liss_nonresponse_collection_readiness <- function(fields, schema_version = "3.0.0") {
  if (identical(schema_version, "4.0.0"))
    return(.liss_nonresponse_mixed_readiness(fields))
  ids <- names(fields)
  if (!is.list(fields) || !identical(names(attributes(fields)), "names") ||
      length(fields) != 2L || anyDuplicated(ids) ||
      !identical(sort(ids), c("CI23P337_FIELD", "CI23P338_FIELD")))
    .liss_nonresponse_abort("source nonresponse binding blocked: collection readiness requires both named fields")
  for (id in ids) {
    readiness <- fields[[id]]
    count <- function(value)
      is.numeric(value) && length(value) == 1L && !is.na(value) &&
        is.finite(value) && value >= 0 && value == trunc(value)
    required <- c("status", "field_id", "field_version", "evaluated_cells",
                   "classified_cells", "unresolved_cells", "case_counts", "role", "scope")
    if (!is.list(readiness) ||
        !identical(names(attributes(readiness)), "names") ||
        !identical(sort(names(readiness)), sort(required)) ||
        !identical(readiness$field_id, id) ||
        !all(vapply(readiness[c("evaluated_cells", "classified_cells",
                                "unresolved_cells")], count, logical(1))) ||
        !is.character(readiness$status) || length(readiness$status) != 1L ||
        is.na(readiness$status) ||
        !(readiness$status %in% c("passed", "not_evaluated")) ||
        readiness$classified_cells > readiness$evaluated_cells ||
        readiness$unresolved_cells != 0)
      .liss_nonresponse_abort("source nonresponse binding blocked: invalid field readiness in collection")
  }
  statuses <- vapply(fields, function(value) value$status, character(1))
  if (length(unique(statuses)) != 1L)
    .liss_nonresponse_abort("source nonresponse binding blocked: required collection fields have inconsistent readiness")
  total <- function(key) sum(vapply(fields, function(value) value[[key]], numeric(1)))
  list(schema_version = "3.0.0", kind = "source_nonresponse_collection",
       status = statuses[[1L]],
       scope = paste0("selected CI23p taxable fields only (", paste(ids, collapse = ", "),
                      "); counts are field-cells and do not certify household income"),
       evaluated_cells = total("evaluated_cells"),
       classified_cells = total("classified_cells"),
       unresolved_cells = total("unresolved_cells"), fields = fields)
}

#' @noRd
.liss_nonresponse_assert_rebind <- function(data, rule) {
  if (.liss_nonresponse_claims_collection(data))
    .liss_nonresponse_abort("source nonresponse binding blocked: existing collection lineage cannot be discarded by rebinding")
  wrapper <- is.list(data) && !is.data.frame(data) && !is.null(data$data)
  frame <- if (wrapper) data$data else data
  states <- list(attr(data, "liss_source_nonresponse", exact = TRUE),
                 attr(frame, "liss_source_nonresponse", exact = TRUE))
  readiness <- list(attr(data, "source_nonresponse_readiness", exact = TRUE),
                    attr(frame, "source_nonresponse_readiness", exact = TRUE),
                    attr(data, "liss_source_nonresponse_readiness", exact = TRUE),
                    attr(frame, "liss_source_nonresponse_readiness", exact = TRUE))
  if (wrapper) {
    states <- c(states, list(data[["source_nonresponse"]]))
    readiness <- c(readiness, list(data[["source_nonresponse_readiness"]]))
  }
  guarded_class <- any(grepl("source_nonresponse|guarded", class(data))) ||
    any(grepl("source_nonresponse|guarded", class(frame)))
  if (.liss_nonresponse_is_field(rule) &&
      (any(!vapply(states, is.null, logical(1))) ||
       any(!vapply(readiness, is.null, logical(1))) || guarded_class))
    .liss_nonresponse_abort("source nonresponse binding blocked: new taxable bindings require unguarded input; existing guarded lineage cannot be replaced")
  tax_state <- any(vapply(states, function(state)
    !is.null(state) && (!is.list(state) ||
      !identical(state$schema_version, "1.0.0") || !is.null(state$field)), logical(1)))
  tax_readiness <- any(vapply(readiness, function(value)
    is.list(value) && (!is.null(value$field_id) || !is.null(value$field_version) ||
      !is.null(value$case_counts) ||
      (!is.null(value$schema_version) && !identical(value$schema_version, "1.0.0")) ||
      any(value$role %in% c("respondent_taxable_income_amount",
                            "respondent_estimated_taxable_income_amount"))), logical(1)))
  if (tax_state || tax_readiness || inherits(data, "liss_source_nonresponse_v2_result") ||
      inherits(frame, "liss_source_nonresponse_v2_result"))
    .liss_nonresponse_abort("source nonresponse binding blocked: existing taxable lineage cannot be discarded by rebinding")
  invisible(TRUE)
}

#' @noRd
.liss_nonresponse_declared <- function(values, metadata) {
  declared <- values %in% metadata$na_values
  missing_range <- metadata$na_range
  if (is.numeric(missing_range) && length(missing_range) == 2L &&
      all(is.finite(missing_range)))
    declared <- declared | (values >= missing_range[1L] & values <= missing_range[2L])
  declared
}

#' @noRd
.liss_nonresponse_classify_field <- function(data, source, target, wave_col,
                                            id_col, field) {
  approved <- .liss_nonresponse_approved(field)
  if (is.list(data) && !is.data.frame(data) && !is.null(data$data)) data <- data$data
  if (!is.data.frame(data))
    .liss_nonresponse_abort("`data` must be a data frame or a merge result with $data")
  .liss_nonresponse_column(data, target, "target")
  .liss_nonresponse_target(target, field)
  .liss_nonresponse_column(data, id_col, "id_col")
  if (is.null(wave_col) && !identical(target, field$raw_column))
    .liss_nonresponse_abort("raw source binding requires the exact selected raw target; merged binding requires an explicit `wave_col`")
  if (!is.null(wave_col)) .liss_nonresponse_column(data, wave_col, "wave_col")
  if (!is.character(source) || length(source) != 1L || is.na(source) ||
      !nzchar(source) || !file.exists(source) || dir.exists(source))
    .liss_nonresponse_abort("`source` must be one existing local SAV file")
  receipt <- .liss_nonresponse_source_state(source)
  if (!identical(receipt$filename, field$source_filename) ||
      !identical(receipt$sha256, field$source_sha256))
    .liss_nonresponse_abort("source nonresponse binding blocked: source filename or SHA256 does not match the reviewed CI23p source")
  raw <- .liss_nonresponse_read_source(source)
  if (!identical(receipt, .liss_nonresponse_source_state(source)))
    .liss_nonresponse_abort("source nonresponse binding blocked: source changed while being read")
  .liss_nonresponse_column(raw, field$raw_column, "raw_column")
  .liss_nonresponse_column(raw, "nomem_encr", "source respondent key")
  source_ids <- .liss_nonresponse_ids(raw$nomem_encr)
  if (anyDuplicated(source_ids))
    .liss_nonresponse_abort("source nonresponse binding blocked: source respondent keys are duplicated")
  source_values <- .liss_nonresponse_values(raw[[field$raw_column]])
  selected <- .liss_nonresponse_rows(data, wave_col, field$wave)
  ids <- .liss_nonresponse_ids(data[[id_col]][selected])
  if (anyDuplicated(ids))
    .liss_nonresponse_abort("source nonresponse binding blocked: selected respondent keys are duplicated")
  pos <- match(ids, source_ids)
  if (anyNA(pos))
    .liss_nonresponse_abort("source nonresponse binding blocked: selected respondent keys are absent from the reviewed source")
  values <- .liss_nonresponse_values(data[[target]][selected])
  if (!all(.liss_nonresponse_equal(values, source_values[pos])))
    .liss_nonresponse_abort("source nonresponse binding blocked: selected values differ from the original source")
  coverage <- data.frame(nomem_encr = ids, source_row_index = pos,
                         original_value = source_values[pos], stringsAsFactors = FALSE)
  case_index <- .liss_nonresponse_cases(coverage$original_value, field)
  matched <- !is.na(case_index)
  ledger <- coverage[matched, , drop = FALSE]
  cases <- field$cases[case_index[matched]]
  text_case <- function(key) vapply(cases, function(case) case[[key]], character(1))
  ledger$source_sha256 <- rep(field$source_sha256, nrow(ledger))
  ledger$source_filename <- rep(field$source_filename, nrow(ledger))
  ledger$source_release <- rep(field$source_release, nrow(ledger))
  ledger$source_language <- rep(field$language, nrow(ledger))
  ledger$raw_column <- rep(field$raw_column, nrow(ledger))
  ledger$raw_storage_type <- rep(typeof(raw[[field$raw_column]]), nrow(ledger))
  ledger$raw_value_repr <- sprintf("%.0f", ledger$original_value)
  ledger$current_value_before <- ledger$original_value
  ledger$analysis_value_after <- rep(NA_real_, nrow(ledger))
  ledger$system_missing_before <- rep(FALSE, nrow(ledger))
  original_metadata <- attributes(raw[[field$raw_column]])
  ledger$source_declared_missing <- .liss_nonresponse_declared(ledger$original_value,
                                                             original_metadata)
  ledger$source_metadata_id <- rep(paste(field$source_sha256, field$raw_column,
                                         sep = ":"), nrow(ledger))
  ledger$evidence_ref <- rep(field$evidence_ref, nrow(ledger))
  ledger$wave_id <- rep(field$wave, nrow(ledger))
  ledger$current_column <- rep(target, nrow(ledger))
  ledger$source_key <- if (nrow(ledger))
    paste(field$source_sha256, ledger$source_row_index, field$raw_column, sep = ":")
  else character()
  for (key in c("rule_id", "rule_version", "response_validity", "answer_status",
                 "analysis_disposition")) ledger[[key]] <- text_case(key)
  ledger$no_numeric_proposal <- rep(TRUE, nrow(ledger))
  ledger$measurement_period <- rep(field$measurement_period, nrow(ledger))
  ledger$construct <- rep(field$construct, nrow(ledger))
  ledger$candidate_answer_status <- rep(NA_character_, nrow(ledger))
  ledger$subtype_evidence <- rep("unresolved", nrow(ledger))
  # materialize generated columns and names before R 4.5 serializes growth flags
  ledger <- ledger[seq_along(ledger)]
  state <- list(schema_version = "2.0.0", field = field, source = receipt,
                target = target, wave_col = wave_col, id_col = id_col,
                normalized = FALSE, coverage = coverage, ledger = ledger,
                input_rows = .liss_nonresponse_input_rows(
                  data, target, id_col, wave_col, field$wave),
                metadata = list(raw_column = field$raw_column,
                                attributes = original_metadata,
                                id_attributes = attributes(raw$nomem_encr),
                                codebook_filename = field$codebook_filename,
                                codebook_sha256 = field$codebook_sha256),
                target_metadata = attributes(data[[target]]),
                readiness = .liss_nonresponse_readiness(
                  nrow(coverage), sum(matched), field,
                  .liss_nonresponse_case_counts(coverage$original_value, field)))
  state <- .liss_nonresponse_seal(state)
  structure(list(data = data, source_nonresponse = state,
                 source_nonresponse_readiness = state$readiness),
            class = c("liss_source_nonresponse_v2_result", "liss_source_nonresponse_result"))
}

#' @noRd
.liss_nonresponse_validate_field <- function(data, state, target, wave_col) {
  state_keys <- c("schema_version", "field", "source", "target", "wave_col",
                  "id_col", "normalized", "coverage", "ledger", "input_rows",
                  "metadata", "target_metadata", "readiness", "digest")
  if (!identical(sort(names(state)), sort(state_keys)) ||
      !is.character(state$digest) || length(state$digest) != 1L ||
      !identical(state$digest, .liss_nonresponse_digest(state)))
    .liss_nonresponse_abort("source nonresponse binding blocked: missing or tampered carrier digest")
  field <- .liss_nonresponse_state_rule(state)
  errors <- validate_source_nonresponse_contracts(list(
    meta = list(module = field$module),
    source_nonresponse = list(schema_version = "2.0.0", field = field)))
  if (length(errors) || !identical(state$source$sha256, field$source_sha256) ||
      !identical(state$source$filename, field$source_filename) ||
      !identical(state$metadata$raw_column, field$raw_column) ||
      !identical(state$metadata$codebook_filename, field$codebook_filename) ||
      !identical(state$metadata$codebook_sha256, field$codebook_sha256) ||
      !is.logical(state$normalized) || length(state$normalized) != 1L ||
      is.na(state$normalized))
    .liss_nonresponse_abort("source nonresponse binding blocked: carrier source/contract is unreviewed")
  if (!identical(target, state$target) || !identical(wave_col, state$wave_col))
    .liss_nonresponse_abort("source nonresponse binding blocked: target/wave mapping differs from the sealed carrier")
  .liss_nonresponse_target(target, field)
  .liss_nonresponse_column(data, target, "target")
  .liss_nonresponse_column(data, state$id_col, "id_col")
  if (!identical(attributes(data[[target]]), state$target_metadata))
    .liss_nonresponse_abort("source nonresponse binding blocked: target metadata changed after binding")
  input <- .liss_nonresponse_input_rows(data, target, state$id_col, wave_col, field$wave)
  if (!is.data.frame(state$input_rows) ||
      !identical(names(state$input_rows), c("nomem_encr", "wave_id", "row_key", "value")) ||
      anyDuplicated(state$input_rows$row_key))
    .liss_nonresponse_abort("source nonresponse binding blocked: complete input receipt is missing or duplicated")
  if (!is.data.frame(state$coverage) ||
      !identical(names(state$coverage), c("nomem_encr", "source_row_index", "original_value")) ||
      anyDuplicated(state$coverage$nomem_encr) ||
      anyDuplicated(state$coverage$source_row_index) ||
      !is.numeric(state$coverage$source_row_index) ||
      anyNA(state$coverage$source_row_index) ||
      any(!is.finite(state$coverage$source_row_index) |
          state$coverage$source_row_index < 1 |
          state$coverage$source_row_index != trunc(state$coverage$source_row_index)))
    .liss_nonresponse_abort("source nonresponse binding blocked: coverage receipt is missing or duplicated")
  .liss_nonresponse_values(state$coverage$original_value)
  input_pos <- match(input$row_key, state$input_rows$row_key)
  if (anyNA(input_pos))
    .liss_nonresponse_abort("source nonresponse binding blocked: complete input row/wave mapping changed after binding")
  expected_input <- state$input_rows$value[input_pos]
  equal_input <- .liss_nonresponse_equal(input$value, expected_input)
  coverage_pos <- match(input$nomem_encr, state$coverage$nomem_encr)
  protected <- input$wave_id == field$wave & !is.na(coverage_pos) &
    .liss_nonresponse_match(state$coverage$original_value[coverage_pos], field)
  if (isTRUE(state$normalized)) {
    equal_input[protected] <- is.na(input$value[protected])
  }
  if (!all(equal_input))
    .liss_nonresponse_abort("source nonresponse binding blocked: complete input values changed after binding")
  selected <- .liss_nonresponse_rows(data, wave_col, field$wave)
  ids <- .liss_nonresponse_ids(data[[state$id_col]][selected])
  if (anyDuplicated(ids))
    .liss_nonresponse_abort("source nonresponse binding blocked: respondent mapping is duplicated")
  pos <- match(ids, state$coverage$nomem_encr)
  if (anyNA(pos))
    .liss_nonresponse_abort("source nonresponse binding blocked: respondent mapping is absent from carrier")
  expected <- state$coverage$original_value[pos]
  matched <- .liss_nonresponse_match(expected, field)
  values <- .liss_nonresponse_values(data[[target]][selected])
  equal <- .liss_nonresponse_equal(values, expected)
  if (isTRUE(state$normalized)) equal[matched] <- is.na(values[matched])
  if (!all(equal))
    .liss_nonresponse_abort("source nonresponse binding blocked: selected values changed after binding")
  if (!is.data.frame(state$ledger) || anyDuplicated(state$ledger$source_key))
    .liss_nonresponse_abort("source nonresponse binding blocked: cell ledger is missing or duplicated")
  lock <- rep(FALSE, nrow(data))
  lock[which(selected)[matched]] <- TRUE
  observed <- .liss_nonresponse_values(data[[target]])
  observed[which(selected)[matched]] <- expected[matched]
  state$coverage <- state$coverage[pos, , drop = FALSE]
  state$ledger <- state$ledger[state$ledger$nomem_encr %in% ids, , drop = FALSE]
  state$input_rows <- state$input_rows[input_pos, , drop = FALSE]
  state$readiness <- .liss_nonresponse_readiness(
    length(ids), sum(matched), field, .liss_nonresponse_case_counts(expected, field))
  state <- .liss_nonresponse_seal(state)
  list(lock = lock, state = state, observed = observed, readiness = state$readiness)
}

#' @noRd
.liss_nonresponse_finalize_field <- function(data, state, target, wave_col,
                                            normalized, expected_target_metadata) {
  if (!identical(state$digest, .liss_nonresponse_digest(state)))
    .liss_nonresponse_abort("source nonresponse binding blocked: cannot finalize a tampered carrier")
  field <- .liss_nonresponse_state_rule(state)
  .liss_nonresponse_approved(field)
  .liss_nonresponse_target(target, field)
  .liss_nonresponse_column(data, target, "target")
  unchanged_mapping <- identical(target, state$target) &&
    identical(wave_col, state$wave_col)
  merge_mapping <- identical(state$target, field$raw_column) &&
    is.null(state$wave_col) && identical(target, field$canonical_column) &&
    is.character(wave_col) && length(wave_col) == 1L && !is.na(wave_col) &&
    nzchar(wave_col)
  if ((!unchanged_mapping && !merge_mapping) ||
      !identical(attributes(data[[target]]), expected_target_metadata))
    .liss_nonresponse_abort("source nonresponse binding blocked: unexpected final target/wave mapping or target metadata change")
  original_input <- state$input_rows
  original_wave_col <- state$wave_col
  state$target <- target
  state["wave_col"] <- list(wave_col)
  state$normalized <- isTRUE(normalized)
  state["target_metadata"] <- list(expected_target_metadata)
  state$ledger$current_column <- rep(target, nrow(state$ledger))
  state <- .liss_nonresponse_seal(state)
  selected <- .liss_nonresponse_rows(data, wave_col, field$wave)
  selected_data <- data[selected, , drop = FALSE]
  # base subsetting can strip plain numeric metadata; retain this trusted slice
  attributes(selected_data[[target]]) <- attributes(data[[target]])
  validated <- source_nonresponse_validate(selected_data, state, target, wave_col)
  final_input <- .liss_nonresponse_input_rows(data, target, state$id_col,
                                              wave_col, field$wave)
  old_pos <- match(final_input$row_key, original_input$row_key)
  unknown <- is.na(old_pos)
  if (any(unknown & (final_input$wave_id == field$wave |
                      !is.null(original_wave_col))))
    .liss_nonresponse_abort("source nonresponse binding blocked: unexpected final input row/wave mapping")
  known <- !unknown
  ordinary <- known & !selected
  if (any(ordinary) &&
      !all(.liss_nonresponse_equal(final_input$value[ordinary],
                                   original_input$value[old_pos[ordinary]])))
    .liss_nonresponse_abort("source nonresponse binding blocked: complete input values changed during finalization")
  validated$state$input_rows <- final_input
  .liss_nonresponse_seal(validated$state)
}

#' @noRd
.liss_nonresponse_finalize_collection <- function(data, states, targets,
                                                  wave_col,
                                                  expected_target_metadata,
                                                  schema_version = "3.0.0") {
  if (identical(schema_version, "4.0.0"))
    return(.liss_nonresponse_finalize_mixed(data, states, targets, wave_col,
      expected_target_metadata))
  ids <- names(states)
  if (!is.list(states) || !identical(names(attributes(states)), "names") ||
      length(states) != 2L || anyDuplicated(ids) ||
      !identical(sort(ids), c("CI23P337_FIELD", "CI23P338_FIELD")) ||
      !identical(names(targets), ids) ||
      !is.list(expected_target_metadata) ||
      !identical(names(expected_target_metadata), ids))
    .liss_nonresponse_abort("source nonresponse binding blocked: finalization requires both named field receipts, targets and metadata")
  fields <- lapply(ids, function(id) {
    state <- states[[id]]
    if (!is.list(state) || !identical(state$schema_version, "2.0.0") ||
        !identical(state$field$field_id, id) || !identical(state$normalized, FALSE) ||
        !identical(targets[[id]], state$field$canonical_column))
      .liss_nonresponse_abort("source nonresponse binding blocked: unexpected collection field or final target")
    final_rows <- .liss_nonresponse_input_rows(data, targets[[id]], state$id_col,
                                               wave_col, state$field$wave)
    selected_keys <- final_rows$row_key[final_rows$wave_id == state$field$wave]
    if (!identical(sort(selected_keys), sort(state$input_rows$row_key)))
      .liss_nonresponse_abort("source nonresponse binding blocked: complete collection source row/wave mapping changed during finalization")
    source_nonresponse_finalize(data, state, targets[[id]], wave_col,
      normalized = TRUE, expected_target_metadata = expected_target_metadata[[id]])
  })
  names(fields) <- ids
  state <- list(schema_version = "3.0.0", kind = "source_nonresponse_collection",
                source = fields[[1L]]$source, fields = fields, normalized = TRUE,
                readiness = .liss_nonresponse_collection_readiness(
                  lapply(fields, function(field) field$readiness)))
  state <- .liss_nonresponse_seal(state)
  .liss_nonresponse_validate_collection(data, state)$state
}

#' @noRd
.liss_nonresponse_collection_receipts <- function(state) {
  if (identical(state$schema_version, "4.0.0"))
    return(.liss_nonresponse_mixed_receipts(state))
  ids <- names(state$fields)
  errors <- validate_source_nonresponse_contracts(list(
    meta = list(module = "ci"), source_nonresponse = list(
      schema_version = "3.0.0", fields = unname(lapply(state$fields,
        function(leaf) leaf$field)))))
  if (length(errors)) .liss_nonresponse_abort(errors)
  leaf_keys <- c("schema_version", "field", "source", "target", "wave_col",
                  "id_col", "normalized", "coverage", "ledger", "input_rows",
                  "metadata", "target_metadata", "readiness", "digest")
  for (leaf in state$fields) {
    if (!identical(names(attributes(leaf)), "names") ||
        !identical(sort(names(leaf)), sort(leaf_keys)) ||
        !identical(leaf$digest, .liss_nonresponse_digest(leaf)) ||
        !is.data.frame(leaf$coverage) ||
        !identical(names(leaf$coverage), c("nomem_encr", "source_row_index", "original_value")) ||
        !is.data.frame(leaf$input_rows) ||
        !identical(names(leaf$input_rows), c("nomem_encr", "wave_id", "row_key", "value")))
      .liss_nonresponse_abort("source nonresponse binding blocked: incomplete or tampered collection leaf receipt")
  }
  source <- state$source
  if (!is.list(source) || !identical(names(attributes(source)), "names") ||
      !identical(sort(names(source)), c("filename", "sha256", "size_bytes")) ||
      !is.numeric(source$size_bytes) || length(source$size_bytes) != 1L ||
      is.na(source$size_bytes) || !is.finite(source$size_bytes) ||
      source$size_bytes <= 0 || source$size_bytes != trunc(source$size_bytes))
    .liss_nonresponse_abort("source nonresponse binding blocked: collection source receipt is incomplete")
  reference <- state$fields[[1L]]
  for (id in ids) {
    leaf <- state$fields[[id]]
    field <- leaf$field
    if (!identical(leaf$schema_version, "2.0.0") ||
        !identical(field$field_id, id) ||
        !identical(leaf$digest, .liss_nonresponse_digest(leaf)) ||
        !identical(leaf$source, source) ||
        !identical(source$filename, field$source_filename) ||
        !identical(source$sha256, field$source_sha256) ||
        !identical(leaf$normalized, TRUE) ||
        !identical(leaf$target, field$canonical_column) ||
        !is.character(leaf$wave_col) || length(leaf$wave_col) != 1L ||
        is.na(leaf$wave_col) || !nzchar(leaf$wave_col) ||
        !is.character(leaf$id_col) || length(leaf$id_col) != 1L ||
        is.na(leaf$id_col) || !nzchar(leaf$id_col) ||
        !identical(leaf$wave_col, reference$wave_col) ||
        !identical(leaf$id_col, reference$id_col) ||
        !is.data.frame(leaf$coverage) || !is.data.frame(leaf$input_rows) ||
        !identical(leaf$coverage[c("nomem_encr", "source_row_index")],
                   reference$coverage[c("nomem_encr", "source_row_index")]) ||
        !identical(leaf$input_rows[c("nomem_encr", "wave_id", "row_key")],
                   reference$input_rows[c("nomem_encr", "wave_id", "row_key")]) ||
        !is.list(leaf$metadata) ||
        !identical(sort(names(leaf$metadata)), sort(c("raw_column", "attributes",
          "id_attributes", "codebook_filename", "codebook_sha256"))) ||
        !identical(leaf$metadata$id_attributes, reference$metadata$id_attributes))
      .liss_nonresponse_abort("source nonresponse binding blocked: collection field/source/binding receipts disagree")
    .liss_nonresponse_collection_ledger(leaf)
    expected <- .liss_nonresponse_readiness(nrow(leaf$coverage),
      sum(.liss_nonresponse_match(leaf$coverage$original_value, field)), field,
      .liss_nonresponse_case_counts(leaf$coverage$original_value, field))
    if (!identical(leaf$readiness, expected))
      .liss_nonresponse_abort("source nonresponse binding blocked: collection leaf readiness differs from its receipt")
  }
  expected <- .liss_nonresponse_collection_readiness(lapply(state$fields,
    function(leaf) leaf$readiness))
  if (!identical(state$readiness, expected))
    .liss_nonresponse_abort("source nonresponse binding blocked: collection readiness differs from its complete fields")
  invisible(TRUE)
}

#' @noRd
.liss_nonresponse_collection_ledger <- function(state) {
  field <- state$field
  coverage <- state$coverage
  ledger <- state$ledger
  columns <- c("nomem_encr", "source_row_index", "original_value", "source_sha256",
    "source_filename", "source_release", "source_language", "raw_column",
    "raw_storage_type", "raw_value_repr", "current_value_before", "analysis_value_after",
    "system_missing_before", "source_declared_missing", "source_metadata_id",
    "evidence_ref", "wave_id", "current_column", "source_key", "rule_id", "rule_version",
    "response_validity", "answer_status", "analysis_disposition", "no_numeric_proposal",
    "measurement_period", "construct", "candidate_answer_status", "subtype_evidence")
  invalid <- function()
    .liss_nonresponse_abort("source nonresponse binding blocked: collection leaf cell ledger differs from its complete receipt")
  if (!is.data.frame(coverage) ||
      !identical(names(coverage), c("nomem_encr", "source_row_index", "original_value")) ||
      !is.data.frame(ledger) || !identical(names(ledger), columns) ||
      anyDuplicated(ledger$source_key)) invalid()
  matched <- .liss_nonresponse_match(coverage$original_value, field)
  expected_keys <- paste(field$source_sha256, coverage$source_row_index[matched],
                         field$raw_column, sep = ":")
  if (!sum(matched)) expected_keys <- character()
  if (nrow(ledger) != sum(matched) || !setequal(ledger$source_key, expected_keys))
    invalid()
  pos <- match(ledger$nomem_encr, coverage$nomem_encr)
  if (anyNA(pos) || !identical(ledger$source_row_index, coverage$source_row_index[pos]) ||
      !all(.liss_nonresponse_equal(ledger$original_value, coverage$original_value[pos])))
    invalid()
  case_index <- .liss_nonresponse_cases(ledger$original_value, field)
  if (anyNA(case_index)) invalid()
  cases <- field$cases[case_index]
  repeated <- list(source_sha256 = field$source_sha256,
    source_filename = field$source_filename, source_release = field$source_release,
    source_language = field$language, raw_column = field$raw_column,
    source_metadata_id = paste(field$source_sha256, field$raw_column, sep = ":"),
    evidence_ref = field$evidence_ref, wave_id = field$wave,
    current_column = state$target, no_numeric_proposal = TRUE,
    measurement_period = field$measurement_period, construct = field$construct,
    subtype_evidence = "unresolved")
  for (key in names(repeated))
    if (!identical(ledger[[key]], rep(repeated[[key]], nrow(ledger)))) invalid()
  for (key in c("rule_id", "rule_version", "response_validity", "answer_status",
                 "analysis_disposition"))
    if (!identical(ledger[[key]], vapply(cases, function(case) case[[key]], character(1))))
      invalid()
  if (!all(ledger$raw_storage_type %in% c("double", "integer")) ||
      length(unique(ledger$raw_storage_type)) > 1L ||
      !identical(ledger$raw_value_repr, sprintf("%.0f", ledger$original_value)) ||
      !identical(ledger$current_value_before, ledger$original_value) ||
      !identical(ledger$analysis_value_after, rep(NA_real_, nrow(ledger))) ||
      !identical(ledger$system_missing_before, rep(FALSE, nrow(ledger))) ||
      !identical(ledger$source_declared_missing,
                 .liss_nonresponse_declared(ledger$original_value, state$metadata$attributes)) ||
      !identical(ledger$candidate_answer_status, rep(NA_character_, nrow(ledger))) ||
      !identical(ledger$source_key,
        if (nrow(ledger)) paste(field$source_sha256, ledger$source_row_index,
                                field$raw_column, sep = ":") else character())) invalid()
  invisible(TRUE)
}

#' @noRd
.liss_nonresponse_validate_collection <- function(data, state, wave_col = NULL) {
  if (is.list(state) && identical(state$schema_version, "4.0.0"))
    return(.liss_nonresponse_validate_mixed(data, state, wave_col))
  keys <- c("schema_version", "kind", "source", "fields", "normalized", "readiness", "digest")
  if (!is.list(state) || !identical(names(attributes(state)), "names") ||
      !identical(sort(names(state)), sort(keys)) ||
      !identical(state$schema_version, "3.0.0") ||
      !identical(state$kind, "source_nonresponse_collection") ||
      !identical(state$normalized, TRUE) ||
      !is.character(state$digest) || length(state$digest) != 1L ||
      !identical(state$digest, .liss_nonresponse_digest(state)) ||
      !is.list(state$fields) ||
      !identical(names(attributes(state$fields)), "names") ||
      length(state$fields) != 2L || anyDuplicated(names(state$fields)) ||
      !identical(sort(names(state$fields)), c("CI23P337_FIELD", "CI23P338_FIELD")) ||
      !all(vapply(state$fields, is.list, logical(1))))
    .liss_nonresponse_abort("source nonresponse binding blocked: missing, unsupported or tampered collection carrier")
  wrapper <- is.list(data) && !is.data.frame(data) && !is.null(data$data)
  if (wrapper) {
    if (!identical(data[["source_nonresponse"]], state) ||
        !identical(data[["source_nonresponse_readiness"]], state$readiness))
      .liss_nonresponse_abort("source nonresponse binding blocked: collection wrapper and supplied carrier disagree")
    wrapper_marker <- attr(data, "liss_source_nonresponse", exact = TRUE)
    if (!is.null(wrapper_marker) && !identical(wrapper_marker, state))
      .liss_nonresponse_abort("source nonresponse binding blocked: collection wrapper marker contradicts its carrier")
    for (key in c("source_nonresponse_readiness", "liss_source_nonresponse_readiness")) {
      readiness <- attr(data, key, exact = TRUE)
      if (!is.null(readiness) && !identical(readiness, state$readiness))
        .liss_nonresponse_abort("source nonresponse binding blocked: collection wrapper readiness contradicts its carrier")
    }
    data <- data$data
  }
  if (!is.data.frame(data))
    .liss_nonresponse_abort("source nonresponse collection validation requires a data frame")
  marker <- attr(data, "liss_source_nonresponse", exact = TRUE)
  if ((!is.null(marker) && !identical(marker, state)) ||
      (inherits(data, "liss_source_nonresponse_collection_result") && is.null(marker)))
    .liss_nonresponse_abort("source nonresponse binding blocked: collection frame and supplied carrier disagree")
  for (key in c("source_nonresponse_readiness", "liss_source_nonresponse_readiness")) {
    readiness <- attr(data, key, exact = TRUE)
    if (!is.null(readiness) && !identical(readiness, state$readiness))
      .liss_nonresponse_abort("source nonresponse binding blocked: collection frame readiness contradicts its carrier")
  }
  .liss_nonresponse_collection_receipts(state)
  if (!is.null(wave_col) &&
      !identical(wave_col, state$fields[[1L]]$wave_col))
    .liss_nonresponse_abort("source nonresponse binding blocked: collection wave mapping differs from the sealed carrier")
  # each complete leaf sees the same current frame before any returned resealing
  fields <- lapply(state$fields, function(leaf)
    .liss_nonresponse_validate_field(data, leaf, leaf$target, leaf$wave_col))
  state$fields <- lapply(fields, function(field) field$state)
  state$readiness <- .liss_nonresponse_collection_readiness(
    lapply(fields, function(field) field$readiness))
  state <- .liss_nonresponse_seal(state)
  .liss_nonresponse_collection_receipts(state)
  list(state = state, readiness = state$readiness, fields = fields)
}

#' @noRd
.liss_nonresponse_collection_adapter_path <- function() {
  system.file("schema", "source_nonresponse_collection_adapter.yml", package = "lissr")
}

#' @noRd
.liss_nonresponse_collection_adapter <- function() {
  path <- .liss_nonresponse_collection_adapter_path()
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path) || !file.exists(path) || dir.exists(path))
    .liss_nonresponse_abort("reviewed source nonresponse collection adapter is missing")
  bytes <- tryCatch(readBin(path, "raw", n = file.info(path)$size),
                    error = function(e) NULL)
  hash <- if (!is.null(bytes)) as.character(openssl::sha256(bytes)) else ""
  attributes(hash) <- NULL
  if (!identical(hash, "5e3235f1b4f78a589622ce27064eecf120c919e81fd95b84fe4a370126ac6c3f"))
    .liss_nonresponse_abort("reviewed source nonresponse collection adapter integrity check failed")
  adapter <- tryCatch(yaml::yaml.load(rawToChar(bytes), eval.expr = FALSE),
                      error = function(e) NULL)
  .liss_nonresponse_collection_adapter_validate(adapter, .liss_nonresponse_registry())
  adapter
}

#' @noRd
.liss_nonresponse_collection_adapter_validate <- function(adapter, registry) {
  invalid <- function()
    .liss_nonresponse_abort("reviewed source nonresponse collection adapter schema or authority is unsupported or ambiguous")
  keys <- function(value, expected)
    is.list(value) && identical(names(attributes(value)), "names") &&
      identical(sort(names(value)), sort(expected))
  required <- c("schema_version", "adapter_id", "adapter_version",
    "recipe_schema_version", "carrier_schema_version", "kind", "module",
    "authority_registry", "members", "consumers")
  fixed <- list(schema_version = "1.0.0",
    adapter_id = "CI23P_337_338_339_COLLECTION", adapter_version = "1.0.0",
    recipe_schema_version = "4.0.0", carrier_schema_version = "4.0.0",
    kind = "source_nonresponse_collection", module = "ci")
  if (!keys(adapter, required) ||
      !all(vapply(names(fixed), function(key) identical(adapter[[key]], fixed[[key]]), logical(1))) ||
      !keys(adapter$authority_registry, c("schema_version", "registry_version", "sha256")) ||
      !identical(adapter$authority_registry$schema_version, "2.0.0") ||
      !identical(adapter$authority_registry$registry_version, "2.0.0") ||
      !identical(adapter$authority_registry$sha256,
        "a2e6694acc5855d2df389df46d66e0c94b705637626309cc309ceee8d840ddc3") ||
      !identical(adapter$consumers, list()) || !is.list(adapter$members) ||
      !is.null(attributes(adapter$members)) || length(adapter$members) != 3L)
    invalid()
  .liss_nonresponse_registry_validate(registry)
  if (!identical(registry$schema_version, adapter$authority_registry$schema_version) ||
      !identical(registry$registry_version, adapter$authority_registry$registry_version))
    invalid()
  ids <- c("CI23P337_FIELD", "CI23P338_FIELD", "CI23P339_FIELD")
  for (index in seq_along(ids)) {
    member <- adapter$members[[index]]
    household <- index == 3L
    if (!keys(member, c("field_id", "scalar_schema_version", "authority_kind",
                        "authority_id", "authority_version")) ||
        !identical(member$field_id, ids[[index]]) ||
        !identical(member$scalar_schema_version, if (household) "1.0.0" else "2.0.0") ||
        !identical(member$authority_kind, if (household) "contract" else "field") ||
        !identical(member$authority_id, if (household) "CI23P339_N01" else ids[[index]]) ||
        !identical(member$authority_version, "1.0.0")) invalid()
    records <- if (household) lapply(registry$records, function(record) record$contract)
               else registry$fields
    selected <- vapply(records, function(rule)
      identical(if (household) rule$rule_id else rule$field_id, member$authority_id) &&
      identical(if (household) rule$rule_version else rule$field_version,
                member$authority_version), logical(1))
    if (sum(selected) != 1L) invalid()
  }
  invisible(TRUE)
}

#' @noRd
.liss_nonresponse_collection_selection <- function(block) {
  invalid <- function()
    .liss_nonresponse_abort("source nonresponse schema 4 requires exactly the three complete reviewed scalar selectors")
  keys <- function(value, expected)
    is.list(value) && identical(names(attributes(value)), "names") &&
      identical(sort(names(value)), sort(expected))
  if (!keys(block, c("schema_version", "fields")) ||
      !identical(block$schema_version, "4.0.0") ||
      !is.list(block$fields) || !is.null(attributes(block$fields)) ||
      length(block$fields) != 3L) invalid()
  adapter <- .liss_nonresponse_collection_adapter()
  member_ids <- vapply(adapter$members, function(member) member$field_id, character(1))
  ids <- vapply(block$fields, function(selection) {
    if (!keys(selection, c("field_id", "scalar")) ||
        !is.character(selection$field_id) || length(selection$field_id) != 1L ||
        !is.null(attributes(selection$field_id)) || is.na(selection$field_id)) invalid()
    selection$field_id
  }, character(1))
  if (anyDuplicated(ids) || !identical(sort(ids), sort(member_ids))) invalid()
  rules <- lapply(seq_along(ids), function(index) {
    member <- adapter$members[[match(ids[[index]], member_ids)]]
    scalar <- block$fields[[index]]$scalar
    household <- identical(member$scalar_schema_version, "1.0.0")
    if (!keys(scalar, if (household) c("schema_version", "contracts")
                                 else c("schema_version", "field")) ||
        !identical(scalar$schema_version, member$scalar_schema_version)) invalid()
    if (household) {
      if (!is.list(scalar$contracts) || !is.null(attributes(scalar$contracts)) ||
          length(scalar$contracts) != 1L) invalid()
      rule <- scalar$contracts[[1L]]
    } else rule <- scalar$field
    approved <- .liss_nonresponse_approved(rule)
    authority <- if (household) approved$contract else approved
    if (!identical(if (household) authority$rule_id else authority$field_id,
                   member$authority_id) ||
        !identical(if (household) authority$rule_version else authority$field_version,
                   member$authority_version) ||
        !identical(.liss_nonresponse_is_field(rule), !household)) invalid()
    rule
  })
  stats::setNames(rules, ids)
}

#' @noRd
.liss_nonresponse_mixed_scalar <- function(rule) {
  if (.liss_nonresponse_is_field(rule))
    list(schema_version = "2.0.0", field = rule)
  else list(schema_version = "1.0.0", contracts = list(rule))
}

#' @noRd
.liss_nonresponse_mixed_rules <- function(states) {
  if (!is.list(states) || !identical(names(attributes(states)), "names"))
    .liss_nonresponse_abort("source nonresponse binding blocked: mixed collection members must be named")
  block <- list(schema_version = "4.0.0", fields = unname(lapply(names(states), function(id)
    list(field_id = id, scalar = .liss_nonresponse_mixed_scalar(
      .liss_nonresponse_state_rule(states[[id]]))))))
  rules <- .liss_nonresponse_collection_selection(block)
  for (id in names(states)) {
    schema <- if (.liss_nonresponse_is_field(rules[[id]])) "2.0.0" else "1.0.0"
    if (!identical(states[[id]]$schema_version, schema))
      .liss_nonresponse_abort("source nonresponse binding blocked: mixed collection scalar schema disagrees with its adapter member")
  }
  rules
}

#' @noRd
.liss_nonresponse_preflight_mixed <- function(data, source, plan, id_col) {
  rules <- .liss_nonresponse_collection_selection(list(
    schema_version = "4.0.0", fields = plan$selections))
  if (!identical(rules, plan$rules) || !identical(plan$wave, rules[[1L]]$wave))
    .liss_nonresponse_abort("source nonresponse binding blocked: mixed collection plan identities disagree")
  # taxable rebinding rules deny every claimed lineage, even in household-first order
  taxable <- which(vapply(rules, .liss_nonresponse_is_field, logical(1)))[[1L]]
  .liss_nonresponse_assert_rebind(data, rules[[taxable]])
  if (!is.data.frame(data))
    .liss_nonresponse_abort("joint source nonresponse preflight requires the original source data frame")
  .liss_nonresponse_column(data, id_col, "id_col")
  for (rule in rules) .liss_nonresponse_column(data, rule$raw_column, "raw_column")
  states <- lapply(rules, function(rule) {
    classified <- if (.liss_nonresponse_is_field(rule))
      .liss_nonresponse_classify_field(data, source, rule$raw_column, NULL, id_col, rule)
    else liss_classify_source_nonresponse(data, source, rule$raw_column,
      id_col = id_col, recipe = list(meta = list(module = "ci"),
        source_nonresponse = .liss_nonresponse_mixed_scalar(rule)))
    state <- classified$source_nonresponse
    if (!identical(state$target_metadata, state$metadata$attributes) ||
        !identical(attributes(data[[id_col]]), state$metadata$id_attributes))
      .liss_nonresponse_abort("source nonresponse binding blocked: original source metadata differs from the reviewed read")
    source_nonresponse_validate(data, state, rule$raw_column)$state
  })
  reference <- states[[1L]]
  for (state in states)
    if (!identical(state$source, reference$source) ||
        !identical(state$id_col, reference$id_col) ||
        !identical(state$coverage[c("nomem_encr", "source_row_index")],
                   reference$coverage[c("nomem_encr", "source_row_index")]) ||
        !identical(state$input_rows[c("nomem_encr", "wave_id", "row_key")],
                   reference$input_rows[c("nomem_encr", "wave_id", "row_key")]) ||
        !identical(state$metadata$id_attributes, reference$metadata$id_attributes))
      .liss_nonresponse_abort("source nonresponse binding blocked: selected field source or row/key receipts disagree")
  states
}

#' @noRd
.liss_nonresponse_mixed_readiness <- function(fields) {
  adapter <- .liss_nonresponse_collection_adapter()
  ids <- names(fields)
  expected_ids <- vapply(adapter$members, function(member) member$field_id, character(1))
  if (!is.list(fields) || !identical(names(attributes(fields)), "names") ||
      anyDuplicated(ids) || !identical(sort(ids), sort(expected_ids)))
    .liss_nonresponse_abort("source nonresponse binding blocked: mixed collection readiness requires all three named members")
  registry <- .liss_nonresponse_registry()
  for (id in ids) {
    member <- adapter$members[[match(id, expected_ids)]]
    household <- identical(member$authority_kind, "contract")
    rule <- if (household) registry$records[[1L]]$contract
            else registry$fields[[which(vapply(registry$fields, function(field)
              identical(field$field_id, id), logical(1)))]]
    value <- fields[[id]]
    count <- function(x) is.numeric(x) && is.null(attributes(x)) && length(x) == 1L &&
      !is.na(x) && is.finite(x) && x >= 0 && x == trunc(x)
    if (!is.list(value) || !identical(names(attributes(value)), "names") ||
        !all(vapply(value[c("evaluated_cells", "classified_cells", "unresolved_cells")], count, logical(1))) ||
        value$classified_cells > value$evaluated_cells || value$unresolved_cells != 0)
      .liss_nonresponse_abort("source nonresponse binding blocked: invalid native readiness in mixed collection")
    expected <- .liss_nonresponse_readiness(value$evaluated_cells,
      value$classified_cells, rule, if (household) NULL else value$case_counts)
    if (!identical(value, expected))
      .liss_nonresponse_abort("source nonresponse binding blocked: mixed collection readiness differs from native authority")
  }
  statuses <- vapply(fields, function(value) value$status, character(1))
  if (length(unique(statuses)) != 1L)
    .liss_nonresponse_abort("source nonresponse binding blocked: required collection fields have inconsistent readiness")
  total <- function(key) sum(vapply(fields, function(value) value[[key]], numeric(1)))
  list(schema_version = "4.0.0", kind = "source_nonresponse_collection",
    status = statuses[[1L]],
    scope = paste0("selected CI23p fields only (", paste(ids, collapse = ", "),
      "); counts are field-cells and do not approve household collection cleaning"),
    evaluated_cells = total("evaluated_cells"), classified_cells = total("classified_cells"),
    unresolved_cells = total("unresolved_cells"), fields = fields)
}

#' @noRd
.liss_nonresponse_finalize_mixed <- function(data, states, targets, wave_col,
                                             expected_target_metadata) {
  rules <- .liss_nonresponse_mixed_rules(states)
  ids <- names(rules)
  if (!identical(names(targets), ids) ||
      !is.list(expected_target_metadata) ||
      !identical(names(expected_target_metadata), ids))
    .liss_nonresponse_abort("source nonresponse binding blocked: mixed finalization requires all named targets and metadata")
  fields <- lapply(ids, function(id) {
    state <- states[[id]]
    rule <- rules[[id]]
    if (!identical(state$normalized, FALSE) ||
        !identical(state$target, rule$raw_column) || !is.null(state$wave_col) ||
        !identical(targets[[id]], rule$canonical_column))
      .liss_nonresponse_abort("source nonresponse binding blocked: unexpected mixed collection field or final target")
    .liss_nonresponse_column(data, targets[[id]], "canonical target")
    if (!identical(attributes(data[[targets[[id]]]]), expected_target_metadata[[id]]))
      .liss_nonresponse_abort("source nonresponse binding blocked: mixed canonical target metadata changed during finalization")
    final_rows <- .liss_nonresponse_input_rows(data, targets[[id]], state$id_col,
                                               wave_col, rule$wave)
    selected_keys <- final_rows$row_key[final_rows$wave_id == rule$wave]
    if (!identical(sort(selected_keys), sort(state$input_rows$row_key)))
      .liss_nonresponse_abort("source nonresponse binding blocked: complete collection source row/wave mapping changed during finalization")
    # the legacy finalizer's internal slice needs a metadata-preserving frame
    final_data <- if (.liss_nonresponse_is_field(rule)) data else tibble::as_tibble(data)
    final <- source_nonresponse_finalize(final_data, state, targets[[id]], wave_col,
      normalized = TRUE, expected_target_metadata = expected_target_metadata[[id]])
    if (!identical(final$target_metadata, expected_target_metadata[[id]]) ||
        !identical(attributes(data[[targets[[id]]]]), expected_target_metadata[[id]]))
      .liss_nonresponse_abort("source nonresponse binding blocked: mixed canonical metadata was repinned during finalization")
    # retain the native metadata slot when the legacy assignment drops NULL
    final["target_metadata"] <- list(expected_target_metadata[[id]])
    if (!identical(sort(names(final)), sort(names(state))))
      .liss_nonresponse_abort("source nonresponse binding blocked: finalized native scalar receipt changed shape")
    final <- final[names(state)]
    final <- .liss_nonresponse_seal(final)
    .liss_nonresponse_mixed_normalized(data, final)
    final
  })
  names(fields) <- ids
  state <- list(schema_version = "4.0.0", kind = "source_nonresponse_collection",
    source = fields[[1L]]$source, fields = fields, normalized = TRUE,
    readiness = .liss_nonresponse_mixed_readiness(lapply(fields, function(leaf) leaf$readiness)))
  state <- .liss_nonresponse_seal(state)
  .liss_nonresponse_validate_mixed(data, state)$state
}

#' @noRd
.liss_nonresponse_mixed_normalized <- function(data, leaf) {
  rule <- .liss_nonresponse_state_rule(leaf)
  .liss_nonresponse_column(data, leaf$target, "target")
  .liss_nonresponse_column(data, leaf$id_col, "id_col")
  selected <- .liss_nonresponse_rows(data, leaf$wave_col, rule$wave)
  ids <- .liss_nonresponse_ids(data[[leaf$id_col]][selected])
  pos <- match(ids, leaf$coverage$nomem_encr)
  if (anyNA(pos))
    .liss_nonresponse_abort("source nonresponse binding blocked: mixed protected rows are absent from their receipt")
  protected <- .liss_nonresponse_match(leaf$coverage$original_value[pos], rule)
  if (any(!is.na(.liss_nonresponse_values(data[[leaf$target]][selected])[protected])))
    .liss_nonresponse_abort("source nonresponse binding blocked: normalized mixed output reintroduced a source nonresponse amount")
  invisible(TRUE)
}

#' @noRd
.liss_nonresponse_mixed_receipts <- function(state) {
  rules <- .liss_nonresponse_mixed_rules(state$fields)
  source <- state$source
  invalid <- function()
    .liss_nonresponse_abort("source nonresponse binding blocked: mixed collection field/source/binding receipts disagree")
  if (!is.list(source) || !identical(names(attributes(source)), "names") ||
      !identical(sort(names(source)), c("filename", "sha256", "size_bytes")) ||
      !is.numeric(source$size_bytes) || !is.null(attributes(source$size_bytes)) ||
      length(source$size_bytes) != 1L || is.na(source$size_bytes) ||
      !is.finite(source$size_bytes) || source$size_bytes <= 0 ||
      source$size_bytes != trunc(source$size_bytes)) invalid()
  reference <- state$fields[[1L]]
  for (id in names(rules)) {
    leaf <- state$fields[[id]]
    rule <- rules[[id]]
    authority_key <- if (.liss_nonresponse_is_field(rule)) "field" else "rule"
    leaf_keys <- c("schema_version", authority_key, "source", "target", "wave_col",
      "id_col", "normalized", "coverage", "ledger", "input_rows", "metadata",
      "target_metadata", "readiness", "digest")
    if (!identical(names(attributes(leaf)), "names") ||
        !identical(sort(names(leaf)), sort(leaf_keys)) ||
        !is.character(leaf$digest) || length(leaf$digest) != 1L ||
        !identical(leaf$digest, .liss_nonresponse_digest(leaf)) ||
        !identical(leaf$source, source) ||
        !identical(source$filename, rule$source_filename) ||
        !identical(source$sha256, rule$source_sha256) ||
        !identical(leaf$normalized, TRUE) ||
        !identical(leaf$target, rule$canonical_column) ||
        !is.character(leaf$wave_col) || length(leaf$wave_col) != 1L ||
        is.na(leaf$wave_col) || !nzchar(leaf$wave_col) ||
        !is.character(leaf$id_col) || length(leaf$id_col) != 1L ||
        is.na(leaf$id_col) || !nzchar(leaf$id_col) ||
        !identical(leaf$wave_col, reference$wave_col) ||
        !identical(leaf$id_col, reference$id_col) ||
        !is.list(leaf$metadata) || !identical(names(attributes(leaf$metadata)), "names") ||
        !identical(sort(names(leaf$metadata)), sort(c("raw_column", "attributes",
          "id_attributes", "codebook_filename", "codebook_sha256"))) ||
        !identical(leaf$metadata$raw_column, rule$raw_column) ||
        !identical(leaf$metadata$codebook_filename, rule$codebook_filename) ||
        !identical(leaf$metadata$codebook_sha256, rule$codebook_sha256) ||
        !identical(leaf$metadata$id_attributes, reference$metadata$id_attributes)) invalid()
    .liss_nonresponse_mixed_rows(leaf, rule)
    if (!identical(leaf$coverage[c("nomem_encr", "source_row_index")],
                   reference$coverage[c("nomem_encr", "source_row_index")]) ||
        !identical(leaf$input_rows[c("nomem_encr", "wave_id", "row_key")],
                   reference$input_rows[c("nomem_encr", "wave_id", "row_key")])) invalid()
    if (.liss_nonresponse_is_field(rule)) .liss_nonresponse_collection_ledger(leaf)
    else .liss_nonresponse_mixed_household_ledger(leaf)
    expected <- .liss_nonresponse_readiness(nrow(leaf$coverage),
      sum(.liss_nonresponse_match(leaf$coverage$original_value, rule)), rule,
      if (.liss_nonresponse_is_field(rule))
        .liss_nonresponse_case_counts(leaf$coverage$original_value, rule) else NULL)
    if (!identical(leaf$readiness, expected)) invalid()
  }
  expected <- .liss_nonresponse_mixed_readiness(lapply(state$fields, function(leaf) leaf$readiness))
  if (!identical(state$readiness, expected)) invalid()
  invisible(TRUE)
}

#' @noRd
.liss_nonresponse_mixed_rows <- function(leaf, rule) {
  invalid <- function()
    .liss_nonresponse_abort("source nonresponse binding blocked: mixed collection complete row/value receipt is invalid")
  coverage <- leaf$coverage
  input <- leaf$input_rows
  if (!is.data.frame(coverage) ||
      !identical(names(coverage), c("nomem_encr", "source_row_index", "original_value")) ||
      !is.character(coverage$nomem_encr) || anyNA(coverage$nomem_encr) ||
      any(!grepl("^[0-9]+$", coverage$nomem_encr)) || anyDuplicated(coverage$nomem_encr) ||
      !is.numeric(coverage$source_row_index) || anyNA(coverage$source_row_index) ||
      any(!is.finite(coverage$source_row_index) | coverage$source_row_index < 1 |
        coverage$source_row_index != trunc(coverage$source_row_index)) ||
      anyDuplicated(coverage$source_row_index) || !is.data.frame(input) ||
      !identical(names(input), c("nomem_encr", "wave_id", "row_key", "value")) ||
      !is.character(input$nomem_encr) || anyNA(input$nomem_encr) ||
      any(!grepl("^[0-9]+$", input$nomem_encr)) ||
      !is.character(input$wave_id) || anyNA(input$wave_id) || any(!nzchar(input$wave_id)) ||
      !identical(input$row_key, paste(input$nomem_encr, input$wave_id, sep = ":")) ||
      anyDuplicated(input$row_key)) invalid()
  .liss_nonresponse_values(coverage$original_value)
  .liss_nonresponse_values(input$value)
  selected <- input$wave_id == rule$wave
  if (!identical(input$nomem_encr[selected], coverage$nomem_encr)) invalid()
  expected <- coverage$original_value
  expected[.liss_nonresponse_match(expected, rule)] <- NA_real_
  if (!all(.liss_nonresponse_equal(input$value[selected], expected))) invalid()
  invisible(TRUE)
}

#' @noRd
.liss_nonresponse_mixed_household_ledger <- function(state) {
  rule <- state$rule
  approved <- .liss_nonresponse_approved(rule)
  coverage <- state$coverage
  ledger <- state$ledger
  columns <- c("nomem_encr", "source_row_index", "original_value", "source_sha256",
    "source_filename", "source_release", "source_language", "raw_column",
    "raw_storage_type", "raw_value_repr", "current_value_before", "analysis_value_after",
    "system_missing_before", "source_declared_missing", "source_metadata_id",
    "evidence_ref", "wave_id", "current_column", "source_key", "rule_id", "rule_version",
    "response_validity", "answer_status", "analysis_disposition", "no_numeric_proposal",
    "measurement_period", "candidate_answer_status", "subtype_evidence")
  invalid <- function()
    .liss_nonresponse_abort("source nonresponse binding blocked: mixed household cell ledger differs from its native receipt")
  if (!is.data.frame(ledger) || !identical(names(ledger), columns) ||
      anyDuplicated(ledger$source_key)) invalid()
  matched <- .liss_nonresponse_match(coverage$original_value, rule)
  expected_keys <- if (sum(matched)) paste(rule$source_sha256,
    coverage$source_row_index[matched], rule$raw_column, sep = ":") else character()
  if (nrow(ledger) != sum(matched) || !setequal(ledger$source_key, expected_keys)) invalid()
  pos <- match(ledger$nomem_encr, coverage$nomem_encr)
  if (anyNA(pos) || !identical(ledger$source_row_index, coverage$source_row_index[pos]) ||
      !all(.liss_nonresponse_equal(ledger$original_value, coverage$original_value[pos]))) invalid()
  repeated <- list(source_sha256 = rule$source_sha256,
    source_filename = rule$source_filename, source_release = approved$source_release,
    source_language = rule$language, raw_column = rule$raw_column,
    source_metadata_id = paste(rule$source_sha256, rule$raw_column, sep = ":"),
    evidence_ref = approved$evidence_ref, wave_id = rule$wave, current_column = state$target,
    rule_id = rule$rule_id, rule_version = rule$rule_version,
    response_validity = rule$response_validity, answer_status = rule$answer_status,
    analysis_disposition = rule$analysis_disposition,
    no_numeric_proposal = rule$no_numeric_proposal, measurement_period = rule$measurement_period)
  for (key in names(repeated))
    if (!identical(ledger[[key]], rep(repeated[[key]], nrow(ledger)))) invalid()
  candidate <- ledger$candidate_answer_status
  evidence <- ledger$subtype_evidence
  expected_evidence <- if (length(candidate))
    ifelse(is.na(candidate), "unresolved", approved$routing$subtype_evidence)
    else character()
  if (!is.character(candidate) || !is.character(evidence) || anyNA(evidence) ||
      any(!is.na(candidate) & candidate != approved$routing$candidate_answer_status) ||
      !identical(evidence, expected_evidence) ||
      !all(ledger$raw_storage_type %in% c("double", "integer")) ||
      length(unique(ledger$raw_storage_type)) > 1L ||
      !identical(ledger$raw_value_repr, sprintf("%.0f", ledger$original_value)) ||
      !identical(ledger$current_value_before, ledger$original_value) ||
      !identical(ledger$analysis_value_after, rep(NA_real_, nrow(ledger))) ||
      !identical(ledger$system_missing_before, rep(FALSE, nrow(ledger))) ||
      !identical(ledger$source_declared_missing,
        .liss_nonresponse_declared(ledger$original_value, state$metadata$attributes)) ||
      !identical(ledger$source_key, if (nrow(ledger)) paste(rule$source_sha256,
        ledger$source_row_index, rule$raw_column, sep = ":") else character())) invalid()
  invisible(TRUE)
}

#' @noRd
.liss_nonresponse_validate_mixed <- function(data, state, wave_col = NULL) {
  keys <- c("schema_version", "kind", "source", "fields", "normalized", "readiness", "digest")
  if (!is.list(state) || !identical(names(attributes(state)), "names") ||
      !identical(sort(names(state)), sort(keys)) ||
      !identical(state$schema_version, "4.0.0") ||
      !identical(state$kind, "source_nonresponse_collection") ||
      !identical(state$normalized, TRUE) ||
      !is.character(state$digest) || length(state$digest) != 1L ||
      !identical(state$digest, .liss_nonresponse_digest(state)))
    .liss_nonresponse_abort("source nonresponse binding blocked: missing, unsupported or tampered mixed collection carrier")
  wrapper <- is.list(data) && !is.data.frame(data) && !is.null(data$data)
  if (wrapper) {
    if (!identical(data[["source_nonresponse"]], state) ||
        !identical(data[["source_nonresponse_readiness"]], state$readiness))
      .liss_nonresponse_abort("source nonresponse binding blocked: collection wrapper and supplied carrier disagree")
    wrapper_marker <- attr(data, "liss_source_nonresponse", exact = TRUE)
    if (!is.null(wrapper_marker) && !identical(wrapper_marker, state))
      .liss_nonresponse_abort("source nonresponse binding blocked: collection wrapper marker contradicts its carrier")
    for (key in c("source_nonresponse_readiness", "liss_source_nonresponse_readiness")) {
      readiness <- attr(data, key, exact = TRUE)
      if (!is.null(readiness) && !identical(readiness, state$readiness))
        .liss_nonresponse_abort("source nonresponse binding blocked: collection wrapper readiness contradicts its carrier")
    }
    data <- data$data
  }
  if (!is.data.frame(data))
    .liss_nonresponse_abort("source nonresponse collection validation requires a data frame")
  marker <- attr(data, "liss_source_nonresponse", exact = TRUE)
  if ((!is.null(marker) && !identical(marker, state)) ||
      (inherits(data, "liss_source_nonresponse_collection_result") && is.null(marker)))
    .liss_nonresponse_abort("source nonresponse binding blocked: collection frame and supplied carrier disagree")
  for (key in c("source_nonresponse_readiness", "liss_source_nonresponse_readiness")) {
    readiness <- attr(data, key, exact = TRUE)
    if (!is.null(readiness) && !identical(readiness, state$readiness))
      .liss_nonresponse_abort("source nonresponse binding blocked: collection frame readiness contradicts its carrier")
  }
  .liss_nonresponse_mixed_receipts(state)
  if (!is.null(wave_col) && !identical(wave_col, state$fields[[1L]]$wave_col))
    .liss_nonresponse_abort("source nonresponse binding blocked: collection wave mapping differs from the sealed carrier")
  fields <- lapply(state$fields, function(leaf) {
    .liss_nonresponse_column(data, leaf$id_col, "id_col")
    .liss_nonresponse_mixed_normalized(data, leaf)
    source_nonresponse_validate(data, leaf, leaf$target, leaf$wave_col)
  })
  state$fields <- lapply(fields, function(leaf) leaf$state)
  state$readiness <- .liss_nonresponse_mixed_readiness(lapply(fields, function(leaf) leaf$readiness))
  state <- .liss_nonresponse_seal(state)
  .liss_nonresponse_mixed_receipts(state)
  list(state = state, readiness = state$readiness, fields = fields)
}

#' @noRd
.liss_nonresponse_collection_consumers_path <- function() {
  system.file("schema", "source_nonresponse_collection_consumers.yml", package = "lissr")
}

#' @noRd
.liss_nonresponse_collection_consumers <- function() {
  path <- .liss_nonresponse_collection_consumers_path()
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path) || !file.exists(path) || dir.exists(path))
    .liss_nonresponse_abort("reviewed source nonresponse collection consumer policy is missing")
  bytes <- tryCatch(readBin(path, "raw", n = file.info(path)$size),
                    error = function(e) NULL)
  hash <- if (!is.null(bytes)) as.character(openssl::sha256(bytes)) else ""
  attributes(hash) <- NULL
  if (!identical(hash, "41fe0edfa8a3e3f3ee467873a1350c50c112bcefbbf0664beceec393115d6d9f"))
    .liss_nonresponse_abort("reviewed source nonresponse collection consumer policy integrity check failed")
  policy <- tryCatch(yaml::yaml.load(rawToChar(bytes), eval.expr = FALSE),
                     error = function(e) NULL)
  .liss_nonresponse_collection_consumers_validate(policy, .liss_nonresponse_registry(),
                                                 .liss_nonresponse_collection_adapter())
  policy
}

#' @noRd
.liss_nonresponse_collection_consumers_validate <- function(policy, registry, adapter) {
  invalid <- function()
    .liss_nonresponse_abort("reviewed source nonresponse collection consumer policy schema or authority is unsupported or ambiguous")
  expected <- list(schema_version = "1.0.0", policy_id = "CI23P339_V4_FLAG_CONSUMER",
    policy_version = "1.0.0", authority_registry = list(schema_version = "2.0.0",
      registry_version = "2.0.0",
      sha256 = "a2e6694acc5855d2df389df46d66e0c94b705637626309cc309ceee8d840ddc3"),
    collection_adapter = list(adapter_id = "CI23P_337_338_339_COLLECTION",
      adapter_version = "1.0.0",
      sha256 = "5e3235f1b4f78a589622ce27064eecf120c919e81fd95b84fe4a370126ac6c3f"),
    capabilities = list(list(capability_id = "CI23P339_V4_FLAG",
      capability_version = "1.0.0", consumer = "liss_clean_income",
      carrier_schema_version = "4.0.0", field_id = "CI23P339_FIELD",
      scalar_schema_version = "1.0.0", authority_kind = "contract",
      authority_id = "CI23P339_N01", authority_version = "1.0.0",
      construct = "household_net_income", unit = "EUR", measurement_period = 2022L,
      canonical_column = "s339", input_scope = "complete_validated_collection",
      selection = "explicit_variables_target", mode = "flag",
      retained_scope = "complete_collection")))
  equal <- function(value, approved) {
    if (!is.list(approved)) return(identical(value, approved))
    if (!is.list(value) || length(value) != length(approved)) return(FALSE)
    if (is.null(names(approved))) {
      if (!is.null(attributes(value))) return(FALSE)
      return(all(vapply(seq_along(approved), function(index)
        equal(value[[index]], approved[[index]]), logical(1))))
    }
    if (!identical(names(attributes(value)), "names") ||
        !identical(sort(names(value)), sort(names(approved)))) return(FALSE)
    all(vapply(names(approved), function(key)
      equal(value[[key]], approved[[key]]), logical(1)))
  }
  if (!equal(policy, expected)) invalid()
  .liss_nonresponse_registry_validate(registry)
  .liss_nonresponse_collection_adapter_validate(adapter, registry)
  invisible(TRUE)
}

#' @noRd
.liss_nonresponse_claims_source <- function(data) {
  wrapper <- is.list(data) && !is.data.frame(data) &&
    !is.null(data[["data", exact = FALSE]])
  frame <- if (wrapper) data[["data", exact = FALSE]] else data
  claim <- function(value) {
    inherits(value, c("liss_source_nonresponse_result",
                      "liss_source_nonresponse_v2_result",
                      "liss_source_nonresponse_collection_result")) ||
      any(vapply(c("liss_source_nonresponse", "source_nonresponse_readiness",
                   "liss_source_nonresponse_readiness"), function(key)
        !is.null(attr(value, key, exact = TRUE)), logical(1)))
  }
  claim(data) || claim(frame) ||
    (wrapper && !is.null(data[["source_nonresponse", exact = TRUE]]))
}

#' @noRd
.liss_nonresponse_claims_consumer <- function(data) {
  if (.liss_nonresponse_claims_applied(data)) return(TRUE)
  wrapper <- is.list(data) && !is.data.frame(data)
  frame <- if (wrapper) data[["data", exact = FALSE]] else data
  claim <- function(value) inherits(value, "liss_clean_collection_flag_result") ||
    !is.null(attr(value, "source_nonresponse_consumer_readiness", exact = TRUE)) ||
    !is.null(attr(value, "liss_source_nonresponse_consumer_readiness", exact = TRUE))
  collection <- function(value) is.list(value) &&
    (identical(value$schema_version, "3.0.0") ||
     identical(value$schema_version, "4.0.0") ||
     identical(value$kind, "source_nonresponse_collection") || !is.null(value$fields))
  states <- list(attr(data, "liss_source_nonresponse", exact = TRUE),
    attr(frame, "liss_source_nonresponse", exact = TRUE),
    attr(data, "source_nonresponse_readiness", exact = TRUE),
    attr(frame, "source_nonresponse_readiness", exact = TRUE),
    attr(data, "liss_source_nonresponse_readiness", exact = TRUE),
    attr(frame, "liss_source_nonresponse_readiness", exact = TRUE))
  if (wrapper) states <- c(states, list(data[["source_nonresponse", exact = TRUE]],
    data[["source_nonresponse_readiness", exact = TRUE]]))
  residual <- (inherits(data, "liss_clean_result") || inherits(frame, "liss_clean_result")) &&
    (inherits(data, "liss_source_nonresponse_collection_result") ||
     inherits(frame, "liss_source_nonresponse_collection_result") ||
     any(vapply(states, collection, logical(1))))
  claim(data) || claim(frame) || (wrapper &&
    !is.null(data[["source_nonresponse_consumer_readiness", exact = TRUE]])) || residual
}

#' @noRd
.liss_nonresponse_claims_guarded <- function(data) {
  wrapper <- is.list(data) && !is.data.frame(data)
  frame <- if (wrapper) data[["data", exact = TRUE]] else data
  claim <- function(value) any(grepl("source_nonresponse|guarded", class(value))) ||
    !is.null(attr(value, "liss_source_nonresponse", exact = TRUE)) ||
    !is.null(attr(value, "source_nonresponse_readiness", exact = TRUE)) ||
    !is.null(attr(value, "liss_source_nonresponse_readiness", exact = TRUE))
  .liss_nonresponse_claims_consumer(data) || claim(data) || claim(frame) ||
    (wrapper && (!is.null(data[["source_nonresponse", exact = TRUE]]) ||
      !is.null(data[["source_nonresponse_readiness", exact = TRUE]])))
}

#' @noRd
.liss_nonresponse_consumer_roles <- function(vars, state) {
  targets <- vapply(state$fields, function(leaf) leaf$target, character(1))
  roles <- c("person", "household", "wave", "personal_net", "personal_gross",
    "household_size", "category_code", "category_min", "category_max",
    "donor_keys", "birth_year", "age")
  secondary <- unlist(vars[roles], use.names = FALSE)
  if (!identical(vars$target, state$fields[["CI23P339_FIELD"]]$target) ||
      any(secondary %in% targets))
    .liss_nonresponse_abort("sealed collection amount targets cannot serve in any secondary household cleaner role")
  invisible(TRUE)
}

#' @noRd
.liss_nonresponse_consumer_columns <- function(data, target, background = FALSE) {
  reserved <- c(".lissr_join_person", ".lissr_join_wavenr")
  audit <- paste0(target, c("_observed", "_proposed", "_proposed_status",
                            "_dataset_flag", "_clean_status"))
  if (anyNA(names(data)) || anyDuplicated(names(data)) ||
      any(!nzchar(names(data))) || any(reserved %in% names(data)) ||
      (!background && any(audit %in% names(data))))
    .liss_nonresponse_abort("collection consumer input has reserved join or household audit column collisions")
  invisible(TRUE)
}

#' @noRd
.liss_nonresponse_consumer_unchanged <- function(data, original) {
  count <- length(original)
  if (nrow(data) != nrow(original) || length(data) < count ||
      !identical(names(data)[seq_len(count)], names(original)) ||
      !all(vapply(names(original), function(name)
        identical(data[[name]], original[[name]]), logical(1))))
    .liss_nonresponse_abort("collection flag consumer changed an original input column or row order")
  invisible(TRUE)
}

#' @noRd
.liss_nonresponse_consumer_result_digest <- function(result) {
  summary <- result$summary
  summary$duration_s <- NULL
  payload <- list(data = result$data, decisions = result$decisions,
    ruleset = result$ruleset, variables = result$variables, mode = result$mode,
    summary = summary)
  .liss_identity_hash(payload)
}

#' @noRd
.liss_nonresponse_consumer_record <- function(result, policy) {
  leaf <- result$source_nonresponse$fields[["CI23P339_FIELD"]]
  readiness <- leaf$readiness
  .liss_nonresponse_seal(list(schema_version = "1.0.0",
    kind = "source_nonresponse_consumer_readiness", consumer = "liss_clean_income",
    policy_id = policy$policy_id, policy_version = policy$policy_version,
    policy_sha256 = "41fe0edfa8a3e3f3ee467873a1350c50c112bcefbbf0664beceec393115d6d9f",
    capability_id = "CI23P339_V4_FLAG", capability_version = "1.0.0",
    carrier_schema_version = "4.0.0", carrier_digest = result$source_nonresponse$digest,
    field_id = "CI23P339_FIELD", rule_id = "CI23P339_N01", target = leaf$target,
    mode = "flag", status = if (readiness$evaluated_cells > 0L) "passed" else "not_evaluated",
    scope = "household339_flag_source_exclusion_only", input_rows = as.integer(nrow(result$data)),
    evaluated_cells = as.integer(readiness$evaluated_cells),
    classified_cells = as.integer(readiness$classified_cells),
    unresolved_cells = as.integer(readiness$unresolved_cells),
    result_digest = .liss_nonresponse_consumer_result_digest(result)))
}

#' @noRd
.liss_nonresponse_validate_consumer_result <- function(data, state) {
  invalid <- function()
    .liss_nonresponse_abort("source nonresponse complete consumer result is missing, unsupported or tampered")
  classes <- c("liss_clean_collection_flag_result", "liss_clean_result",
    "liss_source_nonresponse_collection_result", "liss_source_nonresponse_result")
  slots <- c("data", "decisions", "log", "summary", "ruleset", "variables", "mode",
    "source_nonresponse_readiness", "source_nonresponse", "source_nonresponse_consumer_readiness")
  if (!is.list(data) || is.data.frame(data) || !identical(class(data), classes) ||
      !identical(sort(names(attributes(data))), c("class", "names")) ||
      !identical(names(data), slots) || !is.data.frame(data$data) ||
      !identical(data$mode, "flag") ||
      .liss_nonresponse_claims_consumer(data$data) ||
      !is.list(state) || !identical(state$schema_version, "4.0.0") ||
      !identical(data$source_nonresponse, state) || !is.list(data$summary) ||
      !is.data.frame(data$decisions) || !is.list(data$variables) ||
      !inherits(data$ruleset, "liss_cleaning_ruleset")) invalid()
  policy <- .liss_nonresponse_collection_consumers()
  checked <- .liss_nonresponse_validate_mixed(data, state)
  if (!identical(checked$state, state)) invalid()
  .liss_nonresponse_consumer_roles(data$variables, state)
  record <- data$source_nonresponse_consumer_readiness
  expected <- .liss_nonresponse_consumer_record(data, policy)
  if (!identical(record, expected)) invalid()
  field <- checked$fields[["CI23P339_FIELD"]]
  target <- state$fields[["CI23P339_FIELD"]]$target
  observed <- data$data[[paste0(target, "_observed"), exact = TRUE]]
  proposed <- data$data[[paste0(target, "_proposed"), exact = TRUE]]
  status <- data$data[[paste0(target, "_proposed_status"), exact = TRUE]]
  if (!identical(observed, field$observed) || !is.numeric(proposed) ||
      length(proposed) != nrow(data$data) || !is.character(status) ||
      length(status) != nrow(data$data) ||
      any(!is.na(proposed[field$lock])) ||
      anyNA(status[field$lock]) ||
      any(status[field$lock] != "source_nonresponse:missing_amount")) invalid()
  checked$consumer_readiness <- record
  checked
}

#' @noRd
.liss_nonresponse_abort <- function(message) {
  cli::cli_abort(message, class = "liss_source_nonresponse_blocked",
                 source_nonresponse_readiness = list(
                   status = "blocked", reason = "reviewed source approval or binding failed"),
                 .envir = parent.frame())
}
