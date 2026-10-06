#' Attach explicitly selected local Background Variables snapshots
#'
#' Attach selected covariates from local SPSS SAV snapshots by respondent and an
#' explicitly chosen survey month. The helper preserves survey rows, order,
#' original columns and metadata. It does not select releases, infer an item
#' month, download files, harmonize covariates or establish analytical validity.
#'
#' @param data A plain data frame or ungrouped tibble containing `nomem_encr` and
#'   the column named by `month_col`. For a merge result, pass `result$data`.
#' @param sources A nonempty plain data frame or ungrouped tibble with character
#'   `sav_path` and numeric or character `expected_month` columns. Each row selects
#'   an existing local `.sav` file and its expected YYYYMM. Optional character
#'   columns `archive_path` and `archive_member` must both be present; on each row
#'   they must both be supplied or both be `NA`. Archive members must be exact
#'   basename-only SAV names. Other manifest columns are not accepted.
#' @param month_col Required exact name of the survey's month column. Choose its
#'   item or survey-part meaning deliberately. No default or fallback is used.
#' @param variables Required unique, nonempty character vector of exact covariate
#'   names present in every SAV. `nomem_encr` and `wave` cannot be selected.
#' @param prefix Nonempty character prefix for appended names, default `"avars_"`.
#'   Existing survey columns are never overwritten.
#'
#' @details
#' Only local, regular `.sav` files with the standard SPSS `$FL2` signature are
#' admitted, before reading with `haven::read_sav(user_na = TRUE)`. Compressed
#' containers and `$FL3` ZSAV files are rejected. Both supplied and symlink-resolved
#' paths must have the required extension. Each source must have one observed
#' valid `wave`, equal to `expected_month`, complete unique respondent-month keys,
#' and a different month from every other source. Recognized Background filenames
#' are checked against that month; renamed standalone SAVs remain usable with
#' unknown filename language and version.
#'
#' Respondent keys must be positive integer-valued numbers no larger than
#' `2^53 - 1`, or canonical unsigned digit text without leading zeroes. Months
#' must be integer-valued YYYYMM numbers or six-digit text, with date whitespace
#' permitted, in the fixed 1900--2100 plausibility window. Factors, logicals,
#' dates and other unsupported key classes are rejected. Ordinary, tagged and
#' declared SPSS missing keys, including harmonized missing-declaration stashes,
#' never match. Invalid nonmissing survey keys abort even when the other key is
#' missing. Repeated survey keys are retained as separate observations.
#'
#' Selected columns must agree in storage type, classes and metadata across all
#' sources. Value labels are compared by their code-label associations. Conflicting
#' labels, missing definitions or display formats abort; no automatic metadata
#' harmonization or sentinel recoding occurs. A successful key match is independent
#' of covariate missingness. Unmarked empty or whitespace strings remain covariate
#' values. `nohouse_encr` may be selected descriptively but is
#' never a join key.
#'
#' Optional ZIP lineage is checked without extraction: the member must occur
#' exactly once and its streamed uncompressed SHA256 must equal the SAV SHA256.
#' Recognized archive/member months and languages must agree; release-version
#' differences are recorded. Hash, size and modification-time checks bracket the
#' reads. These checks establish byte identities, not authenticity, and cannot
#' detect an edit restored between observations. No files are written.
#'
#' Nonempty surveys with no eligible keys or no matches abort. Partial coverage
#' and unused supplied months produce one aggregate warning. Empty surveys return
#' validated typed empty output, with undefined match fractions represented by
#' `NA`. Diagnostics contain aggregate counts and source names, never respondent
#' or household identifiers or data-row samples. The snapshot month does not prove
#' that every item was freshly answered in that month. Runtime validation does not
#' constitute whole-corpus empirical validation of all historical releases.
#' Selected metadata remains complete on the returned columns and is used in full
#' for compatibility checks. Provenance records only metadata receipts: storage
#' type, classes, attribute names and a SHA256 fingerprint of the canonical
#' metadata encoded with R serialization version 2. Raw value-label codes or text
#' and missing-definition values are omitted from provenance. Recorded invocation
#' strings retain their exact contents without incidental caller attributes.
#'
#' @return A plain list with `data`, `audit` and `provenance`. `data` appends the
#'   prefixed selected variables in requested order. `audit` records key validation,
#'   row counts, eligible and all-row match fractions, exhaustive `row_status`,
#'   `status_counts`, per-source coverage and per-variable missingness using output
#'   rows as the denominator. Statuses are `missing_both_keys`, `missing_person`,
#'   `missing_month`, `month_not_supplied`, `respondent_not_found` and `matched`.
#'   `provenance` records invocation choices, versions, read settings, paths,
#'   filename tokens, file hashes, verified lineage, schema and metadata receipts.
#'   No global `valid_for_analysis` assertion is made.
#' @export
#' @examples
#' \dontrun{
#' sources <- data.frame(
#'   sav_path = "/local/background/avars_202401_EN_1.0p.sav",
#'   expected_month = 202401
#' )
#' attached <- liss_attach_background(
#'   survey, sources, month_col = "fieldwork_ym", variables = "leeftijd"
#' )
#' attached$audit$status_counts
#' }
liss_attach_background <- function(data, sources, month_col, variables,
                                   prefix = "avars_") {
  .liss_bg_frame(data, "data")
  .liss_bg_frame(sources, "sources")
  .liss_bg_scalar(month_col, "month_col")
  .liss_bg_scalar(prefix, "prefix")
  if (month_col == "nomem_encr")
    .liss_bg_abort("month_col cannot be nomem_encr.", "invalid_month_column")
  if (!all(c("nomem_encr", month_col) %in% names(data)))
    .liss_bg_abort("Survey data must contain nomem_encr and month_col.",
                  "missing_survey_keys")
  if (!is.character(variables) || is.object(variables) || !is.null(dim(variables)) ||
      !length(variables) || anyNA(variables) || any(!nzchar(variables)) ||
      anyDuplicated(variables))
    .liss_bg_abort("variables must be unique nonempty exact character names.",
                  "invalid_variables")
  if (any(variables %in% c("nomem_encr", "wave")))
    .liss_bg_abort("nomem_encr and wave cannot be selected covariates.",
                  "key_variable_requested")
  attributes(month_col) <- NULL
  attributes(variables) <- NULL
  attributes(prefix) <- NULL
  output_names <- paste0(prefix, variables)
  if (anyDuplicated(output_names) || any(output_names %in% names(data)))
    .liss_bg_abort("Prefixed output names collide with survey columns.",
                  "output_collision")

  required <- c("sav_path", "expected_month")
  archive_fields <- c("archive_path", "archive_member")
  if (!nrow(sources) || !all(required %in% names(sources)) ||
      any(!names(sources) %in% c(required, archive_fields)) ||
      sum(archive_fields %in% names(sources)) %in% 1L)
    .liss_bg_abort("sources must have sav_path and expected_month, with optional paired archive columns only.",
                  "invalid_manifest")
  path_fields <- intersect(c("sav_path", archive_fields), names(sources))
  for (field in path_fields) {
    if (!is.character(sources[[field]]) || is.object(sources[[field]]) ||
        !is.null(dim(sources[[field]])))
      .liss_bg_abort(paste0("Manifest ", field, " must be a plain character column."),
                    "invalid_manifest")
  }
  has_archive <- archive_fields[[1]] %in% names(sources)
  if (has_archive && any(xor(is.na(sources$archive_path),
                            is.na(sources$archive_member))))
    .liss_bg_abort("archive_path and archive_member must both be supplied or both be NA.",
                  "incomplete_archive_pair")
  expected <- .liss_bg_month(sources$expected_month)
  if (any(expected$class != "valid"))
    .liss_bg_abort("Manifest expected_month must contain valid nonmissing YYYYMM values.",
                  "invalid_expected_month", expected$counts)
  survey_person <- .liss_bg_person(data$nomem_encr)
  survey_month <- .liss_bg_month(data[[month_col]])
  .liss_bg_check_key(survey_person, "Survey respondent", background = FALSE)
  .liss_bg_check_key(survey_month, "Survey month", background = FALSE)

  paths <- vapply(sources$sav_path, .liss_bg_local_path, character(1),
                  extension = "sav")
  if (anyDuplicated(paths))
    .liss_bg_abort("Duplicate resolved SAV paths are not allowed.",
                  "duplicate_source_path")
  source_data <- vector("list", nrow(sources))
  source_keys <- vector("list", nrow(sources))
  source_records <- vector("list", nrow(sources))
  source_validation <- vector("list", nrow(sources))
  source_states <- vector("list", nrow(sources))
  archive_states <- vector("list", nrow(sources))
  observed <- numeric(nrow(sources))

  for (i in seq_len(nrow(sources))) {
    path <- paths[[i]]
    .liss_bg_sav_signature(path)
    source_states[[i]] <- .liss_bg_file_state(path)
    frame <- .liss_bg_read_sav(path, i)
    .liss_bg_frame(frame, paste0("SAV source ", i))
    if (!nrow(frame))
      .liss_bg_abort(paste0("SAV source ", i, " has zero background rows."),
                    "empty_background_source")
    if (!all(c("nomem_encr", "wave") %in% names(frame)))
      .liss_bg_abort(paste0("SAV source ", i, " lacks nomem_encr or wave."),
                    "missing_background_keys")
    person <- .liss_bg_person(frame$nomem_encr)
    month <- .liss_bg_month(frame$wave)
    .liss_bg_check_key(person, paste0("Background respondent source ", i), TRUE)
    .liss_bg_check_key(month, paste0("Background month source ", i), TRUE)
    months <- unique(month$value)
    if (length(months) != 1L)
      .liss_bg_abort(paste0("SAV source ", i, " must contain one observed wave month."),
                    "mixed_background_months", list(n_months = length(months)))
    observed[[i]] <- months[[1]]
    if (observed[[i]] != expected$value[[i]])
      .liss_bg_abort(paste0("SAV source ", i, " observed wave differs from expected_month."),
                    "expected_month_mismatch")
    key <- paste0(month$value, ":", person$value)
    if (anyDuplicated(key))
      .liss_bg_abort(paste0("SAV source ", i, " has duplicate respondent-month keys."),
                    "duplicate_background_keys", list(n_duplicates = sum(duplicated(key))))
    missing_variables <- setdiff(variables, names(frame))
    if (length(missing_variables))
      .liss_bg_abort(paste0("SAV source ", i, " is missing requested variables: ",
                           paste(missing_variables, collapse = ", "), "."),
                    "missing_selected_variables",
                    list(source_index = i, observed_month = observed[[i]],
                         variables = missing_variables))
    filename <- .liss_bg_filename(basename(path), "sav")
    supplied_filename <- .liss_bg_filename(basename(sources$sav_path[[i]]), "sav")
    .liss_bg_filename_month(filename, observed[[i]], i, "resolved SAV")
    .liss_bg_filename_month(supplied_filename, observed[[i]], i, "supplied SAV")
    .liss_bg_filename_language(list(filename, supplied_filename), i)
    source_data[[i]] <- frame[variables]
    source_keys[[i]] <- key
    source_validation[[i]] <- list(person = person$counts, month = month$counts,
                                  person_missing = person$missing_counts,
                                  month_missing = month$missing_counts,
                                  person_normalization = person$normalization,
                                  month_normalization = month$normalization)
    lineage <- list(status = "standalone", archive_path_supplied = NA_character_,
                    archive_path_resolved = NA_character_, archive_basename = NA_character_,
                    archive_member = NA_character_, archive_sha256 = NA_character_,
                    member_sha256 = NA_character_, archive_filename = NULL,
                    member_filename = NULL)
    if (has_archive && !is.na(sources$archive_path[[i]])) {
      archive <- .liss_bg_local_path(sources$archive_path[[i]], "zip")
      member <- sources$archive_member[[i]]
      archive_states[[i]] <- .liss_bg_file_state(archive)
      lineage <- .liss_bg_archive(archive, sources$archive_path[[i]], member,
                                  source_states[[i]]$sha256, observed[[i]],
                                  list(filename, supplied_filename), i,
                                  archive_states[[i]])
    }
    schema <- lapply(frame, function(x) list(type = typeof(x), class = class(x)))
    source_records[[i]] <- list(
      source_index = i, sav_path_supplied = sources$sav_path[[i]],
      sav_path_resolved = path, sav_basename = basename(path),
      expected_month = expected$value[[i]], observed_month = observed[[i]],
      filename = filename, supplied_filename = supplied_filename,
      hash_algorithm = "SHA256", sav_sha256 = source_states[[i]]$sha256,
      size_bytes = source_states[[i]]$size_bytes,
      mtime = source_states[[i]]$mtime, n_source_rows = nrow(frame),
      schema = schema, lineage = lineage)
  }
  if (anyDuplicated(observed))
    .liss_bg_abort("Exactly one selected SAV source per observed month is required.",
                  "competing_source_months", list(n_duplicate_months = sum(duplicated(observed))))

  selected_metadata <- lapply(source_data[[1]], .liss_bg_metadata)
  for (i in seq_along(source_data)) {
    for (variable in variables) {
      metadata <- .liss_bg_metadata(source_data[[i]][[variable]])
      if (!identical(selected_metadata[[variable]], metadata))
        .liss_bg_abort(paste0("Selected variable ", variable,
                             " has incompatible storage, classes or metadata in source ", i, "."),
                      "incompatible_selected_metadata",
                      list(source_index = i, variable = variable,
                           baseline_source_index = 1L))
    }
  }

  n <- nrow(data)
  eligible <- survey_person$class == "valid" & survey_month$class == "valid"
  person_missing <- is.na(survey_person$value)
  month_missing <- is.na(survey_month$value)
  row_status <- rep("respondent_not_found", n)
  row_status[person_missing & month_missing] <- "missing_both_keys"
  row_status[person_missing & !month_missing] <- "missing_person"
  row_status[!person_missing & month_missing] <- "missing_month"
  row_status[eligible & !survey_month$value %in% observed] <- "month_not_supplied"
  combined_keys <- unlist(source_keys, use.names = FALSE)
  match_index <- rep(NA_integer_, n)
  match_index[eligible] <- match(paste0(survey_month$value[eligible], ":",
                                      survey_person$value[eligible]), combined_keys)
  matched <- !is.na(match_index)
  row_status[matched] <- "matched"
  levels <- c("missing_both_keys", "missing_person", "missing_month",
              "month_not_supplied", "respondent_not_found", "matched")
  status_counts <- .liss_bg_counts(row_status, levels)
  n_eligible <- sum(eligible)
  n_matched <- sum(matched)
  if (n && !n_eligible)
    .liss_bg_abort("No eligible survey respondent-month keys remain.",
                  "no_eligible_survey_keys", list(status_counts = status_counts))
  if (n && !n_matched)
    .liss_bg_abort("No survey rows matched the supplied Background snapshots.",
                  "no_matched_survey_rows", list(n_eligible = n_eligible,
                                                status_counts = status_counts))

  source_lengths <- lengths(source_keys)
  source_starts <- c(0L, utils::head(cumsum(source_lengths), -1L))
  output <- data
  original_attributes <- attributes(data)
  variable_missingness <- vector("list", length(variables))
  source_missingness <- vector("list", length(variables) * length(source_data))
  for (j in seq_along(variables)) {
    variable <- variables[[j]]
    prototype <- source_data[[1]][[variable]]
    values <- prototype[rep(NA_integer_, n)]
    matched_missing <- rep(FALSE, n)
    for (i in seq_along(source_data)) {
      source_values <- source_data[[i]][[variable]]
      source_missing <- .liss_bg_missing(source_values, include_blank = FALSE,
                                         coerce_declarations = FALSE)$missing
      rows <- which(matched & match_index > source_starts[[i]] &
                      match_index <= source_starts[[i]] + source_lengths[[i]])
      source_rows <- match_index[rows] - source_starts[[i]]
      values[rows] <- source_values[source_rows]
      matched_missing[rows] <- source_missing[source_rows]
      source_missingness[[(j - 1L) * length(source_data) + i]] <- data.frame(
        source_index = i, observed_month = observed[[i]], variable = variable,
        n_source_rows = length(source_values), n_source_missing = sum(source_missing),
        stringsAsFactors = FALSE)
    }
    column_attributes <- attributes(prototype)
    column_attributes$names <- NULL
    attributes(values) <- column_attributes
    output[[output_names[[j]]]] <- values
    variable_missingness[[j]] <- data.frame(
      variable = variable, output_name = output_names[[j]],
      n_matched_nonmissing = sum(matched & !matched_missing),
      n_matched_source_missing = sum(matched_missing), n_unmatched = n - n_matched,
      stringsAsFactors = FALSE)
  }
  original_attributes$names <- c(names(data), output_names)
  attributes(output) <- original_attributes
  unchanged_columns <- vapply(names(data), function(name)
    identical(output[[name]], data[[name]]), logical(1))
  survey_attribute_names <- setdiff(names(attributes(data)), "names")
  if (!all(unchanged_columns) || nrow(output) != n ||
      !identical(attributes(output)[survey_attribute_names],
                 attributes(data)[survey_attribute_names]))
    .liss_bg_abort("Internal attachment invariant failed to preserve the survey.",
                  "survey_preservation_failure")
  source_coverage <- data.frame(
    source_index = seq_along(source_data), expected_month = expected$value,
    observed_month = observed, n_source_rows = source_lengths,
    n_eligible = vapply(observed, function(x) sum(eligible & survey_month$value %in% x), integer(1)),
    n_matched = vapply(observed, function(x) sum(matched & survey_month$value %in% x), integer(1)),
    stringsAsFactors = FALSE)
  source_coverage$unused <- source_coverage$n_eligible == 0L
  audit <- list(
    n_input = n, n_output = nrow(output), n_eligible = n_eligible,
    n_matched = n_matched,
    eligible_match_fraction = if (n_eligible) n_matched / n_eligible else NA_real_,
    all_row_match_fraction = if (n) n_matched / n else NA_real_,
    row_status = row_status, status_counts = status_counts,
    key_validation = list(
      survey = list(person = survey_person$counts, month = survey_month$counts,
                    person_missing = survey_person$missing_counts,
                    month_missing = survey_month$missing_counts,
                    person_normalization = survey_person$normalization,
                    month_normalization = survey_month$normalization),
      sources = source_validation),
    source_coverage = source_coverage,
    variable_missingness = do.call(rbind, variable_missingness),
    source_variable_missingness = do.call(rbind, source_missingness),
    filename_unknown = which(!vapply(source_records,
                                     function(x) x$filename$recognized, logical(1))))
  invocation_sources <- as.data.frame(lapply(sources, function(x) {
    attributes(x) <- NULL
    x
  }), stringsAsFactors = FALSE, optional = TRUE)
  provenance <- list(
    package_version = as.character(utils::packageVersion("lissr")),
    r_version = as.character(getRversion()),
    haven_version = as.character(utils::packageVersion("haven")),
    openssl_version = as.character(utils::packageVersion("openssl")),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    invocation = list(month_col = month_col, variables = variables, prefix = prefix,
                      sources = invocation_sources),
    read_settings = list(format = "SAV", signature = "$FL2", user_na = TRUE,
                         .name_repair = "check_unique", hash_algorithm = "SHA256"),
    sources = source_records,
    selected_metadata = lapply(selected_metadata, .liss_bg_metadata_receipt),
    column_mapping = data.frame(output_name = output_names, source_name = variables,
                                stringsAsFactors = FALSE))
  for (i in seq_along(paths)) {
    .liss_bg_unchanged(paths[[i]], source_states[[i]], i, "SAV")
    if (!is.null(archive_states[[i]]))
      .liss_bg_unchanged(source_records[[i]]$lineage$archive_path_resolved,
                         archive_states[[i]], i, "archive")
  }
  if (n && (n_matched < n || any(source_coverage$unused))) {
    reasons <- status_counts[names(status_counts) != "matched" & status_counts > 0L]
    parts <- character()
    if (length(reasons))
      parts <- c(parts, paste0("unmatched rows: ",
                              paste(paste0(names(reasons), "=", reasons), collapse = ", ")))
    if (any(source_coverage$unused))
      parts <- c(parts, paste0("unused supplied source months=", sum(source_coverage$unused)))
    condition <- structure(list(message = paste0("Background attachment coverage: ",
                                                 paste(parts, collapse = "; "), "."),
                                call = NULL, code = "partial_background_coverage",
                                diagnostics = list(status_counts = status_counts,
                                                   n_unused_sources = sum(source_coverage$unused))),
                           class = c("liss_background_warning", "warning", "condition"))
    warning(condition)
  }
  list(data = output, audit = audit, provenance = provenance)
}

# small internal guards keep file admission and diagnostics separate from values.
.liss_bg_abort <- function(message, code, diagnostics = NULL) {
  stop(structure(list(message = message, call = NULL, code = code,
                      diagnostics = diagnostics),
                 class = c("liss_background_error", "error", "condition")))
}

.liss_bg_scalar <- function(x, name) {
  if (!is.character(x) || is.object(x) || !is.null(dim(x)) || length(x) != 1L ||
      is.na(x) || !nzchar(x))
    .liss_bg_abort(paste0(name, " must be a nonmissing nonempty character scalar."),
                  "invalid_argument")
}

.liss_bg_frame <- function(x, name) {
  supported <- identical(class(x), "data.frame") ||
    identical(class(x), c("tbl_df", "tbl", "data.frame"))
  if (!supported)
    .liss_bg_abort(paste0(name, " must be a plain data.frame or ungrouped tibble."),
                  "unsupported_data_frame")
  fields <- names(x)
  if (is.null(fields) || anyNA(fields) || any(!nzchar(fields)) || anyDuplicated(fields))
    .liss_bg_abort(paste0(name, " must have unique nonempty column names."),
                  "invalid_column_names")
}

.liss_bg_underlying <- function(x) {
  allowed_classes <- c("haven_labelled_spss", "haven_labelled", "vctrs_vctr",
                       "double", "integer", "character")
  supported <- is.null(dim(x)) && typeof(x) %in% c("double", "integer", "character") &&
    (!is.object(x) || (inherits(x, "haven_labelled") &&
                       all(class(x) %in% allowed_classes)))
  if (!supported) return(NULL)
  attr(x, "class") <- NULL
  x
}

.liss_bg_counts <- function(x, levels) {
  counts <- as.integer(table(factor(x, levels = levels)))
  names(counts) <- levels
  counts
}

.liss_bg_missing <- function(x, include_blank = TRUE, coerce_declarations = TRUE) {
  declarations <- yyyymm_declarations(x)
  raw <- x
  attr(raw, "class") <- NULL
  ordinary <- is.na(raw)
  tagged <- if (typeof(raw) == "double") haven::is_tagged_na(raw) else rep(FALSE, length(raw))
  blank <- if (include_blank && is.character(raw))
    !is.na(raw) & !nzchar(trimws(raw)) else rep(FALSE, length(raw))
  declared <- rep(FALSE, length(raw))
  if (length(declarations$na_values)) {
    declared <- !ordinary & raw %in% declarations$na_values
    if (coerce_declarations && is.character(raw))
      declared <- declared | (!ordinary & trimws(raw) %in% trimws(as.character(declarations$na_values)))
  }
  if (is.numeric(raw) || is.character(raw)) {
    num <- suppressWarnings(as.numeric(raw))
    if (coerce_declarations) {
      declared_values <- suppressWarnings(as.numeric(declarations$na_values))
      declared_values <- declared_values[!is.na(declared_values)]
      if (length(declared_values))
        declared <- declared | (!is.na(num) & num %in% declared_values)
    }
    for (range in declarations$na_range) {
      range <- suppressWarnings(as.numeric(range))
      if (length(range) == 2L && !anyNA(range))
        declared <- declared | (!is.na(num) & num >= range[[1]] & num <= range[[2]])
    }
  }
  declared <- declared & !ordinary & !blank
  classification <- rep("nonmissing", length(raw))
  classification[declared] <- "declared_missing"
  classification[blank] <- "blank_missing"
  classification[ordinary & !tagged] <- "ordinary_missing"
  classification[tagged] <- "tagged_missing"
  list(missing = ordinary | blank | declared,
       ordinary = ordinary | blank, declared = declared,
       counts = .liss_bg_counts(classification, c("ordinary_missing", "tagged_missing",
                                                  "blank_missing", "declared_missing")))
}

.liss_bg_person <- function(x) {
  classes <- c("valid", "source_missing", "declared_missing", "invalid",
               "outside_window", "unsupported")
  raw <- .liss_bg_underlying(x)
  value <- rep(NA_character_, length(x))
  normalization <- c(n_numeric = 0L, n_character = 0L)
  if (is.null(raw))
    return(list(value = value, class = rep("unsupported", length(x)),
                counts = stats::setNames(c(0L, 0L, 0L, 0L, 0L, length(x)), classes),
                missing_counts = NULL, normalization = normalization,
                supported = FALSE))
  missing <- .liss_bg_missing(x)
  class <- rep("invalid", length(raw))
  max_id <- 2^53 - 1
  if (is.character(raw)) {
    canonical <- !missing$missing & grepl("^[1-9][0-9]*$", raw)
    boundary <- "9007199254740991"
    in_range <- canonical & (nchar(raw) < nchar(boundary) |
                              (nchar(raw) == nchar(boundary) & raw <= boundary))
    rows <- which(in_range)
    num <- suppressWarnings(as.numeric(raw[rows]))
    exact <- is.finite(num) & num <= max_id & sprintf("%.0f", num) == raw[rows]
    valid <- rows[exact]
    value[valid] <- raw[valid]
    normalization[["n_character"]] <- length(valid)
  } else {
    valid <- which(!missing$missing & is.finite(raw) & raw > 0 &
                     raw <= max_id & raw == floor(raw))
    value[valid] <- sprintf("%.0f", raw[valid])
    normalization[["n_numeric"]] <- length(valid)
  }
  class[!is.na(value)] <- "valid"
  class[missing$declared] <- "declared_missing"
  class[missing$ordinary] <- "source_missing"
  list(value = value, class = class, counts = .liss_bg_counts(class, classes),
       missing_counts = missing$counts, normalization = normalization,
       supported = TRUE)
}

.liss_bg_month <- function(x) {
  raw <- .liss_bg_underlying(x)
  if (is.null(raw))
    return(list(value = rep(NA_real_, length(x)), class = rep("unsupported", length(x)),
                counts = stats::setNames(c(0L, 0L, 0L, 0L, 0L, length(x)),
                                  c("valid", "source_missing", "declared_missing", "invalid",
                                    "outside_window", "unsupported")),
                missing_counts = NULL, normalization = c(n_numeric = 0L, n_character = 0L),
                supported = FALSE))
  declarations <- yyyymm_declarations(x)
  parsed <- parse_yyyymm(raw, declarations$na_values, declarations$na_range)
  parsed$missing_counts <- .liss_bg_missing(x)$counts
  n_valid <- sum(parsed$class == "valid")
  parsed$normalization <- c(n_numeric = if (is.numeric(raw)) n_valid else 0L,
                            n_character = if (is.character(raw)) n_valid else 0L)
  parsed$supported <- TRUE
  parsed
}

.liss_bg_check_key <- function(key, label, background) {
  bad <- key$class %in% c("invalid", "outside_window", "unsupported")
  if (!key$supported || any(bad) || (background && any(key$class != "valid")))
    .liss_bg_abort(paste0(label, " keys are unsupported, invalid, outside-window",
                         if (background) " or missing." else "."),
                  "invalid_keys", key$counts)
}

.liss_bg_local_path <- function(path, extension) {
  .liss_bg_scalar(path, paste0(extension, " path"))
  remote <- grepl("^[A-Za-z][A-Za-z0-9+.-]*:", path) &&
    !grepl("^[A-Za-z]:[/\\\\]", path)
  if (remote || tolower(tools::file_ext(path)) != extension ||
      !file.exists(path) || !utils::file_test("-f", path) || file.access(path, 4L) != 0L)
    .liss_bg_abort(paste0("An existing readable regular local .", extension,
                         " file is required."), "invalid_local_file")
  resolved <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (tolower(tools::file_ext(resolved)) != extension ||
      !utils::file_test("-f", resolved) || file.access(resolved, 4L) != 0L)
    .liss_bg_abort(paste0("Resolved local file must also be a readable regular .", extension, " file."),
                  "invalid_resolved_file")
  resolved
}

.liss_bg_sav_signature <- function(path) {
  connection <- file(path, open = "rb", raw = TRUE)
  on.exit(close(connection), add = TRUE)
  signature <- readBin(connection, what = "raw", n = 4L)
  if (!identical(signature, charToRaw("$FL2")))
    .liss_bg_abort("Only standard SPSS $FL2 SAV input is supported; containers and ZSAV are rejected before reading.",
                  "unsupported_sav_signature")
  invisible(NULL)
}

.liss_bg_sha256_connection <- function(connection) {
  hash <- as.character(openssl::sha256(connection))
  attributes(hash) <- NULL
  hash
}

.liss_bg_file_state <- function(path) {
  info <- file.info(path)
  connection <- file(path, open = "rb", raw = TRUE)
  on.exit(close(connection), add = TRUE)
  hash <- .liss_bg_sha256_connection(connection)
  after <- file.info(path)
  if (anyNA(c(info$size, after$size, as.numeric(info$mtime), as.numeric(after$mtime))) ||
      !identical(info$size, after$size) || !identical(info$mtime, after$mtime))
    .liss_bg_abort("Input file changed during hashing.", "input_changed")
  list(size_bytes = info$size, mtime = info$mtime, sha256 = hash)
}

.liss_bg_unchanged <- function(path, state, source_index, kind) {
  current <- .liss_bg_file_state(path)
  if (!identical(current, state))
    .liss_bg_abort(paste0(kind, " input for source ", source_index,
                         " changed during attachment."), "input_changed")
  invisible(NULL)
}

.liss_bg_read_sav <- function(path, source_index) {
  n_warnings <- 0L
  frame <- tryCatch(withCallingHandlers(
    haven::read_sav(path, user_na = TRUE, .name_repair = "check_unique"),
    warning = function(w) {
      n_warnings <<- n_warnings + 1L
      invokeRestart("muffleWarning")
    }), error = function(e) .liss_bg_abort(paste0("Cannot read SAV source ", source_index,
                                                " (", basename(path), ")."),
                                          "sav_read_error"))
  if (n_warnings)
    .liss_bg_abort(paste0("SAV reader emitted unexpected warnings for source ",
                         source_index, "."), "sav_read_warning",
                  list(source_index = source_index, n_warnings = n_warnings))
  frame
}

.liss_bg_filename <- function(name, extension) {
  pattern <- paste0("(?i)^avars_([0-9]{6})_([a-z]{2})_([0-9]+[._][0-9]+p)\\.",
                     extension, "$")
  parts <- regmatches(name, regexec(pattern, name, perl = TRUE))[[1]]
  if (!length(parts))
    return(list(recognized = FALSE, basename = name, month = NA_real_,
                language = "UNKNOWN", version = "UNKNOWN"))
  list(recognized = TRUE, basename = name, month = as.numeric(parts[[2]]),
       language = parts[[3]], version = parts[[4]])
}

.liss_bg_filename_month <- function(filename, month, source_index, kind) {
  if (filename$recognized && filename$month != month)
    .liss_bg_abort(paste0("Recognized ", kind, " filename month disagrees in source ",
                         source_index, "."), "filename_month_mismatch")
  invisible(NULL)
}

.liss_bg_filename_language <- function(filenames, source_index) {
  languages <- vapply(Filter(function(x) x$recognized, filenames),
                      function(x) tolower(x$language), character(1))
  if (length(unique(languages)) > 1L)
    .liss_bg_abort(paste0("Recognized filename languages disagree in source ",
                         source_index, "."), "filename_language_mismatch")
  invisible(NULL)
}

.liss_bg_archive <- function(path, supplied_path, member, sav_hash, month,
                             sav_filenames, source_index, state) {
  .liss_bg_scalar(member, "archive_member")
  if (grepl("[/\\\\:]", member) || tolower(tools::file_ext(member)) != "sav")
    .liss_bg_abort("archive_member must be an exact basename-only SAV name, without path syntax.",
                  "invalid_archive_member")
  n_warnings <- 0L
  warning_handler <- function(w) {
    n_warnings <<- n_warnings + 1L
    invokeRestart("muffleWarning")
  }
  listing <- tryCatch(withCallingHandlers(utils::unzip(path, list = TRUE),
                                          warning = warning_handler),
                      error = function(e) NULL)
  if (n_warnings || is.null(listing) || !is.data.frame(listing) || !"Name" %in% names(listing))
    .liss_bg_abort(paste0("Cannot inspect ZIP archive for source ", source_index, "."),
                  "invalid_archive")
  if (anyDuplicated(listing$Name))
    .liss_bg_abort(paste0("ZIP archive for source ", source_index,
                         " contains duplicate member names."), "duplicate_archive_members")
  if (sum(listing$Name == member) != 1L)
    .liss_bg_abort(paste0("Exact SAV archive member is absent from source ", source_index, "."),
                  "archive_member_absent")
  connection <- tryCatch(withCallingHandlers(unz(path, member, open = "rb"),
                                              warning = warning_handler),
                          error = function(e) NULL)
  if (is.null(connection))
    .liss_bg_abort(paste0("Cannot open archive member for source ", source_index, "."),
                  "archive_member_read_error")
  on.exit(close(connection), add = TRUE)
  hash <- tryCatch(withCallingHandlers(.liss_bg_sha256_connection(connection),
                                       warning = warning_handler),
                   error = function(e) NULL)
  if (n_warnings)
    .liss_bg_abort(paste0("Archive reader emitted unexpected warnings for source ",
                         source_index, "."), "archive_read_warning",
                  list(source_index = source_index, n_warnings = n_warnings))
  if (is.null(hash) || !identical(hash, sav_hash))
    .liss_bg_abort(paste0("Archive member content differs from SAV source ", source_index, "."),
                  "archive_member_content_mismatch")
  archive_filename <- .liss_bg_filename(basename(path), "zip")
  supplied_filename <- .liss_bg_filename(basename(supplied_path), "zip")
  member_filename <- .liss_bg_filename(member, "sav")
  .liss_bg_filename_month(archive_filename, month, source_index, "archive")
  .liss_bg_filename_month(supplied_filename, month, source_index, "supplied archive")
  .liss_bg_filename_month(member_filename, month, source_index, "archive member")
  .liss_bg_filename_language(c(sav_filenames,
                              list(archive_filename, supplied_filename, member_filename)), source_index)
  list(status = "verified", archive_path_supplied = supplied_path,
       archive_path_resolved = path, archive_basename = basename(path),
       archive_member = member, hash_algorithm = "SHA256",
       archive_sha256 = state$sha256, member_sha256 = hash,
       archive_size_bytes = state$size_bytes, archive_mtime = state$mtime,
       archive_filename = archive_filename, supplied_archive_filename = supplied_filename,
       member_filename = member_filename)
}

.liss_bg_metadata <- function(x) {
  attributes <- attributes(x)
  attributes$names <- NULL
  labels <- attributes$labels
  if (!is.null(labels)) {
    order <- order(labels, names(labels), na.last = TRUE)
    attributes$labels <- labels[order]
  }
  if (length(attributes)) attributes <- attributes[sort(names(attributes))]
  list(type = typeof(x), class = class(x), attributes = attributes)
}

.liss_bg_metadata_receipt <- function(metadata) {
  serialized <- serialize(metadata, connection = NULL, version = 2L)
  hash <- as.character(openssl::sha256(serialized))
  attributes(hash) <- NULL
  list(type = metadata$type, class = metadata$class,
       attribute_names = names(metadata$attributes), metadata_sha256 = hash,
       hash_algorithm = "SHA256",
       serialization = list(format = "R serialize", version = 2L))
}
