# all respondent values and files are invented; source admission is mocked explicitly
.nrca025_columns <- function() c(
  "nomem_encr", "ca20g_m", "ca20g001", "ca20g002", "ca20g003", "ca20g082",
  "ca20g078", "ca20g005", "ca20g006", "ca20g007", "ca20g008", "ca20g009",
  "ca20g010", "ca20g011", "ca20g012", "ca20g013", "ca20g014", "ca20g015",
  "ca20g016", "ca20g017", "ca20g018", "ca20g019", "ca20g020", "ca20g021",
  "ca20g022", "ca20g023", "ca20g024", "ca20g025", "ca20g026", "ca20g027",
  "ca20g028", "ca20g079", "ca20g030", "ca20g034", "ca20g035", "ca20g036",
  "ca20g080", "ca20g041", "ca20g083", "ca20g084", "ca20g081", "ca20g048",
  "ca20g085", "ca20g086", "ca20g054", "ca20g055", "ca20g056", "ca20g057",
  "ca20g058", "ca20g087", "ca20g060", "ca20g061", "ca20g062", "ca20g063",
  "ca20g064", "ca20g065", "ca20g066", "ca20g067", "ca20g068", "ca20g069",
  "ca20g070", "ca20g071", "ca20g072", "ca20g073", "ca20g074", "ca20g075")

.nrca025_source <- function() list(filename = "ca20g_EN_1.0p.sav",
  sha256 = "d30acac3d2ca17533fb33ac724e43ff7dc40698139cec42221e1cae104684e47",
  size_bytes = 917084)

.nrca025_hash <- function(value) {
  digest <- as.character(openssl::sha256(serialize(value, NULL, version = 2L)))
  attributes(digest) <- NULL
  digest
}

.nrca025_equal <- function(left, right) identical(left, right) &&
  identical(serialize(left, NULL, version = 2L), serialize(right, NULL, version = 2L))

.nrca025_unmark <- function(frame) {
  attr(frame, "liss_source_nonresponse") <- NULL
  attr(frame, "source_nonresponse_readiness") <- NULL
  class(frame) <- c("tbl_df", "tbl", "data.frame")
  frame
}

.nrca025_values <- function() c(-9, -8, -9, -8, 8, 9,
  abs(as.double(0)), -abs(as.double(0)), -275, 1, 999999999, 0.25,
  1000000000, -1000000000, NA_real_, NaN, Inf, -Inf, haven::tagged_na("b"),
  -9999999999, -9999999998, -7, -10, 2^40, NA_real_)

.nrca025_frame <- function(values = .nrca025_values()) {
  count <- 5849L
  frame <- stats::setNames(lapply(.nrca025_columns(), function(column)
    if (column %in% sprintf("ca20g%03d", 71:74))
      rep(paste0("invented025_", column), count) else rep(250, count)), .nrca025_columns())
  frame$nomem_encr <- as.double(860000L + seq_len(count))
  attributes(frame$nomem_encr) <- list(label = "Number of household member encrypted",
    format.spss = "F7.0", display_width = 12L)
  amount_attributes <- function(label, format, large_labels = FALSE) list(
    label = label, na_values = c(9999999998, 9999999999),
    class = c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", "double"),
    format.spss = format, display_width = 15L,
    labels = if (large_labels) c("I prefer not to say" = 99999999998, "I don't know" = 99999999999)
      else c("I prefer not to say" = 9999999998, "I don't know" = 9999999999))
  gate_attributes <- function(label) list(label = label, format.spss = "F1.0", display_width = 15L,
    class = c("haven_labelled", "vctrs_vctr", "double"), labels = c(no = 0, yes = 1))
  # one complete invented original carries the admission metadata of all five authorities
  frame$ca20g012 <- c(-9999999999, -9999999998, rep(250, count - 2L))
  attributes(frame$ca20g012) <- amount_attributes(paste0(
    "What was the total balance of your banking account or giro (current accounts), ",
    "savings accounts, term deposit accounts, savings bonds or savings certificates and bank ",
    "savings schemes on 31 December 2019?"), "F11.0", TRUE)
  frame$ca20g014 <- c(-9, -8, rep(250, count - 2L))
  attributes(frame$ca20g014) <- amount_attributes(paste0(
    "What was the total sum of the guaranteed minimum payout of your single-premium ",
    "or life annuity insurances, or the total savings amount of your endowment insurance on 31 December 2019?"), "F6.0")
  frame$ca20g016 <- c(-9999999999, -9999999998, rep(250, count - 2L))
  attributes(frame$ca20g016) <- amount_attributes(paste0(
    "What was the total value of your investments (growth funds, share funds, ",
    "bonds, debentures, stocks, options, warrants) on 31 December 2019?"), "F11.0", TRUE)
  frame$ca20g023 <- c(-9, -8, rep(250, count - 2L))
  attributes(frame$ca20g023) <- amount_attributes(paste0(
    "Can you estimate the total sales value of the car(s), motorcycle(s), boat(s), ",
    "(static) caravan(s) that you owned on 31 December 2019?"), "F6.0")
  gate_prefix <- "On 31 December 2019, did you possess one or more of the following assets? - "
  gate_labels <- list(ca20g078 = paste0(gate_prefix,
      "Banking accounts, savings accounts, term deposit accounts, savings bonds or ",
      "savings certificates, banksparen"),
    ca20g005 = paste0(gate_prefix,
      "Single-premium insurance policy, life annuity insurance, endowment insurance (not linked to a mortgage)"),
    ca20g006 = paste0(gate_prefix,
      "Investments (growth funds, share funds, bonds, debentures, stocks, options, warrants, and so on)"),
    ca20g008 = paste0(gate_prefix, "Cars, motorcycles, boats, (static) caravans"))
  for (field in names(gate_labels)) {
    frame[[field]] <- rep(1, count)
    attributes(frame[[field]]) <- gate_attributes(gate_labels[[field]])
  }
  frame$ca20g025 <- c(as.double(values), rep(250, count - length(values)))
  zero_rows <- which(!is.na(values) & values == 0)
  if (length(zero_rows) == 2L)
    expect_identical(1 / frame$ca20g025[zero_rows], c(Inf, -Inf))
  attributes(frame$ca20g025) <- amount_attributes(paste0(
    "You indicated that you had loaned money to family, friends or acquaintances. What amount did you have ",
    "loaned out to family, friends or acquaintances on 31 December 2019?"), "F6.0")
  frame$ca20g010 <- rep(2, count)
  frame$ca20g010[c(1:4, 15L, 25L)] <- 1
  attributes(frame$ca20g010) <- list(label = paste0(
    "Did you have any money loaned out to family, friends or acquaintances on 31 December 2019?"),
    format.spss = "F1.0", display_width = 15L,
    class = c("haven_labelled", "vctrs_vctr", "double"), labels = c(yes = 1, no = 2))
  frame$ca20g026 <- c(1, -9, 999, NA_real_, rep(NA_real_, count - 4L))
  attributes(frame$ca20g026) <- list(label = paste0(
    "What amount did you have loaned out to family, friends or acquaintances - ",
    "To what category did the amount belong on 31 December 2019?"), na_values = 999,
    class = c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", "double"),
    format.spss = "F2.0", display_width = 15L,
    labels = stats::setNames(c(as.double(1:14), 999), c("less than € 500", "€ 500 to € 1,500",
      "€ 1,500 to € 2,500", "€ 2,500 to € 5,000", "€ 5,000 to € 7,500", "€ 7,500 to € 10,000",
      "€ 10,000 to € 12,000", "€ 12,000 to € 15,000", "€ 15,000 to € 20,000", "€ 20,000 to € 25,000",
      "€ 25,000 to € 50,000", "€ 50,000 to € 75,000", "€ 75,000 to € 100,000", "€ 100,000 or more", "I don't know")))
  frame$ca20g035 <- structure(frame$ca20g035, label = "invented025 non-target metadata", format.spss = "F8.0")
  frame$ca20g071 <- structure(frame$ca20g071, label = "invented025 text metadata")
  tibble::as_tibble(frame, .name_repair = "minimal")
}

.nrca025_fixture <- function(frame = .nrca025_frame(), .local_envir = parent.frame()) {
  directory <- withr::local_tempdir("invented_ca025_", .local_envir = .local_envir)
  source <- file.path(directory, "ca20g_EN_1.0p.sav")
  file.create(source)
  calls <- new.env(parent = emptyenv())
  calls$steps <- character()
  receipt <- .nrca025_source()
  if ("ca20g025" %in% names(frame)) {
    target_values <- as.double(unclass(frame$ca20g025))
    zero_rows <- which(!is.na(target_values) & target_values == 0)
    if (length(zero_rows) == 2L)
      expect_identical(1 / target_values[zero_rows], c(Inf, -Inf))
  }
  # the file and imported frame are invented; this seam does not prove real admission
  testthat::local_mocked_bindings(.liss_ca025_source_state = function(path) {
    calls$steps <- c(calls$steps, "state")
    receipt
  }, .liss_ca025_read_source = function(path) {
    calls$steps <- c(calls$steps, "read")
    frame
  }, .package = "lissr", .env = .local_envir)
  list(source = source, frame = frame, calls = calls, directory = directory)
}

.nrca025_expected <- function(frame) {
  values <- as.double(unclass(frame$ca20g025))
  rows <- which(is.finite(values) & values %in% c(-9, -8))
  keys <- sprintf("%.0f", as.double(unclass(frame$nomem_encr)))
  projected <- frame
  target <- values
  for (row in rows) target[[row]] <- NA_real_
  attributes(target) <- attributes(frame$ca20g025)
  projected$ca20g025 <- target
  count <- length(rows)
  ledger <- data.frame(source_cell_key = if (count) paste(.nrca025_source()$sha256, rows, "ca20g025", sep = ":") else character(),
    source_row_index = as.integer(rows), nomem_key = keys[rows],
    source_sha256 = rep(.nrca025_source()$sha256, count), source_filename = rep("ca20g_EN_1.0p.sav", count),
    raw_column = rep("ca20g025", count), case_id = if (count) ifelse(values[rows] == -9, "CA20G025_N01", "CA20G025_N02") else character(),
    original_value = values[rows], raw_value_repr = sprintf("%.0f", values[rows]), source_declared_missing = rep(FALSE, count),
    answer_status = rep("nonresponse_unknown", count), candidate_answer_status = rep(NA_character_, count),
    subtype_evidence = rep("unresolved", count), analysis_disposition = rep("missing_amount", count),
    analysis_value_after = rep(NA_real_, count), no_numeric_proposal = rep(TRUE, count),
    route_column = rep("ca20g010", count), route_value = rep(1, count), stringsAsFactors = FALSE)
  coverage <- data.frame(source_row_index = seq_along(values), nomem_key = keys,
    original_value = values, classified = seq_along(values) %in% rows, stringsAsFactors = FALSE)
  list(projection = projected, ledger = ledger, coverage = coverage, rows = rows)
}

.nrca025_reseal <- function(result) {
  record <- result$source_nonresponse
  record$original_digest <- .nrca025_hash(result$original_data)
  record$analysis_digest <- .nrca025_hash(.nrca025_unmark(result$analysis$data))
  record$ledger_digest <- .nrca025_hash(result$analysis$ledger)
  record$readiness_digest <- .nrca025_hash(result$source_nonresponse_readiness)
  record$digest <- NULL
  record$digest <- .nrca025_hash(record)
  result$source_nonresponse <- record
  marker <- attr(result$analysis$data, "liss_source_nonresponse")
  marker$parent_digest <- record$original_digest
  marker$analysis_digest <- record$analysis_digest
  marker$classification_digest <- record$digest
  attr(result$analysis$data, "liss_source_nonresponse") <- marker
  result$data <- result$analysis$data
  result
}

test_that("CA025 keeps an independent complete projection, typed ledger and coverage", {
  fixture <- .nrca025_fixture()
  expect_identical(1 / as.double(unclass(fixture$frame$ca20g025))[7:8], c(Inf, -Inf))
  before <- serialize(fixture$frame, NULL, version = 2L)
  expected <- .nrca025_expected(fixture$frame)
  result <- lissr::liss_classify_ca20g025(fixture$source)
  expect_identical(fixture$calls$steps, c("state", "read", "state"))
  expect_identical(names(result), c("original_data", "analysis", "data", "source_nonresponse", "source_nonresponse_readiness"))
  expect_identical(names(result$analysis), c("data", "ledger"))
  expect_identical(class(result), "liss_source_nonresponse_collection_result")
  expect_identical(class(result$data), c("liss_source_nonresponse_collection_result", "tbl_df", "tbl", "data.frame"))
  expect_true(.nrca025_equal(result$original_data, fixture$frame))
  expect_true(.nrca025_equal(.nrca025_unmark(result$data), expected$projection))
  expect_true(.nrca025_equal(result$analysis$ledger, expected$ledger))
  expect_true(.nrca025_equal(result$source_nonresponse$coverage, expected$coverage))
  expect_identical(1 / as.double(unclass(result$data$ca20g025))[7:8], c(Inf, -Inf))
  expect_identical(result$data, result$analysis$data)
  expect_identical(dim(result$data), c(5849L, 66L))
  expect_identical(names(result$data), .nrca025_columns())
  for (field in setdiff(.nrca025_columns(), "ca20g025"))
    expect_true(.nrca025_equal(result$data[[field]], fixture$frame[[field]]), info = field)
  expect_true(.nrca025_equal(attributes(result$data$ca20g025), attributes(fixture$frame$ca20g025)))
  expect_identical(serialize(fixture$frame, NULL, version = 2L), before)
  record <- result$source_nonresponse
  expect_identical(record$schema_version, "1.0.0")
  expect_identical(record$kind, "ca20g025_classification")
  expect_identical(record$authority_id, "CA20G025_SOURCE")
  expect_identical(record$authority_version, "1.0.0")
  expect_identical(record$source, .nrca025_source())
  expect_identical(record$construct, "money_loaned_out_to_family_friends_or_acquaintances")
  expect_identical(record$unit, "EUR_whole")
  expect_identical(record$reference_date, "2019-12-31")
  expect_identical(record$date_scope, "amount asked for on this date; distinct from September and October 2020 collection")
  expect_identical(record$ownership_scope, "source household-role and own/joint ownership context; no uniform household total or income interpretation")
  expect_identical(record$analysis_metadata_status, "retained source description only; does not attest changed analysis values")
  expect_identical(record$source_context, list(module = "ca", wave = "ca20g", language = "EN", release = "1.0p"))
  expect_identical(record$codebook, list(filename = "codebook_ca20g_EN_1.0.pdf",
    sha256 = "de43bf124f42083b8817b3cbd5ea1f5dd401a3e084227c850e9482a7756f8780", size_bytes = 601298))
  expect_identical(record$reader, list(package = "haven", user_na = TRUE, name_repair = "minimal", retained_scope = "exact_haven_import"))
  expect_identical(record$documented_amount_range, c(1, 999999999))
  expect_identical(record$source_metadata_nonresponse_forms, list(na_values = c(9999999998, 9999999999),
    value_labels = c(9999999998, 9999999999), pdf_forms = c(99999999998, 99999999999)))
  expect_identical(names(record$fields), "CA20G025_FIELD")
  expect_identical(names(record$fields$CA20G025_FIELD$metadata_sha256), c("nomem_encr", "ca20g025", "ca20g010"))
  expect_identical(record$contextual_metadata, list(ca20g026 = list(role = "context_only",
    reference_sha256 = "9cf94a1f7d85f7b5380f56501d8cd1dfc1dff0bce3b7edf2323037d957aa11b2",
    actual_sha256 = .nrca025_hash(attributes(fixture$frame$ca20g026)), required_for_admission = FALSE)))
  expect_identical(result$source_nonresponse_readiness$kind, "ca20g025_readiness")
  expect_identical(result$source_nonresponse_readiness$overall_amount_validity, "not_evaluated")
  field <- result$source_nonresponse_readiness$fields$CA20G025_FIELD
  expect_identical(field$evaluated_cells, 5849L)
  expect_identical(field$classified_cells, 4L)
  expect_identical(field$unresolved_cells, 0L)
  checked <- lissr::liss_validate_ca20g025(result)
  expect_true(checked$valid)
  expect_identical(checked$classified_cells, 4L)
  expect_identical(checked$validation, "structural")
  expect_identical(checked$source_consistency, "not_requested")
  expect_identical(checked$source_authentication, FALSE)
  expect_identical(checked$statistical_validity, FALSE)
})

test_that("CA025 refuses invalid paths and unverified invented bytes without parsing", {
  directory <- withr::local_tempdir("invented_ca025_bad_bytes_")
  source <- file.path(directory, "ca20g_EN_1.0p.sav")
  writeBin(charToRaw("explicitly invented wrong bytes"), source)
  parsed <- 0L
  testthat::local_mocked_bindings(read_sav = function(...) {parsed <<- parsed + 1L; stop("unchecked bytes reached Haven")}, .package = "haven")
  expect_error(lissr::liss_classify_ca20g025(), class = "liss_ca20g025_blocked")
  for (path in list(NULL, NA_character_, "", FALSE, c(source, source), factor(source),
    structure(source, label = "invented claim"), directory, file.path(directory, "missing.sav"), source))
    expect_error(lissr::liss_classify_ca20g025(path), class = "liss_ca20g025_blocked")
  expect_error(lissr:::.liss_ca025_read_source(source), class = "liss_ca20g025_blocked")
  writeBin(raw(917084L), source)
  expect_error(lissr::liss_classify_ca20g025(source), class = "liss_ca20g025_blocked")
  expect_error(lissr:::.liss_ca025_read_source(source), class = "liss_ca20g025_blocked")
  for (filename in c("ca20g_NL_1.0p.sav", "ca20g_EN_1.1p.sav", "ca21h_EN_1.0p.sav")) {
    path <- file.path(directory, filename)
    file.create(path)
    expect_error(lissr::liss_classify_ca20g025(path), class = "liss_ca20g025_blocked")
  }
  expect_identical(parsed, 0L)
})

test_that("CA025 gives Haven exactly the independently checked invented buffer and settings", {
  bytes <- charToRaw("explicitly invented025 verified-buffer probe")
  digest <- as.character(openssl::sha256(bytes))
  attributes(digest) <- NULL
  path <- withr::local_tempfile(fileext = ".sav")
  writeBin(bytes, path)
  captured <- NULL
  parsed <- 0L
  expected <- .nrca025_frame()
  testthat::local_mocked_bindings(.liss_ca025_policy = function()
    list(source = list(size_bytes = length(bytes), sha256 = digest)), .package = "lissr")
  testthat::local_mocked_bindings(read_sav = function(file, user_na, .name_repair) {
    parsed <<- parsed + 1L
    captured <<- list(bytes = file, user_na = user_na, name_repair = .name_repair)
    expected
  }, .package = "haven")
  expect_identical(lissr:::.liss_ca025_read_source(path), expected)
  expect_identical(captured, list(bytes = bytes, user_na = TRUE, name_repair = "minimal"))
  for (replacement in list(c(bytes, as.raw(1)), bytes[-1L], rev(bytes))) {
    writeBin(replacement, path)
    expect_error(lissr:::.liss_ca025_read_source(path), class = "liss_ca20g025_blocked")
  }
  expect_identical(parsed, 1L)
  writeBin(bytes, path)
  local({
    testthat::local_mocked_bindings(read_sav = function(...) stop("invented parser failure"),
      .package = "haven", .env = environment())
    expect_error(lissr:::.liss_ca025_read_source(path), class = "liss_ca20g025_blocked")
  })
  expect_identical(parsed, 1L)
  unlink(path)
  expect_error(suppressWarnings(lissr:::.liss_ca025_read_source(path)), class = "liss_ca20g025_blocked")
})

test_that("CA025 checks both source identities before returning any result", {
  for (phase in c(1L, 2L)) for (field in c("filename", "sha256", "size_bytes")) local({
    fixture <- .nrca025_fixture(.local_envir = environment())
    result <- lissr::liss_classify_ca20g025(fixture$source)
    states <- 0L
    fixture$calls$steps <- character()
    testthat::local_mocked_bindings(.liss_ca025_source_state = function(path) {
      states <<- states + 1L
      receipt <- .nrca025_source()
      if (states == phase) receipt[[field]] <- switch(field,
        filename = "ca21h_EN_1.0p.sav", sha256 = strrep("0", 64L), size_bytes = 917085)
      receipt
    }, .package = "lissr", .env = environment())
    expect_error(lissr::liss_classify_ca20g025(fixture$source), class = "liss_ca20g025_blocked")
    expect_identical(fixture$calls$steps, if (phase == 1L) character() else "read")
    states <- 0L
    fixture$calls$steps <- character()
    expect_error(lissr::liss_validate_ca20g025(result, source = fixture$source), class = "liss_ca20g025_blocked")
    expect_identical(fixture$calls$steps, if (phase == 1L) character() else "read")
  })
})

test_that("CA025 refuses malformed complete schema, keys and admission metadata before projection", {
  original <- .nrca025_frame()
  projected <- 0L
  testthat::local_mocked_bindings(.liss_ca025_build = function(input, checked) {
    force(checked)
    projected <<- projected + 1L
    stop("malformed source reached projection")
  }, .package = "lissr")
  replace_column <- function(frame, column) {
    values <- unclass(frame)
    values$ca20g035 <- column
    attributes(values) <- attributes(frame)
    values
  }
  changes <- list(
    field_alias = function(x) { names(x)[28L] <- "invented_target_alias"; x },
    column_order = function(x) x[, rev(names(x)), drop = FALSE],
    row_names_order = function(x) { attr(x, "row.names") <- rev(seq_len(nrow(x))); x },
    incomplete_rows = function(x) x[-1L, , drop = FALSE],
    empty_rows = function(x) x[FALSE, , drop = FALSE],
    missing_context_column = function(x) { x$ca20g026 <- NULL; x },
    missing_column = function(x) { x$ca20g075 <- NULL; x },
    extra_column = function(x) { x$invented_extra <- rep(0, nrow(x)); x },
    wrong_type = function(x) { x$ca20g001 <- as.integer(x$ca20g001); x },
    extra_attribute = function(x) { attr(x, "invented_extra") <- TRUE; x },
    wrong_class = function(x) { class(x) <- "data.frame"; x },
    non_frame = function(x) unclass(x),
    target_metadata = function(x) { attr(x$ca20g025, "label") <- "invented wrong field"; x },
    gate_metadata = function(x) { attr(x$ca20g010, "label") <- "invented wrong gate"; x },
    gate_attribute_order = function(x) { attributes(x$ca20g010) <- rev(attributes(x$ca20g010)); x },
    id_metadata = function(x) { attr(x$nomem_encr, "label") <- "invented wrong id"; x },
    duplicate_key = function(x) { x$nomem_encr[2L] <- x$nomem_encr[1L]; x },
    missing_key = function(x) { x$nomem_encr[1L] <- NA_real_; x },
    negative_key = function(x) { x$nomem_encr[1L] <- -1; x },
    fractional_key = function(x) { x$nomem_encr[1L] <- 1.5; x },
    lossy_key = function(x) { x$nomem_encr[1L] <- 2^53; x },
    infinite_key = function(x) { x$nomem_encr[1L] <- Inf; x },
    short_column = function(x) replace_column(x, -325),
    long_column = function(x) replace_column(x, rep(-325, 5850L)),
    matrix_column = function(x) replace_column(x, matrix(rep(-325, 5849L), ncol = 1L)))
  for (name in names(changes)) local({
    malformed <- changes[[name]](original)
    before <- serialize(malformed, NULL, version = 2L)
    fixture <- .nrca025_fixture(malformed, .local_envir = environment())
    expect_error(lissr::liss_classify_ca20g025(fixture$source), class = "liss_ca20g025_blocked", info = name)
    expect_identical(serialize(malformed, NULL, version = 2L), before, info = name)
  })
  expect_identical(projected, 0L)
})

test_that("CA025 retains zero and upper lossless keys without renumbering", {
  frame <- .nrca025_frame()
  frame$nomem_encr[c(1L, 5849L)] <- c(0, 2^53 - 1)
  fixture <- .nrca025_fixture(frame)
  result <- lissr::liss_classify_ca20g025(fixture$source)
  expect_identical(result$analysis$ledger$nomem_key[1L], "0")
  expect_identical(result$source_nonresponse$coverage$nomem_key[5849L], "9007199254740991")
  expect_identical(result$original_data$nomem_encr, frame$nomem_encr)
  expect_true(lissr::liss_validate_ca20g025(result)$valid)
})

test_that("CA025 requires010 gate one only for exact codes and permits missing026 and eligible NA", {
  for (gate in c(0, -1, 2, 1.5, NA_real_, NaN, Inf, -Inf, haven::tagged_na("c"))) local({
    frame <- .nrca025_frame()
    frame$ca20g010[1L] <- gate
    fixture <- .nrca025_fixture(frame, .local_envir = environment())
    expect_error(lissr::liss_classify_ca20g025(fixture$source), class = "liss_ca20g025_blocked")
  })
  frame <- .nrca025_frame()
  frame$ca20g010[5:10] <- c(0, -1, NA_real_, NaN, Inf, -Inf)
  frame$ca20g006 <- rep(0, 5849L)
  frame$ca20g008 <- rep(0, 5849L)
  frame$ca20g078 <- rep(0, 5849L)
  frame$ca20g026[] <- NA_real_
  fixture <- .nrca025_fixture(frame)
  result <- lissr::liss_classify_ca20g025(fixture$source)
  expect_true(lissr::liss_validate_ca20g025(result)$valid)
  for (column in c("ca20g010", "ca20g006", "ca20g008", "ca20g078", "ca20g026"))
    expect_identical(result$data[[column]], frame[[column]], info = column)
  expect_identical(as.double(unclass(result$data$ca20g025))[5:10], c(8, 9, abs(as.double(0)), -abs(as.double(0)), -275, 1))
  expect_identical(as.double(unclass(result$data$ca20g010))[15L], 1)
  expect_true(is.na(result$data$ca20g025[15L]))
  expect_false(result$source_nonresponse$coverage$classified[15L])
  expect_false(15L %in% result$analysis$ledger$source_row_index)
  expect_true(all(is.na(result$data$ca20g026)))
  expect_identical(result$analysis$ledger$source_row_index, 1:4)
})

test_that("CA025 records026 attributes as context and admits consistent contextual substitutions", {
  fixture <- .nrca025_fixture()
  original <- fixture$frame
  original_result <- lissr::liss_classify_ca20g025(fixture$source)
  expect_identical(original_result$source_nonresponse$contextual_metadata$ca20g026$actual_sha256,
    "9cf94a1f7d85f7b5380f56501d8cd1dfc1dff0bce3b7edf2323037d957aa11b2")
  changes <- list(
    changed_label = function(x) { attr(x$ca20g026, "label") <- "invented contextual label"; x },
    no_attributes = function(x) { attributes(x$ca20g026) <- NULL; x },
    attribute_order = function(x) { attributes(x$ca20g026) <- rev(attributes(x$ca20g026)); x },
    missing_values = function(x) { x$ca20g026[] <- NA_real_; x },
    ordinary_values = function(x) { x$ca20g026[] <- -123.5; x })
  for (name in names(changes)) {
    changed <- changes[[name]](original)
    rebuilt <- lissr:::.liss_ca025_build(changed,
      lissr:::.liss_ca025_check_raw(changed, lissr:::.liss_ca025_policy()))
    context <- rebuilt$source_nonresponse$contextual_metadata$ca20g026
    expect_identical(names(context), c("role", "reference_sha256", "actual_sha256", "required_for_admission"), info = name)
    expect_identical(context$role, "context_only", info = name)
    expect_identical(context$reference_sha256,
      "9cf94a1f7d85f7b5380f56501d8cd1dfc1dff0bce3b7edf2323037d957aa11b2", info = name)
    expect_identical(context$actual_sha256, .nrca025_hash(attributes(changed$ca20g026)), info = name)
    expect_identical(context$required_for_admission, FALSE, info = name)
    expect_identical(rebuilt$data$ca20g026, changed$ca20g026, info = name)
    expect_true(lissr::liss_validate_ca20g025(rebuilt)$valid, info = name)
    expect_error(lissr::liss_validate_ca20g025(rebuilt, source = fixture$source),
      class = "liss_ca20g025_blocked", info = name)
  }
  changed <- original
  attr(changed$ca20g026, "label") <- "invented contextual creation"
  local({
    contextual_fixture <- .nrca025_fixture(changed, .local_envir = environment())
    result <- lissr::liss_classify_ca20g025(contextual_fixture$source)
    expect_true(lissr::liss_validate_ca20g025(result)$valid)
    expect_identical(lissr::liss_validate_ca20g025(result, source = contextual_fixture$source)$source_consistency, "matched")
    expect_false(identical(result$source_nonresponse$contextual_metadata$ca20g026$actual_sha256,
      result$source_nonresponse$contextual_metadata$ca20g026$reference_sha256))
  })
})

test_that("CA025 rejects every unsupported positive form for whole creation and retained-original validation", {
  fixture <- .nrca025_fixture()
  result <- lissr::liss_classify_ca20g025(fixture$source)
  testthat::local_mocked_bindings(.liss_ca025_build = function(input, checked) {
    force(checked)
    stop("positive original reached projection")
  }, .package = "lissr")
  for (code in c(9999999998, 9999999999, 99999999998, 99999999999)) local({
    frame <- .nrca025_frame()
    frame$ca20g025[4L] <- code
    before <- serialize(frame, NULL, version = 2L)
    positive_fixture <- .nrca025_fixture(frame, .local_envir = environment())
    expect_error(lissr::liss_classify_ca20g025(positive_fixture$source), class = "liss_ca20g025_blocked")
    expect_identical(serialize(frame, NULL, version = 2L), before)
  })
  for (code in c(9999999998, 9999999999, 99999999998, 99999999999)) {
    changed <- result
    changed$original_data$ca20g025[4L] <- code
    changed <- .nrca025_reseal(changed)
    before <- serialize(changed, NULL, version = 2L)
    fixture$calls$steps <- character()
    for (source in list(NULL, fixture$source))
      expect_error(lissr::liss_validate_ca20g025(changed, source = source),
        class = "liss_ca20g025_blocked", regexp = "positive")
    expect_identical(fixture$calls$steps, character())
    expect_identical(serialize(changed, NULL, version = 2L), before)
  }
})

test_that("CA025 no-hit sources retain complete coverage and the complete typed empty ledger", {
  fixture <- .nrca025_fixture(.nrca025_frame(c(8, 9, abs(as.double(0)), -abs(as.double(0)), -12.5, 0.25, -9999999999, -9999999998, NA_real_, NaN, Inf, -Inf)))
  result <- lissr::liss_classify_ca20g025(fixture$source)
  expect_identical(serialize(.nrca025_unmark(result$data), NULL, version = 2L),
    serialize(fixture$frame, NULL, version = 2L))
  expect_identical(nrow(result$source_nonresponse$coverage), 5849L)
  expect_identical(result$source_nonresponse$coverage$classified, rep(FALSE, 5849L))
  expected_ledger <- data.frame(source_cell_key = character(), source_row_index = integer(),
    nomem_key = character(), source_sha256 = character(), source_filename = character(), raw_column = character(),
    case_id = character(), original_value = double(), raw_value_repr = character(), source_declared_missing = logical(),
    answer_status = character(), candidate_answer_status = character(), subtype_evidence = character(),
    analysis_disposition = character(), analysis_value_after = double(), no_numeric_proposal = logical(),
    route_column = character(), route_value = double(), stringsAsFactors = FALSE)
  expect_identical(result$analysis$ledger, expected_ledger)
  expect_identical(result$source_nonresponse_readiness$status, "passed")
  expect_identical(result$source_nonresponse_readiness$fields$CA20G025_FIELD$classified_cells, 0L)
  expect_identical(1 / as.double(unclass(fixture$frame$ca20g025))[3:4], c(Inf, -Inf))
  expect_identical(1 / as.double(unclass(result$data$ca20g025))[3:4], c(Inf, -Inf))
  expect_true(lissr::liss_validate_ca20g025(result)$valid)
})

test_that("CA025 closed reconstruction rejects stale and resealed payload, marker and readiness changes", {
  fixture <- .nrca025_fixture()
  result <- lissr::liss_classify_ca20g025(fixture$source)
  for (piece in list(result$data, result$original_data, result$analysis))
    expect_error(lissr::liss_validate_ca20g025(piece), class = "liss_ca20g025_blocked")
  changes <- list(
    missing_original = function(x) { x$original_data <- NULL; x },
    missing_analysis = function(x) { x$analysis <- NULL; x },
    extra_wrapper_field = function(x) { x$invented_extra <- TRUE; x },
    wrapper_attribute = function(x) { attr(x, "invented_extra") <- TRUE; x },
    wrapper_class = function(x) { class(x) <- "list"; x },
    analysis_shape = function(x) { x$analysis$invented_extra <- TRUE; x },
    analysis_attribute = function(x) { attr(x$analysis, "invented_extra") <- TRUE; x },
    duplicate_data = function(x) { x$data$ca20g025[1L] <- 1; x },
    locked_value = function(x) { x$analysis$data$ca20g025[1L] <- 1; x },
    positive_amount_to_missing = function(x) { x$analysis$data$ca20g025[5L] <- NA_real_; x },
    non_target014 = function(x) { x$analysis$data$ca20g014[1L] <- NA_real_; x },
    non_target012 = function(x) { x$analysis$data$ca20g012[1L] <- NA_real_; x },
    non_target016 = function(x) { x$analysis$data$ca20g016[1L] <- NA_real_; x },
    non_target023 = function(x) { x$analysis$data$ca20g023[1L] <- NA_real_; x },
    bracket_inference = function(x) { x$analysis$data$ca20g026[1L] <- 2; x },
    target_metadata = function(x) { attr(x$analysis$data$ca20g025, "label") <- "invented changed label"; x },
    ledger_row = function(x) { x$analysis$ledger$source_row_index[1L] <- 4L; x },
    ledger_type = function(x) { x$analysis$ledger$source_row_index <- as.double(x$analysis$ledger$source_row_index); x },
    ledger_value = function(x) { x$analysis$ledger$original_value[1L] <- -8; x },
    ledger_key = function(x) { x$analysis$ledger$source_cell_key[1L] <- "invented forged key"; x },
    ledger_subtype = function(x) { x$analysis$ledger$answer_status[1L] <- "dont_know"; x },
    ledger_numeric_candidate = function(x) { x$analysis$ledger$analysis_value_after[1L] <- 10; x },
    readiness = function(x) { x$source_nonresponse_readiness$status <- "certified"; x },
    overall_amount = function(x) { x$source_nonresponse_readiness$overall_amount_validity <- "passed"; x },
    ownership = function(x) { x$source_nonresponse$ownership_scope <- "uniform household income"; x },
    reference_date = function(x) { x$source_nonresponse$reference_date <- "2020-01-01"; x },
    date_scope = function(x) { x$source_nonresponse$date_scope <- "collection time"; x },
    metadata_forms = function(x) { x$source_nonresponse$source_metadata_nonresponse_forms$value_labels <- c(8, 9); x },
    analysis_metadata_claim = function(x) { x$source_nonresponse$analysis_metadata_status <- "attested"; x },
    fields = function(x) { x$source_nonresponse$fields$CA20G012_FIELD <- list(); x },
    context_actual_digest = function(x) { x$source_nonresponse$contextual_metadata$ca20g026$actual_sha256 <- strrep("0", 64L); x },
    context_reference_digest = function(x) { x$source_nonresponse$contextual_metadata$ca20g026$reference_sha256 <- strrep("0", 64L); x },
    context_admission = function(x) { x$source_nonresponse$contextual_metadata$ca20g026$required_for_admission <- TRUE; x },
    context_extra = function(x) { x$source_nonresponse$contextual_metadata$ca20g026$invented_extra <- TRUE; x },
    marker_missing = function(x) { attr(x$analysis$data, "liss_source_nonresponse") <- NULL; x },
    marker_field = function(x) { attr(x$analysis$data, "liss_source_nonresponse")$kind <- "invented"; x },
    marker_attribute = function(x) { attr(attr(x$analysis$data, "liss_source_nonresponse"), "invented_extra") <- TRUE; x },
    frame_readiness_missing = function(x) { attr(x$analysis$data, "source_nonresponse_readiness") <- NULL; x },
    frame_readiness_value = function(x) { attr(x$analysis$data, "source_nonresponse_readiness")$status <- "certified"; x },
    native_schema = function(x) { x$source_nonresponse$schema_version <- "4.0.0"; x })
  for (name in names(changes)) {
    changed <- changes[[name]](result)
    before <- serialize(changed, NULL, version = 2L)
    expect_error(lissr::liss_validate_ca20g025(changed), class = "liss_ca20g025_blocked", info = name)
    expect_identical(serialize(changed, NULL, version = 2L), before, info = name)
    if (name == "duplicate_data") next
    resealed <- tryCatch(.nrca025_reseal(changed), error = identity)
    if (!inherits(resealed, "error"))
      expect_error(lissr::liss_validate_ca20g025(resealed), class = "liss_ca20g025_blocked",
        info = paste(name, "resealed"))
  }
})

test_that("CA025 strict equality distinguishes signed zeros, missing payloads and attribute order", {
  fixture <- .nrca025_fixture()
  result <- lissr::liss_classify_ca20g025(fixture$source)
  negative_zero <- -abs(as.double(0))
  positive_zero <- abs(negative_zero)
  expect_true(identical(negative_zero, positive_zero))
  expect_identical(1 / negative_zero, -Inf)
  expect_identical(1 / positive_zero, Inf)
  expect_false(identical(serialize(negative_zero, NULL, version = 2L),
    serialize(positive_zero, NULL, version = 2L)))
  zero <- result
  target <- as.double(unclass(zero$analysis$data$ca20g025))
  expect_identical(1 / target[7L], Inf)
  target <- c(target[seq_len(6L)], negative_zero, target[8:length(target)])
  attributes(target) <- attributes(result$analysis$data$ca20g025)
  zero$analysis$data$ca20g025 <- target
  zero$data <- zero$analysis$data
  expect_identical(1 / as.double(unclass(zero$data$ca20g025))[7L], -Inf)
  reordered <- result
  attributes(reordered$analysis$data$ca20g025) <- rev(attributes(reordered$analysis$data$ca20g025))
  reordered$data <- reordered$analysis$data
  tagged <- result
  tagged$analysis$ledger$analysis_value_after[1L] <- haven::tagged_na("a")
  tagged$data <- tagged$analysis$data
  expect_true(is.na(tagged$analysis$ledger$analysis_value_after[1L]))
  expect_identical(haven::na_tag(tagged$analysis$ledger$analysis_value_after[1L]), "a")
  expect_true(is.na(haven::na_tag(result$analysis$ledger$analysis_value_after[1L])))
  for (changed in list(zero, reordered, tagged)) {
    expect_true(identical(changed, result))
    expect_false(identical(serialize(changed, NULL, version = 2L), serialize(result, NULL, version = 2L)))
    for (source in list(NULL, fixture$source)) {
      expect_error(lissr::liss_validate_ca20g025(changed, source = source), class = "liss_ca20g025_blocked")
      expect_error(lissr::liss_validate_ca20g025(.nrca025_reseal(changed), source = source),
        class = "liss_ca20g025_blocked")
    }
  }
})

test_that("CA025 structural consistency permits substituted originals but source mode compares every column", {
  fixture <- .nrca025_fixture()
  original <- fixture$frame
  result <- lissr::liss_classify_ca20g025(fixture$source)
  changes <- list(
    bracket_value = function(x) { x$ca20g026[1L] <- 2; x },
    non_target_value = function(x) { x$ca20g035[1L] <- 42; x },
    non_target_metadata = function(x) { attr(x$ca20g035, "label") <- "invented substitution"; x },
    non_target_attribute_order = function(x) { attributes(x$ca20g035) <- rev(attributes(x$ca20g035)); x },
    target_amount = function(x) { x$ca20g025[5L] <- 18; x })
  for (name in names(changes)) {
    changed <- changes[[name]](original)
    rebuilt <- lissr:::.liss_ca025_build(changed,
      lissr:::.liss_ca025_check_raw(changed, lissr:::.liss_ca025_policy()))
    checked <- lissr::liss_validate_ca20g025(rebuilt)
    expect_true(checked$valid, info = name)
    expect_identical(checked$source_authentication, FALSE, info = name)
    expect_identical(checked$statistical_validity, FALSE, info = name)
    expect_error(lissr::liss_validate_ca20g025(rebuilt, source = fixture$source),
      class = "liss_ca20g025_blocked", info = name)
  }
  checked <- lissr::liss_validate_ca20g025(result, source = fixture$source)
  expect_identical(checked$source_consistency, "matched")
  expect_identical(checked$validation, "source_consistency")
  expect_identical(checked$source_authentication, FALSE)
})

test_that("CA025 reference attributes require R identity as well as equal serialized contents", {
  original <- .nrca025_frame()
  attr(original$ca20g035, "invented_context") <- new.env(parent = emptyenv())
  fixture <- .nrca025_fixture(original)
  result <- lissr::liss_classify_ca20g025(fixture$source)
  changed <- result
  attr(changed$analysis$data$ca20g035, "invented_context") <- new.env(parent = emptyenv())
  changed$data <- changed$analysis$data
  expect_false(identical(changed, result))
  expect_identical(serialize(.nrca025_unmark(changed$analysis$data), NULL, version = 2L),
    serialize(.nrca025_unmark(result$analysis$data), NULL, version = 2L))
  expect_error(lissr::liss_validate_ca20g025(changed), class = "liss_ca20g025_blocked")
  expect_error(lissr::liss_validate_ca20g025(.nrca025_reseal(changed)), class = "liss_ca20g025_blocked")
  substituted <- original
  attr(substituted$ca20g035, "invented_context") <- new.env(parent = emptyenv())
  expect_false(identical(substituted, original))
  expect_identical(serialize(substituted, NULL, version = 2L), serialize(original, NULL, version = 2L))
  rebuilt <- lissr:::.liss_ca025_build(substituted,
    lissr:::.liss_ca025_check_raw(substituted, lissr:::.liss_ca025_policy()))
  expect_true(lissr::liss_validate_ca20g025(rebuilt)$valid)
  expect_error(lissr::liss_validate_ca20g025(rebuilt, source = fixture$source), class = "liss_ca20g025_blocked")
})

test_that("CA025 permits moved guard placement only with exact payload, marker and readiness values", {
  fixture <- .nrca025_fixture()
  result <- lissr::liss_classify_ca20g025(fixture$source)
  changed <- result
  frame_class <- class(changed$analysis$data)
  class(changed$analysis$data) <- NULL
  class(changed$analysis$data) <- frame_class
  changed$data <- changed$analysis$data
  expect_true(identical(changed, result))
  expect_false(identical(serialize(changed, NULL, version = 2L), serialize(result, NULL, version = 2L)))
  expect_identical(serialize(.nrca025_unmark(changed$data), NULL, version = 2L),
    serialize(.nrca025_unmark(result$data), NULL, version = 2L))
  expect_true(lissr::liss_validate_ca20g025(changed)$valid)
  expect_identical(lissr::liss_validate_ca20g025(changed, source = fixture$source)$source_consistency, "matched")
})

test_that("CA025 whole-result V2 and default V3 files preserve contextual, no-code and reference cases", {
  frames <- list(ordinary = .nrca025_frame(), no_code = .nrca025_frame(c(8, 9, abs(as.double(0)), -abs(as.double(0)), NA_real_, -10)),
    contextual = .nrca025_frame(), reference = .nrca025_frame(), moved_guard = .nrca025_frame())
  attr(frames$contextual$ca20g026, "label") <- "invented contextual transport"
  attr(frames$reference$ca20g035, "invented_context") <- new.env(parent = emptyenv())
  for (name in names(frames)) local({
    fixture <- .nrca025_fixture(frames[[name]], .local_envir = environment())
    result <- lissr::liss_classify_ca20g025(fixture$source)
    if (name == "moved_guard") {
      frame_class <- class(result$analysis$data)
      class(result$analysis$data) <- NULL
      class(result$analysis$data) <- frame_class
      result$data <- result$analysis$data
    }
    before <- serialize(result, NULL, version = 2L)
    for (version in c("2", "default")) {
      path <- withr::local_tempfile(pattern = paste0("invented_ca025_", name, "_", version, "_"), fileext = ".rds")
      if (version == "2") saveRDS(result, path, version = 2L) else saveRDS(result, path)
      file_hash <- as.character(openssl::sha256(readBin(path, "raw", n = file.info(path)$size)))
      expect_identical(infoRDS(path)$version, if (version == "2") 2L else 3L)
      expect_identical(as.character(openssl::sha256(readBin(path, "raw", n = file.info(path)$size))), file_hash)
      restored <- readRDS(path)
      expect_identical(serialize(restored, NULL, version = 2L), before, info = paste(name, version))
      expect_true(lissr::liss_validate_ca20g025(restored)$valid, info = paste(name, version))
      if (name == "reference") {
        expect_error(lissr::liss_validate_ca20g025(restored, source = fixture$source), class = "liss_ca20g025_blocked")
      } else {
        expect_identical(lissr::liss_validate_ca20g025(restored, source = fixture$source)$source_consistency, "matched")
      }
      expect_identical(as.character(openssl::sha256(readBin(path, "raw", n = file.info(path)$size))), file_hash)
    }
    expect_identical(serialize(result, NULL, version = 2L), before)
  })
})

test_that("CA025 authority is separately pinned and has no runtime CA023 or frozen-engine dependency", {
  policy <- lissr:::.liss_ca025_policy()
  expect_identical(policy$authority_id, "CA20G025_SOURCE")
  expect_identical(policy$authority_version, "1.0.0")
  expect_identical(policy$raw_column, "ca20g025")
  expect_identical(policy$route_column, "ca20g010")
  expect_identical(policy$source, .nrca025_source())
  expect_identical(policy$columns, .nrca025_columns())
  expect_identical(policy$codes, c(-9, -8))
  expect_identical(policy$required_route_for_coded_rows, 1)
  expect_identical(policy$unsupported_positive_forms, c(9999999998, 9999999999, 99999999998, 99999999999))
  expect_identical(policy$consumers, list())
  expect_identical(policy$case_ids, c("CA20G025_N01", "CA20G025_N02"))
  expect_identical(policy$metadata_sha256, list(
    nomem_encr = "a85d081e81be1b50c2ac82f5fe3012b3275d73d3c7556796b5bd923b8f90a1d1",
    ca20g025 = "5e662b5f3784fd751c7c39ad9760e35e51f4286a7a405b95c8d611337e26464f",
    ca20g010 = "cb0d2eacccd20532d5035ec77340a2ca6590f4900ae1f424a3a4251b398b50a9"))
  expect_identical(policy$contextual_metadata_sha256,
    list(ca20g026 = "9cf94a1f7d85f7b5380f56501d8cd1dfc1dff0bce3b7edf2323037d957aa11b2"))
  for (column in names(policy$metadata_sha256))
    expect_identical(.nrca025_hash(attributes(.nrca025_frame()[[column]])), policy$metadata_sha256[[column]], info = column)
  expect_identical(lissr:::.liss_ca025_policy_sha256(),
    "c8797a949d41fce0a93824cc62326af18fa49b60b4a08d44af689c55b594d293")
  expect_identical(lissr:::.liss_ca014_policy()$authority_id, "CA20G014_SOURCE")
  expect_identical(lissr:::.liss_ca012_policy()$authority_id, "CA20G012_SOURCE")
  expect_identical(lissr:::.liss_ca016_policy()$authority_id, "CA20G016_SOURCE")
  expect_identical(lissr:::.liss_ca023_policy()$authority_id, "CA20G023_SOURCE")
  for (hash in c(lissr:::.liss_ca012_policy_sha256(), lissr:::.liss_ca014_policy_sha256(),
    lissr:::.liss_ca016_policy_sha256(), lissr:::.liss_ca023_policy_sha256()))
    expect_false(identical(lissr:::.liss_ca025_policy_sha256(), hash))
  engine <- lissr:::.liss_applied_engine()
  expect_length(engine$functions, 113L)
  expect_false(any(grepl("ca025", names(engine$functions))))
  fixture <- .nrca025_fixture()
  local({
    testthat::local_mocked_bindings(.liss_ca012_policy = function() stop("CA025 borrowed CA012 policy"),
      .liss_ca014_policy = function() stop("CA025 borrowed CA014 policy"),
      .liss_ca016_policy = function() stop("CA025 borrowed CA016 policy"),
      .liss_ca023_policy = function() stop("CA025 borrowed CA023 policy"),
      .liss_ca023_policy_path = function() stop("CA025 borrowed CA023 path"), .package = "lissr", .env = environment())
    result <- lissr::liss_classify_ca20g025(fixture$source)
    expect_true(lissr::liss_validate_ca20g025(result)$valid)
  })
  path <- lissr:::.liss_ca025_policy_path()
  expect_identical(basename(path), "source_nonresponse_ca20g025.yml")
  changed <- withr::local_tempfile(fileext = ".yml")
  writeLines(c(readLines(path, warn = FALSE), "unreviewed: true"), changed)
  testthat::local_mocked_bindings(.liss_ca025_policy_path = function() changed, .package = "lissr")
  expect_error(lissr:::.liss_ca025_policy(), class = "liss_ca20g025_blocked")
})



test_that("CA025 and the four existing authorities validate only their own complete carrier", {
  fixture <- .nrca025_fixture()
  frame <- fixture$frame
  testthat::local_mocked_bindings(
    .liss_ca012_source_state = function(path) .nrca025_source(), .liss_ca012_read_source = function(path) frame,
    .liss_ca014_source_state = function(path) .nrca025_source(), .liss_ca014_read_source = function(path) frame,
    .liss_ca016_source_state = function(path) .nrca025_source(), .liss_ca016_read_source = function(path) frame,
    .liss_ca023_source_state = function(path) .nrca025_source(), .liss_ca023_read_source = function(path) frame,
    .package = "lissr")
  creators <- list(CA012 = lissr::liss_classify_ca20g012, CA014 = lissr::liss_classify_ca20g014,
    CA016 = lissr::liss_classify_ca20g016, CA023 = lissr::liss_classify_ca20g023,
    CA025 = lissr::liss_classify_ca20g025)
  validators <- list(CA012 = lissr::liss_validate_ca20g012, CA014 = lissr::liss_validate_ca20g014,
    CA016 = lissr::liss_validate_ca20g016, CA023 = lissr::liss_validate_ca20g023,
    CA025 = lissr::liss_validate_ca20g025)
  policies <- list(CA012 = lissr:::.liss_ca012_policy(), CA014 = lissr:::.liss_ca014_policy(),
    CA016 = lissr:::.liss_ca016_policy(), CA023 = lissr:::.liss_ca023_policy(), CA025 = lissr:::.liss_ca025_policy())
  for (name in names(policies)) for (column in names(policies[[name]]$metadata_sha256))
    expect_identical(.nrca025_hash(attributes(frame[[column]])), policies[[name]]$metadata_sha256[[column]],
      info = paste(name, column))
  results <- lapply(creators, function(create) create(fixture$source))
  targets <- c(CA012 = "ca20g012", CA014 = "ca20g014", CA016 = "ca20g016", CA023 = "ca20g023", CA025 = "ca20g025")
  for (name in names(results)) {
    result <- results[[name]]
    expect_identical(names(result), c("original_data", "analysis", "data", "source_nonresponse", "source_nonresponse_readiness"), info = name)
    expect_true(.nrca025_equal(result$original_data, frame), info = name)
    expect_true(validators[[name]](result)$valid, info = name)
    expect_identical(validators[[name]](result, source = fixture$source)$source_consistency, "matched", info = name)
    for (column in setdiff(unname(targets), targets[[name]]))
      expect_true(.nrca025_equal(result$data[[column]], frame[[column]]), info = paste(name, column))
    for (foreign in setdiff(names(validators), name))
      expect_error(validators[[foreign]](result), class = paste0("liss_ca20g", substring(foreign, 3L), "_blocked"),
        info = paste(name, foreign))
  }
  # an invented former private eight-slot shape is a separate negative control
  private_shape <- list(original_data = frame, analysis = results$CA025$analysis,
    data = results$CA025$data, source_nonresponse = results$CA025$source_nonresponse,
    source_nonresponse_readiness = results$CA025$source_nonresponse_readiness,
    oracle_sha256 = strrep("0", 64L), invented_only = TRUE, actual_source_admission = FALSE)
  class(private_shape) <- "liss_source_nonresponse_collection_result"
  expect_length(private_shape, 8L)
  expect_error(lissr::liss_validate_ca20g025(private_shape), class = "liss_ca20g025_blocked")
})

test_that("CA025 recognized complete, residual, partial and empty claims deny every consumer", {
  fixture <- .nrca025_fixture()
  result <- lissr::liss_classify_ca20g025(fixture$source)
  bare <- .nrca025_unmark(result$data)
  class_only <- bare
  class(class_only) <- class(result$data)
  marker_only <- bare
  attr(marker_only, "liss_source_nonresponse") <- attr(result$data, "liss_source_nonresponse")
  readiness_only <- bare
  attr(readiness_only, "source_nonresponse_readiness") <- result$source_nonresponse_readiness
  partial <- result
  partial$data <- partial$data[1:2, , drop = FALSE]
  attr(partial$data, "liss_source_nonresponse") <- attr(result$data, "liss_source_nonresponse")
  attr(partial$data, "source_nonresponse_readiness") <- result$source_nonresponse_readiness
  empty <- result$data[FALSE, , drop = FALSE]
  attr(empty, "liss_source_nonresponse") <- attr(result$data, "liss_source_nonresponse")
  attr(empty, "source_nonresponse_readiness") <- result$source_nonresponse_readiness
  no_code_frame <- .nrca025_frame(c(8, 9, abs(as.double(0)), -abs(as.double(0)), NA_real_, -10))
  no_code <- lissr:::.liss_ca025_build(no_code_frame,
    lissr:::.liss_ca025_check_raw(no_code_frame, lissr:::.liss_ca025_policy()))
  forms <- list(whole = result, frame = result$data, class_only = class_only,
    marker_only = marker_only, readiness_only = readiness_only, wrapper = list(data = result$data),
    residual = list(data = bare, source_nonresponse_readiness = result$source_nonresponse_readiness),
    partial_whole = partial, partial_frame = partial$data, empty_frame = empty,
    no_code_whole = no_code, no_code_frame = no_code$data)
  inc <- data.frame(nomem_encr = c(860005, 860005, 860007), nohouse_encr = c(1, 1, 2),
    wavenr = c(1, 2, 1), nethh = c(21000, 21500, 30000))
  for (name in names(forms)) {
    input <- forms[[name]]
    before <- serialize(input, NULL, version = 2L)
    expect_true(lissr:::.liss_nonresponse_claims_guarded(input), info = name)
    for (mode in c("flag", "correct", "na_only"))
      expect_error(lissr::liss_clean_income(input, mode = mode, background = NULL,
        variables = list(target = "s339"), ruleset = stop("CA025 forced ruleset"), verbose = FALSE),
        class = "liss_source_nonresponse_blocked", info = paste(name, mode))
    expect_error(lissr::liss_clean_income(inc, background = input,
      variables = list(personal_net = "ca20g025"), verbose = FALSE),
      class = "liss_source_nonresponse_blocked", info = name)
    expect_error(lissr::liss_attach_background(input, sources = stop("CA025 forced sources"),
      month_col = stop("CA025 forced month"), variables = stop("CA025 forced variables")),
      class = "liss_source_nonresponse_blocked", info = name)
    path <- file.path(fixture$directory, paste0("denied_", name))
    expect_error(lissr::liss_cleaning_report(input, path), class = "liss_source_nonresponse_blocked", info = name)
    for (join in c("full", "inner", "left"))
      expect_error(suppressMessages(lissr::merge_liss_panel(list(ca = input), join_type = join,
        write_to = file.path(path, "panel.sav"))), class = "liss_source_nonresponse_blocked", info = name)
    expect_error(lissr::liss_classify_source_nonresponse(input, stop("CA025 forced source"),
      "s339", "wave_id"), class = "liss_source_nonresponse_blocked", info = name)
    expect_error(lissr::liss_create_applied_income_view(input, row_keys = stop("CA025 forced row keys")),
      class = "liss_source_nonresponse_blocked", info = name)
    expect_error(lissr::liss_validate_applied_income(input), class = "liss_source_nonresponse_blocked", info = name)
    expect_error(lissr::liss_validate_applied_income_view(input), class = "liss_applied_income_view_blocked", info = name)
    expect_false(file.exists(path), info = name)
    expect_identical(serialize(input, NULL, version = 2L), before, info = name)
  }
  ordinary <- lissr::liss_clean_income(inc, background = bare, verbose = FALSE)
  expect_identical(as.double(unclass(ordinary$data$ca20g025)), c(8, 8, abs(as.double(0))))
  control <- lissr::liss_clean_income(inc, disable = "P01", verbose = FALSE)
  ignored <- lissr::liss_clean_income(inc, background = result$data, disable = "P01", verbose = FALSE)
  expect_identical(ignored$data, control$data)
  expect_identical(ignored$decisions, control$decisions)
})
