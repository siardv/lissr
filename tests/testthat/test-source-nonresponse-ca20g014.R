# respondent values and temporary files are invented; no real SAV or PDF is read
.nrca014_columns <- function() {
  c("nomem_encr", "ca20g_m", "ca20g001", "ca20g002", "ca20g003", "ca20g082",
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
}

.nrca014_receipt <- function() list(filename = "ca20g_EN_1.0p.sav",
  sha256 = "d30acac3d2ca17533fb33ac724e43ff7dc40698139cec42221e1cae104684e47",
  size_bytes = 917084)

.nrca014_values <- function() {
  negative_zero <- -abs(as.double(0))
  c(-8, -9, -8, 8, 9, abs(negative_zero), -275, 1, 999999999, 0.25,
    1000000000, -1000000000, NA_real_, NaN, Inf, -Inf, negative_zero,
    haven::tagged_na("b"), -9999999999, -9999999998, -7, -10, 2^40, 7)
}

.nrca014_frame <- function(values = .nrca014_values()) {
  count <- 5849L
  columns <- .nrca014_columns()
  frame <- stats::setNames(lapply(columns, function(column)
    if (column %in% sprintf("ca20g%03d", 71:74)) rep("invented014 context", count) else rep(0, count)), columns)
  frame$nomem_encr <- as.double(830000L + seq_len(count))
  attributes(frame$nomem_encr) <- list(label = "Number of household member encrypted",
    format.spss = "F7.0", display_width = 12L)
  frame$ca20g014 <- c(as.double(values), rep(250, count - length(values)))
  attributes(frame$ca20g014) <- list(
    label = paste0("What was the total sum of the guaranteed minimum payout of your single-premium ",
      "or life annuity insurances, or the total savings amount of your endowment insurance on 31 December 2019?"),
    na_values = c(9999999998, 9999999999),
    class = c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", "double"),
    format.spss = "F6.0", display_width = 15L,
    labels = c("I prefer not to say" = 9999999998, "I don't know" = 9999999999))
  frame$ca20g005 <- rep(1, count)
  attributes(frame$ca20g005) <- list(label = paste0(
    "On 31 December 2019, did you possess one or more of the following assets? - ",
    "Single-premium insurance policy, life annuity insurance, endowment insurance (not linked to a mortgage)"),
    format.spss = "F1.0", display_width = 15L,
    class = c("haven_labelled", "vctrs_vctr", "double"), labels = c(no = 0, yes = 1))
  frame$ca20g015 <- c(NA_real_, 999, 14, rep(4, count - 3L))
  attributes(frame$ca20g015) <- list(label = paste0(
    "To what category did the sum of the guaranteed minimum payout of your single-premium ",
    "or life annuity insurances, or the total savings amount of your endowment insurance belong?"),
    na_values = 999,
    class = c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", "double"),
    format.spss = "F2.0", display_width = 15L,
    labels = stats::setNames(c(as.double(1:14), 999), c("less than € 500", "€ 500 to € 1,500",
      "€ 1,500 to € 2,500", "€ 2,500 to € 5,000", "€ 5,000 to € 7,500", "€ 7,500 to € 10,000",
      "€ 10,000 to € 12,000", "€ 12,000 to € 15,000", "€ 15,000 to € 20,000", "€ 20,000 to € 25,000",
      "€ 25,000 to € 50,000", "€ 50,000 to € 75,000", "€ 75,000 to € 100,000", "€ 100,000 or more", "I don't know")))
  frame$ca20g012 <- rep(-9999999999, count)
  frame$ca20g016 <- rep(-9999999998, count)
  frame$ca20g023 <- rep(-9, count)
  frame$ca20g035 <- structure(rep(-325, count), label = "invented014 signed control", format.spss = "F8.0")
  frame$ca20g071 <- structure(frame$ca20g071, label = "invented014 text control")
  tibble::as_tibble(frame, .name_repair = "minimal")
}

.nrca014_fixture <- function(frame = .nrca014_frame(), .local_envir = parent.frame()) {
  directory <- withr::local_tempdir("ca014_invented_", .local_envir = .local_envir)
  source <- file.path(directory, "ca20g_EN_1.0p.sav")
  file.create(source)
  calls <- new.env(parent = emptyenv())
  calls$steps <- character()
  receipt <- .nrca014_receipt()
  # these mocks assert an invented import, never authentic source admission
  testthat::local_mocked_bindings(.liss_ca014_source_state = function(path) {
    calls$steps <- c(calls$steps, "state")
    receipt
  }, .liss_ca014_read_source = function(path) {
    calls$steps <- c(calls$steps, "read")
    frame
  }, .package = "lissr", .env = .local_envir)
  list(source = source, frame = frame, calls = calls, directory = directory)
}

.nrca014_hash <- function(value) {
  .nr_identity_hash(value)
}

.nrca014_unmark <- function(frame) {
  attr(frame, "liss_source_nonresponse") <- NULL
  attr(frame, "source_nonresponse_readiness") <- NULL
  class(frame) <- c("tbl_df", "tbl", "data.frame")
  frame
}

.nrca014_reseal <- function(result) {
  record <- result$source_nonresponse
  record$original_digest <- .nrca014_hash(result$original_data)
  record$analysis_digest <- .nrca014_hash(.nrca014_unmark(result$analysis$data))
  record$ledger_digest <- .nrca014_hash(result$analysis$ledger)
  record$readiness_digest <- .nrca014_hash(result$source_nonresponse_readiness)
  record$digest <- NULL
  record$digest <- .nrca014_hash(record)
  result$source_nonresponse <- record
  marker <- attr(result$analysis$data, "liss_source_nonresponse")
  marker$parent_digest <- record$original_digest
  marker$analysis_digest <- record$analysis_digest
  marker$classification_digest <- record$digest
  attr(result$analysis$data, "liss_source_nonresponse") <- marker
  result$data <- result$analysis$data
  result
}

test_that("CA014 has a separately invented complete literal projection, ledger and coverage", {
  fixture <- .nrca014_fixture()
  before <- serialize(fixture$frame, NULL, version = 2L)
  result <- lissr::liss_classify_ca20g014(fixture$source)
  expect_identical(fixture$calls$steps, c("state", "read", "state"))
  expect_identical(names(result), c("original_data", "analysis", "data",
    "source_nonresponse", "source_nonresponse_readiness"))
  expect_identical(names(result$analysis), c("data", "ledger"))
  expect_identical(class(result), "liss_source_nonresponse_collection_result")
  expect_identical(class(result$data), c("liss_source_nonresponse_collection_result", "tbl_df", "tbl", "data.frame"))
  expect_identical(serialize(result$original_data, NULL, version = 2L), before)
  expect_identical(serialize(fixture$frame, NULL, version = 2L), before)
  expect_identical(result$data, result$analysis$data)
  expect_identical(dim(result$data), c(5849L, 66L))
  expect_identical(names(result$data), .nrca014_columns())
  expect_identical(unname(vapply(result$data, typeof, character(1))),
    ifelse(.nrca014_columns() %in% c("ca20g071", "ca20g072", "ca20g073", "ca20g074"), "character", "double"))
  negative_zero <- -abs(as.double(0))
  positive_zero <- abs(negative_zero)
  expected <- c(NA_real_, NA_real_, NA_real_, 8, 9, positive_zero, -275, 1, 999999999,
    0.25, 1000000000, -1000000000, NA_real_, NaN, Inf, -Inf, negative_zero,
    haven::tagged_na("b"), -9999999999, -9999999998, -7, -10, 2^40, 7, rep(250, 5825L))
  expect_identical(serialize(as.double(unclass(result$data$ca20g014)), NULL, version = 2L),
    serialize(expected, NULL, version = 2L))
  expect_identical(attributes(result$data$ca20g014), attributes(fixture$frame$ca20g014))
  for (column in setdiff(.nrca014_columns(), "ca20g014"))
    expect_identical(serialize(result$data[[column]], NULL, version = 2L),
      serialize(fixture$frame[[column]], NULL, version = 2L), info = column)
  expect_identical(1 / unname(attr(fixture$frame$ca20g005, "labels")[["no"]]), Inf)
  record <- result$source_nonresponse
  expect_identical(record$authority_id, "CA20G014_SOURCE")
  expect_identical(record$authority_version, "1.0.0")
  expect_identical(record$schema_version, "1.0.0")
  expect_identical(record$kind, "ca20g014_classification")
  expect_identical(record$construct, "guaranteed_minimum_insurance_payout_or_endowment_savings")
  expect_identical(record$unit, "EUR_whole")
  expect_identical(record$reference_date, "2019-12-31")
  expect_identical(record$date_scope, "amount asked for on this date; distinct from September and October 2020 collection")
  expect_false(any(c("measurement_date", "documented_signed_range") %in% names(record)))
  expect_identical(record$ownership_scope,
    "source household-role and own/joint ownership context; no uniform household total or income interpretation")
  expect_identical(record$analysis_metadata_status,
    "retained source description only; does not attest changed analysis values")
  expect_identical(record$documented_amount_range, c(1, 999999999))
  expect_identical(record$source_metadata_nonresponse_forms, list(
    na_values = c(9999999998, 9999999999), value_labels = c(9999999998, 9999999999),
    pdf_forms = c(99999999998, 99999999999)))
  expect_identical(names(record$fields), "CA20G014_FIELD")
  expect_identical(names(record$fields$CA20G014_FIELD$metadata_sha256), c("nomem_encr", "ca20g014", "ca20g005"))
  expect_identical(record$contextual_metadata, list(ca20g015 = list(role = "context_only",
    reference_sha256 = "43765b1a533dfcfec21f20f102ad9790dc23c73120d931f01565d659ef1f5fda",
    actual_sha256 = .nrca014_hash(attributes(fixture$frame$ca20g015)), required_for_admission = FALSE)))
  expect_identical(record$source, .nrca014_receipt())
  expected_coverage <- data.frame(source_row_index = seq_len(5849L),
    nomem_key = as.character(830000L + seq_len(5849L)),
    original_value = c(-8, -9, -8, 8, 9, positive_zero, -275, 1, 999999999,
      0.25, 1000000000, -1000000000, NA_real_, NaN, Inf, -Inf, negative_zero,
      haven::tagged_na("b"), -9999999999, -9999999998, -7, -10, 2^40, 7, rep(250, 5825L)),
    classified = c(rep(TRUE, 3L), rep(FALSE, 5846L)), stringsAsFactors = FALSE)
  expect_identical(serialize(record$coverage, NULL, version = 2L),
    serialize(expected_coverage, NULL, version = 2L))
  expected_ledger <- data.frame(
    source_cell_key = paste(.nrca014_receipt()$sha256, 1:3, "ca20g014", sep = ":"),
    source_row_index = 1:3, nomem_key = c("830001", "830002", "830003"),
    source_sha256 = rep(.nrca014_receipt()$sha256, 3L),
    source_filename = rep("ca20g_EN_1.0p.sav", 3L), raw_column = rep("ca20g014", 3L),
    case_id = c("CA20G014_N02", "CA20G014_N01", "CA20G014_N02"),
    original_value = c(-8, -9, -8), raw_value_repr = c("-8", "-9", "-8"),
    source_declared_missing = rep(FALSE, 3L), answer_status = rep("nonresponse_unknown", 3L),
    candidate_answer_status = rep(NA_character_, 3L), subtype_evidence = rep("unresolved", 3L),
    analysis_disposition = rep("missing_amount", 3L), analysis_value_after = rep(NA_real_, 3L),
    no_numeric_proposal = rep(TRUE, 3L), route_column = rep("ca20g005", 3L),
    route_value = rep(1, 3L), stringsAsFactors = FALSE)
  expect_identical(result$analysis$ledger, expected_ledger)
  readiness <- result$source_nonresponse_readiness
  expect_identical(readiness$overall_amount_validity, "not_evaluated")
  expect_identical(names(readiness$fields), "CA20G014_FIELD")
  expect_identical(readiness$fields$CA20G014_FIELD$evaluated_cells, 5849L)
  expect_identical(readiness$fields$CA20G014_FIELD$classified_cells, 3L)
  expect_identical(readiness$fields$CA20G014_FIELD$unresolved_cells, 0L)
  expect_identical(readiness$fields$CA20G014_FIELD$untargeted_nonfinite_cells, 5L)
  checked <- lissr::liss_validate_ca20g014(result)
  expect_true(checked$valid)
  expect_identical(checked$source_authentication, FALSE)
  expect_identical(checked$statistical_validity, FALSE)
  expect_identical(checked$source_consistency, "not_requested")
})

test_that("CA014 rejects wrong local paths and invented bytes before parsing", {
  directory <- withr::local_tempdir("ca014_bad_bytes_")
  source <- file.path(directory, "ca20g_EN_1.0p.sav")
  writeBin(charToRaw("invented014 wrong source"), source)
  parsed <- 0L
  testthat::local_mocked_bindings(read_sav = function(...) {
    parsed <<- parsed + 1L
    stop("unverified bytes reached haven")
  }, .package = "haven")
  expect_error(lissr::liss_classify_ca20g014(), class = "liss_ca20g014_blocked")
  for (path in list(NULL, NA_character_, "", FALSE, c(source, source), factor(source),
    structure(source, label = "invented claim"), directory, file.path(directory, "missing.sav"), source))
    expect_error(lissr::liss_classify_ca20g014(path), class = "liss_ca20g014_blocked")
  expect_error(lissr:::.liss_ca014_read_source(source), class = "liss_ca20g014_blocked")
  writeBin(raw(917084L), source)
  expect_error(lissr::liss_classify_ca20g014(source), class = "liss_ca20g014_blocked")
  expect_error(lissr:::.liss_ca014_read_source(source), class = "liss_ca20g014_blocked")
  for (filename in c("ca20g_NL_1.0p.sav", "ca20g_EN_1.1p.sav", "ca21h_EN_1.0p.sav")) {
    path <- file.path(directory, filename)
    file.create(path)
    expect_error(lissr::liss_classify_ca20g014(path), class = "liss_ca20g014_blocked")
  }
  expect_identical(parsed, 0L)
})

test_that("CA014 hands haven the exact verified invented buffer and import settings", {
  bytes <- charToRaw("explicitly invented014 parser-buffer probe")
  digest <- as.character(openssl::sha256(bytes))
  attributes(digest) <- NULL
  path <- withr::local_tempfile(pattern = "ca014_buffer_", fileext = ".sav")
  writeBin(bytes, path)
  seen <- NULL
  settings <- NULL
  parsed <- 0L
  expected <- .nrca014_frame()
  testthat::local_mocked_bindings(.liss_ca014_policy = function()
    list(source = list(size_bytes = length(bytes), sha256 = digest)), .package = "lissr")
  testthat::local_mocked_bindings(read_sav = function(file, user_na, .name_repair) {
    parsed <<- parsed + 1L
    seen <<- file
    settings <<- list(user_na = user_na, name_repair = .name_repair)
    expected
  }, .package = "haven")
  expect_identical(lissr:::.liss_ca014_read_source(path), expected)
  expect_identical(seen, bytes)
  expect_identical(settings, list(user_na = TRUE, name_repair = "minimal"))
  for (replacement in list(c(bytes, as.raw(1)), bytes[-1L], rev(bytes))) {
    writeBin(replacement, path)
    expect_error(lissr:::.liss_ca014_read_source(path), class = "liss_ca20g014_blocked")
  }
  expect_identical(parsed, 1L)
})

test_that("CA014 requires exact pre-import and post-import identities", {
  for (phase in c(1L, 2L)) for (field in c("filename", "sha256", "size_bytes")) local({
    fixture <- .nrca014_fixture(.local_envir = environment())
    result <- lissr::liss_classify_ca20g014(fixture$source)
    fixture$calls$steps <- character()
    states <- 0L
    testthat::local_mocked_bindings(.liss_ca014_source_state = function(path) {
      states <<- states + 1L
      receipt <- .nrca014_receipt()
      if (states == phase) receipt[[field]] <- switch(field,
        filename = "ca21h_EN_1.0p.sav", sha256 = strrep("0", 64L), size_bytes = 917085)
      receipt
    }, .package = "lissr", .env = environment())
    expect_error(lissr::liss_classify_ca20g014(fixture$source), class = "liss_ca20g014_blocked",
      info = paste(phase, field))
    expect_identical(fixture$calls$steps, if (phase == 1L) character() else "read")
    states <- 0L
    fixture$calls$steps <- character()
    expect_error(lissr::liss_validate_ca20g014(result, source = fixture$source), class = "liss_ca20g014_blocked",
      info = paste("validation", phase, field))
    expect_identical(fixture$calls$steps, if (phase == 1L) character() else "read")
  })
})

test_that("CA014 refuses malformed complete schema, keys and admission metadata before projection", {
  original <- .nrca014_frame()
  projected <- 0L
  testthat::local_mocked_bindings(.liss_ca014_build = function(input, checked) {
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
    field_alias = function(x) { names(x)[17L] <- "invented_target_alias"; x },
    column_order = function(x) x[, rev(names(x)), drop = FALSE],
    row_names_order = function(x) { attr(x, "row.names") <- rev(seq_len(nrow(x))); x },
    incomplete_rows = function(x) x[-1L, , drop = FALSE],
    empty_rows = function(x) x[FALSE, , drop = FALSE],
    missing_context_column = function(x) { x$ca20g015 <- NULL; x },
    missing_column = function(x) { x$ca20g075 <- NULL; x },
    extra_column = function(x) { x$invented_extra <- rep(0, nrow(x)); x },
    wrong_type = function(x) { x$ca20g001 <- as.integer(x$ca20g001); x },
    extra_attribute = function(x) { attr(x, "invented_extra") <- TRUE; x },
    wrong_class = function(x) { class(x) <- "data.frame"; x },
    non_frame = function(x) unclass(x),
    target_metadata = function(x) { attr(x$ca20g014, "label") <- "invented wrong field"; x },
    gate_metadata = function(x) { attr(x$ca20g005, "label") <- "invented wrong gate"; x },
    gate_negative_zero_label = function(x) { attr(x$ca20g005, "labels")[["no"]] <- -abs(as.double(0)); x },
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
    fixture <- .nrca014_fixture(malformed, .local_envir = environment())
    expect_error(lissr::liss_classify_ca20g014(fixture$source), class = "liss_ca20g014_blocked", info = name)
    expect_identical(serialize(malformed, NULL, version = 2L), before, info = name)
  })
  expect_identical(projected, 0L)
})

test_that("CA014 retains zero and upper lossless keys without renumbering", {
  frame <- .nrca014_frame()
  frame$nomem_encr[c(1L, 5849L)] <- c(0, 2^53 - 1)
  fixture <- .nrca014_fixture(frame)
  result <- lissr::liss_classify_ca20g014(fixture$source)
  expect_identical(result$analysis$ledger$nomem_key[1L], "0")
  expect_identical(result$source_nonresponse$coverage$nomem_key[5849L], "9007199254740991")
  expect_identical(result$original_data$nomem_encr, frame$nomem_encr)
  expect_true(lissr::liss_validate_ca20g014(result)$valid)
})

test_that("CA014 requires005 gate one only for exact codes and permits missing015 and eligible NA", {
  for (gate in c(0, -1, 2, 1.5, NA_real_, NaN, Inf, -Inf, haven::tagged_na("c"))) local({
    frame <- .nrca014_frame()
    frame$ca20g005[1L] <- gate
    fixture <- .nrca014_fixture(frame, .local_envir = environment())
    expect_error(lissr::liss_classify_ca20g014(fixture$source), class = "liss_ca20g014_blocked")
  })
  frame <- .nrca014_frame()
  frame$ca20g005[4:8] <- c(0, -1, NA_real_, NaN, Inf)
  frame$ca20g006 <- rep(0, 5849L)
  frame$ca20g008 <- rep(0, 5849L)
  frame$ca20g078 <- rep(0, 5849L)
  frame$ca20g015[] <- NA_real_
  fixture <- .nrca014_fixture(frame)
  result <- lissr::liss_classify_ca20g014(fixture$source)
  expect_true(lissr::liss_validate_ca20g014(result)$valid)
  for (column in c("ca20g005", "ca20g006", "ca20g008", "ca20g078", "ca20g015"))
    expect_identical(result$data[[column]], frame[[column]], info = column)
  expect_identical(as.double(unclass(result$data$ca20g014))[4:8], c(8, 9, 0, -275, 1))
  expect_identical(as.double(unclass(result$data$ca20g005))[13L], 1)
  expect_true(is.na(result$data$ca20g014[13L]))
  expect_false(result$source_nonresponse$coverage$classified[13L])
  expect_false(13L %in% result$analysis$ledger$source_row_index)
  expect_true(all(is.na(result$data$ca20g015)))
  expect_identical(result$analysis$ledger$source_row_index, 1:3)
})

test_that("CA014 records015 attributes as context and admits consistent contextual substitutions", {
  fixture <- .nrca014_fixture()
  original <- fixture$frame
  original_result <- lissr::liss_classify_ca20g014(fixture$source)
  expect_identical(original_result$source_nonresponse$contextual_metadata$ca20g015$actual_sha256,
    "43765b1a533dfcfec21f20f102ad9790dc23c73120d931f01565d659ef1f5fda")
  changes <- list(
    changed_label = function(x) { attr(x$ca20g015, "label") <- "invented contextual label"; x },
    no_attributes = function(x) { attributes(x$ca20g015) <- NULL; x },
    attribute_order = function(x) { attributes(x$ca20g015) <- rev(attributes(x$ca20g015)); x },
    missing_values = function(x) { x$ca20g015[] <- NA_real_; x },
    ordinary_values = function(x) { x$ca20g015[] <- -123.5; x })
  for (name in names(changes)) {
    changed <- changes[[name]](original)
    rebuilt <- lissr:::.liss_ca014_build(changed,
      lissr:::.liss_ca014_check_raw(changed, lissr:::.liss_ca014_policy()))
    context <- rebuilt$source_nonresponse$contextual_metadata$ca20g015
    expect_identical(names(context), c("role", "reference_sha256", "actual_sha256", "required_for_admission"), info = name)
    expect_identical(context$role, "context_only", info = name)
    expect_identical(context$reference_sha256,
      "43765b1a533dfcfec21f20f102ad9790dc23c73120d931f01565d659ef1f5fda", info = name)
    expect_identical(context$actual_sha256, .nrca014_hash(attributes(changed$ca20g015)), info = name)
    expect_identical(context$required_for_admission, FALSE, info = name)
    expect_identical(rebuilt$data$ca20g015, changed$ca20g015, info = name)
    expect_true(lissr::liss_validate_ca20g014(rebuilt)$valid, info = name)
    expect_error(lissr::liss_validate_ca20g014(rebuilt, source = fixture$source),
      class = "liss_ca20g014_blocked", info = name)
  }
  changed <- original
  attr(changed$ca20g015, "label") <- "invented contextual creation"
  local({
    contextual_fixture <- .nrca014_fixture(changed, .local_envir = environment())
    result <- lissr::liss_classify_ca20g014(contextual_fixture$source)
    expect_true(lissr::liss_validate_ca20g014(result)$valid)
    expect_identical(lissr::liss_validate_ca20g014(result, source = contextual_fixture$source)$source_consistency, "matched")
    expect_false(identical(result$source_nonresponse$contextual_metadata$ca20g015$actual_sha256,
      result$source_nonresponse$contextual_metadata$ca20g015$reference_sha256))
  })
})

test_that("CA014 rejects every unsupported positive form for whole creation and retained-original validation", {
  fixture <- .nrca014_fixture()
  result <- lissr::liss_classify_ca20g014(fixture$source)
  testthat::local_mocked_bindings(.liss_ca014_build = function(input, checked) {
    force(checked)
    stop("positive original reached projection")
  }, .package = "lissr")
  for (code in c(9999999998, 9999999999, 99999999998, 99999999999)) local({
    frame <- .nrca014_frame()
    frame$ca20g014[4L] <- code
    before <- serialize(frame, NULL, version = 2L)
    positive_fixture <- .nrca014_fixture(frame, .local_envir = environment())
    expect_error(lissr::liss_classify_ca20g014(positive_fixture$source), class = "liss_ca20g014_blocked")
    expect_identical(serialize(frame, NULL, version = 2L), before)
  })
  for (code in c(9999999998, 9999999999, 99999999998, 99999999999)) {
    changed <- result
    changed$original_data$ca20g014[4L] <- code
    changed <- .nrca014_reseal(changed)
    before <- serialize(changed, NULL, version = 2L)
    fixture$calls$steps <- character()
    for (source in list(NULL, fixture$source))
      expect_error(lissr::liss_validate_ca20g014(changed, source = source),
        class = "liss_ca20g014_blocked", regexp = "positive")
    expect_identical(fixture$calls$steps, character())
    expect_identical(serialize(changed, NULL, version = 2L), before)
  }
})

test_that("CA014 no-hit sources retain complete coverage and the complete typed empty ledger", {
  fixture <- .nrca014_fixture(.nrca014_frame(c(8, 9, 0, -12.5, 0.25, -9999999999, -9999999998, NA_real_, NaN, Inf, -Inf)))
  result <- lissr::liss_classify_ca20g014(fixture$source)
  expect_identical(serialize(.nrca014_unmark(result$data), NULL, version = 2L),
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
  expect_identical(result$source_nonresponse_readiness$fields$CA20G014_FIELD$classified_cells, 0L)
  expect_true(lissr::liss_validate_ca20g014(result)$valid)
})

test_that("CA014 closed reconstruction rejects stale and resealed payload, marker and readiness changes", {
  fixture <- .nrca014_fixture()
  result <- lissr::liss_classify_ca20g014(fixture$source)
  for (piece in list(result$data, result$original_data, result$analysis))
    expect_error(lissr::liss_validate_ca20g014(piece), class = "liss_ca20g014_blocked")
  changes <- list(
    missing_original = function(x) { x$original_data <- NULL; x },
    missing_analysis = function(x) { x$analysis <- NULL; x },
    extra_wrapper_field = function(x) { x$invented_extra <- TRUE; x },
    wrapper_attribute = function(x) { attr(x, "invented_extra") <- TRUE; x },
    wrapper_class = function(x) { class(x) <- "list"; x },
    analysis_shape = function(x) { x$analysis$invented_extra <- TRUE; x },
    analysis_attribute = function(x) { attr(x$analysis, "invented_extra") <- TRUE; x },
    duplicate_data = function(x) { x$data$ca20g014[1L] <- 1; x },
    locked_value = function(x) { x$analysis$data$ca20g014[1L] <- 1; x },
    sign_repair = function(x) { x$analysis$data$ca20g014[4L] <- NA_real_; x },
    non_target012 = function(x) { x$analysis$data$ca20g012[1L] <- NA_real_; x },
    non_target016 = function(x) { x$analysis$data$ca20g016[1L] <- NA_real_; x },
    non_target023 = function(x) { x$analysis$data$ca20g023[1L] <- NA_real_; x },
    bracket_inference = function(x) { x$analysis$data$ca20g015[1L] <- 1; x },
    target_metadata = function(x) { attr(x$analysis$data$ca20g014, "label") <- "invented changed label"; x },
    ledger_row = function(x) { x$analysis$ledger$source_row_index[1L] <- 4L; x },
    ledger_type = function(x) { x$analysis$ledger$source_row_index <- as.double(x$analysis$ledger$source_row_index); x },
    ledger_value = function(x) { x$analysis$ledger$original_value[1L] <- -9; x },
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
    context_actual_digest = function(x) { x$source_nonresponse$contextual_metadata$ca20g015$actual_sha256 <- strrep("0", 64L); x },
    context_reference_digest = function(x) { x$source_nonresponse$contextual_metadata$ca20g015$reference_sha256 <- strrep("0", 64L); x },
    context_admission = function(x) { x$source_nonresponse$contextual_metadata$ca20g015$required_for_admission <- TRUE; x },
    context_extra = function(x) { x$source_nonresponse$contextual_metadata$ca20g015$invented_extra <- TRUE; x },
    marker_missing = function(x) { attr(x$analysis$data, "liss_source_nonresponse") <- NULL; x },
    marker_field = function(x) { attr(x$analysis$data, "liss_source_nonresponse")$kind <- "invented"; x },
    marker_attribute = function(x) { attr(attr(x$analysis$data, "liss_source_nonresponse"), "invented_extra") <- TRUE; x },
    frame_readiness_missing = function(x) { attr(x$analysis$data, "source_nonresponse_readiness") <- NULL; x },
    frame_readiness_value = function(x) { attr(x$analysis$data, "source_nonresponse_readiness")$status <- "certified"; x },
    native_schema = function(x) { x$source_nonresponse$schema_version <- "4.0.0"; x })
  for (name in names(changes)) {
    changed <- changes[[name]](result)
    before <- serialize(changed, NULL, version = 2L)
    expect_error(lissr::liss_validate_ca20g014(changed), class = "liss_ca20g014_blocked", info = name)
    expect_identical(serialize(changed, NULL, version = 2L), before, info = name)
    if (name == "duplicate_data") next
    resealed <- tryCatch(.nrca014_reseal(changed), error = identity)
    if (!inherits(resealed, "error"))
      expect_error(lissr::liss_validate_ca20g014(resealed), class = "liss_ca20g014_blocked",
        info = paste(name, "resealed"))
  }
})

test_that("CA014 strict equality distinguishes signed zeros, missing payloads and attribute order", {
  fixture <- .nrca014_fixture()
  result <- lissr::liss_classify_ca20g014(fixture$source)
  negative_zero <- -abs(as.double(0))
  positive_zero <- abs(negative_zero)
  expect_true(identical(negative_zero, positive_zero))
  expect_identical(1 / negative_zero, -Inf)
  expect_identical(1 / positive_zero, Inf)
  expect_false(identical(serialize(negative_zero, NULL, version = 2L),
    serialize(positive_zero, NULL, version = 2L)))
  zero <- result
  target <- as.double(unclass(zero$analysis$data$ca20g014))
  expect_identical(1 / target[6L], Inf)
  target[6L] <- negative_zero
  attributes(target) <- attributes(result$analysis$data$ca20g014)
  zero$analysis$data$ca20g014 <- target
  zero$data <- zero$analysis$data
  expect_identical(1 / as.double(unclass(zero$data$ca20g014))[6L], -Inf)
  reordered <- result
  attributes(reordered$analysis$data$ca20g014) <- rev(attributes(reordered$analysis$data$ca20g014))
  reordered$data <- reordered$analysis$data
  tagged <- result
  tagged$analysis$ledger$analysis_value_after[1L] <- haven::tagged_na("a")
  tagged$data <- tagged$analysis$data
  for (changed in list(zero, reordered, tagged)) {
    expect_true(identical(changed, result))
    expect_false(identical(serialize(changed, NULL, version = 2L), serialize(result, NULL, version = 2L)))
    for (source in list(NULL, fixture$source)) {
      expect_error(lissr::liss_validate_ca20g014(changed, source = source), class = "liss_ca20g014_blocked")
      expect_error(lissr::liss_validate_ca20g014(.nrca014_reseal(changed), source = source),
        class = "liss_ca20g014_blocked")
    }
  }
})

test_that("CA014 structural consistency permits substituted originals but source mode compares every column", {
  fixture <- .nrca014_fixture()
  original <- fixture$frame
  result <- lissr::liss_classify_ca20g014(fixture$source)
  changes <- list(
    bracket_value = function(x) { x$ca20g015[1L] <- 2; x },
    non_target_value = function(x) { x$ca20g035[1L] <- 42; x },
    non_target_metadata = function(x) { attr(x$ca20g035, "label") <- "invented substitution"; x },
    non_target_attribute_order = function(x) { attributes(x$ca20g035) <- rev(attributes(x$ca20g035)); x },
    target_amount = function(x) { x$ca20g014[4L] <- 18; x })
  for (name in names(changes)) {
    changed <- changes[[name]](original)
    rebuilt <- lissr:::.liss_ca014_build(changed,
      lissr:::.liss_ca014_check_raw(changed, lissr:::.liss_ca014_policy()))
    checked <- lissr::liss_validate_ca20g014(rebuilt)
    expect_true(checked$valid, info = name)
    expect_identical(checked$source_authentication, FALSE, info = name)
    expect_identical(checked$statistical_validity, FALSE, info = name)
    expect_error(lissr::liss_validate_ca20g014(rebuilt, source = fixture$source),
      class = "liss_ca20g014_blocked", info = name)
  }
  checked <- lissr::liss_validate_ca20g014(result, source = fixture$source)
  expect_identical(checked$source_consistency, "matched")
  expect_identical(checked$validation, "source_consistency")
  expect_identical(checked$source_authentication, FALSE)
})

test_that("CA014 reference attributes require R identity as well as equal serialized contents", {
  original <- .nrca014_frame()
  attr(original$ca20g035, "invented_context") <- new.env(parent = emptyenv())
  fixture <- .nrca014_fixture(original)
  result <- lissr::liss_classify_ca20g014(fixture$source)
  changed <- result
  attr(changed$analysis$data$ca20g035, "invented_context") <- new.env(parent = emptyenv())
  changed$data <- changed$analysis$data
  expect_false(identical(changed, result))
  expect_identical(serialize(.nrca014_unmark(changed$analysis$data), NULL, version = 2L),
    serialize(.nrca014_unmark(result$analysis$data), NULL, version = 2L))
  expect_error(lissr::liss_validate_ca20g014(changed), class = "liss_ca20g014_blocked")
  expect_error(lissr::liss_validate_ca20g014(.nrca014_reseal(changed)), class = "liss_ca20g014_blocked")
  substituted <- original
  attr(substituted$ca20g035, "invented_context") <- new.env(parent = emptyenv())
  expect_false(identical(substituted, original))
  expect_identical(serialize(substituted, NULL, version = 2L), serialize(original, NULL, version = 2L))
  rebuilt <- lissr:::.liss_ca014_build(substituted,
    lissr:::.liss_ca014_check_raw(substituted, lissr:::.liss_ca014_policy()))
  expect_true(lissr::liss_validate_ca20g014(rebuilt)$valid)
  expect_error(lissr::liss_validate_ca20g014(rebuilt, source = fixture$source), class = "liss_ca20g014_blocked")
})

test_that("CA014 permits moved guard placement only with exact payload, marker and readiness values", {
  fixture <- .nrca014_fixture()
  result <- lissr::liss_classify_ca20g014(fixture$source)
  changed <- result
  frame_class <- class(changed$analysis$data)
  class(changed$analysis$data) <- NULL
  class(changed$analysis$data) <- frame_class
  changed$data <- changed$analysis$data
  expect_true(identical(changed, result))
  expect_false(identical(serialize(changed, NULL, version = 2L), serialize(result, NULL, version = 2L)))
  expect_identical(serialize(.nrca014_unmark(changed$data), NULL, version = 2L),
    serialize(.nrca014_unmark(result$data), NULL, version = 2L))
  expect_true(lissr::liss_validate_ca20g014(changed)$valid)
  expect_identical(lissr::liss_validate_ca20g014(changed, source = fixture$source)$source_consistency, "matched")
})

test_that("CA014 whole-result V2 and default V3 files preserve contextual, no-code and reference cases", {
  frames <- list(ordinary = .nrca014_frame(), no_code = .nrca014_frame(c(8, 9, NA_real_, -10)),
    contextual = .nrca014_frame(), reference = .nrca014_frame())
  attr(frames$contextual$ca20g015, "label") <- "invented contextual transport"
  attr(frames$reference$ca20g035, "invented_context") <- new.env(parent = emptyenv())
  for (name in names(frames)) local({
    fixture <- .nrca014_fixture(frames[[name]], .local_envir = environment())
    result <- lissr::liss_classify_ca20g014(fixture$source)
    before <- serialize(result, NULL, version = 2L)
    for (version in c("2", "default")) {
      path <- withr::local_tempfile(pattern = paste0("invented_ca014_", name, "_", version, "_"), fileext = ".rds")
      if (version == "2") saveRDS(result, path, version = 2L) else saveRDS(result, path)
      file_hash <- as.character(openssl::sha256(readBin(path, "raw", n = file.info(path)$size)))
      expect_identical(infoRDS(path)$version, if (version == "2") 2L else 3L)
      expect_identical(as.character(openssl::sha256(readBin(path, "raw", n = file.info(path)$size))), file_hash)
      restored <- readRDS(path)
      expect_identical(serialize(restored, NULL, version = 2L), before, info = paste(name, version))
      expect_true(lissr::liss_validate_ca20g014(restored)$valid, info = paste(name, version))
      if (name == "reference") {
        expect_error(lissr::liss_validate_ca20g014(restored, source = fixture$source), class = "liss_ca20g014_blocked")
      } else {
        expect_identical(lissr::liss_validate_ca20g014(restored, source = fixture$source)$source_consistency, "matched")
      }
      expect_identical(as.character(openssl::sha256(readBin(path, "raw", n = file.info(path)$size))), file_hash)
    }
    expect_identical(serialize(result, NULL, version = 2L), before)
  })
})

test_that("CA014 authority is separately pinned and has no runtime CA023 or frozen-engine dependency", {
  policy <- lissr:::.liss_ca014_policy()
  expect_identical(policy$authority_id, "CA20G014_SOURCE")
  expect_identical(policy$authority_version, "1.0.0")
  expect_identical(policy$raw_column, "ca20g014")
  expect_identical(policy$route_column, "ca20g005")
  expect_identical(policy$source, .nrca014_receipt())
  expect_identical(policy$columns, .nrca014_columns())
  expect_identical(policy$codes, c(-9, -8))
  expect_identical(policy$case_ids, c("CA20G014_N01", "CA20G014_N02"))
  expect_identical(policy$metadata_sha256, list(
    nomem_encr = "a85d081e81be1b50c2ac82f5fe3012b3275d73d3c7556796b5bd923b8f90a1d1",
    ca20g014 = "3839ce375078db0ceb2a4f910e05af8ccf1f6faa2e4bfc4068e23103bf5ea041",
    ca20g005 = "f8e1b6c08d61fc04c1ffb0b8ed47f15b062bb0aa473ada85f21088cfdd50ae86"))
  expect_identical(policy$contextual_metadata_sha256,
    list(ca20g015 = "43765b1a533dfcfec21f20f102ad9790dc23c73120d931f01565d659ef1f5fda"))
  for (column in names(policy$metadata_sha256))
    expect_identical(.nrca014_hash(attributes(.nrca014_frame()[[column]])), policy$metadata_sha256[[column]], info = column)
  expect_identical(lissr:::.liss_ca014_policy_sha256(),
    "b0db75337aee32e6c274ef04b68613b5f699d215c128ac570f4b0149e0f8b2f8")
  expect_identical(lissr:::.liss_ca012_policy()$authority_id, "CA20G012_SOURCE")
  expect_identical(lissr:::.liss_ca016_policy()$authority_id, "CA20G016_SOURCE")
  expect_identical(lissr:::.liss_ca023_policy()$authority_id, "CA20G023_SOURCE")
  for (hash in c(lissr:::.liss_ca012_policy_sha256(), lissr:::.liss_ca016_policy_sha256(), lissr:::.liss_ca023_policy_sha256()))
    expect_false(identical(lissr:::.liss_ca014_policy_sha256(), hash))
  engine <- lissr:::.liss_applied_engine()
  expect_length(engine$functions, 117L)
  expect_false(any(grepl("ca014", names(engine$functions))))
  fixture <- .nrca014_fixture()
  local({
    testthat::local_mocked_bindings(.liss_ca023_policy = function() stop("CA014 borrowed CA023 policy"),
      .liss_ca023_policy_path = function() stop("CA014 borrowed CA023 path"), .package = "lissr", .env = environment())
    result <- lissr::liss_classify_ca20g014(fixture$source)
    expect_true(lissr::liss_validate_ca20g014(result)$valid)
  })
  path <- lissr:::.liss_ca014_policy_path()
  expect_identical(basename(path), "source_nonresponse_ca20g014.yml")
  changed <- withr::local_tempfile(fileext = ".yml")
  writeLines(c(readLines(path, warn = FALSE), "unreviewed: true"), changed)
  testthat::local_mocked_bindings(.liss_ca014_policy_path = function() changed, .package = "lissr")
  expect_error(lissr:::.liss_ca014_policy(), class = "liss_ca20g014_blocked")
})

test_that("CA012 CA014 CA016 and CA023 keep mutually exclusive standalone validators", {
  frame <- .nrca014_frame()
  attributes(frame$ca20g012) <- list(
    label = paste0("What was the total balance of your banking account or giro (current accounts), ",
      "savings accounts, term deposit accounts, savings bonds or savings certificates and bank ",
      "savings schemes on 31 December 2019?"),
    na_values = c(9999999998, 9999999999),
    class = c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", "double"),
    format.spss = "F11.0", display_width = 15L,
    labels = c("I prefer not to say" = 99999999998, "I don't know" = 99999999999))
  frame$ca20g078 <- rep(1, 5849L)
  attributes(frame$ca20g078) <- list(label = paste0(
    "On 31 December 2019, did you possess one or more of the following assets? - ",
    "Banking accounts, savings accounts, term deposit accounts, savings bonds or ",
    "savings certificates, banksparen"), format.spss = "F1.0", display_width = 15L,
    class = c("haven_labelled", "vctrs_vctr", "double"), labels = c(no = 0, yes = 1))
  attributes(frame$ca20g016) <- list(
    label = paste0("What was the total value of your investments (growth funds, share funds, ",
      "bonds, debentures, stocks, options, warrants) on 31 December 2019?"),
    na_values = c(9999999998, 9999999999),
    class = c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", "double"),
    format.spss = "F11.0", display_width = 15L,
    labels = c("I prefer not to say" = 99999999998, "I don't know" = 99999999999))
  frame$ca20g006 <- rep(1, 5849L)
  attributes(frame$ca20g006) <- list(label = paste0(
    "On 31 December 2019, did you possess one or more of the following assets? - ",
    "Investments (growth funds, share funds, bonds, debentures, stocks, options, warrants, and so on)"),
    format.spss = "F1.0", display_width = 15L,
    class = c("haven_labelled", "vctrs_vctr", "double"), labels = c(no = 0, yes = 1))
  attributes(frame$ca20g023) <- list(
    label = paste0("Can you estimate the total sales value of the car(s), motorcycle(s), boat(s), ",
      "(static) caravan(s) that you owned on 31 December 2019?"),
    na_values = c(9999999998, 9999999999),
    class = c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", "double"),
    format.spss = "F6.0", display_width = 15L,
    labels = c("I prefer not to say" = 9999999998, "I don't know" = 9999999999))
  frame$ca20g008 <- rep(1, 5849L)
  attributes(frame$ca20g008) <- list(label = paste0(
    "On 31 December 2019, did you possess one or more of the following assets? - ",
    "Cars, motorcycles, boats, (static) caravans"),
    format.spss = "F1.0", display_width = 15L,
    class = c("haven_labelled", "vctrs_vctr", "double"), labels = c(no = 0, yes = 1))
  fixture <- .nrca014_fixture(frame)
  testthat::local_mocked_bindings(.liss_ca012_source_state = function(path) .nrca014_receipt(),
    .liss_ca012_read_source = function(path) frame,
    .liss_ca016_source_state = function(path) .nrca014_receipt(),
    .liss_ca016_read_source = function(path) frame,
    .liss_ca023_source_state = function(path) .nrca014_receipt(),
    .liss_ca023_read_source = function(path) frame, .package = "lissr")
  results <- list(CA012 = lissr::liss_classify_ca20g012(fixture$source),
    CA014 = lissr::liss_classify_ca20g014(fixture$source),
    CA016 = lissr::liss_classify_ca20g016(fixture$source),
    CA023 = lissr::liss_classify_ca20g023(fixture$source))
  validators <- list(CA012 = lissr::liss_validate_ca20g012, CA014 = lissr::liss_validate_ca20g014,
    CA016 = lissr::liss_validate_ca20g016, CA023 = lissr::liss_validate_ca20g023)
  for (name in names(results)) {
    expect_true(validators[[name]](results[[name]])$valid, info = name)
    expect_identical(results[[name]]$original_data, frame, info = name)
    for (foreign in setdiff(names(validators), name))
      expect_error(validators[[foreign]](results[[name]]),
        class = paste0("liss_ca20g", substring(foreign, 3L), "_blocked"), info = paste(name, foreign))
  }
  for (column in c("ca20g012", "ca20g016", "ca20g023"))
    expect_identical(results$CA014$data[[column]], frame[[column]], info = column)
  for (name in c("CA012", "CA016", "CA023"))
    expect_identical(results[[name]]$data$ca20g014, frame$ca20g014, info = name)
})

test_that("CA014 recognized whole, frame, residual, partial, empty and no-code claims deny consumers", {
  fixture <- .nrca014_fixture()
  result <- lissr::liss_classify_ca20g014(fixture$source)
  bare <- .nrca014_unmark(result$data)
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
  no_code_frame <- .nrca014_frame(c(8, 9, NA_real_, -10))
  no_code <- lissr:::.liss_ca014_build(no_code_frame,
    lissr:::.liss_ca014_check_raw(no_code_frame, lissr:::.liss_ca014_policy()))
  forms <- list(whole = result, frame = result$data, class_only = class_only,
    marker_only = marker_only, readiness_only = readiness_only, wrapper = list(data = result$data),
    residual = list(data = bare, source_nonresponse_readiness = result$source_nonresponse_readiness),
    partial_whole = partial, partial_frame = partial$data, empty_frame = empty,
    no_code_whole = no_code, no_code_frame = no_code$data)
  inc <- data.frame(nomem_encr = c(830004, 830004, 830006), nohouse_encr = c(1, 1, 2),
    wavenr = c(1, 2, 1), nethh = c(21000, 21500, 30000))
  for (name in names(forms)) {
    input <- forms[[name]]
    before <- serialize(input, NULL, version = 2L)
    expect_true(lissr:::.liss_nonresponse_claims_guarded(input), info = name)
    for (mode in c("flag", "correct", "na_only"))
      expect_error(lissr::liss_clean_income(input, mode = mode, background = NULL,
        variables = list(target = "s339"), ruleset = stop("CA014 forced ruleset"), verbose = FALSE),
        class = "liss_source_nonresponse_blocked", info = paste(name, mode))
    expect_error(lissr::liss_clean_income(inc, background = input,
      variables = list(personal_net = "ca20g014"), verbose = FALSE),
      class = "liss_source_nonresponse_blocked", info = name)
    expect_error(lissr::liss_attach_background(input, sources = stop("CA014 forced sources"),
      month_col = stop("CA014 forced month"), variables = stop("CA014 forced variables")),
      class = "liss_source_nonresponse_blocked", info = name)
    path <- file.path(fixture$directory, paste0("denied_", name))
    expect_error(lissr::liss_cleaning_report(input, path), class = "liss_source_nonresponse_blocked", info = name)
    for (join in c("full", "inner", "left"))
      expect_error(suppressMessages(lissr::merge_liss_panel(list(ca = input), join_type = join,
        write_to = file.path(path, "panel.sav"))), class = "liss_source_nonresponse_blocked", info = name)
    expect_error(lissr::liss_classify_source_nonresponse(input, stop("CA014 forced source"),
      "s339", "wave_id"), class = "liss_source_nonresponse_blocked", info = name)
    expect_error(lissr::liss_create_applied_income_view(input, row_keys = stop("CA014 forced row keys")),
      class = "liss_source_nonresponse_blocked", info = name)
    expect_error(lissr::liss_validate_applied_income(input), class = "liss_source_nonresponse_blocked", info = name)
    expect_error(lissr::liss_validate_applied_income_view(input), class = "liss_applied_income_view_blocked", info = name)
    expect_false(file.exists(path), info = name)
    expect_identical(serialize(input, NULL, version = 2L), before, info = name)
  }
  ordinary <- lissr::liss_clean_income(inc, background = bare, verbose = FALSE)
  expect_identical(as.double(unclass(ordinary$data$ca20g014)), c(8, 8, 0))
  control <- lissr::liss_clean_income(inc, disable = "P01", verbose = FALSE)
  ignored <- lissr::liss_clean_income(inc, background = result$data, disable = "P01", verbose = FALSE)
  expect_identical(ignored$data, control$data)
  expect_identical(ignored$decisions, control$decisions)
})
