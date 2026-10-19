# all values, keys and files below are invented; the real SAV is never read
.nrca016_columns <- function() {
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

.nrca016_receipt <- function() list(filename = "ca20g_EN_1.0p.sav",
  sha256 = "d30acac3d2ca17533fb33ac724e43ff7dc40698139cec42221e1cae104684e47",
  size_bytes = 917084)

.nrca016_values <- function() {
  c(-9999999999, -9999999998, -9999999999, -9, -8, 0, -150,
    -999999999, 999999999, -1000000000, 1000000000, 0.125,
    NA_real_, NaN, Inf, -Inf, -9999999997)
}

.nrca016_frame <- function(values = .nrca016_values()) {
  count <- 5849L
  columns <- .nrca016_columns()
  frame <- stats::setNames(lapply(columns, function(column)
    if (column %in% sprintf("ca20g%03d", 71:74)) rep("invented context", count) else rep(0, count)), columns)
  frame$nomem_encr <- as.double(700000L + seq_len(count))
  attributes(frame$nomem_encr) <- list(label = "Number of household member encrypted",
    format.spss = "F7.0", display_width = 12L)
  frame$ca20g016 <- c(as.double(values), rep(100, count - length(values)))
  attributes(frame$ca20g016) <- list(
    label = paste0("What was the total value of your investments (growth funds, share funds, ",
      "bonds, debentures, stocks, options, warrants) on 31 December 2019?"),
    na_values = c(9999999998, 9999999999),
    class = c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", "double"),
    format.spss = "F11.0", display_width = 15L,
    labels = c("I prefer not to say" = 99999999998, "I don't know" = 99999999999))
  frame$ca20g006 <- rep(1, count)
  attributes(frame$ca20g006) <- list(label = paste0(
    "On 31 December 2019, did you possess one or more of the following assets? - ",
    "Investments (growth funds, share funds, bonds, debentures, stocks, options, warrants, and so on)"),
    format.spss = "F1.0", display_width = 15L,
    class = c("haven_labelled", "vctrs_vctr", "double"), labels = c(no = 0, yes = 1))
  frame$ca20g012 <- rep(-9999999999, count)
  frame$ca20g013 <- structure(rep(-9, count), label = "invented independent012 bracket")
  frame$ca20g017 <- structure(rep(14, count), label = "invented016 bracket; not an amount")
  frame$ca20g035 <- structure(rep(-200, count), label = "invented signed control", format.spss = "F8.0")
  frame$ca20g071 <- structure(frame$ca20g071, label = "invented text control")
  tibble::as_tibble(frame, .name_repair = "minimal")
}

.nrca016_fixture <- function(frame = .nrca016_frame(), .local_envir = parent.frame()) {
  directory <- withr::local_tempdir("ca016_invented_", .local_envir = .local_envir)
  source <- file.path(directory, "ca20g_EN_1.0p.sav")
  file.create(source)
  calls <- new.env(parent = emptyenv())
  calls$steps <- character()
  receipt <- .nrca016_receipt()
  # these two boundaries deliberately assert an invented import, not real admission
  testthat::local_mocked_bindings(.liss_ca016_source_state = function(path) {
    calls$steps <- c(calls$steps, "state")
    receipt
  }, .liss_ca016_read_source = function(path) {
    calls$steps <- c(calls$steps, "read")
    frame
  }, .package = "lissr", .env = .local_envir)
  list(source = source, frame = frame, calls = calls, directory = directory)
}

.nrca016_hash <- function(value) {
  .nr_identity_hash(value)
}

.nrca016_unmark <- function(frame) {
  attr(frame, "liss_source_nonresponse") <- NULL
  attr(frame, "source_nonresponse_readiness") <- NULL
  class(frame) <- c("tbl_df", "tbl", "data.frame")
  frame
}

.nrca016_reseal <- function(result) {
  record <- result$source_nonresponse
  record$original_digest <- .nrca016_hash(result$original_data)
  record$analysis_digest <- .nrca016_hash(.nrca016_unmark(result$analysis$data))
  record$ledger_digest <- .nrca016_hash(result$analysis$ledger)
  record$readiness_digest <- .nrca016_hash(result$source_nonresponse_readiness)
  record$digest <- NULL
  record$digest <- .nrca016_hash(record)
  result$source_nonresponse <- record
  marker <- attr(result$analysis$data, "liss_source_nonresponse")
  marker$parent_digest <- record$original_digest
  marker$analysis_digest <- record$analysis_digest
  marker$classification_digest <- record$digest
  attr(result$analysis$data, "liss_source_nonresponse") <- marker
  result$data <- result$analysis$data
  result
}

test_that("CA016 has an independent complete-source authority and literal projected result", {
  fixture <- .nrca016_fixture()
  before <- serialize(fixture$frame, NULL, version = 2L)
  result <- lissr::liss_classify_ca20g016(fixture$source)
  expect_identical(fixture$calls$steps, c("state", "read", "state"))
  expect_identical(names(result), c("original_data", "analysis", "data",
    "source_nonresponse", "source_nonresponse_readiness"))
  expect_identical(class(result), "liss_source_nonresponse_collection_result")
  expect_identical(class(result$data), c("liss_source_nonresponse_collection_result", "tbl_df", "tbl", "data.frame"))
  expect_identical(serialize(result$original_data, NULL, version = 2L), before)
  expect_identical(serialize(fixture$frame, NULL, version = 2L), before)
  expect_identical(result$data, result$analysis$data)
  expect_identical(dim(result$data), c(5849L, 66L))
  expect_identical(names(result$data), .nrca016_columns())
  expected <- c(rep(NA_real_, 3L), -9, -8, 0, -150, -999999999, 999999999,
    -1000000000, 1000000000, 0.125, NA_real_, NaN, Inf, -Inf, -9999999997, rep(100, 5832L))
  expect_identical(as.double(unclass(result$data$ca20g016)), expected)
  expect_identical(attributes(result$data$ca20g016), attributes(fixture$frame$ca20g016))
  for (column in setdiff(.nrca016_columns(), "ca20g016"))
    expect_identical(serialize(result$data[[column]], NULL, version = 2L),
      serialize(fixture$frame[[column]], NULL, version = 2L), info = column)
  record <- result$source_nonresponse
  expect_identical(record$authority_id, "CA20G016_SOURCE")
  expect_identical(record$authority_version, "1.0.0")
  expect_identical(record$kind, "ca20g016_classification")
  expect_identical(record$construct, "reported_investment_value")
  expect_identical(record$unit, "EUR_whole")
  expect_identical(record$measurement_date, "2019-12-31")
  expect_identical(record$ownership_scope,
    "source household-role and ownership preloads; not asserted to be uniform household total or income")
  expect_identical(record$analysis_metadata_status,
    "retained source description only; does not attest changed analysis values")
  expect_identical(record$documented_signed_range, c(-999999999, 999999999))
  expect_identical(names(record$fields), "CA20G016_FIELD")
  expect_identical(record$source, .nrca016_receipt())
  expected_coverage <- data.frame(source_row_index = seq_len(5849L),
    nomem_key = as.character(700000L + seq_len(5849L)),
    original_value = c(.nrca016_values(), rep(100, 5832L)),
    classified = c(rep(TRUE, 3L), rep(FALSE, 5846L)), stringsAsFactors = FALSE)
  expect_identical(record$coverage, expected_coverage)
  expected_ledger <- data.frame(
    source_cell_key = paste(.nrca016_receipt()$sha256, 1:3, "ca20g016", sep = ":"),
    source_row_index = 1:3, nomem_key = c("700001", "700002", "700003"),
    source_sha256 = rep(.nrca016_receipt()$sha256, 3L),
    source_filename = rep("ca20g_EN_1.0p.sav", 3L), raw_column = rep("ca20g016", 3L),
    case_id = c("CA20G016_N01", "CA20G016_N02", "CA20G016_N01"),
    original_value = c(-9999999999, -9999999998, -9999999999),
    raw_value_repr = c("-9999999999", "-9999999998", "-9999999999"),
    source_declared_missing = rep(FALSE, 3L), answer_status = rep("nonresponse_unknown", 3L),
    candidate_answer_status = rep(NA_character_, 3L), subtype_evidence = rep("unresolved", 3L),
    analysis_disposition = rep("missing_amount", 3L), analysis_value_after = rep(NA_real_, 3L),
    no_numeric_proposal = rep(TRUE, 3L), route_column = rep("ca20g006", 3L),
    route_value = rep(1, 3L), stringsAsFactors = FALSE)
  expect_identical(result$analysis$ledger, expected_ledger)
  readiness <- result$source_nonresponse_readiness
  expect_identical(readiness$overall_amount_validity, "not_evaluated")
  expect_identical(names(readiness$fields), "CA20G016_FIELD")
  expect_identical(readiness$fields$CA20G016_FIELD$evaluated_cells, 5849L)
  expect_identical(readiness$fields$CA20G016_FIELD$classified_cells, 3L)
  expect_identical(readiness$fields$CA20G016_FIELD$unresolved_cells, 0L)
  checked <- lissr::liss_validate_ca20g016(result)
  expect_true(checked$valid)
  expect_identical(checked$source_authentication, FALSE)
  expect_identical(checked$statistical_validity, FALSE)
  expect_identical(checked$source_consistency, "not_requested")
})

test_that("CA016 rejects wrong local paths and unverified invented bytes before parsing", {
  directory <- withr::local_tempdir("ca016_bad_bytes_")
  source <- file.path(directory, "ca20g_EN_1.0p.sav")
  writeBin(charToRaw("invented wrong source"), source)
  parsed <- 0L
  testthat::local_mocked_bindings(read_sav = function(...) {
    parsed <<- parsed + 1L
    stop("unverified bytes reached haven")
  }, .package = "haven")
  expect_error(lissr::liss_classify_ca20g016(), class = "liss_ca20g016_blocked")
  for (path in list(NULL, NA_character_, "", FALSE, c(source, source), factor(source),
    structure(source, label = "invented claim"), directory, file.path(directory, "missing.sav"), source))
    expect_error(lissr::liss_classify_ca20g016(path), class = "liss_ca20g016_blocked")
  expect_error(lissr:::.liss_ca016_read_source(source), class = "liss_ca20g016_blocked")
  writeBin(raw(917084L), source)
  expect_error(lissr::liss_classify_ca20g016(source), class = "liss_ca20g016_blocked")
  expect_error(lissr:::.liss_ca016_read_source(source), class = "liss_ca20g016_blocked")
  for (filename in c("ca20g_NL_1.0p.sav", "ca20g_EN_1.1p.sav", "ca21h_EN_1.0p.sav")) {
    path <- file.path(directory, filename)
    file.create(path)
    expect_error(lissr::liss_classify_ca20g016(path), class = "liss_ca20g016_blocked")
  }
  expect_identical(parsed, 0L)
})

test_that("CA016 passes the exact verified invented buffer to haven under a test identity", {
  bytes <- charToRaw("explicitly invented parser-buffer probe")
  digest <- as.character(openssl::sha256(bytes))
  attributes(digest) <- NULL
  path <- withr::local_tempfile(pattern = "ca016_buffer_", fileext = ".sav")
  writeBin(bytes, path)
  seen <- NULL
  settings <- NULL
  expected <- .nrca016_frame()
  testthat::local_mocked_bindings(.liss_ca016_policy = function()
    list(source = list(size_bytes = length(bytes), sha256 = digest)), .package = "lissr")
  testthat::local_mocked_bindings(read_sav = function(file, user_na, .name_repair) {
    seen <<- file
    settings <<- list(user_na = user_na, name_repair = .name_repair)
    expected
  }, .package = "haven")
  expect_identical(lissr:::.liss_ca016_read_source(path), expected)
  expect_identical(seen, bytes)
  expect_identical(settings, list(user_na = TRUE, name_repair = "minimal"))
  writeBin(c(bytes, as.raw(1)), path)
  expect_error(lissr:::.liss_ca016_read_source(path), class = "liss_ca20g016_blocked")
})

test_that("CA016 requires exact before and after identities around an invented import", {
  for (phase in c(1L, 2L)) for (field in c("filename", "sha256", "size_bytes")) local({
    fixture <- .nrca016_fixture(.local_envir = environment())
    states <- 0L
    testthat::local_mocked_bindings(.liss_ca016_source_state = function(path) {
      states <<- states + 1L
      receipt <- .nrca016_receipt()
      if (states == phase) receipt[[field]] <- switch(field,
        filename = "ca21h_EN_1.0p.sav", sha256 = strrep("0", 64L), size_bytes = 917085)
      receipt
    }, .package = "lissr", .env = environment())
    expect_error(lissr::liss_classify_ca20g016(fixture$source), class = "liss_ca20g016_blocked",
      info = paste(phase, field))
    expect_identical(fixture$calls$steps, if (phase == 1L) character() else "read")
  })
})

test_that("CA016 refuses malformed complete schemas, keys and pinned metadata", {
  original <- .nrca016_frame()
  replace_column <- function(frame, column) {
    values <- unclass(frame)
    values$ca20g035 <- column
    attributes(values) <- attributes(frame)
    values
  }
  changes <- list(
    field_alias = function(x) { names(x)[19L] <- "invented_target_alias"; x },
    column_order = function(x) x[, rev(names(x)), drop = FALSE],
    row_names_order = function(x) { attr(x, "row.names") <- rev(seq_len(nrow(x))); x },
    incomplete_rows = function(x) x[-1L, , drop = FALSE],
    missing_column = function(x) { x$ca20g075 <- NULL; x },
    wrong_type = function(x) { x$ca20g001 <- as.integer(x$ca20g001); x },
    extra_attribute = function(x) { attr(x, "invented_extra") <- TRUE; x },
    wrong_class = function(x) { class(x) <- "data.frame"; x },
    target_metadata = function(x) { attr(x$ca20g016, "label") <- "invented wrong field"; x },
    gate_metadata = function(x) { attr(x$ca20g006, "label") <- "invented wrong gate"; x },
    id_metadata = function(x) { attr(x$nomem_encr, "label") <- "invented wrong id"; x },
    duplicate_key = function(x) { x$nomem_encr[2L] <- x$nomem_encr[1L]; x },
    missing_key = function(x) { x$nomem_encr[1L] <- NA_real_; x },
    negative_key = function(x) { x$nomem_encr[1L] <- -1; x },
    fractional_key = function(x) { x$nomem_encr[1L] <- 1.5; x },
    lossy_key = function(x) { x$nomem_encr[1L] <- 2^53; x },
    infinite_key = function(x) { x$nomem_encr[1L] <- Inf; x },
    short_column = function(x) replace_column(x, -200),
    long_column = function(x) replace_column(x, rep(-200, 5850L)),
    matrix_column = function(x) replace_column(x, matrix(rep(-200, 5849L), ncol = 1L)))
  for (name in names(changes)) local({
    malformed <- changes[[name]](original)
    before <- serialize(malformed, NULL, version = 2L)
    fixture <- .nrca016_fixture(malformed, .local_envir = environment())
    expect_error(lissr::liss_classify_ca20g016(fixture$source), class = "liss_ca20g016_blocked", info = name)
    expect_identical(serialize(malformed, NULL, version = 2L), before, info = name)
  })
})

test_that("CA016 requires006 gate one only on its two exact coded values", {
  for (gate in c(0, -1, NA_real_, NaN, Inf)) local({
    frame <- .nrca016_frame()
    frame$ca20g006[1L] <- gate
    fixture <- .nrca016_fixture(frame, .local_envir = environment())
    expect_error(lissr::liss_classify_ca20g016(fixture$source), class = "liss_ca20g016_blocked")
  })
  frame <- .nrca016_frame()
  frame$ca20g006[4:8] <- c(0, -1, NA_real_, NaN, Inf)
  frame$ca20g078 <- rep(0, 5849L)
  fixture <- .nrca016_fixture(frame)
  result <- lissr::liss_classify_ca20g016(fixture$source)
  expect_true(lissr::liss_validate_ca20g016(result)$valid)
  expect_identical(result$data$ca20g006, frame$ca20g006)
  expect_identical(result$data$ca20g078, frame$ca20g078)
  expect_identical(as.double(unclass(result$data$ca20g016))[4:8], c(-9, -8, 0, -150, -999999999))
})

test_that("CA016 rejects all four positive forms during classification and retained-original validation", {
  fixture <- .nrca016_fixture()
  result <- lissr::liss_classify_ca20g016(fixture$source)
  for (code in c(9999999998, 9999999999, 99999999998, 99999999999)) local({
    frame <- .nrca016_frame()
    frame$ca20g016[4L] <- code
    positive_fixture <- .nrca016_fixture(frame, .local_envir = environment())
    expect_error(lissr::liss_classify_ca20g016(positive_fixture$source), class = "liss_ca20g016_blocked")
  })
  testthat::local_mocked_bindings(.liss_ca016_build = function(...) stop("positive original reached projection"),
    .package = "lissr")
  for (code in c(9999999998, 9999999999, 99999999998, 99999999999)) {
    changed <- result
    changed$original_data$ca20g016[4L] <- code
    changed <- .nrca016_reseal(changed)
    before <- serialize(changed, NULL, version = 2L)
    fixture$calls$steps <- character()
    for (source in list(NULL, fixture$source))
      expect_error(lissr::liss_validate_ca20g016(changed, source = source),
        class = "liss_ca20g016_blocked", regexp = "positive")
    expect_identical(fixture$calls$steps, character())
    expect_identical(serialize(changed, NULL, version = 2L), before)
  }
})

test_that("CA016 no-hit sources have a complete coverage table and typed empty ledger", {
  fixture <- .nrca016_fixture(.nrca016_frame(c(-9, -8, 0, -10.5, 0.125, NA_real_, NaN, Inf, -Inf)))
  result <- lissr::liss_classify_ca20g016(fixture$source)
  expect_identical(serialize(.nrca016_unmark(result$data), NULL, version = 2L),
    serialize(fixture$frame, NULL, version = 2L))
  expect_identical(nrow(result$source_nonresponse$coverage), 5849L)
  expect_identical(result$source_nonresponse$coverage$classified, rep(FALSE, 5849L))
  ledger <- result$analysis$ledger
  expect_identical(ledger$source_row_index, integer())
  expect_identical(ledger$source_cell_key, character())
  expect_identical(ledger$original_value, double())
  expect_identical(ledger$analysis_value_after, double())
  expect_identical(ledger$source_declared_missing, logical())
  expect_identical(result$source_nonresponse_readiness$status, "passed")
  expect_true(lissr::liss_validate_ca20g016(result)$valid)
})

test_that("CA016 closed whole reconstruction rejects stale and resealed mutations", {
  fixture <- .nrca016_fixture()
  result <- lissr::liss_classify_ca20g016(fixture$source)
  changes <- list(
    missing_original = function(x) { x$original_data <- NULL; x },
    missing_analysis = function(x) { x$analysis <- NULL; x },
    extra_wrapper_field = function(x) { x$invented_extra <- TRUE; x },
    wrapper_attribute = function(x) { attr(x, "invented_extra") <- TRUE; x },
    wrapper_class = function(x) { class(x) <- "list"; x },
    analysis_shape = function(x) { x$analysis$invented_extra <- TRUE; x },
    duplicate_data = function(x) { x$data$ca20g016[1L] <- 1; x },
    locked_value = function(x) { x$analysis$data$ca20g016[1L] <- 1; x },
    sign_repair = function(x) { x$analysis$data$ca20g016[4L] <- 9; x },
    non_target012 = function(x) { x$analysis$data$ca20g012[1L] <- NA_real_; x },
    target_metadata = function(x) { attr(x$analysis$data$ca20g016, "label") <- "invented changed label"; x },
    ledger_row = function(x) { x$analysis$ledger$source_row_index[1L] <- 4L; x },
    ledger_type = function(x) { x$analysis$ledger$source_row_index <- as.double(x$analysis$ledger$source_row_index); x },
    ledger_value = function(x) { x$analysis$ledger$original_value[1L] <- -9; x },
    ledger_key = function(x) { x$analysis$ledger$source_cell_key[1L] <- "invented forged key"; x },
    ledger_subtype = function(x) { x$analysis$ledger$answer_status[1L] <- "dont_know"; x },
    ledger_numeric_candidate = function(x) { x$analysis$ledger$analysis_value_after[1L] <- 10; x },
    readiness = function(x) { x$source_nonresponse_readiness$status <- "certified"; x },
    overall_amount = function(x) { x$source_nonresponse_readiness$overall_amount_validity <- "passed"; x },
    ownership = function(x) { x$source_nonresponse$ownership_scope <- "uniform household income"; x },
    analysis_metadata_claim = function(x) { x$source_nonresponse$analysis_metadata_status <- "attested"; x },
    fields = function(x) { x$source_nonresponse$fields$CA20G012_FIELD <- list(); x },
    native_schema = function(x) { x$source_nonresponse$schema_version <- "4.0.0"; x })
  for (name in names(changes)) {
    changed <- changes[[name]](result)
    expect_error(lissr::liss_validate_ca20g016(changed), class = "liss_ca20g016_blocked", info = name)
    if (name == "duplicate_data") next
    resealed <- tryCatch(.nrca016_reseal(changed), error = identity)
    if (!inherits(resealed, "error"))
      expect_error(lissr::liss_validate_ca20g016(resealed), class = "liss_ca20g016_blocked",
        info = paste(name, "resealed"))
  }
})

test_that("CA016 strict validation retains signed-zero, attribute-order and missing payload distinctions", {
  fixture <- .nrca016_fixture()
  result <- lissr::liss_classify_ca20g016(fixture$source)
  negative_zero <- -1 * 0
  expect_true(identical(negative_zero, 0))
  expect_false(identical(serialize(negative_zero, NULL, version = 2L), serialize(0, NULL, version = 2L)))
  zero <- result
  target <- as.double(unclass(zero$analysis$data$ca20g016))
  target[6L] <- negative_zero
  attributes(target) <- attributes(result$analysis$data$ca20g016)
  zero$analysis$data$ca20g016 <- target
  zero$data <- zero$analysis$data
  reordered <- result
  attributes(reordered$analysis$data$ca20g016) <- rev(attributes(reordered$analysis$data$ca20g016))
  reordered$data <- reordered$analysis$data
  tagged <- result
  tagged$analysis$ledger$analysis_value_after[1L] <- haven::tagged_na("a")
  for (changed in list(zero, reordered, tagged)) {
    expect_true(identical(changed, result))
    expect_false(identical(serialize(changed, NULL, version = 2L), serialize(result, NULL, version = 2L)))
    expect_error(lissr::liss_validate_ca20g016(changed), class = "liss_ca20g016_blocked")
  }
})

test_that("CA016 source consistency distinguishes complete-original substitution from authentication", {
  fixture <- .nrca016_fixture()
  original <- fixture$frame
  result <- lissr::liss_classify_ca20g016(fixture$source)
  changed <- original
  changed$ca20g017[1L] <- 1
  rebuilt <- lissr:::.liss_ca016_build(changed,
    lissr:::.liss_ca016_check_raw(changed, lissr:::.liss_ca016_policy()))
  checked <- lissr::liss_validate_ca20g016(rebuilt)
  expect_true(checked$valid)
  expect_identical(checked$source_authentication, FALSE)
  expect_error(lissr::liss_validate_ca20g016(rebuilt, source = fixture$source), class = "liss_ca20g016_blocked")
  changed <- original
  attributes(changed$ca20g035) <- rev(attributes(changed$ca20g035))
  expect_true(identical(changed, original))
  expect_false(identical(serialize(changed, NULL, version = 2L), serialize(original, NULL, version = 2L)))
  rebuilt <- lissr:::.liss_ca016_build(changed,
    lissr:::.liss_ca016_check_raw(changed, lissr:::.liss_ca016_policy()))
  expect_true(lissr::liss_validate_ca20g016(rebuilt)$valid)
  expect_error(lissr::liss_validate_ca20g016(rebuilt, source = fixture$source), class = "liss_ca20g016_blocked")
  expect_identical(lissr::liss_validate_ca20g016(result, source = fixture$source)$source_consistency, "matched")
})

test_that("CA016 source equality also requires reference identity", {
  original <- .nrca016_frame()
  attr(original$ca20g035, "invented_context") <- new.env(parent = emptyenv())
  fixture <- .nrca016_fixture(original)
  changed <- original
  attr(changed$ca20g035, "invented_context") <- new.env(parent = emptyenv())
  expect_false(identical(changed, original))
  expect_identical(serialize(changed, NULL, version = 2L), serialize(original, NULL, version = 2L))
  rebuilt <- lissr:::.liss_ca016_build(changed,
    lissr:::.liss_ca016_check_raw(changed, lissr:::.liss_ca016_policy()))
  expect_true(lissr::liss_validate_ca20g016(rebuilt)$valid)
  expect_error(lissr::liss_validate_ca20g016(rebuilt, source = fixture$source), class = "liss_ca20g016_blocked")
})

test_that("CA016 accepts whole-result file transport with version2 and current default RDS", {
  fixture <- .nrca016_fixture()
  result <- lissr::liss_classify_ca20g016(fixture$source)
  before <- serialize(result, NULL, version = 2L)
  for (version in c("2", "default")) {
    path <- withr::local_tempfile(pattern = paste0("invented_ca016_", version, "_"), fileext = ".rds")
    if (version == "2") saveRDS(result, path, version = 2L) else saveRDS(result, path)
    restored <- readRDS(path)
    expect_identical(serialize(restored, NULL, version = 2L), before)
    expect_true(lissr::liss_validate_ca20g016(restored)$valid)
    expect_identical(lissr::liss_validate_ca20g016(restored, source = fixture$source)$source_consistency, "matched")
  }
  expect_identical(serialize(result, NULL, version = 2L), before)
})

test_that("CA016 tolerates moved guard placement only when the unmarked payload stays exact", {
  fixture <- .nrca016_fixture()
  result <- lissr::liss_classify_ca20g016(fixture$source)
  changed <- result
  frame_class <- class(changed$analysis$data)
  class(changed$analysis$data) <- NULL
  class(changed$analysis$data) <- frame_class
  changed$data <- changed$analysis$data
  expect_true(identical(changed, result))
  expect_false(identical(serialize(changed, NULL, version = 2L), serialize(result, NULL, version = 2L)))
  expect_identical(serialize(.nrca016_unmark(changed$data), NULL, version = 2L),
    serialize(.nrca016_unmark(result$data), NULL, version = 2L))
  expect_true(lissr::liss_validate_ca20g016(changed)$valid)
  expect_identical(lissr::liss_validate_ca20g016(changed, source = fixture$source)$source_consistency, "matched")
})

test_that("CA016 authority bytes and helpers remain separate from CA012 and the frozen engine", {
  policy <- lissr:::.liss_ca016_policy()
  expect_identical(policy$authority_id, "CA20G016_SOURCE")
  expect_identical(policy$authority_version, "1.0.0")
  expect_identical(policy$raw_column, "ca20g016")
  expect_identical(policy$route_column, "ca20g006")
  expect_identical(policy$source, .nrca016_receipt())
  expect_identical(policy$codes, c(-9999999999, -9999999998))
  expect_identical(policy$case_ids, c("CA20G016_N01", "CA20G016_N02"))
  expect_identical(policy$metadata_sha256$ca20g016,
    "bf0dd684c20158e9c4eb195d8c42145254a9cfa00ebf53ccae3d71c8a1531de5")
  expect_identical(policy$metadata_sha256$ca20g006,
    "1283d2c8c53dd98e5d72d9baef5aaed83cc969a90104aa691e3e9fdaa48f06f4")
  expect_identical(lissr:::.liss_ca012_policy()$authority_id, "CA20G012_SOURCE")
  expect_false(identical(lissr:::.liss_ca016_policy_sha256(), lissr:::.liss_ca012_policy_sha256()))
  engine <- lissr:::.liss_applied_engine()
  expect_false(any(grepl("ca016", names(engine$functions))))
  path <- lissr:::.liss_ca016_policy_path()
  expect_identical(basename(path), "source_nonresponse_ca20g016.yml")
  changed <- withr::local_tempfile(fileext = ".yml")
  writeLines(c(readLines(path, warn = FALSE), "unreviewed: true"), changed)
  testthat::local_mocked_bindings(.liss_ca016_policy_path = function() changed, .package = "lissr")
  expect_error(lissr:::.liss_ca016_policy(), class = "liss_ca20g016_blocked")
})

test_that("CA012 and CA016 validators reject each other's complete result from one dual-metadata fixture", {
  frame <- .nrca016_frame()
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
  fixture <- .nrca016_fixture(frame)
  testthat::local_mocked_bindings(.liss_ca012_source_state = function(path) .nrca016_receipt(),
    .liss_ca012_read_source = function(path) frame, .package = "lissr")
  result016 <- lissr::liss_classify_ca20g016(fixture$source)
  result012 <- lissr::liss_classify_ca20g012(fixture$source)
  expect_true(lissr::liss_validate_ca20g016(result016)$valid)
  expect_true(lissr::liss_validate_ca20g012(result012)$valid)
  expect_error(lissr::liss_validate_ca20g012(result016), class = "liss_ca20g012_blocked")
  expect_error(lissr::liss_validate_ca20g016(result012), class = "liss_ca20g016_blocked")
  expect_identical(result016$original_data, result012$original_data)
  expect_identical(result016$data$ca20g012, frame$ca20g012)
  expect_identical(result012$data$ca20g016, frame$ca20g016)
})

test_that("emitted CA016 results and recognized residual claims deny unsupported consumers", {
  fixture <- .nrca016_fixture()
  result <- lissr::liss_classify_ca20g016(fixture$source)
  bare <- .nrca016_unmark(result$data)
  class_only <- bare
  class(class_only) <- class(result$data)
  marker_only <- bare
  attr(marker_only, "liss_source_nonresponse") <- attr(result$data, "liss_source_nonresponse")
  readiness_only <- bare
  attr(readiness_only, "source_nonresponse_readiness") <- result$source_nonresponse_readiness
  forms <- list(whole = result, frame = result$data, class_only = class_only,
    marker_only = marker_only, readiness_only = readiness_only, wrapper = list(data = result$data))
  inc <- data.frame(nomem_encr = c(700004, 700004, 700006), nohouse_encr = c(1, 1, 2),
    wavenr = c(1, 2, 1), nethh = c(21000, 21500, 30000))
  for (name in names(forms)) {
    input <- forms[[name]]
    before <- serialize(input, NULL, version = 2L)
    expect_true(lissr:::.liss_nonresponse_claims_guarded(input), info = name)
    for (mode in c("flag", "correct", "na_only"))
      expect_error(lissr::liss_clean_income(input, mode = mode, background = NULL,
        variables = list(target = "s339"), ruleset = stop("CA016 forced ruleset"), verbose = FALSE),
        class = "liss_source_nonresponse_blocked", info = paste(name, mode))
    expect_error(lissr::liss_clean_income(inc, background = input,
      variables = list(personal_net = "ca20g016"), verbose = FALSE),
      class = "liss_source_nonresponse_blocked", info = name)
    expect_error(lissr::liss_attach_background(input, sources = stop("CA016 forced sources"),
      month_col = stop("CA016 forced month"), variables = stop("CA016 forced variables")),
      class = "liss_source_nonresponse_blocked", info = name)
    path <- file.path(fixture$directory, paste0("denied_", name))
    expect_error(lissr::liss_cleaning_report(input, path), class = "liss_source_nonresponse_blocked", info = name)
    for (join in c("full", "inner", "left"))
      expect_error(suppressMessages(lissr::merge_liss_panel(list(ca = input), join_type = join,
        write_to = file.path(path, "panel.sav"))), class = "liss_source_nonresponse_blocked", info = name)
    expect_error(lissr::liss_classify_source_nonresponse(input, stop("CA016 forced source"),
      "s339", "wave_id"), class = "liss_source_nonresponse_blocked", info = name)
    expect_error(lissr::liss_create_applied_income_view(input, row_keys = stop("CA016 forced row keys")),
      class = "liss_source_nonresponse_blocked", info = name)
    expect_false(file.exists(path), info = name)
    expect_identical(serialize(input, NULL, version = 2L), before, info = name)
  }
  expect_error(lissr::liss_validate_applied_income(result), class = "liss_source_nonresponse_blocked")
  expect_error(lissr::liss_validate_applied_income_view(result), class = "liss_applied_income_view_blocked")
  ordinary <- lissr::liss_clean_income(inc, background = bare, verbose = FALSE)
  expect_identical(as.double(unclass(ordinary$data$ca20g016)), c(-9, -9, 0))
  control <- lissr::liss_clean_income(inc, disable = "P01", verbose = FALSE)
  ignored <- lissr::liss_clean_income(inc, background = result$data, disable = "P01", verbose = FALSE)
  expect_identical(ignored$data, control$data)
  expect_identical(ignored$decisions, control$decisions)
})
