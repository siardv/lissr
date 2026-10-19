# every row and file used here is invented; the real CA source is never read
.nrca_columns <- function() {
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

.nrca_receipt <- function() {
  list(filename = "ca20g_EN_1.0p.sav",
    sha256 = "d30acac3d2ca17533fb33ac724e43ff7dc40698139cec42221e1cae104684e47",
    size_bytes = 917084)
}

.nrca_frame <- function(values = c(-9999999999, -9999999998, -9999999999,
    -9, -8, 0, -999999999, 999999999, -9999999997, -10000000000,
    123, NA_real_, NaN, Inf, -Inf)) {
  count <- 5849L
  columns <- .nrca_columns()
  frame <- stats::setNames(lapply(columns, function(name)
    if (name %in% sprintf("ca20g%03d", 71:74)) rep("invented", count) else rep(0, count)), columns)
  frame$nomem_encr <- as.double(50000L + seq_len(count))
  attributes(frame$nomem_encr) <- list(label = "Number of household member encrypted",
    format.spss = "F7.0", display_width = 12L)
  frame$ca20g012 <- c(as.double(values), rep(100, count - length(values)))
  attributes(frame$ca20g012) <- list(
    label = paste0("What was the total balance of your banking account or giro (current accounts), ",
      "savings accounts, term deposit accounts, savings bonds or savings certificates and bank ",
      "savings schemes on 31 December 2019?"),
    na_values = c(9999999998, 9999999999),
    class = c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", "double"),
    format.spss = "F11.0", display_width = 15L,
    labels = c("I prefer not to say" = 99999999998, "I don't know" = 99999999999))
  frame$ca20g078 <- rep(1, count)
  attributes(frame$ca20g078) <- list(label = paste0(
    "On 31 December 2019, did you possess one or more of the following assets? - ",
    "Banking accounts, savings accounts, term deposit accounts, savings bonds or ",
    "savings certificates, banksparen"), format.spss = "F1.0", display_width = 15L,
    class = c("haven_labelled", "vctrs_vctr", "double"), labels = c(no = 0, yes = 1))
  frame$ca20g013[1:3] <- c(-9, NA_real_, 2)
  frame$ca20g016 <- rep(-9999999999, count)
  frame$ca20g035 <- structure(rep(-200, count), label = "invented negative equity control")
  frame$ca20g071 <- structure(frame$ca20g071, label = "invented text context")
  tibble::as_tibble(frame, .name_repair = "minimal")
}

.nrca_fixture <- function(frame = .nrca_frame(), .local_envir = parent.frame()) {
  directory <- withr::local_tempdir("ca012_invented_", .local_envir = .local_envir)
  source <- file.path(directory, "ca20g_EN_1.0p.sav")
  file.create(source)
  calls <- new.env(parent = emptyenv())
  calls$steps <- character()
  receipt <- .nrca_receipt()
  testthat::local_mocked_bindings(.liss_ca012_source_state = function(path) {
    calls$steps <- c(calls$steps, "state")
    receipt
  }, .liss_ca012_read_source = function(path) {
    calls$steps <- c(calls$steps, "read")
    frame
  }, .package = "lissr", .env = .local_envir)
  list(source = source, frame = frame, calls = calls, directory = directory)
}

.nrca_hash <- function(value) {
  .nr_identity_hash(value)
}

.nrca_unmark <- function(frame) {
  attr(frame, "liss_source_nonresponse") <- NULL
  attr(frame, "source_nonresponse_readiness") <- NULL
  class(frame) <- c("tbl_df", "tbl", "data.frame")
  frame
}

.nrca_reseal <- function(result) {
  record <- result$source_nonresponse
  record$original_digest <- .nrca_hash(result$original_data)
  record$analysis_digest <- .nrca_hash(.nrca_unmark(result$analysis$data))
  record$ledger_digest <- .nrca_hash(result$analysis$ledger)
  record$readiness_digest <- .nrca_hash(result$source_nonresponse_readiness)
  record$digest <- NULL
  record$digest <- .nrca_hash(record)
  result$source_nonresponse <- record
  marker <- attr(result$analysis$data, "liss_source_nonresponse")
  marker$parent_digest <- record$original_digest
  marker$analysis_digest <- record$analysis_digest
  marker$classification_digest <- record$digest
  attr(result$analysis$data, "liss_source_nonresponse") <- marker
  result$data <- result$analysis$data
  result
}

test_that("standalone CA classification retains the complete imported source and exact two-code projection", {
  fixture <- .nrca_fixture()
  before <- serialize(fixture$frame, NULL, version = 2L)
  result <- lissr::liss_classify_ca20g012(fixture$source)
  expect_identical(fixture$calls$steps, c("state", "read", "state"))
  expect_identical(names(result), c("original_data", "analysis", "data",
    "source_nonresponse", "source_nonresponse_readiness"))
  expect_identical(class(result), "liss_source_nonresponse_collection_result")
  expect_identical(class(result$data), c("liss_source_nonresponse_collection_result", "tbl_df", "tbl", "data.frame"))
  expect_identical(result$original_data, fixture$frame)
  expect_identical(serialize(fixture$frame, NULL, version = 2L), before)
  expect_identical(result$data, result$analysis$data)
  expect_identical(names(result$data), .nrca_columns())
  expect_identical(dim(result$data), c(5849L, 66L))
  expect_identical(row.names(result$data), row.names(fixture$frame))
  original <- as.double(unclass(fixture$frame$ca20g012))
  expected <- original
  lock <- is.finite(original) & original %in% c(-9999999999, -9999999998)
  expected[lock] <- NA_real_
  expect_identical(as.double(unclass(result$data$ca20g012)), expected)
  expect_identical(attributes(result$data$ca20g012), attributes(fixture$frame$ca20g012))
  for (name in setdiff(names(fixture$frame), "ca20g012"))
    expect_identical(result$data[[name]], fixture$frame[[name]], info = name)
  ledger <- result$analysis$ledger
  expect_identical(ledger$source_row_index, c(1L, 2L, 3L))
  expect_identical(ledger$original_value, original[lock])
  expect_identical(ledger$case_id, c("CA20G012_N01", "CA20G012_N02", "CA20G012_N01"))
  expect_identical(ledger$source_cell_key,
    paste(.nrca_receipt()$sha256, which(lock), "ca20g012", sep = ":"))
  expect_false(anyDuplicated(ledger$source_cell_key) > 0L)
  expect_true(all(ledger$answer_status == "nonresponse_unknown"))
  expect_true(all(is.na(ledger$candidate_answer_status)))
  expect_true(all(ledger$subtype_evidence == "unresolved"))
  expect_true(all(ledger$analysis_disposition == "missing_amount"))
  expect_true(all(!ledger$source_declared_missing))
  expect_true(all(ledger$no_numeric_proposal))
  expect_true(all(is.na(ledger$analysis_value_after)))
  expect_identical(result$source_nonresponse$schema_version, "1.0.0")
  expect_identical(result$source_nonresponse$kind, "ca20g012_classification")
  expect_identical(result$source_nonresponse$construct, "banking_and_savings_balance")
  expect_identical(result$source_nonresponse$unit, "EUR_whole")
  expect_identical(result$source_nonresponse$measurement_date, "2019-12-31")
  expect_identical(names(result$source_nonresponse$fields), "CA20G012_FIELD")
  expect_identical(names(result$source_nonresponse_readiness$fields), "CA20G012_FIELD")
  expect_identical(result$source_nonresponse_readiness$overall_amount_validity, "not_evaluated")
  fixture$calls$steps <- character()
  validation <- lissr::liss_validate_ca20g012(result)
  expect_true(validation$valid)
  expect_identical(validation$validation, "structural")
  expect_identical(validation$source_consistency, "not_requested")
  expect_identical(validation$source_authentication, FALSE)
  expect_identical(validation$statistical_validity, FALSE)
  expect_identical(fixture$calls$steps, character())
  source_validation <- lissr::liss_validate_ca20g012(result, source = fixture$source)
  expect_true(source_validation$valid)
  expect_identical(source_validation$validation, "source_consistency")
  expect_identical(source_validation$source_consistency, "matched")
  expect_identical(fixture$calls$steps, c("state", "read", "state"))
})

test_that("source path checks and source identity mismatches reject before import", {
  reached <- FALSE
  testthat::local_mocked_bindings(.liss_ca012_read_source = function(path) {
    reached <<- TRUE
    stop("incorrect identity reached import")
  }, .package = "lissr")
  directory <- withr::local_tempdir("ca012_wrong_source_")
  source <- file.path(directory, "ca20g_EN_1.0p.sav")
  writeBin(charToRaw("invented wrong source bytes"), source)
  expect_error(lissr::liss_classify_ca20g012(), class = "liss_ca20g012_blocked")
  invalid <- list(NULL, NA_character_, "", c(source, source), factor(source),
    structure(source, label = "claim"), list(source), directory,
    file.path(directory, "missing_ca20g_EN_1.0p.sav"), source)
  for (path in invalid)
    expect_error(lissr::liss_classify_ca20g012(path), class = "liss_ca20g012_blocked")
  expect_false(reached)
  expect_error(lissr:::.liss_ca012_source_state(source), class = "liss_ca20g012_blocked")
  for (name in c("ca22h_EN_1.0p.sav", "ca20g_NL_1.0p.sav", "ca20g_EN_1.1p.sav")) {
    path <- file.path(directory, name)
    file.copy(source, path)
    expect_error(lissr::liss_classify_ca20g012(path), class = "liss_ca20g012_blocked")
  }
  expect_false(reached)
  if (.Platform$OS.type == "unix" && nzchar(Sys.which("mkfifo"))) {
    fifo_directory <- file.path(directory, "fifo")
    dir.create(fifo_directory)
    fifo_path <- file.path(fifo_directory, "ca20g_EN_1.0p.sav")
    expect_identical(system2(Sys.which("mkfifo"), shQuote(fifo_path)), 0L)
    expect_error(lissr::liss_classify_ca20g012(fifo_path), class = "liss_ca20g012_blocked")
    expect_false(reached)
  }
})

test_that("before and after source identities are exact and changed files cannot return a result", {
  fixture <- .nrca_fixture()
  for (when in c("before", "after")) for (key in c("filename", "sha256", "size_bytes")) local({
    calls <- character()
    count <- 0L
    testthat::local_mocked_bindings(.liss_ca012_source_state = function(path) {
      count <<- count + 1L
      calls <<- c(calls, "state")
      receipt <- .nrca_receipt()
      if ((when == "before" && count == 1L) || (when == "after" && count == 2L))
        receipt[[key]] <- switch(key, filename = "ca22h_EN_1.0p.sav", sha256 = strrep("a", 64), size_bytes = 917085)
      receipt
    }, .liss_ca012_read_source = function(path) { calls <<- c(calls, "read"); fixture$frame },
    .package = "lissr", .env = environment())
    expect_error(lissr::liss_classify_ca20g012(fixture$source), class = "liss_ca20g012_blocked")
    expect_identical(calls, if (when == "before") "state" else c("state", "read", "state"))
  })
})

test_that("the real byte reader rejects unapproved bytes and imports verified invented bytes unchanged", {
  directory <- withr::local_tempdir("ca012_byte_reader_")
  path <- file.path(directory, "ca20g_EN_1.0p.sav")
  writeBin(charToRaw("invented unapproved bytes"), path)
  reached <- FALSE
  local({
    testthat::local_mocked_bindings(read_sav = function(...) {
      reached <<- TRUE
      stop("unapproved bytes reached haven")
    }, .package = "haven", .env = environment())
    expect_error(lissr:::.liss_ca012_read_source(path), class = "liss_ca20g012_blocked")
    writeBin(raw(917084L), path)
    expect_error(lissr:::.liss_ca012_read_source(path), class = "liss_ca20g012_blocked")
  })
  expect_false(reached)
  invented <- data.frame(amount = haven::labelled_spss(c(1, 99, -9),
    na_values = 99, label = "invented transport only"))
  haven::write_sav(invented, path)
  expected <- haven::read_sav(path, user_na = TRUE, .name_repair = "minimal")
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  policy <- lissr:::.liss_ca012_policy()
  policy$source$size_bytes <- as.numeric(length(bytes))
  sha <- as.character(openssl::sha256(bytes))
  attributes(sha) <- NULL
  policy$source$sha256 <- sha
  arguments <- NULL
  testthat::local_mocked_bindings(.liss_ca012_policy = function() policy, .package = "lissr")
  expect_identical(lissr:::.liss_ca012_read_source(path), expected)
  testthat::local_mocked_bindings(read_sav = function(file, user_na, .name_repair) {
    arguments <<- list(file = file, user_na = user_na, name_repair = .name_repair)
    expected
  }, .package = "haven")
  expect_identical(lissr:::.liss_ca012_read_source(path), expected)
  expect_identical(arguments, list(file = bytes, user_na = TRUE, name_repair = "minimal"))
  expect_identical(class(expected), c("tbl_df", "tbl", "data.frame"))
})

test_that("complete source schema keys metadata and coded routing are required", {
  original <- .nrca_frame()
  changes <- list(
    partial_rows = function(x) x[-1L, , drop = FALSE],
    empty = function(x) x[integer(), , drop = FALSE],
    partial_columns = function(x) x[setdiff(names(x), "ca20g035")],
    order = function(x) x[rev(names(x))],
    renamed_alias = function(x) { names(x)[names(x) == "ca20g012"] <- "s012"; x },
    duplicate_name = function(x) { names(x)[1L] <- "ca20g012"; x },
    plain_frame = function(x) as.data.frame(x),
    target_integer = function(x) { x$ca20g012 <- rep(1L, nrow(x)); x },
    text_type = function(x) { x$ca20g071 <- rep(0, nrow(x)); x },
    target_label = function(x) { attr(x$ca20g012, "label") <- "different balance"; x },
    target_na = function(x) { attr(x$ca20g012, "na_values") <- c(-9999999999, -9999999998); x },
    target_labels = function(x) { attr(x$ca20g012, "labels") <- NULL; x },
    id_metadata = function(x) { attr(x$nomem_encr, "format.spss") <- "F8.0"; x },
    gate_metadata = function(x) { attr(x$ca20g078, "label") <- "claim"; x },
    duplicate_id = function(x) { x$nomem_encr[2L] <- x$nomem_encr[1L]; x },
    absent_id = function(x) { x$nomem_encr[1L] <- NA_real_; x },
    infinite_id = function(x) { x$nomem_encr[1L] <- Inf; x },
    negative_id = function(x) { x$nomem_encr[1L] <- -1; x },
    fractional_id = function(x) { x$nomem_encr[1L] <- 0.5; x },
    lossy_id = function(x) { x$nomem_encr[1L] <- 2^53; x },
    gate_no = function(x) { x$ca20g078[1L] <- 0; x },
    gate_na = function(x) { x$ca20g078[1L] <- NA_real_; x },
    gate_inf = function(x) { x$ca20g078[1L] <- Inf; x })
  for (name in names(changes)) local({
    fixture <- .nrca_fixture(changes[[name]](original), .local_envir = environment())
    expect_error(lissr::liss_classify_ca20g012(fixture$source),
      class = "liss_ca20g012_blocked", info = name)
  })
})

test_that("all four positive forms reject classification and no-hit sources retain typed empty ledgers", {
  for (code in c(9999999998, 9999999999, 99999999998, 99999999999)) local({
    fixture <- .nrca_fixture(.nrca_frame(c(-9999999999, code, -9)), .local_envir = environment())
    before <- serialize(fixture$frame, NULL, version = 2L)
    expect_error(lissr::liss_classify_ca20g012(fixture$source), class = "liss_ca20g012_blocked")
    expect_identical(serialize(fixture$frame, NULL, version = 2L), before)
  })
  fixture <- .nrca_fixture(.nrca_frame(c(-9, -8, 0, NA_real_, NaN, Inf, -Inf)))
  result <- lissr::liss_classify_ca20g012(fixture$source)
  expect_identical(as.double(unclass(result$data$ca20g012)),
    as.double(unclass(fixture$frame$ca20g012)))
  expect_identical(result$analysis$ledger$source_row_index, integer())
  expect_identical(result$analysis$ledger$source_cell_key, character())
  expect_identical(result$analysis$ledger$analysis_value_after, double())
  expect_identical(result$source_nonresponse_readiness$status, "passed")
  expect_no_error(lissr::liss_validate_ca20g012(result))
})

test_that("CA validation rejects positive originals before reconstruction or source access", {
  fixture <- .nrca_fixture()
  result <- lissr::liss_classify_ca20g012(fixture$source)
  expect_true(lissr::liss_validate_ca20g012(result)$valid)
  original <- serialize(result, NULL, version = 2L)
  testthat::local_mocked_bindings(.liss_ca012_build = function(...) {
    stop("positive original reached reconstruction")
  }, .package = "lissr")
  for (code in c(9999999998, 9999999999, 99999999998, 99999999999)) {
    changed <- result
    changed$original_data$ca20g012[4L] <- code
    changed <- .nrca_reseal(changed)
    expect_identical(.nrca_hash(changed$original_data), changed$source_nonresponse$original_digest)
    before <- serialize(changed, NULL, version = 2L)
    fixture$calls$steps <- character()
    for (source in list(NULL, fixture$source)) {
      expect_error(lissr::liss_validate_ca20g012(changed, source = source),
        regexp = "positive declared or documented nonresponse forms", class = "liss_ca20g012_blocked")
    }
    expect_identical(fixture$calls$steps, character())
    expect_identical(serialize(changed, NULL, version = 2L), before)
  }
  expect_identical(serialize(result, NULL, version = 2L), original)
})

test_that("declared full row names cannot conceal malformed source column shapes", {
  fixture <- .nrca_fixture()
  original <- fixture$frame
  result <- lissr::liss_classify_ca20g012(fixture$source)
  replace_column <- function(frame, column) {
    columns <- unclass(frame)
    columns[["ca20g035"]] <- column
    attributes(columns) <- attributes(frame)
    columns
  }
  columns <- list(short = -200, long = rep(-200, 5850L),
    matrix = matrix(rep(-200, 5849L), ncol = 1L))
  for (name in names(columns)) local({
    malformed <- replace_column(original, columns[[name]])
    expect_identical(nrow(malformed), 5849L)
    expect_identical(row.names(malformed), row.names(original))
    expect_identical(typeof(malformed$ca20g035), "double")
    malformed_fixture <- .nrca_fixture(malformed, .local_envir = environment())
    expect_error(lissr::liss_classify_ca20g012(malformed_fixture$source),
      class = "liss_ca20g012_blocked", info = name)
    changed <- result
    changed$original_data <- malformed
    changed$analysis$data <- replace_column(changed$analysis$data, columns[[name]])
    changed$data <- changed$analysis$data
    expect_error(lissr::liss_validate_ca20g012(changed),
      class = "liss_ca20g012_blocked", info = name)
    expect_error(lissr::liss_validate_ca20g012(.nrca_reseal(changed)),
      class = "liss_ca20g012_blocked", info = paste(name, "consistently resealed"))
  })
})

test_that("the separate CA authority is byte-pinned without changing existing CI resources", {
  policy <- lissr:::.liss_ca012_policy()
  expect_identical(policy$authority_id, "CA20G012_SOURCE")
  expect_identical(policy$authority_version, "1.0.0")
  expect_identical(policy$source, .nrca_receipt())
  expect_identical(policy$codes, c(-9999999999, -9999999998))
  expect_identical(policy$case_ids, c("CA20G012_N01", "CA20G012_N02"))
  expect_identical(policy$unsupported_positive_forms,
    c(9999999998, 9999999999, 99999999998, 99999999999))
  expect_identical(policy$documented_signed_range, c(-999999999, 999999999))
  path <- lissr:::.liss_ca012_policy_path()
  expect_identical(basename(path), "source_nonresponse_ca20g012.yml")
  changed <- withr::local_tempfile(fileext = ".yml")
  writeLines(c(readLines(path, warn = FALSE), "unreviewed: true"), changed)
  testthat::local_mocked_bindings(.liss_ca012_policy_path = function() changed, .package = "lissr")
  expect_error(lissr:::.liss_ca012_policy(), class = "liss_ca20g012_blocked")
})

test_that("closed result reconstruction denies plain and consistently resealed projection mutations", {
  fixture <- .nrca_fixture()
  result <- lissr::liss_classify_ca20g012(fixture$source)
  changes <- list(
    missing_original = function(x) { x$original_data <- NULL; x },
    missing_analysis = function(x) { x$analysis <- NULL; x },
    extra = function(x) { x$unsupported <- TRUE; x },
    wrapper_metadata = function(x) { attr(x, "unsupported") <- TRUE; x },
    wrapper_class = function(x) { class(x) <- "list"; x },
    analysis_extra = function(x) { x$analysis$unsupported <- TRUE; x },
    data_duplicate = function(x) { x$data$ca20g012[1L] <- 1; x },
    original_metadata = function(x) { attr(x$original_data$ca20g012, "label") <- "changed"; x },
    original_order = function(x) { x$original_data <- x$original_data[rev(seq_len(nrow(x$original_data))), , drop = FALSE]; x },
    original_partial = function(x) { x$original_data <- x$original_data[-1L, , drop = FALSE]; x },
    finite_locked = function(x) { x$analysis$data$ca20g012[1L] <- 1; x },
    sign_repair = function(x) { x$analysis$data$ca20g012[4L] <- 9; x },
    non_target = function(x) { x$analysis$data$ca20g035[1L] <- 200; x },
    target_metadata = function(x) { attr(x$analysis$data$ca20g012, "label") <- "changed"; x },
    ledger_row = function(x) { x$analysis$ledger$source_row_index[1L] <- 4L; x },
    ledger_type = function(x) { x$analysis$ledger$source_row_index <- as.double(x$analysis$ledger$source_row_index); x },
    ledger_key = function(x) { x$analysis$ledger$source_cell_key[1L] <- "claimed"; x },
    ledger_value = function(x) { x$analysis$ledger$original_value[1L] <- -9; x },
    ledger_subtype = function(x) { x$analysis$ledger$answer_status[1L] <- "dont_know"; x },
    ledger_candidate = function(x) { x$analysis$ledger$candidate_answer_status[1L] <- "refusal"; x },
    ledger_proposal = function(x) { x$analysis$ledger$analysis_value_after[1L] <- 50; x },
    readiness = function(x) { x$source_nonresponse_readiness$status <- "certified"; x },
    overall_validity = function(x) { x$source_nonresponse_readiness$overall_amount_validity <- "passed"; x },
    fields = function(x) { x$source_nonresponse$fields$OTHER_FIELD <- list(); x },
    native_schema = function(x) { x$source_nonresponse$schema_version <- "4.0.0"; x })
  for (name in names(changes)) {
    changed <- changes[[name]](result)
    expect_error(lissr::liss_validate_ca20g012(changed), class = "liss_ca20g012_blocked", info = name)
    if (name == "data_duplicate") next
    resealed <- tryCatch(.nrca_reseal(changed), error = identity)
    if (!inherits(resealed, "error"))
      expect_error(lissr::liss_validate_ca20g012(resealed), class = "liss_ca20g012_blocked", info = paste(name, "resealed"))
  }
})

test_that("structural consistency is distinct from rereading the exact retained source", {
  fixture <- .nrca_fixture()
  result <- lissr::liss_classify_ca20g012(fixture$source)
  changed <- result$original_data
  changed$ca20g035[1L] <- -201
  forged <- lissr:::.liss_ca012_build(changed,
    lissr:::.liss_ca012_check_raw(changed, lissr:::.liss_ca012_policy()))
  expect_no_error(lissr::liss_validate_ca20g012(forged))
  expect_error(lissr::liss_validate_ca20g012(forged, source = fixture$source),
    class = "liss_ca20g012_blocked")
  for (path in list(NA_character_, "", FALSE, c(fixture$source, fixture$source),
    structure(fixture$source, label = "claim")))
    expect_error(lissr::liss_validate_ca20g012(result, source = path), class = "liss_ca20g012_blocked")
})

test_that("CA reconstruction rejects changed serialized frames and stale ledger digests", {
  fixture <- .nrca_fixture()
  result <- lissr::liss_classify_ca20g012(fixture$source)
  changes <- list(
    top_data_negative_zero = function(x) { x$data$ca20g_m[1L] <- -0; x },
    analysis_negative_zero = function(x) { x$analysis$data$ca20g_m[1L] <- -0; x },
    target_negative_zero = function(x) { x$analysis$data$ca20g012[6L] <- -0; x },
    analysis_attribute_order = function(x) {
      attributes(x$analysis$data$ca20g012) <- rev(attributes(x$analysis$data$ca20g012)); x
    },
    ledger_attribute_order = function(x) {
      attributes(x$analysis$ledger) <- rev(attributes(x$analysis$ledger)); x
    },
    ledger_tagged_missing = function(x) {
      x$analysis$ledger$analysis_value_after[1L] <- haven::tagged_na("a"); x
    })
  for (name in names(changes)) {
    changed <- changes[[name]](result)
    expect_identical(changed$source_nonresponse, result$source_nonresponse, info = name)
    expect_true(identical(changed, result), info = name)
    expect_false(identical(serialize(changed, NULL, version = 2L),
      serialize(result, NULL, version = 2L)), info = name)
    if (startsWith(name, "ledger_")) {
      expect_false(identical(.nrca_hash(changed$analysis$ledger),
        changed$source_nonresponse$ledger_digest), info = name)
    } else {
      frame <- if (name == "top_data_negative_zero") changed$data else changed$analysis$data
      expect_false(identical(.nrca_hash(.nrca_unmark(frame)),
        changed$source_nonresponse$analysis_digest), info = name)
      expect_false(identical(serialize(changed$data, NULL, version = 2L),
        serialize(changed$analysis$data, NULL, version = 2L)), info = name)
    }
    expect_error(lissr::liss_validate_ca20g012(changed), class = "liss_ca20g012_blocked", info = name)
  }
})

test_that("CA source consistency compares the complete imported serialization", {
  original <- .nrca_frame()
  attr(original$ca20g035, "format.spss") <- "F8.0"
  fixture <- .nrca_fixture(original)
  changes <- list(
    signed_zero = function(x) { x$ca20g_m[1L] <- -0; x },
    attribute_order = function(x) {
      attributes(x$ca20g035) <- rev(attributes(x$ca20g035)); x
    },
    tagged_missing = function(x) { x$ca20g013[2L] <- haven::tagged_na("a"); x })
  for (name in names(changes)) {
    changed <- changes[[name]](original)
    expect_true(identical(changed, original), info = name)
    expect_false(identical(serialize(changed, NULL, version = 2L),
      serialize(original, NULL, version = 2L)), info = name)
    rebuilt <- lissr:::.liss_ca012_build(changed,
      lissr:::.liss_ca012_check_raw(changed, lissr:::.liss_ca012_policy()))
    expect_true(lissr::liss_validate_ca20g012(rebuilt)$valid, info = name)
    fixture$calls$steps <- character()
    expect_error(lissr::liss_validate_ca20g012(rebuilt, source = fixture$source),
      class = "liss_ca20g012_blocked", info = name)
    expect_identical(fixture$calls$steps, c("state", "read", "state"), info = name)
  }
})

test_that("CA serialized validation accepts independent construction and whole RDS transport", {
  fixture <- .nrca_fixture()
  result <- lissr::liss_classify_ca20g012(fixture$source)
  independent <- .nrca_frame()
  rebuilt <- lissr:::.liss_ca012_build(independent,
    lissr:::.liss_ca012_check_raw(independent, lissr:::.liss_ca012_policy()))
  path <- withr::local_tempfile(pattern = "invented_ca012_exact_", fileext = ".rds")
  saveRDS(result, path, version = 2L)
  restored <- readRDS(path)
  for (candidate in list(result, rebuilt, restored)) {
    expect_identical(serialize(candidate, NULL, version = 2L), serialize(result, NULL, version = 2L))
    expect_identical(.nrca_hash(candidate$original_data), candidate$source_nonresponse$original_digest)
    expect_identical(.nrca_hash(.nrca_unmark(candidate$analysis$data)), candidate$source_nonresponse$analysis_digest)
    expect_identical(.nrca_hash(candidate$analysis$ledger), candidate$source_nonresponse$ledger_digest)
    expect_true(lissr::liss_validate_ca20g012(candidate)$valid)
    expect_identical(lissr::liss_validate_ca20g012(candidate, source = fixture$source)$source_consistency, "matched")
  }
})

test_that("default whole CA RDS file transport validates in the same runtime", {
  fixture <- .nrca_fixture()
  result <- lissr::liss_classify_ca20g012(fixture$source)
  before <- serialize(result, NULL, version = 2L)
  path <- withr::local_tempfile(pattern = "invented_ca012_default_", fileext = ".rds")
  saveRDS(result, path)
  restored <- readRDS(path)
  expect_identical(serialize(restored, NULL, version = 2L), before)
  expect_true(lissr::liss_validate_ca20g012(restored)$valid)
  expect_identical(lissr::liss_validate_ca20g012(restored, source = fixture$source)$source_consistency, "matched")
  expect_identical(serialize(result, NULL, version = 2L), before)
})

test_that("CA validation retains guard attribute placement when all bound content is exact", {
  fixture <- .nrca_fixture()
  result <- lissr::liss_classify_ca20g012(fixture$source)
  changed <- result
  frame_class <- class(changed$analysis$data)
  class(changed$analysis$data) <- NULL
  class(changed$analysis$data) <- frame_class
  changed$data <- changed$analysis$data
  expect_true(identical(changed, result))
  expect_false(identical(serialize(changed, NULL, version = 2L), serialize(result, NULL, version = 2L)))
  expect_identical(.nrca_hash(.nrca_unmark(changed$analysis$data)), changed$source_nonresponse$analysis_digest)
  expect_identical(.nrca_hash(changed$original_data), changed$source_nonresponse$original_digest)
  expect_identical(.nrca_hash(changed$analysis$ledger), changed$source_nonresponse$ledger_digest)
  expect_identical(.nrca_hash(changed$source_nonresponse_readiness), changed$source_nonresponse$readiness_digest)
  expect_identical(attr(changed$analysis$data, "liss_source_nonresponse", exact = TRUE),
    attr(result$analysis$data, "liss_source_nonresponse", exact = TRUE))
  expect_identical(attr(changed$analysis$data, "source_nonresponse_readiness", exact = TRUE),
    attr(result$analysis$data, "source_nonresponse_readiness", exact = TRUE))
  expect_true(lissr::liss_validate_ca20g012(changed)$valid)
  expect_identical(lissr::liss_validate_ca20g012(changed, source = fixture$source)$source_consistency, "matched")
})

test_that("CA source comparison retains rejection of distinct environment identities", {
  original <- .nrca_frame()
  attr(original$ca20g035, "invented_context") <- new.env(parent = emptyenv())
  fixture <- .nrca_fixture(original)
  changed <- original
  attr(changed$ca20g035, "invented_context") <- new.env(parent = emptyenv())
  expect_false(identical(changed, original))
  expect_identical(serialize(changed, NULL, version = 2L), serialize(original, NULL, version = 2L))
  rebuilt <- lissr:::.liss_ca012_build(changed,
    lissr:::.liss_ca012_check_raw(changed, lissr:::.liss_ca012_policy()))
  expect_true(lissr::liss_validate_ca20g012(rebuilt)$valid)
  expect_error(lissr::liss_validate_ca20g012(rebuilt, source = fixture$source),
    class = "liss_ca20g012_blocked")
})

test_that("CA claims are denied by cleaning, reporting, panel, rebinding and validation", {
  fixture <- .nrca_fixture()
  result <- lissr::liss_classify_ca20g012(fixture$source)
  bare <- .nrca_unmark(result$data)
  class_only <- result$data
  attr(class_only, "liss_source_nonresponse") <- NULL
  attr(class_only, "source_nonresponse_readiness") <- NULL
  marker_only <- bare
  attr(marker_only, "liss_source_nonresponse") <- attr(result$data, "liss_source_nonresponse")
  readiness <- bare
  attr(readiness, "source_nonresponse_readiness") <- result$source_nonresponse_readiness
  forms <- list(result, result$data, list(data = result$data), list(data_partial = result$data),
    class_only, marker_only, readiness,
    list(data = bare, source_nonresponse_readiness = result$source_nonresponse_readiness),
    list(data = bare, source_nonresponse = result$source_nonresponse))
  calls <- character()
  forbidden <- function(name) function(...) { calls <<- c(calls, name); stop("downstream work reached") }
  testthat::local_mocked_bindings(numeric_view = forbidden("numeric"), liss_cleaning_ruleset = forbidden("rules"),
    attach_background_frame = forbidden("background"), .liss_nonresponse_source_state = forbidden("source"),
    .liss_nonresponse_read_source = forbidden("source_read"), write_clean_jsonl = forbidden("jsonl"),
    build_cleaning_report_md = forbidden("report"), sanitize_spss_names = forbidden("sanitize"), .package = "lissr")
  testthat::local_mocked_bindings(full_join = forbidden("full_join"), inner_join = forbidden("inner_join"),
    left_join = forbidden("left_join"), .package = "dplyr")
  tax <- readRDS(testthat::test_path("fixtures", "source_nonresponse_v2_synthetic.rds"))
  for (input in forms) {
    expect_true(lissr:::.liss_nonresponse_claims_collection(input))
    for (mode in c("flag", "correct", "na_only"))
      expect_error(lissr::liss_clean_income(input, mode = mode, variables = list(target = "s339"),
        background = NULL, ruleset = stop("ruleset promise was forced"), verbose = FALSE),
        class = "liss_source_nonresponse_blocked")
    path <- tempfile("ca012_export_denied_")
    expect_error(lissr::liss_cleaning_report(input, path), class = "liss_source_nonresponse_blocked")
    for (join in c("full", "inner", "left"))
      expect_error(suppressMessages(lissr::merge_liss_panel(list(ca = input), join_type = join,
        write_to = file.path(path, "panel.sav"))), class = "liss_source_nonresponse_blocked")
    for (recipe in list(NULL, tax$fields[["337"]]$recipe, tax$fields[["338"]]$recipe))
      expect_error(lissr::liss_classify_source_nonresponse(input, "missing_ci23p_EN_1.0p.sav",
        "s339", "wave_id", recipe = recipe), class = "liss_source_nonresponse_blocked")
    expect_false(file.exists(path))
    expect_identical(calls, character())
  }
  expect_error(lissr::liss_validate_applied_income(result), class = "liss_source_nonresponse_blocked")
  expect_error(lissr::liss_validate_applied_income_view(result), class = "liss_applied_income_view_blocked")
})

test_that("CA claims cannot create applied views before row keys or projection are evaluated", {
  fixture <- .nrca_fixture()
  result <- lissr::liss_classify_ca20g012(fixture$source)
  bare <- .nrca_unmark(result$data)
  class_only <- bare
  class(class_only) <- class(result$data)
  marker_only <- bare
  attr(marker_only, "liss_source_nonresponse") <- attr(result$data, "liss_source_nonresponse")
  readiness_only <- bare
  attr(readiness_only, "source_nonresponse_readiness") <- result$source_nonresponse_readiness
  forms <- list(whole = result, frame = result$data, class_only = class_only,
    marker_only = marker_only, readiness_only = readiness_only)
  calls <- character()
  forbidden <- function(name) function(...) {
    calls <<- c(calls, name)
    stop("CA claim reached view projection")
  }
  testthat::local_mocked_bindings(.liss_view_policy = forbidden("policy"),
    .liss_view_selection = forbidden("selection"), .liss_view_project = forbidden("projection"),
    .package = "lissr")
  for (name in names(forms)) {
    before <- serialize(forms[[name]], NULL, version = 2L)
    forced <- FALSE
    expect_error(lissr::liss_create_applied_income_view(forms[[name]], row_keys = {
      forced <- TRUE
      stop("CA claim forced row keys")
    }), class = "liss_source_nonresponse_blocked", info = name)
    expect_false(forced, info = name)
    expect_identical(serialize(forms[[name]], NULL, version = 2L), before, info = name)
  }
  expect_identical(calls, character())
})

test_that("ordinary income cleaning refuses complete CA backgrounds and residual claims", {
  fixture <- .nrca_fixture()
  result <- lissr::liss_classify_ca20g012(fixture$source)
  expect_true(lissr::liss_validate_ca20g012(result)$valid)
  inc <- data.frame(nomem_encr = c(50004, 50004, 50006),
    nohouse_encr = c(1, 1, 2), wavenr = c(1, 2, 1), nethh = c(21000, 21500, 30000))
  bare <- .nrca_unmark(result$data)
  class_only <- bare
  class(class_only) <- class(result$data)
  marker_only <- bare
  attr(marker_only, "liss_source_nonresponse") <- attr(result$data, "liss_source_nonresponse")
  readiness_only <- bare
  attr(readiness_only, "source_nonresponse_readiness") <- result$source_nonresponse_readiness
  forms <- list(frame = result$data, whole = result, class_only = class_only,
    marker_only = marker_only, readiness_only = readiness_only)
  before <- serialize(result, NULL, version = 2L)
  for (name in names(forms)) {
    for (mode in c("correct", "flag", "na_only")) {
      expect_error(lissr::liss_clean_income(inc, background = forms[[name]], mode = mode,
        variables = list(personal_net = "ca20g012"), verbose = FALSE),
        class = "liss_source_nonresponse_blocked", info = paste(name, mode))
    }
  }
  expect_identical(serialize(result, NULL, version = 2L), before)
  ordinary <- lissr::liss_clean_income(inc, background = bare, verbose = FALSE)
  expect_identical(as.double(ordinary$data$ca20g012), c(-9, -9, 0))
  control <- lissr::liss_clean_income(inc, disable = "P01", verbose = FALSE)
  ignored <- lissr::liss_clean_income(inc, background = result$data,
    disable = "P01", verbose = FALSE)
  expect_identical(ignored$data, control$data)
  expect_identical(ignored$decisions, control$decisions)
})

test_that("whole invented production CA RDS validates structurally in a fresh process", {
  fixture <- .nrca_fixture()
  result <- lissr::liss_classify_ca20g012(fixture$source)
  path <- withr::local_tempfile(pattern = "invented_ca012_whole_", fileext = ".rds")
  saveRDS(result, path, version = 2L)
  expect_identical(readRDS(path), result)
  package_root <- getNamespaceInfo(asNamespace("lissr"), "path")
  child <- withr::local_tempfile(pattern = "ca012_child_", fileext = ".R")
  writeLines(c("arguments <- commandArgs(trailingOnly = TRUE)", "root <- arguments[2L]",
    "if (file.exists(file.path(root, 'R', 'liss_clean_income.R'))) {",
    "  pkgload::load_all(root, quiet = TRUE)", "} else {",
    "  loadNamespace('lissr', lib.loc = dirname(root))", "}",
    "result <- readRDS(arguments[1L])", "stopifnot(lissr::liss_validate_ca20g012(result)$valid)",
    "cat('whole_invented_ca012_validated\\n')"), child)
  output <- system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(child), shQuote(path), shQuote(package_root)), stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_true(any(grepl("whole_invented_ca012_validated", output, fixed = TRUE)))
})
