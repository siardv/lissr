# all objects and frozen source receipts below are invented or public metadata
.nrid_sha <- function(bytes) {
  digest <- as.character(openssl::sha256(bytes))
  attributes(digest) <- NULL
  digest
}

.nrid_word <- function(value) {
  writeBin(as.integer(value), raw(), size = 4L, endian = "big")
}

.nrid_reference_bytes <- function(value) {
  bytes <- serialize(value, NULL, ascii = FALSE, xdr = TRUE, version = 2L)
  bytes[7:10] <- as.raw(c(0L, 4L, 6L, 1L))
  bytes
}

test_that("the serialized identity profile is explicit and closed", {
  expect_identical(lissr:::.liss_identity_profile(), list(algorithm = "sha256",
    serialization = "R_XDR_version_2", reference_writer = "4.6.1",
    profile_version = "1.0.0"))
  value <- list(invented = c(1L, NA_integer_), metadata = "invented label")
  expect_identical(lissr:::.liss_identity_bytes(value), .nrid_reference_bytes(value))
  expect_identical(lissr:::.liss_identity_hash(value),
    .nrid_sha(.nrid_reference_bytes(value)))
  expect_null(attributes(lissr:::.liss_identity_hash(value)))
})

test_that("only the four writer bytes are canonicalized in owned XDR v2 streams", {
  value <- list(amount = c(0, 42, NA_real_), label = "invented", keys = 1:3)
  native <- serialize(value, NULL, ascii = FALSE, xdr = TRUE, version = 2L)
  expected <- .nrid_reference_bytes(value)
  expect_identical(native[1:2], charToRaw("X\n"))
  expect_identical(readBin(native[3:6], "integer", n = 1L, size = 4L,
    endian = "big"), 2L)
  expect_identical(readBin(native[11:14], "integer", n = 1L, size = 4L,
    endian = "big"), 131840L)
  writers <- c(131840L, 262403L, 263427L, 263681L, 263936L)
  for (writer in writers) {
    emulated <- native
    emulated[7:10] <- .nrid_word(writer)
    actual <- lissr:::.liss_identity_normalize(emulated)
    expect_identical(actual, expected, info = as.character(writer))
    expect_identical(actual[-(7:10)], emulated[-(7:10)])
    expect_identical(actual[7:10], as.raw(c(0L, 4L, 6L, 1L)))
    expect_identical(length(actual), length(emulated))
    expect_identical(lissr:::.liss_identity_normalize(actual), actual)
  }
  # the internal helper validates the header, not arbitrary serialized bodies
  changed_body <- native
  changed_body[length(changed_body)] <- as.raw(bitwXor(
    as.integer(changed_body[length(changed_body)]), 1L))
  expect_identical(lissr:::.liss_identity_normalize(changed_body)[-(7:10)],
    changed_body[-(7:10)])
  expect_false(identical(lissr:::.liss_identity_normalize(changed_body), expected))
})

test_that("unsupported serialization headers fail before identity hashing", {
  valid <- serialize(NULL, NULL, ascii = FALSE, xdr = TRUE, version = 2L)
  replace_word <- function(start, value) {
    changed <- valid
    changed[start + 0:3] <- .nrid_word(value)
    changed
  }
  wrong_magic <- valid
  wrong_magic[1L] <- charToRaw("B")
  bad <- list(character = "X\n", integer = as.integer(valid), empty = raw(),
    truncated = valid[seq_len(17L)], attributed = structure(valid, invented = TRUE),
    named = stats::setNames(valid, seq_along(valid)), magic = wrong_magic,
    format1 = replace_word(3L, 1L), format3 = replace_word(3L, 3L),
    reader_zero = replace_word(11L, 0L), reader_other = replace_word(11L, 197888L),
    writer_too_old = replace_word(7L, 131584L), writer_negative = replace_word(7L, -1L),
    ascii = serialize(NULL, NULL, ascii = TRUE, version = 2L),
    native = serialize(NULL, NULL, ascii = FALSE, xdr = FALSE, version = 2L),
    version3 = serialize(NULL, NULL, ascii = FALSE, xdr = TRUE, version = 3L))
  for (name in names(bad)) expect_error(lissr:::.liss_identity_normalize(bad[[name]]),
    class = "liss_serialized_identity_unsupported", info = name)
  expect_length(valid, 18L)
  expect_identical(lissr:::.liss_identity_normalize(valid), .nrid_reference_bytes(NULL))
})

test_that("the reference profile retains the three literal CA014 metadata pins", {
  metadata <- list(
    nomem_encr = list(label = "Number of household member encrypted",
      format.spss = "F7.0", display_width = 12L),
    ca20g014 = list(label = paste0(
      "What was the total sum of the guaranteed minimum payout of your single-premium ",
      "or life annuity insurances, or the total savings amount of your endowment insurance on 31 December 2019?"),
      na_values = c(9999999998, 9999999999),
      class = c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", "double"),
      format.spss = "F6.0", display_width = 15L,
      labels = c("I prefer not to say" = 9999999998, "I don't know" = 9999999999)),
    ca20g005 = list(label = paste0(
      "On 31 December 2019, did you possess one or more of the following assets? - ",
      "Single-premium insurance policy, life annuity insurance, endowment insurance (not linked to a mortgage)"),
      format.spss = "F1.0", display_width = 15L,
      class = c("haven_labelled", "vctrs_vctr", "double"), labels = c(no = 0, yes = 1)))
  expected <- c(
    nomem_encr = "a85d081e81be1b50c2ac82f5fe3012b3275d73d3c7556796b5bd923b8f90a1d1",
    ca20g014 = "3839ce375078db0ceb2a4f910e05af8ccf1f6faa2e4bfc4068e23103bf5ea041",
    ca20g005 = "f8e1b6c08d61fc04c1ffb0b8ed47f15b062bb0aa473ada85f21088cfdd50ae86")
  expect_identical(vapply(metadata, lissr:::.liss_identity_hash, character(1)), expected)
})

test_that("types order missingness encoding and signed zero retain distinct bytes", {
  negative_zero <- -abs(as.double(0))
  positive_zero <- abs(negative_zero)
  expect_identical(1 / negative_zero, -Inf)
  expect_identical(1 / positive_zero, Inf)
  expect_identical(negative_zero, positive_zero)
  utf8 <- enc2utf8("caf\u00e9")
  latin1 <- iconv(utf8, from = "UTF-8", to = "latin1")
  Encoding(utf8) <- "UTF-8"
  Encoding(latin1) <- "latin1"
  expect_identical(Encoding(utf8), "UTF-8")
  expect_identical(Encoding(latin1), "latin1")
  attributes_one <- structure(1, label = "invented", unit = "EUR")
  attributes_two <- structure(1, unit = "EUR", label = "invented")
  pairs <- list(integer_double = list(1L, 1), logical_integer = list(TRUE, 1L),
    null_list = list(NULL, list()), na_nan = list(NA_real_, NaN),
    na_type = list(NA_integer_, NA_real_), tagged_a_na = list(haven::tagged_na("a"), NA_real_),
    tagged_a_b = list(haven::tagged_na("a"), haven::tagged_na("b")),
    signed_zero = list(negative_zero, positive_zero), encoding = list(utf8, latin1),
    named_unnamed = list(c(invented = 1), 1),
    list_order = list(list(a = 1L, b = 2L), list(b = 2L, a = 1L)),
    attribute_order = list(attributes_one, attributes_two),
    factor_levels = list(factor("a", levels = c("a", "b")), factor("a", levels = c("b", "a"))),
    runtime_string = list(list(r = "R version invented A"), list(r = "R version invented B")))
  for (name in names(pairs)) {
    pair <- pairs[[name]]
    expect_false(identical(lissr:::.liss_identity_bytes(pair[[1L]]),
      lissr:::.liss_identity_bytes(pair[[2L]])), info = name)
    expect_false(identical(lissr:::.liss_identity_hash(pair[[1L]]),
      lissr:::.liss_identity_hash(pair[[2L]])), info = name)
  }
})

test_that("serialized identity does not replace reference equality", {
  left <- list(context = new.env(parent = emptyenv()))
  right <- list(context = new.env(parent = emptyenv()))
  expect_false(identical(left, right))
  expect_identical(lissr:::.liss_identity_bytes(left), lissr:::.liss_identity_bytes(right))
  expect_identical(lissr:::.liss_identity_hash(left), lissr:::.liss_identity_hash(right))
  expect_false(lissr:::.liss_ca025_equal(left, right))
  expect_true(lissr:::.liss_ca025_equal(left, left))
  shared <- new.env(parent = emptyenv())
  one_reference <- list(a = shared, b = shared)
  two_references <- list(a = new.env(parent = emptyenv()), b = new.env(parent = emptyenv()))
  expect_false(identical(lissr:::.liss_identity_hash(one_reference),
    lissr:::.liss_identity_hash(two_references)))
})

test_that("all four frozen invented fixtures retain their stored state digests", {
  fixture_sha <- c(
    "368ee201f3b1b162b7c8d0b69d5b08d8bd7553479cf2ce6b2d7c8df193e38297",
    "b20f8f7ccaba3943961bb6a395b08a9cade742745e41bc5b971afeaf2dd5b775",
    "5442ec37f50850d8574ae882a0725643999b068b9dab46276fbb73f788d80adc",
    "df88ede7a355b4e54c44e4d2ffdc622f3c5caed2227d7ce12661dd94dc88bdf8")
  expected_counts <- c(20L, 44L, 144L, 699L)
  for (version in 1:4) {
    path <- testthat::test_path("fixtures", paste0("source_nonresponse_v", version, "_synthetic.rds"))
    expect_identical(.nrid_sha(readBin(path, "raw", n = file.info(path)$size)), fixture_sha[[version]])
    fixture <- readRDS(path)
    count <- 0L
    visit <- function(value, depth = 0L) {
      if (!is.list(value) || depth > 18L) return(invisible(NULL))
      if (is.character(value$digest) && length(value$digest) == 1L &&
          is.character(value$schema_version)) {
        stored <- value$digest
        value$digest <- NULL
        expect_identical(lissr:::.liss_identity_hash(value), stored,
          info = paste("fixture", version, "state", count + 1L))
        count <<- count + 1L
      }
      for (item in value) if (is.list(item)) visit(item, depth + 1L)
      invisible(NULL)
    }
    visit(fixture)
    expect_identical(count, expected_counts[[version]])
    expect_identical(.nrid_sha(readBin(path, "raw", n = file.info(path)$size)), fixture_sha[[version]])
  }
})

test_that("authority resource pins remain hashes of exact raw file bytes", {
  expected <- c(
    source_nonresponse_ca20g012.yml = "ad4a39ea498faa5a1511a01d6dbc816a602e5d77c6f94b2af89c043eaf19dd28",
    source_nonresponse_ca20g014.yml = "b0db75337aee32e6c274ef04b68613b5f699d215c128ac570f4b0149e0f8b2f8",
    source_nonresponse_ca20g016.yml = "2cf1ff508f72d2c7f11c46157297966ff6464a26278d92b4ab3cd199505da4e0",
    source_nonresponse_ca20g023.yml = "699ddffc8d2a26a196c76f13bd8b811438dd26bf2c13989de52b79bb89460902",
    source_nonresponse_ca20g025.yml = "c8797a949d41fce0a93824cc62326af18fa49b60b4a08d44af689c55b594d293",
    source_nonresponse_registry.yml = "a2e6694acc5855d2df389df46d66e0c94b705637626309cc309ceee8d840ddc3",
    source_nonresponse_collection_adapter.yml = "5e3235f1b4f78a589622ce27064eecf120c919e81fd95b84fe4a370126ac6c3f",
    source_nonresponse_collection_consumers.yml = "41fe0edfa8a3e3f3ee467873a1350c50c112bcefbbf0664beceec393115d6d9f",
    source_nonresponse_collection_applied.yml = "7c740496b5e7ac28a1b7018addc0c253258bf14c28b74a85506dd33dc65b6d2f",
    source_nonresponse_collection_applied_views.yml = "2907b71ea08338eb572a143d3f382109aac804dd73add6dbdb07bc68f01cd1de")
  for (name in names(expected)) {
    path <- system.file("schema", name, package = "lissr")
    expect_true(nzchar(path), info = name)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    expect_identical(.nrid_sha(bytes), unname(expected[[name]]), info = name)
    expect_identical(lissr:::.liss_ca025_bytes_hash(bytes), unname(expected[[name]]), info = name)
    expect_false(identical(lissr:::.liss_identity_hash(bytes), unname(expected[[name]])), info = name)
  }
})

test_that("the new engine fingerprints the four identity helpers and exact profile", {
  engine <- lissr:::.liss_applied_engine()
  expect_identical(names(engine), c("schema_version", "functions", "constants", "runtime", "serialization"))
  expect_identical(engine$schema_version, "2.0.0")
  expect_identical(engine$serialization, lissr:::.liss_identity_profile())
  expect_length(engine$functions, 117L)
  helpers <- c(".liss_identity_profile", ".liss_identity_normalize",
    ".liss_identity_bytes", ".liss_identity_hash")
  for (name in helpers) {
    fn <- get(name, envir = asNamespace("lissr"), inherits = FALSE)
    payload <- list(formals = paste(deparse(formals(fn), width.cutoff = 500L), collapse = "\n"),
      body = paste(deparse(body(fn), width.cutoff = 500L), collapse = "\n"))
    expect_identical(unname(engine$functions[[name]]), .nrid_sha(.nrid_reference_bytes(payload)), info = name)
  }
  expect_no_error(lissr:::.liss_applied_engine_validate(engine))
  missing_profile <- engine
  missing_profile$serialization <- NULL
  legacy_extra <- engine
  legacy_extra$schema_version <- "1.0.0"
  malformed <- list(missing_profile, legacy_extra)
  for (key in names(engine$serialization)) {
    changed <- engine
    changed$serialization[[key]] <- paste0(changed$serialization[[key]], "_unsupported")
    malformed[[length(malformed) + 1L]] <- changed
  }
  reordered <- engine
  reordered$serialization <- rev(reordered$serialization)
  malformed[[length(malformed) + 1L]] <- reordered
  extra <- engine
  extra$serialization$invented <- TRUE
  malformed[[length(malformed) + 1L]] <- extra
  for (changed in malformed) expect_error(lissr:::.liss_applied_engine_validate(changed),
    class = "liss_source_nonresponse_blocked")
})

test_that("a synthetic legacy engine stays structural and cannot claim new replay", {
  saved <- readRDS(testthat::test_path("fixtures", "source_nonresponse_v4_synthetic.rds"))
  expect_identical(saved$format, "invented_row_t2b_v4_fixture")
  result <- suppressWarnings(lissr::liss_clean_income(saved$orders[[1L]]$input,
    mode = "correct", variables = list(target = "s339"), verbose = FALSE))
  expect_no_error(lissr::liss_validate_applied_income(result, replay = TRUE))
  legacy <- result$applied_lineage$engine
  legacy$schema_version <- "1.0.0"
  legacy$serialization <- NULL
  legacy$functions <- legacy$functions[!grepl("^\\.liss_identity_", names(legacy$functions))]
  expect_length(legacy$functions, 113L)
  expect_no_error(lissr:::.liss_applied_engine_validate(legacy))
  # this is a newly constructed legacy-shaped descriptor, not a historical engine
  result$applied_lineage$engine <- legacy
  result <- lissr:::.liss_applied_reseal(result)
  expect_no_error(lissr::liss_validate_applied_income(result, replay = FALSE))
  expect_error(lissr::liss_validate_applied_income(result, replay = TRUE),
    "replay is unsupported", class = "liss_source_nonresponse_blocked")
})

test_that("new v1 and v2 ledger identities survive serialization roundtrips", {
  reference_bytes <- function(value) {
    bytes <- serialize(value, NULL, ascii = FALSE, xdr = TRUE, version = 2L)
    bytes[7:10] <- as.raw(c(0L, 4L, 6L, 1L))
    bytes
  }
  roundtrip <- function(value) {
    unserialize(serialize(value, NULL, ascii = FALSE, xdr = TRUE, version = 2L))
  }
  check_case <- function(case) {
    directory <- withr::local_tempdir()
    source <- file.path(directory, case$receipt$filename)
    expect_true(file.create(source), info = case$name)
    testthat::local_mocked_bindings(
      .liss_nonresponse_source_state = function(path) case$receipt,
      .liss_nonresponse_read_source = function(path) case$raw,
      .package = "lissr")
    expect_true(nrow(case$raw) > 0L, info = case$name)
    expect_true(nrow(case$ledger) > 0L, info = case$name)
    for (empty in c(FALSE, TRUE)) {
      label <- paste(case$name, if (empty) "empty" else "full")
      input <- if (empty) case$raw[FALSE, , drop = FALSE] else case$raw
      expected_ledger <- if (empty) case$ledger[FALSE, , drop = FALSE] else case$ledger
      result <- lissr::liss_classify_source_nonresponse(input, source,
        case$target, recipe = case$recipe)
      state <- result$source_nonresponse
      expect_identical(reference_bytes(names(state$ledger)),
        reference_bytes(c(names(expected_ledger))), info = label)
      expect_identical(reference_bytes(state$ledger),
        reference_bytes(expected_ledger), info = label)
      restored <- roundtrip(state)
      expect_identical(result$data, input, info = label)
      expect_identical(names(result$data), names(case$raw), info = label)
      expect_identical(state$schema_version, case$schema_version, info = label)
      expect_identical(state$ledger, expected_ledger, info = label)
      expect_identical(attributes(state$ledger), attributes(expected_ledger), info = label)
      expect_identical(names(state$ledger), names(case$ledger), info = label)
      expect_identical(state$ledger$source_key, expected_ledger$source_key, info = label)
      expect_identical(state$readiness$status,
        if (empty) "not_evaluated" else "passed", info = label)
      expect_equal(state$readiness$evaluated_cells, nrow(input), info = label)
      expect_equal(state$readiness$classified_cells, nrow(expected_ledger), info = label)
      expect_identical(nrow(state$coverage), nrow(input), info = label)
      if (empty) {
        expect_identical(nrow(state$ledger), 0L, info = label)
      } else {
        expect_true(nrow(state$ledger) > 0L, info = label)
      }
      expect_identical(state, restored, info = label)
      expect_identical(reference_bytes(state), reference_bytes(restored), info = label)
      expect_identical(lissr:::.liss_identity_bytes(state), reference_bytes(state), info = label)
      expect_identical(lissr:::.liss_identity_bytes(state),
        lissr:::.liss_identity_bytes(restored), info = label)
      expect_identical(reference_bytes(state$ledger),
        reference_bytes(roundtrip(state$ledger)), info = label)
      payload <- state
      payload$digest <- NULL
      hash <- as.character(openssl::sha256(reference_bytes(payload)))
      attributes(hash) <- NULL
      expect_identical(state$digest, hash, info = label)
      expect_identical(lissr:::.liss_nonresponse_digest(restored), state$digest, info = label)
    }
  }
  v1 <- readRDS(testthat::test_path("fixtures", "source_nonresponse_v1_synthetic.rds"))
  v2 <- readRDS(testthat::test_path("fixtures", "source_nonresponse_v2_synthetic.rds"))
  check_case(list(name = "v1 339", raw = v1$inputs$raw$data,
    receipt = v1$inputs$raw$source_nonresponse$source,
    ledger = v1$inputs$raw$source_nonresponse$ledger,
    target = "ci23p339", schema_version = "1.0.0", recipe = NULL))
  for (suffix in c("337", "338")) {
    saved <- v2$fields[[suffix]]
    check_case(list(name = paste("v2", suffix), raw = v2$raw,
      receipt = v2$receipt, ledger = saved$inputs$raw$source_nonresponse$ledger,
      target = paste0("ci23p", suffix), schema_version = "2.0.0", recipe = saved$recipe))
  }
})
