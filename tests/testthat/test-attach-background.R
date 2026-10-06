# synthetic local sources keep attachment checks independent of panel data
.bg_write <- function(directory, data, name = "avars_202301_EN_1.0p.sav") {
  path <- file.path(directory, name)
  haven::write_sav(data, path)
  path
}

.bg_source <- function(person = c(1, 2), month = rep(202301, length(person)),
                       age = seq_along(person) + 30) {
  data.frame(nomem_encr = person, wave = month,
    age = haven::labelled(as.numeric(age), label = "Age in years"))
}

.bg_fixture <- function(.local_envir = parent.frame()) {
  directory <- withr::local_tempdir("attach_background_", .local_envir = .local_envir)
  path <- .bg_write(directory, .bg_source())
  list(directory = directory, path = path,
       sources = data.frame(sav_path = path, expected_month = 202301),
       data = data.frame(nomem_encr = c(2, 1), fieldwork_ym = c(202301, 202301)))
}

.bg_two <- function(.local_envir = parent.frame()) {
  fixture <- .bg_fixture(.local_envir)
  second <- .bg_write(fixture$directory, .bg_source(month = c(202302, 202302),
    age = c(41, 42)), "avars_202302_EN_1.0p.sav")
  fixture$sources <- data.frame(sav_path = c(fixture$path, second),
                               expected_month = c(202301, 202302))
  fixture$data <- data.frame(nomem_encr = c(2, 1, 1),
                            fieldwork_ym = c(202302, 202301, 202302))
  fixture
}

.bg_attach <- function(fixture, data = fixture$data, sources = fixture$sources,
                       variables = "age", prefix = "avars_") {
  lissr::liss_attach_background(data, sources, month_col = "fieldwork_ym",
                               variables = variables, prefix = prefix)
}

.bg_error <- function(expr) {
  testthat::expect_error(expr, class = "liss_background_error")
}

.bg_capture <- function(expr) {
  warnings <- list()
  value <- withCallingHandlers(expr, warning = function(condition) {
    warnings[[length(warnings) + 1L]] <<- condition
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = warnings)
}

.bg_sha256 <- function(path) {
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  paste0(openssl::sha256(bytes))
}

.bg_zip <- function(directory, files, name = "avars_202301_EN_1.0p.zip") {
  if (!nzchar(Sys.which(Sys.getenv("R_ZIPCMD", "zip"))))
    testthat::skip("a native ZIP writer is unavailable")
  archive <- file.path(directory, name)
  withr::with_dir(directory, utils::zip(archive, files = basename(files),
                                      flags = "-q"))
  if (!file.exists(archive)) stop("native ZIP writer did not create the fixture")
  archive
}

.bg_lineage <- function(fixture, archive, member = basename(fixture$path)) {
  sources <- fixture$sources
  sources$archive_path <- archive
  sources$archive_member <- member
  sources
}

.bg_replace_bytes <- function(path, from, to) {
  stopifnot(nchar(from, type = "bytes") == nchar(to, type = "bytes"))
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  old <- charToRaw(from)
  new <- charToRaw(to)
  positions <- which(vapply(seq_len(length(bytes) - length(old) + 1L),
    function(i) identical(bytes[i + seq_along(old) - 1L], old), logical(1)))
  stopifnot(length(positions) > 0L)
  for (i in positions) bytes[i + seq_along(old) - 1L] <- new
  writeBin(bytes, path)
  invisible(path)
}

test_that("background attachment matches exact keys and preserves repeated survey rows", {
  fixture <- .bg_two()
  fixture$data <- fixture$data[c(1, 2, 2, 3), , drop = FALSE]
  rownames(fixture$data) <- c("third", "first", "repeat", "fourth")
  fixture$data$score <- haven::labelled(c(8, 7, 6, 5), labels = c("low" = 5),
                                      label = "Original survey score")
  attr(fixture$data$fieldwork_ym, "label") <- "Caller-selected item month"
  attr(fixture$data, "study") <- "Synthetic repeated observations"
  before <- fixture$data
  saved <- haven::read_sav(fixture$path, user_na = TRUE)$age
  out <- .bg_attach(fixture)
  expect_identical(names(out), c("data", "audit", "provenance"))
  expect_identical(out$data[names(before)], before[names(before)])
  expect_identical(fixture$data, before)
  expect_identical(rownames(out$data), rownames(before))
  expect_identical(attr(out$data, "study"), attr(before, "study"))
  expect_identical(names(out$data), c(names(before), "avars_age"))
  expect_identical(as.numeric(out$data$avars_age), c(42, 31, 31, 41))
  expect_identical(attributes(out$data$avars_age), attributes(saved))
  expect_identical(out$audit$row_status, rep("matched", 4))
  expect_equal(out$audit$n_input, 4)
  expect_equal(out$audit$n_output, 4)
  expect_equal(out$audit$n_matched, 4)
  expect_equal(out$audit$eligible_match_fraction, 1)
  expect_false("valid_for_analysis" %in% names(out))
})

test_that("background attachment requires an explicit month and plain survey frame", {
  fixture <- .bg_fixture()
  expect_error(lissr::liss_attach_background(fixture$data, fixture$sources,
                                           variables = "age"))
  for (bad in list(list(data = fixture$data), dplyr::group_by(fixture$data, nomem_encr),
                  dplyr::rowwise(fixture$data),
                  structure(fixture$data, class = c("custom_frame", "data.frame"))))
    .bg_error(.bg_attach(fixture, data = bad))
  tibble_data <- tibble::as_tibble(fixture$data)
  expect_identical(class(.bg_attach(fixture, data = tibble_data)$data), class(tibble_data))
})

test_that("background attachment validates scalar arguments and exact variable names", {
  fixture <- .bg_fixture()
  for (month in list(NA_character_, "", character(), c("fieldwork_ym", "wave"),
                     202301, "nomem_encr", "absent"))
    .bg_error(lissr::liss_attach_background(fixture$data, fixture$sources,
      month_col = month, variables = "age"))
  for (prefix in list(NULL, NA_character_, "", c("a", "b"), 1))
    .bg_error(.bg_attach(fixture, prefix = prefix))
  for (variables in list(NULL, character(), NA_character_, c("age", "age"),
                         "", "nomem_encr", "wave", 1, factor("age")))
    .bg_error(.bg_attach(fixture, variables = variables))
  collision <- fixture$data
  collision$avars_age <- 99
  .bg_error(.bg_attach(fixture, data = collision))
  .bg_error(.bg_attach(fixture, variables = "Age"))
})

test_that("background attachment rejects ambiguous survey and manifest field names", {
  fixture <- .bg_fixture()
  for (names_bad in list(c("nomem_encr", "nomem_encr"), c("nomem_encr", ""),
                         c("nomem_encr", NA_character_))) {
    data <- fixture$data
    names(data) <- names_bad
    .bg_error(.bg_attach(fixture, data = data))
    sources <- fixture$sources
    names(sources) <- names_bad
    .bg_error(.bg_attach(fixture, sources = sources))
  }
})

test_that("background attachment admits only an explicit well-typed source manifest", {
  fixture <- .bg_fixture()
  bad <- list(NULL, fixture$path, fixture$sources[FALSE, ],
    fixture$sources["sav_path"], fixture$sources["expected_month"],
    transform(fixture$sources, extra = 1),
    transform(fixture$sources, sav_path = factor(sav_path)),
    transform(fixture$sources, sav_path = ""),
    transform(fixture$sources, sav_path = NA_character_),
    transform(fixture$sources, expected_month = 202313),
    transform(fixture$sources, expected_month = 189912),
    transform(fixture$sources, expected_month = TRUE),
    transform(fixture$sources, expected_month = factor(expected_month)),
    dplyr::group_by(fixture$sources, expected_month))
  listed <- fixture$sources
  listed$sav_path <- list(fixture$path)
  bad <- c(bad, list(listed))
  for (sources in bad) .bg_error(.bg_attach(fixture, sources = sources))
})

test_that("background attachment never substitutes source keys or requested columns", {
  fixture <- .bg_fixture()
  for (absent in c("nomem_encr", "wave", "age")) {
    source <- .bg_source()
    source[[absent]] <- NULL
    .bg_write(fixture$directory, source)
    .bg_error(.bg_attach(fixture))
  }
})

test_that("background attachment rejects incomplete duplicate and mixed source keys", {
  fixture <- .bg_fixture()
  bad <- list(.bg_source(person = c(1, NA_real_)),
    .bg_source(month = c(202301, NA_real_)),
    .bg_source(person = c(1, 1)),
    .bg_source(month = c(202301, 202302)),
    .bg_source()[FALSE, ])
  for (source in bad) {
    .bg_write(fixture$directory, source)
    .bg_error(.bg_attach(fixture))
  }
  source <- .bg_source()
  source$nomem_encr <- haven::labelled_spss(c(1, -9), na_values = -9)
  .bg_write(fixture$directory, source)
  .bg_error(.bg_attach(fixture))
  source <- .bg_source()
  source$wave <- haven::labelled_spss(c(202301, -9), na_values = -9)
  .bg_write(fixture$directory, source)
  .bg_error(.bg_attach(fixture))
  source <- .bg_source()
  source$nomem_encr <- haven::labelled_spss(c(1, -3), na_range = c(-5, -1))
  .bg_write(fixture$directory, source)
  .bg_error(.bg_attach(fixture))
})

test_that("background attachment rejects invalid background values without dropping rows", {
  fixture <- .bg_fixture()
  for (person in c(0, -1, 1.5, 9007199254740992)) {
    .bg_write(fixture$directory, .bg_source(person = c(1, person)))
    .bg_error(.bg_attach(fixture))
  }
  for (month in c(202313, 202301.5, 189912, 210101)) {
    .bg_write(fixture$directory, .bg_source(month = c(month, month)))
    .bg_error(.bg_attach(fixture))
  }
  for (person in c("01", "+1", "1.0", " 1", "9007199254740992")) {
    .bg_write(fixture$directory, .bg_source(person = c("2", person)))
    .bg_error(.bg_attach(fixture))
  }
  for (key in c("nomem_encr", "wave")) {
    source <- .bg_source()
    source[[key]] <- as.Date(c("2023-01-01", "2023-01-01"))
    .bg_write(fixture$directory, source)
    .bg_error(.bg_attach(fixture))
  }
})

test_that("background attachment surfaces malformed FL2 and duplicate source fields as read errors", {
  fixture <- .bg_fixture()
  writeBin(c(charToRaw("$FL2"), raw(100)), fixture$path)
  .bg_error(.bg_attach(fixture))
  source <- .bg_source()
  source$dupcol01 <- c(1, 2)
  source$dupcol02 <- c(3, 4)
  .bg_write(fixture$directory, source)
  .bg_replace_bytes(fixture$path, "dupcol02", "dupcol01")
  .bg_error(.bg_attach(fixture))
})

test_that("background attachment validates observed expected and recognized filename months", {
  fixture <- .bg_fixture()
  .bg_error(.bg_attach(fixture, sources = transform(fixture$sources,
                                                      expected_month = 202302)))
  path <- .bg_write(fixture$directory, .bg_source(), "avars_202302_EN_1.0p.sav")
  .bg_error(.bg_attach(fixture, sources = data.frame(sav_path = path,
                                                       expected_month = 202301)))
  path <- .bg_write(fixture$directory, .bg_source(), "avars_202301_EN_1_0p.SAV")
  out <- .bg_attach(fixture, sources = data.frame(sav_path = path,
                                                 expected_month = 202301))
  expect_identical(out$provenance$sources[[1]]$sav_basename, basename(path))
})

test_that("background attachment retains unknown standalone filename provenance", {
  fixture <- .bg_fixture()
  path <- .bg_write(fixture$directory, .bg_source(), "caller_selected.sav")
  capture <- .bg_capture(.bg_attach(fixture, sources = data.frame(sav_path = path,
                                                                expected_month = 202301)))
  expect_length(capture$warnings, 0)
  expect_equal(capture$value$audit$filename_unknown, 1)
  source <- capture$value$provenance$sources[[1]]
  expect_identical(source$filename$language, "UNKNOWN")
  expect_identical(source$filename$version, "UNKNOWN")
  expect_identical(source$lineage$status, "standalone")
  expect_identical(source$sav_sha256, .bg_sha256(path))
})

test_that("background attachment rejects competing sources and resolved duplicate paths", {
  fixture <- .bg_fixture()
  second <- .bg_write(fixture$directory, .bg_source(person = c(3, 4)),
                      "avars_202301_NL_1.0p.sav")
  sources <- data.frame(sav_path = c(fixture$path, second), expected_month = c(202301, 202301))
  .bg_error(.bg_attach(fixture, sources = sources))
  sources$sav_path[[2]] <- file.path(dirname(fixture$path), ".", basename(fixture$path))
  .bg_error(.bg_attach(fixture, sources = sources))
})

test_that("background attachment joins lossless numeric and canonical text identifiers", {
  fixture <- .bg_fixture()
  data <- fixture$data
  data$nomem_encr <- as.character(data$nomem_encr)
  out <- .bg_attach(fixture, data = data)
  expect_identical(out$data$nomem_encr, data$nomem_encr)
  expect_identical(as.numeric(out$data$avars_age), c(32, 31))
  .bg_write(fixture$directory, .bg_source(person = c("1", "2")))
  expect_identical(as.numeric(.bg_attach(fixture)$data$avars_age), c(32, 31))
  boundary <- 9007199254740991
  .bg_write(fixture$directory, .bg_source(person = boundary))
  for (person in list(boundary, "9007199254740991")) {
    data <- data.frame(nomem_encr = person, fieldwork_ym = 202301)
    expect_identical(as.numeric(.bg_attach(fixture, data = data)$data$avars_age), 31)
    expect_identical(.bg_attach(fixture, data = data)$data$nomem_encr, person)
  }
})

test_that("background attachment rejects invalid or inexact survey identifier grammar", {
  fixture <- .bg_fixture()
  bad <- list(0, -1, 1.5, Inf, -Inf, 9007199254740992,
              "0", "01", "+1", "-1", "1.0", "1e0", " 1", "1 ",
              "9007199254740992", "9999999999999999999999999999999999999")
  for (person in bad) {
    data <- data.frame(nomem_encr = person, fieldwork_ym = 202301)
    .bg_error(.bg_attach(fixture, data = data))
  }
})

test_that("background attachment rejects unsupported survey key classes before coercion", {
  fixture <- .bg_fixture()
  unsupported <- list(factor(c("2", "1")), c(TRUE, FALSE),
    as.Date(c("2023-01-01", "2023-01-02")), list(2, 1),
    structure(c(2, 1), class = "custom_numeric"))
  for (key in unsupported) {
    data <- fixture$data
    data$nomem_encr <- key
    .bg_error(.bg_attach(fixture, data = data))
    data <- fixture$data
    data$fieldwork_ym <- key
    .bg_error(.bg_attach(fixture, data = data))
  }
})

test_that("background attachment parses explicit month grammar without modifying survey values", {
  fixture <- .bg_fixture()
  data <- fixture$data
  data$fieldwork_ym <- c(" 202301 ", "202301")
  before <- data
  out <- .bg_attach(fixture, data = data)
  expect_identical(out$data[names(data)], before)
  expect_identical(as.numeric(out$data$avars_age), c(32, 31))
  for (month in list(202301.5, 2023, 202300, 202313, 189912, 210101,
                     "2023-01", "+202301", "202301.0")) {
    for (person in list(1, NA_real_)) {
      bad <- data.frame(nomem_encr = person, fieldwork_ym = month)
      .bg_error(.bg_attach(fixture, data = bad))
    }
  }
})

test_that("background attachment recognizes ordinary tagged blank and declared missing keys", {
  fixture <- .bg_fixture()
  data <- data.frame(nomem_encr = c(1, NA_real_, NaN, haven::tagged_na("a"), 2, 2),
                     fieldwork_ym = c(202301, 202301, 202301, 202301, NA_real_, NaN))
  out <- .bg_capture(.bg_attach(fixture, data = data))
  expect_length(out$warnings, 1)
  expect_identical(out$value$audit$row_status, c("matched", rep("missing_person", 3),
                                               rep("missing_month", 2)))
  expect_identical(out$value$data[names(data)], data)
  data <- data.frame(nomem_encr = c("1", "", "  ", NA_character_),
                     fieldwork_ym = c("202301", "202301", "", NA_character_))
  out <- .bg_capture(.bg_attach(fixture, data = data))$value
  expect_identical(out$audit$row_status,
    c("matched", "missing_person", "missing_both_keys", "missing_both_keys"))
})

test_that("background attachment unions live and stashed survey missing declarations", {
  fixture <- .bg_fixture()
  data <- data.frame(nomem_encr = c(1, -9, -8, -3, -12, 2, 2, 2, 2),
    fieldwork_ym = c(rep(202301, 5), -9, -8, -3, -12))
  for (name in c("nomem_encr", "fieldwork_ym")) {
    attr(data[[name]], "na_values") <- -9
    attr(data[[name]], "_original_na_values") <- -8
    attr(data[[name]], "na_range") <- c(-5, -1)
    attr(data[[name]], "_original_na_range") <- c(-15, -10)
  }
  before <- data
  out <- .bg_capture(.bg_attach(fixture, data = data))$value
  expect_identical(out$audit$row_status,
    c("matched", rep("missing_person", 4), rep("missing_month", 4)))
  expect_identical(out$data[names(data)], before)
  expect_equal(unname(out$audit$key_validation$survey$person[["declared_missing"]]), 4)
  expect_equal(unname(out$audit$key_validation$survey$month[["declared_missing"]]), 4)
})

test_that("background attachment distinguishes coverage reasons and output denominators", {
  fixture <- .bg_fixture()
  data <- data.frame(nomem_encr = c(1, 99, 1, NA, 1, NA),
    fieldwork_ym = c(202301, 202301, 202302, 202301, NA, NA))
  capture <- .bg_capture(.bg_attach(fixture, data = data))
  expect_length(capture$warnings, 1)
  out <- capture$value
  expected <- c("matched", "respondent_not_found", "month_not_supplied",
                "missing_person", "missing_month", "missing_both_keys")
  expect_identical(out$audit$row_status, expected)
  expect_identical(out$data[names(data)], data)
  expect_equal(out$audit$n_eligible, 3)
  expect_equal(out$audit$n_matched, 1)
  expect_equal(out$audit$eligible_match_fraction, 1 / 3)
  expect_equal(out$audit$all_row_match_fraction, 1 / 6)
  expect_equal(sum(out$audit$status_counts), nrow(data))
  expect_equal(unname(out$audit$status_counts[expected]), rep(1, 6))
  counts <- out$audit$variable_missingness
  expect_equal(counts$n_matched_nonmissing, 1)
  expect_equal(counts$n_matched_source_missing, 0)
  expect_equal(counts$n_unmatched, 5)
  expect_true(all(is.na(out$data$avars_age[-1])))
  for (bad in list(data.frame(nomem_encr = NA_real_, fieldwork_ym = 202301),
                  data.frame(nomem_encr = 99, fieldwork_ym = 202301)))
    .bg_error(.bg_attach(fixture, data = bad))
})

test_that("background attachment validates unused months and consolidates coverage warnings", {
  fixture <- .bg_two()
  data <- data.frame(nomem_encr = c(1, 99), fieldwork_ym = c(202301, 202301))
  capture <- .bg_capture(.bg_attach(fixture, data = data))
  expect_length(capture$warnings, 1)
  expect_match(conditionMessage(capture$warnings[[1]]), "unmatched", ignore.case = TRUE)
  expect_match(conditionMessage(capture$warnings[[1]]), "unused", ignore.case = TRUE)
  expect_equal(nrow(capture$value$audit$source_coverage), 2)
  expect_identical(capture$value$audit$source_coverage$unused, c(FALSE, TRUE))
  expect_length(capture$value$provenance$sources, 2)
  second <- .bg_source(month = c(202302, 202302))
  second$age <- NULL
  .bg_write(fixture$directory, second, basename(fixture$sources$sav_path[[2]]))
  .bg_error(.bg_attach(fixture, data = data))
})

test_that("background attachment returns validated typed zero-row survey output", {
  fixture <- .bg_fixture()
  data <- fixture$data[FALSE, , drop = FALSE]
  capture <- .bg_capture(.bg_attach(fixture, data = data))
  expect_length(capture$warnings, 0)
  out <- capture$value
  expect_identical(out$data[names(data)], data)
  expect_identical(nrow(out$data), 0L)
  prototype <- haven::read_sav(fixture$path, user_na = TRUE)$age
  expect_identical(typeof(out$data$avars_age), typeof(prototype))
  expect_identical(class(out$data$avars_age), class(prototype))
  expect_identical(attributes(out$data$avars_age), attributes(prototype))
  expect_length(out$data$avars_age, 0)
  expect_length(out$audit$row_status, 0)
  expect_equal(out$audit$n_output, 0)
  expect_true(is.na(out$audit$eligible_match_fraction))
  expect_true(is.na(out$audit$all_row_match_fraction))
  expect_equal(sum(out$audit$status_counts), 0)
})

test_that("background attachment preserves source missing codes and counts repeated output rows", {
  fixture <- .bg_fixture()
  source <- .bg_source()
  source$age <- haven::labelled_spss(c(-9, NA_real_),
    labels = c("No answer" = -9), na_values = -9, label = "Age in years")
  .bg_write(fixture$directory, source)
  data <- data.frame(nomem_encr = c(1, 1, 2), fieldwork_ym = rep(202301, 3))
  out <- .bg_attach(fixture, data = data)
  expect_identical(out$audit$row_status, rep("matched", 3))
  expect_identical(as.numeric(out$data$avars_age), c(-9, -9, NA_real_))
  saved <- haven::read_sav(fixture$path, user_na = TRUE)$age
  expect_identical(attributes(out$data$avars_age), attributes(saved))
  counts <- out$audit$variable_missingness
  expect_equal(counts$n_matched_source_missing, 3)
  expect_equal(counts$n_matched_nonmissing, 0)
  expect_equal(counts$n_unmatched, 0)
  expect_equal(counts$n_matched_nonmissing + counts$n_matched_source_missing + counts$n_unmatched,
               nrow(data))
  expect_equal(out$audit$source_variable_missingness$n_source_rows, 2)
  expect_equal(out$audit$source_variable_missingness$n_source_missing, 2)
  source$age <- haven::labelled(c(-99, 32), labels = c("Unknown" = -99), label = "Age in years")
  .bg_write(fixture$directory, source)
  out <- .bg_attach(fixture)
  expect_identical(as.numeric(out$data$avars_age), c(32, -99))
  expect_equal(out$audit$variable_missingness$n_matched_source_missing, 0)
  source$age <- haven::labelled_spss(c(-3, 32), na_range = c(-5, -1), label = "Age in years")
  .bg_write(fixture$directory, source)
  out <- .bg_attach(fixture)
  expect_identical(as.numeric(out$data$avars_age), c(32, -3))
  expect_equal(out$audit$variable_missingness$n_matched_source_missing, 1)
})

test_that("background attachment counts unmarked string blanks as matched nonmissing values", {
  fixture <- .bg_fixture()
  source <- .bg_source(person = 1:4)
  source$text <- c("", " ", "abc", "NA")
  .bg_write(fixture$directory, source)
  data <- data.frame(nomem_encr = c(1, 1, 2, 3, 4), fieldwork_ym = rep(202301, 5))
  saved <- haven::read_sav(fixture$path, user_na = TRUE)$text
  out <- .bg_attach(fixture, data = data, variables = "text")
  expect_identical(out$audit$row_status, rep("matched", 5))
  expect_identical(as.character(out$data$avars_text), as.character(saved[c(1, 1, 2, 3, 4)]))
  expect_identical(attributes(out$data$avars_text), attributes(saved))
  expect_equal(out$audit$variable_missingness$n_matched_nonmissing, 5)
  expect_equal(out$audit$variable_missingness$n_matched_source_missing, 0)
  source$text <- haven::labelled_spss(source$text, na_values = "", label = "String item")
  .bg_write(fixture$directory, source)
  saved <- haven::read_sav(fixture$path, user_na = TRUE)$text
  out <- .bg_attach(fixture, data = data, variables = "text")
  expect_identical(as.character(out$data$avars_text), as.character(saved[c(1, 1, 2, 3, 4)]))
  expect_identical(attributes(out$data$avars_text), attributes(saved))
  expect_equal(out$audit$variable_missingness$n_matched_nonmissing, 2)
  expect_equal(out$audit$variable_missingness$n_matched_source_missing, 3)
  expect_equal(out$audit$source_variable_missingness$n_source_missing, 2)
})

test_that("background attachment selected metadata must agree across every snapshot", {
  fixture <- .bg_two()
  mutations <- list(
    function(x) { x$age <- as.character(x$age); x },
    function(x) { x$age <- haven::labelled(as.numeric(x$age), label = "Age wording changed"); x },
    function(x) { x$age <- haven::labelled(as.numeric(x$age), labels = c("One" = 1), label = "Age in years"); x },
    function(x) { x$age <- haven::labelled_spss(as.numeric(x$age), na_values = -9, label = "Age in years"); x },
    function(x) { x$age <- haven::labelled_spss(as.numeric(x$age), na_range = c(-9, -1), label = "Age in years"); x },
    function(x) { attr(x$age, "format.spss") <- "F12.2"; x })
  for (mutate in mutations) {
    source <- mutate(.bg_source(month = c(202302, 202302), age = c(41, 42)))
    .bg_write(fixture$directory, source, basename(fixture$sources$sav_path[[2]]))
    .bg_error(.bg_attach(fixture))
  }
})

test_that("background attachment compares value-label associations independently of order", {
  fixture <- .bg_two()
  first <- .bg_source()
  first$age <- haven::labelled(c(31, 32), labels = c("Thirty one" = 31, "Thirty two" = 32),
                              label = "Age in years")
  second <- .bg_source(month = c(202302, 202302))
  second$age <- haven::labelled(c(31, 32), labels = c("Thirty two" = 32, "Thirty one" = 31),
                               label = "Age in years")
  first$unrequested <- c(1, 2)
  second$unrequested <- c("different", "storage")
  .bg_write(fixture$directory, first)
  .bg_write(fixture$directory, second, basename(fixture$sources$sav_path[[2]]))
  expect_identical(as.numeric(.bg_attach(fixture)$data$avars_age), c(32, 31, 31))
})

test_that("background attachment copies household fields descriptively and isolates month policy", {
  fixture <- .bg_fixture()
  source <- .bg_source()
  source$nohouse_encr <- c(7, 7)
  .bg_write(fixture$directory, source)
  data <- fixture$data
  data$nohouse_encr <- c(99, 98)
  data$s_m1 <- c(202301, 202301)
  data$s_m2 <- c(202302, 202302)
  data$year <- c(2020, 2020)
  data$income <- haven::labelled(c(-99, 1000), labels = c("Unknown" = -99))
  out <- .bg_attach(fixture, data = data, variables = c("nohouse_encr", "age"))
  expect_identical(out$data[names(data)], data)
  expect_identical(as.numeric(out$data$avars_nohouse_encr), c(7, 7))
  expect_identical(tail(names(out$data), 2), c("avars_nohouse_encr", "avars_age"))
  data$fieldwork_ym <- c(202302, 202302)
  .bg_error(.bg_attach(fixture, data = data))
})

test_that("background attachment guards all nonlocal and non-SAV inputs before reading", {
  fixture <- .bg_fixture()
  calls <- 0L
  testthat::local_mocked_bindings(.liss_bg_read_sav = function(...) {
    calls <<- calls + 1L
    stop("reader should not be reached")
  })
  other <- file.path(fixture$directory, "source.dta")
  file.copy(fixture$path, other)
  for (extension in c("csv", "zip", "zsav")) {
    path <- file.path(fixture$directory, paste0("wrong_extension.", extension))
    file.copy(fixture$path, path)
    .bg_error(.bg_attach(fixture, sources = data.frame(sav_path = path, expected_month = 202301)))
  }
  for (path in c("https://example.invalid/avars.sav", "file:///tmp/avars.sav",
                 fixture$directory, other, file.path(fixture$directory, "absent.sav")))
    .bg_error(.bg_attach(fixture, sources = data.frame(sav_path = path, expected_month = 202301)))
  for (value in list(as.raw(0), list(fixture$path))) {
    sources <- fixture$sources
    sources$sav_path <- value
    .bg_error(.bg_attach(fixture, sources = sources))
  }
  expect_identical(calls, 0L)
})

test_that("background attachment rejects disguised containers from raw signatures before haven", {
  fixture <- .bg_fixture()
  calls <- 0L
  testthat::local_mocked_bindings(.liss_bg_read_sav = function(...) {
    calls <<- calls + 1L
    stop("reader should not be reached")
  })
  signatures <- list(zip = as.raw(c(80, 75, 3, 4)), gzip = as.raw(c(31, 139, 8, 0)),
    xz = as.raw(c(253, 55, 122, 88, 90, 0)), bzip2 = charToRaw("BZh9"),
    zsav = charToRaw("$FL3"), unknown = as.raw(c(0, 1, 2, 3)))
  for (name in names(signatures)) {
    path <- file.path(fixture$directory, paste0(name, ".sav"))
    writeBin(c(signatures[[name]], raw(100)), path)
    .bg_error(.bg_attach(fixture, sources = data.frame(sav_path = path, expected_month = 202301)))
  }
  expect_identical(calls, 0L)
})

test_that("background attachment preserves local symlinks and rejects extension bypasses", {
  fixture <- .bg_fixture()
  link <- file.path(fixture$directory, "linked.sav")
  if (!isTRUE(file.symlink(fixture$path, link))) testthat::skip("local symlinks are unavailable")
  sources <- data.frame(sav_path = link, expected_month = 202301)
  out <- .bg_attach(fixture, sources = sources)
  expect_identical(out$provenance$sources[[1]]$sav_path_supplied, link)
  expect_identical(out$provenance$sources[[1]]$sav_path_resolved, normalizePath(fixture$path, winslash = "/"))
  other <- file.path(fixture$directory, "renamed.bin")
  file.copy(fixture$path, other)
  bad_link <- file.path(fixture$directory, "renamed.sav")
  expect_true(file.symlink(other, bad_link))
  .bg_error(.bg_attach(fixture, sources = data.frame(sav_path = bad_link, expected_month = 202301)))
})

test_that("background attachment verifies exact archive member bytes and preserves corrected versions", {
  fixture <- .bg_fixture()
  archive <- .bg_zip(fixture$directory, fixture$path, "avars_202301_EN_1.1p.zip")
  before <- tools::md5sum(c(fixture$path, archive))
  sources <- .bg_lineage(fixture, archive)
  out <- .bg_attach(fixture, sources = sources)
  provenance <- out$provenance$sources[[1]]
  expect_identical(provenance$lineage$status, "verified")
  expect_identical(provenance$lineage$archive_member, basename(fixture$path))
  expect_identical(provenance$sav_sha256, .bg_sha256(fixture$path))
  expect_identical(provenance$lineage$member_sha256, .bg_sha256(fixture$path))
  expect_identical(provenance$lineage$archive_sha256, .bg_sha256(archive))
  for (hash in list(provenance$sav_sha256, provenance$lineage$member_sha256,
                    provenance$lineage$archive_sha256)) {
    expect_type(hash, "character")
    expect_false(is.object(hash))
  }
  expect_identical(provenance$filename$version, "1.0p")
  expect_identical(provenance$lineage$archive_filename$version, "1.1p")
  expect_no_error(jsonlite::toJSON(out$provenance, auto_unbox = TRUE))
  expect_identical(tools::md5sum(c(fixture$path, archive)), before)
})

test_that("background attachment requires both archive lineage fields or genuine NA omission", {
  fixture <- .bg_fixture()
  for (field in c("archive_path", "archive_member")) {
    sources <- fixture$sources
    sources[[field]] <- "specified"
    .bg_error(.bg_attach(fixture, sources = sources))
  }
  sources <- fixture$sources
  sources$archive_path <- NA_character_
  sources$archive_member <- NA_character_
  expect_identical(.bg_attach(fixture, sources = sources)$provenance$sources[[1]]$lineage$status,
                   "standalone")
  for (pair in list(c("", ""), c("something.zip", NA_character_),
                    c(NA_character_, "something.sav"))) {
    sources$archive_path <- pair[[1]]
    sources$archive_member <- pair[[2]]
    .bg_error(.bg_attach(fixture, sources = sources))
  }
  sources$archive_path <- factor("something.zip")
  sources$archive_member <- "something.sav"
  .bg_error(.bg_attach(fixture, sources = sources))
  sources$archive_path <- "something.zip"
  sources$archive_member <- factor("something.sav")
  .bg_error(.bg_attach(fixture, sources = sources))
})

test_that("background attachment requires an existing local ZIP with archive path identities", {
  fixture <- .bg_fixture()
  archive <- .bg_zip(fixture$directory, fixture$path)
  for (path in c("https://example.invalid/source.zip", fixture$directory,
                  file.path(fixture$directory, "absent.zip"), fixture$path))
    .bg_error(.bg_attach(fixture, sources = .bg_lineage(fixture, path)))
  relative <- basename(fixture$path)
  sources <- data.frame(sav_path = relative, expected_month = 202301,
    archive_path = basename(archive), archive_member = relative)
  out <- withr::with_dir(fixture$directory, .bg_attach(fixture, sources = sources))
  record <- out$provenance$sources[[1]]
  expect_identical(record$sav_path_supplied, relative)
  expect_identical(record$sav_path_resolved, normalizePath(fixture$path, winslash = "/"))
  expect_identical(record$lineage$archive_path_supplied, basename(archive))
  expect_identical(record$lineage$archive_path_resolved, normalizePath(archive, winslash = "/"))
})

test_that("background attachment rejects archive member path syntax on every platform", {
  fixture <- .bg_fixture()
  archive <- .bg_zip(fixture$directory, fixture$path)
  members <- c("../source.sav", "./source.sav", "folder/source.sav", "folder\\source.sav",
               "/source.sav", "C:source.sav", "C:\\source.sav", "\\\\host\\source.sav",
               "source.dta", "missing.sav")
  for (member in members)
    .bg_error(.bg_attach(fixture, sources = .bg_lineage(fixture, archive, member)))
})

test_that("background attachment rejects archive content language and month disagreement", {
  fixture <- .bg_fixture()
  changed <- .bg_write(fixture$directory, .bg_source(age = c(80, 81)), "changed.sav")
  archive <- .bg_zip(fixture$directory, changed, "changed.zip")
  .bg_error(.bg_attach(fixture, sources = .bg_lineage(fixture, archive, "changed.sav")))
  for (name in c("avars_202302_EN_1.0p.zip", "avars_202301_NL_1.0p.zip")) {
    archive <- .bg_zip(fixture$directory, fixture$path, name)
    .bg_error(.bg_attach(fixture, sources = .bg_lineage(fixture, archive)))
  }
  for (name in c("avars_202302_EN_1.0p.sav", "avars_202301_NL_1.0p.sav")) {
    path <- file.path(fixture$directory, name)
    file.copy(fixture$path, path)
    archive <- .bg_zip(fixture$directory, path, paste0(name, ".zip"))
    .bg_error(.bg_attach(fixture, sources = .bg_lineage(fixture, archive, name)))
  }
})

test_that("background attachment rejects duplicate archive member names without extraction", {
  fixture <- .bg_fixture()
  first <- file.path(fixture$directory, "one.sav")
  second <- file.path(fixture$directory, "two.sav")
  file.copy(fixture$path, first)
  file.copy(fixture$path, second)
  archive <- .bg_zip(fixture$directory, c(first, second), "duplicates.zip")
  .bg_replace_bytes(archive, "two.sav", "one.sav")
  expect_equal(sum(utils::unzip(archive, list = TRUE)$Name == "one.sav"), 2)
  before <- list.files(fixture$directory, recursive = TRUE, all.files = TRUE)
  .bg_error(.bg_attach(fixture, sources = .bg_lineage(fixture, archive, "one.sav")))
  expect_identical(list.files(fixture$directory, recursive = TRUE, all.files = TRUE), before)
})

test_that("background attachment reads each source once with user missing metadata retained", {
  fixture <- .bg_two()
  reader <- haven::read_sav
  seen <- character()
  settings <- list()
  testthat::local_mocked_bindings(read_sav = function(file, user_na, .name_repair, ...) {
    seen <<- c(seen, file)
    settings[[length(settings) + 1L]] <<- list(user_na = user_na, .name_repair = .name_repair)
    reader(file, user_na = user_na, .name_repair = .name_repair, ...)
  }, .package = "haven")
  out <- .bg_attach(fixture)
  expect_length(seen, 2)
  expect_setequal(seen, normalizePath(fixture$sources$sav_path, winslash = "/"))
  expect_identical(settings, rep(list(list(user_na = TRUE, .name_repair = "check_unique")), 2))
  expect_identical(out$provenance$read_settings$user_na, TRUE)
  expect_identical(out$provenance$read_settings$.name_repair, "check_unique")
})

test_that("background attachment sanitizes unexpected reader warnings before returning data", {
  fixture <- .bg_fixture()
  reader <- haven::read_sav
  testthat::local_mocked_bindings(read_sav = function(...) {
    warning("unexpected reader warning with person 3141592653589", call. = FALSE)
    reader(...)
  }, .package = "haven")
  condition <- tryCatch(.bg_attach(fixture), error = identity)
  expect_s3_class(condition, "liss_background_error")
  expect_identical(condition$code, "sav_read_warning")
  expect_false(grepl("3141592653589", paste(capture.output(str(condition)), collapse = "\n"),
                     fixed = TRUE))
})

test_that("background attachment rejects observable source changes during its read", {
  fixture <- .bg_fixture()
  reader <- lissr:::.liss_bg_read_sav
  testthat::local_mocked_bindings(.liss_bg_read_sav = function(path, ...) {
    source <- reader(path, ...)
    connection <- file(path, open = "ab")
    writeBin(as.raw(0), connection)
    close(connection)
    source
  })
  .bg_error(.bg_attach(fixture))
})

test_that("background attachment rejects observable archive changes during verification", {
  fixture <- .bg_fixture()
  archive <- .bg_zip(fixture$directory, fixture$path)
  hasher <- lissr:::.liss_bg_sha256_connection
  calls <- 0L
  testthat::local_mocked_bindings(.liss_bg_sha256_connection = function(connection) {
    digest <- hasher(connection)
    calls <<- calls + 1L
    if (calls == 3L) {
      output <- file(archive, open = "ab")
      writeBin(as.raw(0), output)
      close(output)
    }
    digest
  })
  .bg_error(.bg_attach(fixture, sources = .bg_lineage(fixture, archive)))
})

test_that("background attachment hashes raw file bytes with a known SHA256 oracle", {
  fixture <- .bg_fixture()
  path <- file.path(fixture$directory, "known.bin")
  writeBin(charToRaw("abc"), path)
  connection <- file(path, open = "rb", raw = TRUE)
  withr::defer(close(connection))
  hash <- lissr:::.liss_bg_sha256_connection(connection)
  expect_type(hash, "character")
  expect_false(is.object(hash))
  expect_identical(hash,
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
  out <- .bg_attach(fixture)
  expect_identical(out$provenance$sources[[1]]$sav_sha256, .bg_sha256(fixture$path))
  expect_no_error(jsonlite::toJSON(out$provenance, auto_unbox = TRUE))
})

test_that("background attachment diagnostics exclude person values and create no output files", {
  fixture <- .bg_fixture()
  persons <- c(3141592653589, 2718281828459)
  .bg_write(fixture$directory, .bg_source(person = persons))
  data <- data.frame(nomem_encr = c(persons, 1618033988749), fieldwork_ym = rep(202301, 3))
  before <- tools::md5sum(fixture$path)
  inventory <- list.files(fixture$directory, all.files = TRUE, recursive = TRUE)
  capture <- .bg_capture(.bg_attach(fixture, data = data))
  diagnostics <- paste(capture.output(str(list(audit = capture$value$audit,
    provenance = capture$value$provenance, warnings = capture$warnings))), collapse = "\n")
  for (person in c(persons, 1618033988749))
    expect_false(grepl(format(person, scientific = FALSE), diagnostics, fixed = TRUE))
  expect_identical(capture$value$data$nomem_encr, data$nomem_encr)
  expect_identical(tools::md5sum(fixture$path), before)
  expect_identical(list.files(fixture$directory, all.files = TRUE, recursive = TRUE), inventory)
  bad <- data
  bad$nomem_encr[[1]] <- -3141592653589
  condition <- tryCatch(.bg_attach(fixture, data = bad), error = identity)
  expect_s3_class(condition, "error")
  expect_false(grepl("3141592653589", paste(capture.output(str(condition)), collapse = "\n"),
                     fixed = TRUE))
})

test_that("background attachment keeps identifier-valued covariate labels out of provenance", {
  fixture <- .bg_fixture()
  persons <- c(3141592653589, 2718281828459)
  households <- c(1618033988749, 1414213562373)
  source <- .bg_source(person = persons)
  source$nohouse_encr <- haven::labelled(households,
    labels = stats::setNames(households, c("First household", "Second household")),
    label = "Household key")
  source$record_code <- haven::labelled(persons,
    labels = stats::setNames(persons, c("First category", "Second category")),
    label = "Category code")
  .bg_write(fixture$directory, source)
  data <- data.frame(nomem_encr = rev(persons), fieldwork_ym = c(202301, 202301))
  out <- .bg_attach(fixture, data = data, variables = c("nohouse_encr", "record_code"))
  saved <- haven::read_sav(fixture$path, user_na = TRUE)
  for (variable in c("nohouse_encr", "record_code")) {
    expect_identical(as.numeric(out$data[[paste0("avars_", variable)]]),
                     rev(as.numeric(saved[[variable]])))
    expect_identical(attributes(out$data[[paste0("avars_", variable)]]),
                     attributes(saved[[variable]]))
    receipt <- out$provenance$selected_metadata[[variable]]
    expect_named(receipt, c("type", "class", "attribute_names", "metadata_sha256",
                            "hash_algorithm", "serialization"))
    expect_identical(receipt$type, typeof(saved[[variable]]))
    expect_identical(receipt$class, class(saved[[variable]]))
    expect_setequal(receipt$attribute_names, names(attributes(saved[[variable]])))
    expect_type(receipt$metadata_sha256, "character")
    expect_false(is.object(receipt$metadata_sha256))
    expect_match(receipt$metadata_sha256, "^[0-9a-f]{64}$")
    expect_identical(receipt$hash_algorithm, "SHA256")
    expect_identical(receipt$serialization, list(format = "R serialize", version = 2L))
  }
  diagnostics <- jsonlite::toJSON(list(audit = out$audit, provenance = out$provenance),
                                  auto_unbox = TRUE, digits = NA)
  for (identifier in c(persons, households))
    expect_false(grepl(format(identifier, scientific = FALSE), diagnostics, fixed = TRUE))
  expect_false(grepl("First household", diagnostics, fixed = TRUE))
  expect_false(grepl("First category", diagnostics, fixed = TRUE))
})

test_that("background attachment removes incidental argument attributes from invocation provenance", {
  fixture <- .bg_fixture()
  markers <- c("arg_month_identifier_3141592653589", "arg_variables_identifier_2718281828459",
               "arg_prefix_identifier_1618033988749")
  month <- structure("fieldwork_ym", names = markers[[1]], caller_detail = markers[[1]])
  variables <- structure("age", names = markers[[2]], caller_detail = markers[[2]])
  prefix <- structure("avars_", names = markers[[3]], caller_detail = markers[[3]])
  out <- lissr::liss_attach_background(fixture$data, fixture$sources,
    month_col = month, variables = variables, prefix = prefix)
  expect_identical(out$provenance$invocation$month_col, "fieldwork_ym")
  expect_identical(out$provenance$invocation$variables, "age")
  expect_identical(out$provenance$invocation$prefix, "avars_")
  expect_identical(as.numeric(out$data$avars_age), c(32, 31))
  diagnostics <- jsonlite::toJSON(list(audit = out$audit, provenance = out$provenance),
                                  auto_unbox = TRUE, digits = NA)
  for (marker in markers) expect_false(grepl(marker, diagnostics, fixed = TRUE))
})

test_that("background attachment metadata fingerprints preserve canonical label-map semantics", {
  fixture <- .bg_two()
  source <- .bg_source()
  source$age <- haven::labelled(c(31, 32),
    labels = c("Thirty one" = 31, "Thirty two" = 32), label = "Age in years")
  .bg_write(fixture$directory, source)
  source$wave <- c(202302, 202302)
  attr(source$age, "labels") <- c("Thirty two" = 32, "Thirty one" = 31)
  .bg_write(fixture$directory, source, basename(fixture$sources$sav_path[[2]]))
  first <- .bg_attach(fixture,
    data = data.frame(nomem_encr = 1, fieldwork_ym = 202301),
    sources = fixture$sources[1, , drop = FALSE])
  second <- .bg_attach(fixture,
    data = data.frame(nomem_encr = 1, fieldwork_ym = 202302),
    sources = fixture$sources[2, , drop = FALSE])
  expect_identical(first$provenance$selected_metadata$age,
                   second$provenance$selected_metadata$age)
  expect_no_error(.bg_attach(fixture))
  attr(source$age, "labels") <- c("Changed wording" = 32, "Thirty one" = 31)
  .bg_write(fixture$directory, source, basename(fixture$sources$sav_path[[2]]))
  changed <- .bg_attach(fixture,
    data = data.frame(nomem_encr = 1, fieldwork_ym = 202302),
    sources = fixture$sources[2, , drop = FALSE])
  expect_false(identical(first$provenance$selected_metadata$age$metadata_sha256,
                         changed$provenance$selected_metadata$age$metadata_sha256))
  condition <- tryCatch(.bg_attach(fixture), error = identity)
  expect_s3_class(condition, "liss_background_error")
  expect_identical(condition$code, "incompatible_selected_metadata")
})
