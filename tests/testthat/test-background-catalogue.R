.background_cache_reset <- function(.local_envir = parent.frame()) {
  cache <- lissr:::.liss_cache
  saved <- as.list(cache, all.names = TRUE)
  rm(list = ls(cache, all.names = TRUE), envir = cache)
  withr::defer({
    rm(list = ls(cache, all.names = TRUE), envir = cache)
    list2env(saved, envir = cache)
  }, envir = .local_envir)
  cache
}

.background_archive_fixture <- function(extra = "") {
  row <- function(description, file, id) {
    paste0("<div class='row'>", description,
           " <a href='/hosted-files/download/", id, "'>", file,
           "</a></div>")
  }
  list(
    `1` = xml2::read_html(paste0(
      "<div id='id1'><div class='card-body'>",
      "<a href='/study-units/view/322'>1 Background Variables</a></div></div>",
      "<div id='id2'><div class='card-body'>",
      "<a href='/study-units/view/10'>2 Health</a></div></div>")),
    `10` = xml2::read_html(paste0(
      "<div id='id_mes'><a href='/study-units/view/101'>Wave 1</a>",
      "<a href='/study-units/view/102'>Wave 2</a></div>")),
    `101` = xml2::read_html(paste0("<div id='id_dd'>",
      row("SPSS - English", "ch25s_EN_1.0p.sav", 11),
      row("Stata - English", "ch25s_EN_1.0p.dta", 12),
      row("Codebook - English", "ch25s_codebook_EN_1.0.pdf", 13),
      row("Codebook - Dutch", "ch25s_codebook_NL_1.0.pdf", 14), "</div>")),
    `102` = xml2::read_html(paste0("<div id='id_dd'>",
      row("SPSS - English", "ch26t_EN_1.0p.sav", 21), "</div>")),
    `322` = xml2::read_html(paste0("<div id='id_dd'>",
      row("SPSS - English avars_202412_EN_1.0p.zip, recording note",
          "avars_202412_EN_1.0p.zip", 31),
      row("SPSS - English", "avars_202501_EN_1.1p.zip", 32),
      row("SPSS - Dutch", "avars_202501_NL_1_0p.ZIP", 33),
      row("Codebook - English", "codebook_BackgroundVariables_EN_9.4.pdf", 34),
      row("Income imputation", "imputation income LISS from sept 2011.pdf", 35),
      row("Codebook - Dutch", "codeboek_AchtergrondVariabelen_NL_9.4.pdf", 36),
      extra, "</div>"))
  )
}

.background_fixture_reader <- function(pages, seen = NULL) {
  force(pages)
  function(url) {
    if (!is.null(seen)) seen$urls <- c(seen$urls, url)
    id <- sub("^.*/study-units/view/", "", url)
    page <- pages[[id]]
    if (is.null(page)) stop("unmocked archive page: ", url)
    page
  }
}

.background_cached_fixture <- function() {
  cache <- .background_cache_reset(.local_envir = parent.frame())
  pages <- .background_archive_fixture()
  testthat::local_mocked_bindings(
    .liss_archive_html = .background_fixture_reader(pages),
    .env = parent.frame()
  )
  suppressMessages(lissr::liss_blueprint(refresh = TRUE))
}

test_that("the flat background catalogue preserves monthly and core identities", {
  cache <- .background_cache_reset()
  pages <- .background_archive_fixture()
  seen <- new.env(parent = emptyenv())
  seen$urls <- character()
  testthat::local_mocked_bindings(
    .liss_archive_html = .background_fixture_reader(pages, seen)
  )
  bp <- suppressMessages(lissr::liss_blueprint(refresh = TRUE))
  expect_identical(names(bp), c("module", "module_id", "wave", "wave_id",
                                "type", "name", "file", "path"))
  expect_identical(nrow(bp), 11L)
  expect_type(bp$module_id, "integer")
  expect_type(bp$wave, "integer")
  expect_type(bp$wave_id, "integer")
  expect_identical(bp, cache$blueprint)
  expect_setequal(seen$urls, paste0("https://www.dataarchive.lissdata.nl",
                                  "/study-units/view/", c(1, 10, 101, 102, 322)))

  core <- bp[bp$module_id == 10L, ]
  expect_identical(core$wave, c(1L, 1L, 1L, 1L, 2L))
  expect_identical(core$wave_id, c(101L, 101L, 101L, 101L, 102L))
  expect_identical(core$type, c("codebook", "codebook", "spss", "stata", "spss"))
  expect_identical(core$file, c("ch25s_codebook_EN_1.0.pdf", "ch25s_codebook_NL_1.0.pdf",
                                "ch25s_EN_1.0p.sav",
                                "ch25s_EN_1.0p.dta", "ch26t_EN_1.0p.sav"))

  background <- bp[bp$module_id == 322L, ]
  expect_true(all(background$module == "Background Variables"))
  expect_true(all(background$wave_id == 322L))
  archives <- background[background$type == "archive", ]
  expect_identical(archives$wave, c(202412L, 202501L, 202501L))
  expect_setequal(archives$file, c("avars_202412_EN_1.0p.zip",
                                  "avars_202501_EN_1.1p.zip",
                                  "avars_202501_NL_1_0p.ZIP"))
  expect_setequal(archives$path, paste0("/hosted-files/download/", 31:33))
  expect_identical(archives$name[archives$wave == 202412L],
                   "SPSS - English avars_202412_EN_1.0p.zip, recording note")
  documents <- background[background$type == "codebook", ]
  expect_true(all(is.na(documents$wave)))
  expect_setequal(documents$file, c("codebook_BackgroundVariables_EN_9.4.pdf",
                                   "imputation income LISS from sept 2011.pdf",
                                   "codeboek_AchtergrondVariabelen_NL_9.4.pdf"))

  before <- seen$urls
  expect_identical(suppressMessages(lissr::liss_blueprint()), bp)
  expect_identical(seen$urls, before)
  hosted <- lissr:::get_hosted_files()
  expect_identical(hosted$file, c("ch25s_EN_1.0p.sav", "ch26t_EN_1.0p.sav"))
  expect_identical(nrow(lissr:::get_hosted_files(.modules = 322L)), 0L)
  explicit <- lissr:::get_hosted_files(.modules = 322L, .types = "[.]zip$")
  expect_setequal(explicit$file, archives$file)
})

test_that("malformed monthly archive names cannot become inferred dates", {
  .background_cache_reset()
  extra <- paste0(
    "<div class='row'><a href='/hosted-files/download/91'>",
    "avars_202413_EN_1.0p.zip</a></div>",
    "<div class='row'><a href='/hosted-files/download/92'>",
    "avars_000001_EN_1.0p.zip</a></div>",
    "<div class='row'><a href='/hosted-files/download/93'>",
    "avars_20251_EN_1.0p.zip</a></div>",
    "<div class='row'><a href='/hosted-files/download/94'>",
    "notes.zip</a></div>")
  testthat::local_mocked_bindings(
    .liss_archive_html = .background_fixture_reader(.background_archive_fixture(extra))
  )
  expect_warning(bp <- suppressMessages(lissr::liss_blueprint(refresh = TRUE)),
                 "[Bb]ackground|[Uu]nrecogn|[Ii]nvalid")
  expect_identical(nrow(bp[bp$type == "archive", ]), 3L)
  expect_false(any(bp$file %in% c("avars_202413_EN_1.0p.zip",
                                  "avars_000001_EN_1.0p.zip",
                                  "avars_20251_EN_1.0p.zip", "notes.zip")))
})

test_that("catalogue refresh failures preserve the old cache and disclose partial scans", {
  cache <- .background_cache_reset()
  old <- tibble::tibble(module = "old inventory", wave = 1L)
  cache$blueprint <- old
  cache$timestamp <- "old timestamp"
  testthat::local_mocked_bindings(.liss_archive_html = function(url) stop("offline"))
  expect_error(suppressMessages(lissr::liss_blueprint(refresh = TRUE)),
               "module index")
  expect_identical(cache$blueprint, old)
  expect_identical(cache$timestamp, "old timestamp")

  pages <- .background_archive_fixture()
  reader <- .background_fixture_reader(pages)
  testthat::local_mocked_bindings(.liss_archive_html = function(url) {
    if (endsWith(url, "/322")) stop("offline background page")
    reader(url)
  })
  expect_warning(bp <- suppressMessages(lissr::liss_blueprint(refresh = TRUE)),
                 "incomplete")
  expect_identical(nrow(bp), 5L)
  expect_true(all(bp$module_id == 10L))
  expect_identical(bp, cache$blueprint)
})

test_that("a missing flat background layout is disclosed and an empty scan keeps its cache", {
  cache <- .background_cache_reset()
  pages <- .background_archive_fixture()
  pages[["322"]] <- xml2::read_html("<div>No dataset section</div>")
  testthat::local_mocked_bindings(.liss_archive_html = .background_fixture_reader(pages))
  expect_warning(bp <- suppressMessages(lissr::liss_blueprint(refresh = TRUE)),
                 "[Bb]ackground")
  expect_identical(nrow(bp), 5L)
  expect_true(all(bp$module_id == 10L))

  pages[["1"]] <- xml2::read_html(paste0(
    "<div id='id1'><div class='card-body'>",
    "<a href='/study-units/view/322'>1 Background Variables</a></div></div>",
    "<div id='id2'><div class='card-body'></div></div>"))
  testthat::local_mocked_bindings(.liss_archive_html = .background_fixture_reader(pages))
  expect_error(expect_warning(
    suppressMessages(lissr::liss_blueprint(refresh = TRUE)), "[Bb]ackground"),
    "empty blueprint")
  expect_identical(cache$blueprint, bp)
})

test_that("background month input follows calendar ranges without integer gaps", {
  parse <- lissr:::parse_month_input
  expect_identical(parse("202412:202502"), c(202412L, 202501L, 202502L))
  expect_identical(parse("202502:202412"), c(202412L, 202501L, 202502L))
  expect_identical(parse("202501, 202412, 202501"), c(202412L, 202501L))
  expect_identical(parse(""), integer())
  for (bad in c("202413", "202500", "000001", "20251", "1:5", "202412:202513",
                "202501;202502", "202501:202502:202503", "NA", "system('true')"))
    expect_null(parse(bad), info = bad)
})

test_that("mixed interactive selection treats monthly codes separately from core waves", {
  .background_cached_fixture()
  prompts <- character()
  inputs <- c("1", "202412:202501")
  testthat::local_mocked_bindings(
    .liss_select_list = function(choices, multiple = FALSE, title = NULL, ...) {
      if (identical(title, "Select module(s)"))
        return(c("Health", "Background Variables"))
      expect_true(all(c("SPSS (.sav)", "ZIP archives (.zip)") %in% choices))
      c("SPSS (.sav)", "ZIP archives (.zip)")
    },
    .liss_readline = function(prompt = "") {
      prompts <<- c(prompts, prompt)
      answer <- inputs[[length(prompts)]]
      answer
    }
  )
  messages <- testthat::capture_messages(selection <- lissr::liss_select())
  expect_length(prompts, 2L)
  expect_true(any(grepl("month|YYYYMM", prompts, ignore.case = TRUE)))
  expect_false(any(grepl("Some modules are not available", messages, fixed = TRUE)))
  expect_identical(nrow(selection), 4L)
  expect_setequal(selection$file, c("ch25s_EN_1.0p.sav", "avars_202412_EN_1.0p.zip",
                                    "avars_202501_EN_1.1p.zip", "avars_202501_NL_1_0p.ZIP"))
  expect_false(anyNA(selection$wave))
})

test_that("selected background months can carry an explicitly selected unscoped codebook", {
  .background_cached_fixture()
  testthat::local_mocked_bindings(
    .liss_select_list = function(choices, multiple = FALSE, title = NULL, ...) {
      if (identical(title, "Select module(s)")) return("Background Variables")
      c("ZIP archives (.zip)", "Codebook (English)")
    },
    .liss_readline = function(prompt = "") "202501"
  )
  selection <- suppressMessages(lissr::liss_select())
  expect_identical(nrow(selection), 3L)
  expect_setequal(selection$file, c("avars_202501_EN_1.1p.zip",
                                    "avars_202501_NL_1_0p.ZIP",
                                    "codebook_BackgroundVariables_EN_9.4.pdf"))
  expect_false("codeboek_AchtergrondVariabelen_NL_9.4.pdf" %in% selection$file)
  expect_true(all(selection$wave[selection$type == "archive"] == 202501L))
  expect_true(all(is.na(selection$wave[selection$type == "codebook"])))
})

test_that("a core-only selection retains its original file menu and wave prompt", {
  .background_cached_fixture()
  prompts <- character()
  testthat::local_mocked_bindings(
    .liss_select_list = function(choices, multiple = FALSE, title = NULL, ...) {
      if (identical(title, "Select module(s)")) return("Health")
      expect_identical(choices, c("SPSS (.sav)", "Stata (.dta)",
                                  "Codebook (English)", "Codebook (Dutch)"))
      "SPSS (.sav)"
    },
    .liss_readline = function(prompt = "") {
      prompts <<- c(prompts, prompt)
      "'all'"
    }
  )
  selection <- suppressMessages(lissr::liss_select())
  expect_identical(prompts, "Enter waves (e.g., 1:5, 1,3,7, or 'all'): ")
  expect_identical(selection$file, c("ch25s_EN_1.0p.sav", "ch26t_EN_1.0p.sav"))
  expect_identical(selection$wave, c(1L, 2L))

  cache <- lissr:::.liss_cache
  cache$blueprint$file[cache$blueprint$file == "ch25s_codebook_EN_1.0.pdf"] <- "ch25s_EN.pdf"
  cache$blueprint$file[cache$blueprint$file == "ch25s_codebook_NL_1.0.pdf"] <- "ch25s_NL.pdf"
  testthat::local_mocked_bindings(
    .liss_select_list = function(choices, multiple = FALSE, title = NULL, ...) {
      if (identical(title, "Select module(s)")) return("Health")
      "Codebook (English)"
    }
  )
  english <- suppressMessages(lissr::liss_select())
  expect_identical(english$file, "ch25s_EN.pdf")

  cache$blueprint$file[cache$blueprint$file == "ch25s_EN.pdf"] <- "legacyEN.pdf"
  legacy_english <- suppressMessages(lissr::liss_select())
  expect_identical(legacy_english$file, "legacyEN.pdf")
})

test_that("a background page with documents only does not require a fictitious month", {
  .background_cache_reset()
  pages <- .background_archive_fixture()
  pages[["322"]] <- xml2::read_html(paste0(
    "<div id='id_dd'><div class='row'>Income imputation ",
    "<a href='/hosted-files/download/35'>",
    "imputation income LISS from sept 2011.pdf</a></div></div>"))
  testthat::local_mocked_bindings(
    .liss_archive_html = .background_fixture_reader(pages),
    .liss_select_list = function(choices, multiple = FALSE, title = NULL, ...) {
      if (identical(title, "Select module(s)")) return("Background Variables")
      expect_true("Documents (.pdf)" %in% choices)
      "Documents (.pdf)"
    },
    .liss_readline = function(prompt = "") stop("an undated document has no month prompt")
  )
  expect_warning(selection <- suppressMessages(lissr::liss_select()),
                 "No dated Background Variables ZIP releases")
  expect_identical(nrow(selection), 1L)
  expect_identical(selection$file, "imputation income LISS from sept 2011.pdf")
  expect_identical(selection$type, "codebook")
  expect_true(is.na(selection$wave))
})

test_that("cached and uncached background counts use months rather than PDF documents", {
  bp <- .background_cached_fixture()
  overview <- suppressMessages(lissr::liss_modules(.details = TRUE))
  background <- overview[overview$module_id == 322L, ]
  expect_identical(background$waves, 2L)
  expect_identical(background$files, 6L)
  expect_equal(background$archives, 3)
  expect_equal(background$codebooks, 3)
  expect_equal(background$spss, 0)
  core <- overview[overview$module_id == 10L, ]
  expect_identical(core$waves, 2L)
  expect_identical(core$files, 5L)
  expect_equal(core$archives, 0)

  rm(list = c("blueprint", "timestamp"), envir = lissr:::.liss_cache)
  uncached <- suppressMessages(lissr::liss_modules(.details = TRUE))
  expect_identical(uncached$waves[uncached$module_id == 322L], 2L)
  expect_identical(uncached$waves[uncached$module_id == 10L], 2L)
  expect_identical(bp$module_id[bp$type == "archive"], rep(322L, 3L))
})

test_that("the core wave matrix excludes background month codes and unscoped documents", {
  .background_cached_fixture()
  displayed <- utils::capture.output(out <- suppressMessages(lissr::liss_wave_matrix()))
  expect_identical(rownames(out), "Health")
  expect_identical(names(out), c("w1", "w2"))
  expect_identical(unname(as.matrix(out)), matrix(rep("\u00d7", 2L), nrow = 1L))
  expect_false(any(grepl("202412|202501|wNA", displayed)))
})

test_that("an explicitly selected monthly ZIP uses the existing authenticated downloader", {
  .background_cached_fixture()
  cache <- lissr:::.liss_cache
  cache$session <- structure(list(handle = NULL, config = NULL), class = "rvest_session")
  selection <- cache$blueprint[cache$blueprint$file == "avars_202412_EN_1.0p.zip", ]
  seen_url <- character()
  dir <- withr::local_tempdir("lissr_background_download_")
  testthat::local_mocked_bindings(
    .liss_fetch_file = function(session, url, dest, timeout_s) {
      seen_url <<- c(seen_url, url)
      writeBin(charToRaw("synthetic monthly archive"), dest)
      list(url = url, status_code = 200L, headers = list())
    }
  )
  result <- suppressMessages(lissr::liss_download(selection, .dir = dir,
                                                 .waves = 202412L, .unzip = FALSE))
  expect_identical(result$status, "ok")
  expect_identical(result$file, "avars_202412_EN_1.0p.zip")
  expect_identical(seen_url,
                   "https://www.dataarchive.lissdata.nl/hosted-files/download/31")
  expect_identical(readChar(file.path(dir, result$file), 25L),
                   "synthetic monthly archive")
  before <- seen_url
  skipped <- suppressMessages(lissr::liss_download(selection, .dir = dir,
                                                  .unzip = FALSE, .skip_existing = TRUE))
  expect_identical(skipped$status, "skipped_existing")
  expect_identical(seen_url, before)
})

test_that("an explicitly selected monthly ZIP extracts an unchanged synthetic SPSS file", {
  skip_if(Sys.which("zip") == "", "zip utility unavailable")
  .background_cached_fixture()
  cache <- lissr:::.liss_cache
  cache$session <- structure(list(handle = NULL, config = NULL), class = "rvest_session")
  selection <- cache$blueprint[cache$blueprint$file == "avars_202412_EN_1.0p.zip", ]
  source_dir <- withr::local_tempdir("lissr_background_source_")
  output_dir <- withr::local_tempdir("lissr_background_extract_")
  sav_name <- "avars_202412_EN_1.0p.sav"
  sav_path <- file.path(source_dir, sav_name)
  haven::write_sav(data.frame(nomem_encr = 1:2, wave = rep(202412L, 2L),
                              leeftijd = c(39L, 40L)), sav_path)
  expected <- haven::read_sav(sav_path, user_na = TRUE)
  expected_hash <- unname(tools::md5sum(sav_path))
  zip_path <- file.path(source_dir, selection$file)
  withr::with_dir(source_dir, utils::zip(zip_path, files = sav_name, flags = "-q"))
  expect_true(file.exists(zip_path))
  seen <- character()
  testthat::local_mocked_bindings(
    .liss_fetch_file = function(session, url, dest, timeout_s) {
      seen <<- c(seen, url)
      stopifnot(file.copy(zip_path, dest))
      list(url = url, status_code = 200L, headers = list())
    }
  )
  result <- suppressMessages(lissr::liss_download(selection, .dir = output_dir,
                                                 .waves = 202412L, .unzip = TRUE))
  expect_identical(result$status, "ok")
  expect_identical(result$file, selection$file)
  expect_identical(seen,
                   "https://www.dataarchive.lissdata.nl/hosted-files/download/31")
  expect_identical(list.files(output_dir), sav_name)
  output_path <- file.path(output_dir, sav_name)
  expect_identical(unname(tools::md5sum(output_path)), expected_hash)
  expect_identical(haven::read_sav(output_path, user_na = TRUE), expected)
  expect_true(file.exists(zip_path))
  expect_identical(unname(tools::md5sum(sav_path)), expected_hash)
})
