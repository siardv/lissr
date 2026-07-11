# ============================================================================
# stage 7 (v1.4 development): offline tests for the hardened network layer
# ============================================================================
# the two I/O shims (.liss_fetch_file, .liss_head, .liss_jump) are mocked so
# every download and auth error path runs without a network: HTTP errors,
# session expiry with batch abort, corrupt-zip retention, skip-existing,
# content-disposition parsing, filename sanitation, retry logic, the 2FA
# guards, and the RNG-preserving login probe.

# ---- pure helpers: content-disposition and filename sanitation --------------

test_that("parse_content_disposition handles quoted, unquoted, and RFC 5987 forms", {
  pcd <- lissr:::parse_content_disposition
  expect_identical(pcd('attachment; filename="cf08a_EN_1.0p.sav"', "fb.sav"),
                   "cf08a_EN_1.0p.sav")
  # the unquoted token form was previously mangled into the whole header
  expect_identical(pcd("attachment; filename=cf08a_EN_1.0p.sav", "fb.sav"),
                   "cf08a_EN_1.0p.sav")
  expect_identical(pcd("attachment; filename*=UTF-8''cf08a%20v2.sav", "fb.sav"),
                   "cf08a v2.sav")
  # RFC 5987 takes precedence over a plain filename in the same header
  expect_identical(pcd('attachment; filename="a.sav"; filename*=utf-8\'\'b.sav',
                       "fb.sav"),
                   "b.sav")
  expect_identical(pcd(NULL, "fb.sav"), "fb.sav")
  expect_identical(pcd("attachment", "fb.sav"), "fb.sav")
})

test_that("sanitize_filename strips paths, traversal, and reserved characters", {
  sf <- lissr:::sanitize_filename
  expect_identical(sf("../../etc/passwd", "fb.sav"), "passwd")
  expect_identical(sf("C:\\temp\\evil.sav", "fb.sav"), "evil.sav")
  expect_identical(sf("a<b>c.sav", "fb.sav"), "a_b_c.sav")
  expect_identical(sf("..", "fb.sav"), "fb.sav")
  expect_identical(sf("", "fb.sav"), "fb.sav")
  expect_identical(sf(NULL, NULL), "download.bin")
  expect_identical(sf("ok name.sav", "fb.sav"), "ok name.sav")
})

# ---- pure helpers: 2FA normalization and retries -----------------------------

test_that(".normalize_2fa_code strips every whitespace character", {
  nz <- lissr:::.normalize_2fa_code
  expect_identical(nz(" 123 456\n"), "123456")
  expect_identical(nz("\t987654 "), "987654")
  expect_identical(nz(NULL), "")
  expect_identical(nz(NA_character_), "")
})

test_that(".with_retries retries curl errors and 5xx, returns 4xx immediately", {
  wr <- lissr:::.with_retries
  calls <- 0L
  flaky <- function() {
    calls <<- calls + 1L
    if (calls < 3L) stop("connection reset")
    list(status_code = 200L)
  }
  res <- wr(flaky, attempts = 3, wait = 0)
  expect_identical(res$status_code, 200L)
  expect_identical(calls, 3L)

  calls <- 0L
  always_bad <- function() { calls <<- calls + 1L; stop("timeout") }
  expect_error(wr(always_bad, attempts = 2, wait = 0), "timeout")
  expect_identical(calls, 2L)

  calls <- 0L
  flaky5xx <- function() {
    calls <<- calls + 1L
    if (calls < 2L) list(status_code = 503L) else list(status_code = 200L)
  }
  expect_identical(wr(flaky5xx, attempts = 3, wait = 0)$status_code, 200L)
  expect_identical(calls, 2L)

  calls <- 0L
  not_found <- function() { calls <<- calls + 1L; list(status_code = 404L) }
  expect_identical(wr(not_found, attempts = 3, wait = 0)$status_code, 404L)
  expect_identical(calls, 1L)  # deterministic failures are not retried
})

# ---- pure helpers: verified unzip ---------------------------------------------

test_that(".verify_unzip extracts real archives and rejects corrupt ones", {
  dir <- file.path(tempdir(), "lissr-s7-zip")
  unlink(dir, recursive = TRUE)
  dir.create(dir)

  # a corrupt archive must yield NULL and stay on disk untouched
  bad <- file.path(dir, "corrupt.zip")
  writeBin(as.raw(c(0x50, 0x4b, 0x99, 0x00, 0x01, 0x02)), bad)
  expect_null(lissr:::.verify_unzip(bad, dir))
  expect_true(file.exists(bad))

  skip_if(Sys.which("zip") == "", "zip utility unavailable")
  inner <- file.path(dir, "payload.txt")
  writeLines("hello", inner)
  good <- file.path(dir, "good.zip")
  old_wd <- setwd(dir)
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(good, files = "payload.txt", flags = "-q")
  setwd(old_wd)
  unlink(inner)
  out <- lissr:::.verify_unzip(good, dir)
  expect_false(is.null(out))
  expect_true(file.exists(file.path(dir, "payload.txt")))
  unlink(dir, recursive = TRUE)
})

# ---- download engine, fully mocked ---------------------------------------------

.s7_hosted <- function(n = 1) {
  tibble::tibble(
    module = paste0("Mod", seq_len(n)),
    module_id = seq_len(n),
    wave = seq_len(n),
    file = paste0("w", seq_len(n), ".sav"),
    path = paste0("/file/", seq_len(n))
  )
}

.s7_with_session <- function(code) {
  cache <- lissr:::.liss_cache
  cache$session <- structure(list(handle = NULL, config = NULL),
                             class = "rvest_session")
  withr::defer(rm(list = "session", envir = cache))
  force(code)
}

test_that("a successful download streams to disk under the header-assigned name", {
  skip_if_not_installed("withr")
  dir <- file.path(tempdir(), "lissr-s7-ok")
  unlink(dir, recursive = TRUE)
  .s7_with_session({
    testthat::local_mocked_bindings(
      .liss_fetch_file = function(session, url, dest, timeout_s) {
        writeBin(charToRaw("FAKEDATA"), dest)
        list(url = url, status_code = 200L,
             headers = list(`content-disposition` =
                              'attachment; filename="renamed_w1.sav"'))
      }
    )
    res <- suppressMessages(
      lissr::liss_download(.s7_hosted(1), .dir = dir, .unzip = FALSE))
    expect_identical(res$status, "ok")
    expect_identical(res$file, "renamed_w1.sav")
    expect_true(file.exists(file.path(dir, "renamed_w1.sav")))
    expect_identical(readChar(file.path(dir, "renamed_w1.sav"), 8), "FAKEDATA")
  })
  unlink(dir, recursive = TRUE)
})

test_that("an HTTP error is never written to disk as a data file", {
  skip_if_not_installed("withr")
  dir <- file.path(tempdir(), "lissr-s7-404")
  unlink(dir, recursive = TRUE)
  .s7_with_session({
    testthat::local_mocked_bindings(
      .liss_fetch_file = function(session, url, dest, timeout_s) {
        writeBin(charToRaw("<html>Not Found</html>"), dest)
        list(url = url, status_code = 404L, headers = list())
      }
    )
    res <- suppressMessages(
      lissr::liss_download(.s7_hosted(1), .dir = dir, .unzip = FALSE,
                           .retries = 0))
    expect_identical(res$status, "http_404")
    expect_identical(list.files(dir), character(0))
  })
  unlink(dir, recursive = TRUE)
})

test_that("the first session expiry aborts the remaining batch", {
  skip_if_not_installed("withr")
  dir <- file.path(tempdir(), "lissr-s7-expiry")
  unlink(dir, recursive = TRUE)
  .s7_with_session({
    n_calls <- 0L
    testthat::local_mocked_bindings(
      .liss_fetch_file = function(session, url, dest, timeout_s) {
        n_calls <<- n_calls + 1L
        if (n_calls >= 2L) {
          return(list(url = "https://www.dataarchive.lissdata.nl/login",
                      status_code = 200L, headers = list()))
        }
        writeBin(charToRaw("DATA"), dest)
        list(url = url, status_code = 200L, headers = list())
      }
    )
    res <- suppressMessages(
      lissr::liss_download(.s7_hosted(3), .dir = dir, .unzip = FALSE,
                           .retries = 0))
    expect_identical(res$status,
                     c("ok", "session_expired", "skipped_batch_aborted"))
    expect_identical(n_calls, 2L)  # the third file never hit the network
  })
  unlink(dir, recursive = TRUE)
})

test_that("skip_existing spares present files without a network request", {
  skip_if_not_installed("withr")
  dir <- file.path(tempdir(), "lissr-s7-skip")
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  writeLines("already here", file.path(dir, "w1.sav"))
  .s7_with_session({
    n_calls <- 0L
    testthat::local_mocked_bindings(
      .liss_fetch_file = function(session, url, dest, timeout_s) {
        n_calls <<- n_calls + 1L
        writeBin(charToRaw("DATA"), dest)
        list(url = url, status_code = 200L, headers = list())
      }
    )
    res <- suppressMessages(
      lissr::liss_download(.s7_hosted(2), .dir = dir, .unzip = FALSE,
                           .skip_existing = TRUE, .retries = 0))
    expect_identical(res$status, c("skipped_existing", "ok"))
    expect_identical(n_calls, 1L)
    expect_identical(readLines(file.path(dir, "w1.sav")), "already here")
  })
  unlink(dir, recursive = TRUE)
})

test_that("a corrupt zip keeps the archive instead of deleting the only copy", {
  skip_if_not_installed("withr")
  dir <- file.path(tempdir(), "lissr-s7-zipfail")
  unlink(dir, recursive = TRUE)
  .s7_with_session({
    testthat::local_mocked_bindings(
      .liss_fetch_file = function(session, url, dest, timeout_s) {
        writeBin(as.raw(c(0x50, 0x4b, 0x99, 0x01)), dest)
        list(url = url, status_code = 200L,
             headers = list(`content-disposition` =
                              'attachment; filename="w1.zip"'))
      }
    )
    res <- suppressWarnings(suppressMessages(
      lissr::liss_download(.s7_hosted(1), .dir = dir, .unzip = TRUE,
                           .retries = 0)))
    expect_identical(res$status, "unzip_failed_archive_kept")
    expect_true(file.exists(file.path(dir, "w1.zip")))
  })
  unlink(dir, recursive = TRUE)
})

test_that("a full-archive download is refused non-interactively", {
  skip_if_not_installed("withr")
  .s7_with_session({
    testthat::local_mocked_bindings(
      get_hosted_files = function(.modules = NULL, ...) .s7_hosted(3)
    )
    expect_error(
      suppressMessages(lissr::liss_download(.dir = tempdir())),
      "non-interactively"
    )
  })
})

# ---- 2FA guards -----------------------------------------------------------------

test_that("two-factor verification refuses to run non-interactively", {
  expect_error(
    suppressMessages(lissr:::perform_twofactor_authentication(NULL)),
    "interactive"
  )
})

# ---- login probe: cheap, deterministic, RNG-preserving ---------------------------

test_that("liss_is_logged_in without a session is FALSE and preserves the RNG", {
  set.seed(42)
  seed_before <- .Random.seed
  expect_false(lissr::liss_is_logged_in())
  expect_identical(.Random.seed, seed_before)
})

test_that("the blueprint probe HEADs the first sav path and preserves the RNG", {
  skip_if_not_installed("withr")
  cache <- lissr:::.liss_cache
  cache$session <- structure(list(handle = NULL, config = NULL),
                             class = "rvest_session")
  cache$blueprint <- tibble::tibble(
    type = c("codebook", "spss", "spss"),
    path = c("/file/9", "/file/1", "/file/2")
  )
  withr::defer(rm(list = c("session", "blueprint"), envir = cache))

  seen_url <- NULL
  testthat::local_mocked_bindings(
    .liss_head = function(session, url, timeout_s = 20) {
      seen_url <<- url
      list(url = url, status_code = 200L)
    }
  )
  set.seed(7)
  seed_before <- .Random.seed
  expect_true(lissr::liss_is_logged_in())
  expect_identical(.Random.seed, seed_before)
  # deterministic first spss row, never a random draw
  expect_match(seen_url, "/file/1$")

  testthat::local_mocked_bindings(
    .liss_head = function(session, url, timeout_s = 20) {
      list(url = "https://www.dataarchive.lissdata.nl/login",
           status_code = 200L)
    }
  )
  expect_false(lissr::liss_is_logged_in())
})

test_that("the login-page fallback probe reads the page, not the archive", {
  skip_if_not_installed("withr")
  cache <- lissr:::.liss_cache
  cache$session <- structure(list(handle = NULL, config = NULL),
                             class = "rvest_session")
  withr::defer(rm(list = "session", envir = cache))

  fake_page <- function(body) {
    list(url = "https://www.dataarchive.lissdata.nl/login",
         response = list(content = charToRaw(body)))
  }
  testthat::local_mocked_bindings(
    .liss_jump = function(session, url) {
      fake_page("<form><input type='password' name='pass'></form>")
    }
  )
  expect_false(lissr::liss_is_logged_in())

  testthat::local_mocked_bindings(
    .liss_jump = function(session, url) {
      fake_page("<a href='/user/logout'>Log out</a>")
    }
  )
  expect_true(lissr::liss_is_logged_in())
})
