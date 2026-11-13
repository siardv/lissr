# use a private function environment to inject filesystem outcomes without
# changing base bindings, package bindings, sessions, or production helpers.
.finalization_case <- function(header = "received.sav", listed = "listed.sav",
                               payload = charToRaw("invented new bytes"),
                               rename = base::file.rename, unzip = FALSE,
                               skip_existing = FALSE, setup = function(path) NULL,
                               inspect = function(path) NULL, n = 1L,
                               download = lissr::liss_download) {
  root <- tempfile("lissr_finalization_")
  stopifnot(dir.create(root))
  on.exit(unlink(root, recursive = TRUE, expand = FALSE), add = TRUE)
  setup(root)
  state <- new.env(parent = emptyenv())
  state$fetch_calls <- 0L
  state$rename_calls <- 0L
  state$unzip_calls <- 0L
  state$warnings <- character()
  scope <- new.env(parent = environment(download))
  scope$.liss_get_session <- function() list(handle = NULL, config = NULL)
  scope$.liss_fetch_file <- function(session, url, dest, timeout_s) {
    state$fetch_calls <- state$fetch_calls + 1L
    writeBin(payload, dest)
    list(url = url, status_code = 200L,
      headers = if (is.null(header)) list() else
        list(`content-disposition` = paste0('attachment; filename="', header, '"')))
  }
  scope$file.rename <- function(from, to) {
    state$rename_calls <- state$rename_calls + 1L
    rename(from, to)
  }
  scope$.verify_unzip <- function(zip_path, exdir) {
    state$unzip_calls <- state$unzip_calls + 1L
    lissr:::.verify_unzip(zip_path, exdir)
  }
  environment(download) <- scope
  selected <- tibble::tibble(module = rep("Invented", n), module_id = rep(1L, n),
    wave = seq_len(n), file = rep(listed, n), path = paste0("/invented/offline/", seq_len(n)))
  result <- withCallingHandlers(
    withVisible(suppressMessages(download(selected, .dir = root, .unzip = unzip,
      .skip_existing = skip_existing, .retries = 2L))),
    warning = function(condition) {
      state$warnings <- c(state$warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    })
  list(result = result$value, visible = result$visible,
    fetch_calls = state$fetch_calls, rename_calls = state$rename_calls,
    unzip_calls = state$unzip_calls, warnings = state$warnings,
    files = sort(list.files(root, all.files = TRUE, no.. = TRUE)),
    inspection = inspect(root))
}

.finalization_read <- function(path) readBin(path, "raw", n = file.info(path)$size)

# fixed invented ZIP bytes, generated with stored compression and no SAV data.
.finalization_zip <- function() {
  openssl::base64_decode(paste0(
    "UEsDBBQAAAAAAAAAIQA4GYJmFgAAABYAAAAUAAAAaW52ZW50ZWRfcGF5bG9hZC50eHRpbnZlbnRlZCBhcmNoaXZlIGJ5dGVzUEsBAhQDFAAAAAAAAAAhADgZgmYWAAAAFgAAABQAAAAAAAAAAAAAAICBAAAAAGludmVudGVkX3BheWxvYWQudHh0UEsFBgAAAAABAAEAQgAAAEgAAAAAAA=="))
}

test_that("successful finalization retains header naming, bytes and invisible result shape", {
  out <- .finalization_case(inspect = function(path)
    .finalization_read(file.path(path, "received.sav")))
  expect_identical(out$result$file, "received.sav")
  expect_identical(out$result$status, "ok")
  expect_identical(names(out$result), c("file", "status"))
  expect_s3_class(out$result, "tbl_df")
  expect_false(out$visible)
  expect_identical(out$inspection, charToRaw("invented new bytes"))
  expect_identical(out$files, "received.sav")
  expect_identical(out$fetch_calls, 1L)
  expect_identical(out$rename_calls, 1L)
  expect_identical(out$unzip_calls, 0L)
})

test_that("successful finalization retains the listed-name fallback", {
  out <- .finalization_case(header = NULL)
  expect_identical(out$result$file, "listed.sav")
  expect_identical(out$result$status, "ok")
  expect_identical(out$files, "listed.sav")
})

test_that("a false rename result uses the existing error contract and removes the partial", {
  out <- .finalization_case(rename = function(from, to) FALSE)
  expect_identical(out$result$file, "listed.sav")
  expect_identical(out$result$status, "error: cannot finalize downloaded file")
  expect_identical(out$files, character())
  expect_identical(out$fetch_calls, 1L)
  expect_identical(out$rename_calls, 1L)
  expect_identical(out$unzip_calls, 0L)
})

test_that("failed ZIP finalization never enters extraction or claims archive retention", {
  out <- .finalization_case(header = "received.zip", payload = .finalization_zip(),
    rename = function(from, to) FALSE, unzip = TRUE)
  expect_identical(out$result$file, "listed.sav")
  expect_identical(out$result$status, "error: cannot finalize downloaded file")
  expect_identical(out$unzip_calls, 0L)
  expect_identical(out$files, character())
})

test_that("an actual destination directory conflict reports failure and preserves its contents", {
  out <- .finalization_case(setup = function(path) {
    dir.create(file.path(path, "received.sav"))
    writeBin(charToRaw("invented marker"), file.path(path, "received.sav/marker"))
  }, inspect = function(path) .finalization_read(file.path(path, "received.sav/marker")))
  expect_identical(out$result$file, "listed.sav")
  expect_match(out$result$status, "^error: ")
  expect_identical(out$inspection, charToRaw("invented marker"))
  expect_identical(out$files, "received.sav")
  expect_identical(out$rename_calls, 1L)
  expect_identical(out$unzip_calls, 0L)
})

test_that("successful rename retains ordinary existing-file replacement", {
  out <- .finalization_case(setup = function(path)
    writeBin(charToRaw("invented old bytes"), file.path(path, "received.sav")),
    inspect = function(path) .finalization_read(file.path(path, "received.sav")))
  expect_identical(out$result$status, "ok")
  expect_identical(out$inspection, charToRaw("invented new bytes"))
  expect_identical(out$files, "received.sav")
})

test_that("failed rename leaves an existing regular destination unchanged", {
  out <- .finalization_case(rename = function(from, to) FALSE,
    setup = function(path) writeBin(charToRaw("invented old bytes"), file.path(path, "received.sav")),
    inspect = function(path) .finalization_read(file.path(path, "received.sav")))
  expect_identical(out$result$status, "error: cannot finalize downloaded file")
  expect_identical(out$inspection, charToRaw("invented old bytes"))
  expect_identical(out$files, "received.sav")
})

test_that("failed finalization neither extracts nor deletes a stale valid destination ZIP", {
  out <- .finalization_case(header = "received.zip", payload = charToRaw("different new bytes"),
    rename = function(from, to) FALSE, unzip = TRUE,
    setup = function(path) writeBin(.finalization_zip(), file.path(path, "received.zip")),
    inspect = function(path) .finalization_read(file.path(path, "received.zip")))
  expect_identical(out$result$status, "error: cannot finalize downloaded file")
  expect_identical(out$result$file, "listed.sav")
  expect_identical(out$unzip_calls, 0L)
  expect_identical(out$inspection, .finalization_zip())
  expect_identical(out$files, "received.zip")
})

test_that("successful ZIP finalization still extracts and removes the verified archive", {
  out <- .finalization_case(header = "received.zip", payload = .finalization_zip(), unzip = TRUE,
    inspect = function(path) .finalization_read(file.path(path, "invented_payload.txt")))
  expect_identical(out$result$status, "ok")
  expect_identical(out$result$file, "received.zip")
  expect_identical(out$unzip_calls, 1L)
  expect_identical(out$files, "invented_payload.txt")
  expect_identical(out$inspection, charToRaw("invented archive bytes"))
})

test_that("failed extraction after successful finalization still keeps the archive", {
  bytes <- charToRaw("invented corrupt ZIP")
  out <- .finalization_case(header = "received.zip", payload = bytes, unzip = TRUE,
    inspect = function(path) .finalization_read(file.path(path, "received.zip")))
  expect_identical(out$result$status, "unzip_failed_archive_kept")
  expect_identical(out$result$file, "received.zip")
  expect_identical(out$unzip_calls, 1L)
  expect_identical(out$inspection, bytes)
  expect_identical(out$files, "received.zip")
})

test_that("a ZIP destination directory conflict never enters extraction", {
  out <- .finalization_case(header = "received.zip", payload = .finalization_zip(), unzip = TRUE,
    setup = function(path) dir.create(file.path(path, "received.zip")),
    inspect = function(path) dir.exists(file.path(path, "received.zip")))
  expect_match(out$result$status, "^error: ")
  expect_true(out$inspection)
  expect_identical(out$files, "received.zip")
  expect_identical(out$unzip_calls, 0L)
})

test_that("finalization failure continues the batch without retrying the completed fetch", {
  calls <- 0L
  out <- .finalization_case(n = 2L, rename = function(from, to) {
    calls <<- calls + 1L
    if (calls == 1L) FALSE else base::file.rename(from, to)
  })
  expect_identical(out$result$status, c("error: cannot finalize downloaded file", "ok"))
  expect_identical(out$result$file, c("listed.sav", "received.sav"))
  expect_identical(out$fetch_calls, 2L)
  expect_identical(out$rename_calls, 2L)
  expect_identical(out$files, "received.sav")
})

test_that("a thrown rename error retains the pre-existing cleanup and result contract", {
  out <- .finalization_case(rename = function(from, to) stop("invented rename error"))
  expect_identical(out$result$file, "listed.sav")
  expect_identical(out$result$status, "error: invented rename error")
  expect_identical(out$files, character())
  expect_identical(out$unzip_calls, 0L)
})

test_that("a warning followed by a false rename result cannot report success", {
  out <- .finalization_case(rename = function(from, to) {
    warning("invented rename warning")
    FALSE
  })
  expect_identical(out$result$status, "error: cannot finalize downloaded file")
  expect_identical(out$warnings, "invented rename warning")
  expect_identical(out$files, character())
})

test_that("a non-TRUE rename result cannot enter the success branch", {
  out <- .finalization_case(rename = function(from, to) NA)
  expect_identical(out$result$status, "error: cannot finalize downloaded file")
  expect_identical(out$files, character())
})

test_that("skip_existing retains listed-name lookup and avoids finalization", {
  out <- .finalization_case(skip_existing = TRUE, setup = function(path)
    writeBin(charToRaw("invented listed bytes"), file.path(path, "listed.sav")),
    inspect = function(path) .finalization_read(file.path(path, "listed.sav")))
  expect_identical(out$result$status, "skipped_existing")
  expect_identical(out$result$file, "listed.sav")
  expect_identical(out$fetch_calls, 0L)
  expect_identical(out$rename_calls, 0L)
  expect_identical(out$inspection, charToRaw("invented listed bytes"))
  expect_identical(out$files, "listed.sav")
})
