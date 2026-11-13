# all paths and archive bytes in these tests are invented

test_that("archive staging preserves ordinary files, nested names and empty entries", {
  root <- .archive_test_root()
  on.exit(unlink(root, recursive = TRUE, expand = FALSE), add = TRUE)
  archive <- .archive_fixture_write("ordinary", file.path(root, "archive.zip"))
  output <- file.path(root, "output")
  before <- .archive_read(archive)
  result <- lissr:::.verify_unzip(archive, output)
  expect_identical(result, file.path(output, c("plain.txt", "./nested/space name-v_1.txt", "empty.bin")))
  expect_identical(.archive_read(file.path(output, "plain.txt")), charToRaw("plain"))
  expect_identical(.archive_read(file.path(output, "nested", "space name-v_1.txt")), charToRaw("nested"))
  expect_identical(.archive_read(file.path(output, "empty.bin")), raw())
  expect_true(dir.exists(file.path(output, "empty_dir")))
  expect_identical(.archive_read(archive), before)
  expect_identical(.archive_stage_names(output), character())
})

test_that("archive staging accepts benign relative dot and repeated separators", {
  root <- .archive_test_root()
  on.exit(unlink(root, recursive = TRUE, expand = FALSE), add = TRUE)
  archive <- .archive_fixture_write("dot_paths", file.path(root, "archive.zip"))
  output <- file.path(root, "output")
  expect_identical(lissr:::.verify_unzip(archive, output), file.path(output, "./a//./b.txt"))
  expect_identical(.archive_read(file.path(output, "a", "b.txt")), charToRaw("normalized"))
  expect_identical(.archive_stage_names(output), character())
})

test_that("archive planning rejects unsafe and ambiguous members before any promotion", {
  root <- .archive_test_root()
  on.exit(unlink(root, recursive = TRUE, expand = FALSE), add = TRUE)
  names <- c("parent", "nested_parent", "internal_parent", "absolute", "backslash_parent",
    "unc", "drive_absolute", "drive_relative", "ads", "control", "reserved", "reserved_nested",
    "trailing_dot", "trailing_space", "wildcard", "dot_file", "duplicate", "normalized_duplicate",
    "file_parent", "directory_file", "directory_only", "directory_payload", "corrupt")
  outside <- file.path(root, "invented_outside.txt")
  writeBin(charToRaw("outside unchanged"), outside)
  for (name in names) {
    output <- file.path(root, name)
    dir.create(output)
    first <- file.path(output, "first.txt")
    writeBin(charToRaw("first old"), first)
    archive <- .archive_fixture_write(name, file.path(root, paste0(name, ".zip")))
    before <- .archive_read(archive)
    expect_null(lissr:::.verify_unzip(archive, output), info = name)
    expect_identical(.archive_read(archive), before, info = name)
    expect_identical(.archive_read(first), charToRaw("first old"), info = name)
    expect_identical(.archive_read(outside), charToRaw("outside unchanged"), info = name)
    expect_identical(list.files(output, all.files = TRUE, no.. = TRUE), "first.txt", info = name)
  }
})

test_that("archive planning rejects invalid metadata and file parent conflicts", {
  plan <- lissr:::.liss_zip_plan
  ordinary <- data.frame(Name = "payload.txt", Length = 3, stringsAsFactors = FALSE)
  expect_identical(plan(ordinary)$file_indices, 1L)
  invalid <- list(NULL, data.frame(Name = character(), Length = numeric()),
    data.frame(Name = NA_character_, Length = 3),
    data.frame(Name = "payload.txt", Length = -1),
    data.frame(Name = "payload.txt", Length = Inf),
    data.frame(Name = "payload.txt", Length = 1.5),
    data.frame(Name = "payload.txt", Length = NA_real_),
    data.frame(Name = "payload.txt", Length = "3"),
    data.frame(Name = c("child/file", "child"), Length = c(1, 1)),
    data.frame(Name = c("payload.txt", "./payload.txt"), Length = c(1, 1)))
  for (value in invalid) expect_error(plan(value))
})

test_that("archive staging rejects file aliases on filesystems that alias those names", {
  root <- .archive_test_root()
  on.exit(unlink(root, recursive = TRUE, expand = FALSE), add = TRUE)
  for (name in c("case_alias", "unicode_alias")) {
    output <- file.path(root, name)
    dir.create(output)
    names <- if (name == "case_alias") c("Case.txt", "case.txt") else
      c("caf\u00e9.txt", "cafe\u0301.txt")
    probe <- file.path(root, paste0("probe_", name))
    dir.create(probe)
    writeBin(charToRaw("probe"), file.path(probe, names[[1L]]))
    aliases <- file.exists(file.path(probe, names[[2L]]))
    archive <- .archive_fixture_write(name, file.path(root, paste0(name, ".zip")))
    result <- lissr:::.verify_unzip(archive, output)
    if (aliases) {
      expect_null(result, info = name)
      expect_identical(list.files(output, all.files = TRUE, no.. = TRUE), character(), info = name)
    } else {
      expect_length(result, 2L, info = name)
      expect_identical(.archive_read(file.path(output, names[[1L]])), charToRaw("first"))
      expect_identical(.archive_read(file.path(output, names[[2L]])), charToRaw("second"))
    }
    expect_identical(.archive_stage_names(output), character())
  }
})

test_that("archive output never overwrites its own input archive", {
  root <- .archive_test_root()
  on.exit(unlink(root, recursive = TRUE, expand = FALSE), add = TRUE)
  archive <- .archive_fixture_write("self_overwrite", file.path(root, "archive.zip"))
  before <- .archive_read(archive)
  expect_null(lissr:::.verify_unzip(archive, root))
  expect_identical(.archive_read(archive), before)
  expect_identical(.archive_stage_names(root), character())
  .archive_fixture_write("self_alias", archive)
  before <- .archive_read(archive)
  aliases <- file.exists(file.path(root, "ARCHIVE.ZIP"))
  result <- lissr:::.verify_unzip(archive, root)
  if (aliases) expect_null(result) else expect_identical(result, file.path(root, "ARCHIVE.ZIP"))
  expect_identical(.archive_read(archive), before)
  expect_identical(.archive_stage_names(root), character())
})

test_that("archive destinations reject linked parents, leaves and dangling links", {
  root <- .archive_test_root()
  on.exit(unlink(root, recursive = TRUE, expand = FALSE), add = TRUE)
  outside <- file.path(root, "outside")
  dir.create(outside)
  marker <- file.path(outside, "payload.txt")
  writeBin(charToRaw("outside unchanged"), marker)
  nested <- .archive_fixture_write("nested", file.path(root, "nested.zip"))
  single <- .archive_fixture_write("single", file.path(root, "single.zip"))
  for (kind in c("parent", "leaf", "dangling", "inside_parent", "inside_leaf")) {
    output <- file.path(root, kind)
    dir.create(output)
    inside <- file.path(output, "real")
    dir.create(inside)
    inside_file <- file.path(inside, "payload.txt")
    writeBin(charToRaw("inside unchanged"), inside_file)
    parent <- kind %in% c("parent", "inside_parent")
    target <- if (kind == "dangling") file.path(outside, "missing.txt") else if (kind == "parent")
      outside else if (kind == "leaf") marker else if (kind == "inside_parent") inside else inside_file
    link <- file.path(output, if (parent) "child" else "payload.txt")
    expect_true(file.symlink(target, link), info = kind)
    before <- Sys.readlink(link)
    expect_null(lissr:::.verify_unzip(if (parent) nested else single, output), info = kind)
    expect_identical(Sys.readlink(link), before, info = kind)
    expect_identical(.archive_read(marker), charToRaw("outside unchanged"), info = kind)
    expect_identical(.archive_read(inside_file), charToRaw("inside unchanged"), info = kind)
    expect_false(file.exists(file.path(outside, "missing.txt")))
    expect_identical(.archive_stage_names(output), character())
  }
})

test_that("archive staging accepts a caller selected root alias but rejects descendants", {
  root <- .archive_test_root()
  on.exit(unlink(root, recursive = TRUE, expand = FALSE), add = TRUE)
  real <- file.path(root, "real")
  dir.create(real)
  alias <- file.path(root, "alias")
  expect_true(file.symlink(real, alias))
  archive <- .archive_fixture_write("single", file.path(root, "archive.zip"))
  expect_identical(lissr:::.verify_unzip(archive, alias), file.path(alias, "payload.txt"))
  expect_identical(.archive_read(file.path(real, "payload.txt")), charToRaw("new payload"))
  expect_identical(.archive_stage_names(real), character())
})

test_that("archive promotion replaces regular files without writing hard linked aliases", {
  root <- .archive_test_root()
  on.exit(unlink(root, recursive = TRUE, expand = FALSE), add = TRUE)
  output <- file.path(root, "output")
  dir.create(output)
  target <- file.path(output, "payload.txt")
  outside <- file.path(root, "invented_hard_link.txt")
  writeBin(charToRaw("old payload"), target)
  expect_true(Sys.chmod(target, "0640", use_umask = FALSE))
  mode <- file.info(target)$mode
  saved_umask <- Sys.umask("0077")
  on.exit(Sys.umask(saved_umask), add = TRUE)
  expect_true(file.link(target, outside))
  archive <- .archive_fixture_write("single", file.path(root, "archive.zip"))
  expect_identical(lissr:::.verify_unzip(archive, output), target)
  expect_identical(.archive_read(target), charToRaw("new payload"))
  expect_identical(.archive_read(outside), charToRaw("old payload"))
  expect_identical(file.info(target)$mode, mode)
  writeBin(charToRaw("separate identity"), target)
  expect_identical(.archive_read(outside), charToRaw("old payload"))
  expect_identical(.archive_stage_names(output), character())
})

test_that("archive destination type conflicts fail before replacement", {
  root <- .archive_test_root()
  on.exit(unlink(root, recursive = TRUE, expand = FALSE), add = TRUE)
  nested <- .archive_fixture_write("nested", file.path(root, "nested.zip"))
  single <- .archive_fixture_write("single", file.path(root, "single.zip"))
  output <- file.path(root, "output")
  dir.create(output)
  writeBin(charToRaw("old child file"), file.path(output, "child"))
  expect_null(lissr:::.verify_unzip(nested, output))
  expect_identical(.archive_read(file.path(output, "child")), charToRaw("old child file"))
  dir.create(file.path(output, "payload.txt"))
  expect_null(lissr:::.verify_unzip(single, output))
  expect_true(dir.exists(file.path(output, "payload.txt")))
  expect_identical(.archive_stage_names(output), character())
})

test_that("archive metadata for symbolic links is extracted only as regular bytes", {
  root <- .archive_test_root()
  on.exit(unlink(root, recursive = TRUE, expand = FALSE), add = TRUE)
  archive <- .archive_fixture_write("link_metadata", file.path(root, "archive.zip"))
  output <- file.path(root, "output")
  target <- file.path(output, "link.txt")
  expect_identical(lissr:::.verify_unzip(archive, output), target)
  expect_true(utils::file_test("-f", target))
  link <- Sys.readlink(target)
  expect_true(is.na(link) || !nzchar(link))
  expect_identical(.archive_read(target), charToRaw("../invented_outside.txt"))
})

test_that("archive streaming crosses chunk boundaries and enforces reported lengths", {
  root <- .archive_test_root()
  on.exit(unlink(root, recursive = TRUE, expand = FALSE), add = TRUE)
  archive <- .archive_fixture_write("large", file.path(root, "archive.zip"))
  output <- file.path(root, "output")
  expect_identical(lissr:::.verify_unzip(archive, output), file.path(output, "large.bin"))
  expect_identical(.archive_read(file.path(output, "large.bin")), rep(charToRaw("x"), 2097152L + 17L))
  small <- .archive_fixture_write("single", file.path(root, "small.zip"))
  for (size in c(0, 1, 12)) {
    expect_error(lissr:::.liss_zip_stream(small, "payload.txt", file.path(root, paste0("size_", size)), size))
  }
})

test_that("archive staging failure leaves all existing targets and the archive unchanged", {
  root <- .archive_test_root()
  on.exit(unlink(root, recursive = TRUE, expand = FALSE), add = TRUE)
  archive <- .archive_fixture_write("two_files", file.path(root, "archive.zip"))
  output <- file.path(root, "output")
  dir.create(output)
  for (name in c("first.txt", "second.txt")) writeBin(charToRaw("old"), file.path(output, name))
  stream <- lissr:::.liss_zip_stream
  testthat::local_mocked_bindings(.liss_zip_stream = function(zip_path, member, destination, expected_size) {
    if (member == "second.txt") stop("invented stream failure")
    stream(zip_path, member, destination, expected_size)
  }, .package = "lissr")
  before <- .archive_read(archive)
  expect_null(lissr:::.verify_unzip(archive, output))
  for (name in c("first.txt", "second.txt"))
    expect_identical(.archive_read(file.path(output, name)), charToRaw("old"))
  expect_identical(.archive_read(archive), before)
  expect_identical(.archive_stage_names(output), character())
})

test_that("archive promotion failure retains the archive and reports partial in root replacement", {
  root <- .archive_test_root()
  on.exit(unlink(root, recursive = TRUE, expand = FALSE), add = TRUE)
  archive <- .archive_fixture_write("two_files", file.path(root, "archive.zip"))
  output <- file.path(root, "output")
  dir.create(output)
  for (name in c("first.txt", "second.txt")) writeBin(charToRaw("old"), file.path(output, name))
  move <- lissr:::.liss_zip_move
  calls <- 0L
  testthat::local_mocked_bindings(.liss_zip_move = function(from, to) {
    calls <<- calls + 1L
    if (calls == 2L) return(FALSE)
    move(from, to)
  }, .package = "lissr")
  before <- .archive_read(archive)
  expect_null(lissr:::.verify_unzip(archive, output))
  expect_identical(calls, 2L)
  expect_identical(.archive_read(file.path(output, "first.txt")), charToRaw("first new"))
  expect_identical(.archive_read(file.path(output, "second.txt")), charToRaw("old"))
  expect_identical(.archive_read(archive), before)
  expect_identical(.archive_stage_names(output), character())
})

test_that("archive staging cleanup treats root glob characters literally", {
  root <- .archive_test_root()
  on.exit(unlink(root, recursive = TRUE, expand = FALSE), add = TRUE)
  output <- file.path(root, "output[one]")
  sibling <- file.path(root, "outputo")
  dir.create(output)
  dir.create(sibling)
  writeBin(charToRaw("sibling unchanged"), file.path(sibling, "marker"))
  archive <- .archive_fixture_write("single", file.path(root, "archive.zip"))
  expect_identical(lissr:::.verify_unzip(archive, output), file.path(output, "payload.txt"))
  expect_identical(.archive_stage_names(output), character())
  expect_identical(.archive_read(file.path(sibling, "marker")), charToRaw("sibling unchanged"))
})

test_that("the downloader keeps unsafe archives and deletes successfully extracted archives", {
  root <- .archive_test_root()
  on.exit(unlink(root, recursive = TRUE, expand = FALSE), add = TRUE)
  cache <- lissr:::.liss_cache
  had_session <- exists("session", envir = cache, inherits = FALSE)
  saved_session <- if (had_session) cache$session else NULL
  on.exit({
    if (had_session) cache$session <- saved_session else rm(list = "session", envir = cache)
  }, add = TRUE)
  cache$session <- structure(list(handle = NULL, config = NULL), class = "rvest_session")
  selected <- tibble::tibble(module = "Invented", module_id = 1L, wave = 1L,
    file = "archive.zip", path = "/invented/offline")
  fixture <- "parent"
  testthat::local_mocked_bindings(.liss_fetch_file = function(session, url, dest, timeout_s) {
    .archive_fixture_write(fixture, dest)
    list(url = url, status_code = 200L,
      headers = list(`content-disposition` = 'attachment; filename="archive.zip"'))
  }, .package = "lissr")
  output <- file.path(root, "unsafe")
  result <- suppressMessages(lissr::liss_download(selected, .dir = output, .unzip = TRUE, .retries = 0))
  expect_identical(result$status, "unzip_failed_archive_kept")
  expect_identical(.archive_read(file.path(output, "archive.zip")), .archive_fixture_bytes("parent"))
  expect_false(file.exists(file.path(root, "invented_outside.txt")))
  fixture <- "single"
  output <- file.path(root, "safe")
  result <- suppressMessages(lissr::liss_download(selected, .dir = output, .unzip = TRUE, .retries = 0))
  expect_identical(result$status, "ok")
  expect_false(file.exists(file.path(output, "archive.zip")))
  expect_identical(.archive_read(file.path(output, "payload.txt")), charToRaw("new payload"))
})
