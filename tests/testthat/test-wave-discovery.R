.discovery_fixture <- function(files = character(), directories = character(),
                               pattern = "yy01a_*", aux = NULL, pin = NULL) {
  data_dir <- withr::local_tempdir("lissr_discovery_",
                                  .local_envir = parent.frame())
  if (length(files)) stopifnot(all(file.create(file.path(data_dir, files))))
  for (directory in directories) dir.create(file.path(data_dir, directory))
  wave <- list(id = "yy01a", year = 2001L, file_pattern = pattern)
  if (!is.null(aux)) wave$aux_files <- aux
  if (!is.null(pin)) wave$expected_release <- pin
  list(data_dir = data_dir, recipe = list(wave_index = list(wave)))
}

.discovery_with_file_info <- function(file_info, os_type) {
  discover <- lissr:::discover_wave_files
  platform <- .Platform
  platform$OS.type <- os_type
  # override only this copy's lexical bindings, leaving package and base unchanged
  environment(discover) <- list2env(
    list(file.info = file_info, .Platform = platform), parent = environment(discover))
  discover
}

test_that("primary discovery admits each data format but not sidecars or directories", {
  for (extension in c("SAV", "ZSAV", "DTA", "CSV")) {
    data_file <- paste0("YY01A_EN_1.0p.", extension)
    fx <- .discovery_fixture(
      c(data_file, "yy01a_codebook_9.0_EN.pdf", "yy01a_notes.txt"),
      directories = "yy01a_EN_8.0p.sav")
    hits <- lissr:::discover_wave_files(fx$recipe, fx$data_dir)
    expect_length(hits, 1L)
    expect_identical(basename(hits[[1L]]$paths), data_file)
    expect_null(hits[[1L]]$release_decision)
  }
})

test_that("sidecar-only primary matches allow a supported fallback", {
  fx <- .discovery_fixture(
    c("yy01a_codebook.pdf", "yy01a_EN_1.0p.DTA"),
    directories = "yy01a_EN_9.0p.sav", pattern = "yy01a_codebook*")
  expect_message(hits <- lissr:::discover_wave_files(fx$recipe, fx$data_dir),
                 "matched via fallback")
  expect_identical(basename(hits[[1L]]$paths), "yy01a_EN_1.0p.DTA")
})

test_that("data symlinks work while dangling and directory symlinks are excluded", {
  fx <- .discovery_fixture(c("source.sav", "missing.sav"), directories = "nested")
  targets <- file.path(fx$data_dir, c("source.sav", "missing.sav", "nested"))
  links <- file.path(fx$data_dir, c("yy01a_current.sav", "yy01a_missing.sav",
                                   "yy01a_directory.sav"))
  link_created <- rep(FALSE, length(links))
  remove_links <- function() {
    if (any(link_created[1:2])) {
      stopifnot(all(file.remove(links[1:2][link_created[1:2]])))
      link_created[1:2] <<- FALSE
    }
    if (link_created[[3L]]) {
      if (.Platform$OS.type == "windows") {
        # rmdir removes the directory link itself without visiting its target
        directory_link <- chartr("/", "\\", links[[3L]])
        output <- system2(Sys.getenv("COMSPEC", "cmd.exe"), c(
          "/d", "/c", "rmdir", shQuote(directory_link, type = "cmd")
        ), stdout = TRUE, stderr = TRUE)
        status <- attr(output, "status")
        if (!is.null(status) && status != 0L)
          stop("directory symlink cleanup failed: ", paste(output, collapse = "\n"))
      } else {
        stopifnot(unlink(links[[3L]]) == 0L)
      }
      link_created[[3L]] <<- FALSE
    }
  }
  withr::defer(remove_links())
  link_created <- suppressWarnings(file.symlink(targets, links))
  if (!all(link_created))
    skip("file symlinks unavailable")
  stopifnot(file.remove(targets[[2L]]))
  hits <- lissr:::discover_wave_files(fx$recipe, fx$data_dir)
  expect_identical(basename(hits[[1L]]$paths), "yy01a_current.sav")
  expect_null(hits[[1L]]$release_decision)
  remove_links()
  expect_true(file.exists(targets[[1L]]))
  expect_true(dir.exists(targets[[3L]]))
})

test_that("Windows target metadata filters primary, fallback and auxiliary matches", {
  original_file_info <- base::file.info
  for (case in c("primary", "fallback", "auxiliary")) {
    primary_file <- "yy01a_EN_1.0p.sav"
    missing_file <- if (case == "auxiliary") "supplement.csv" else "yy01a_EN_9.0p.sav"
    fx <- .discovery_fixture(
      c(primary_file, missing_file, "supplement.sav", "yy01a_codebook.pdf"),
      pattern = if (case == "fallback") "yy01a_codebook*" else "yy01a_EN_*",
      aux = if (case == "auxiliary") "supplement.csv" else NULL)
    missing_path <- file.path(fx$data_dir, missing_file)
    target_file_info <- function(..., extra_cols = TRUE) {
      info <- original_file_info(..., extra_cols = extra_cols)
      missing <- rownames(info) == missing_path
      if (any(missing)) {
        # model Windows retaining a file classification after target-open failure
        info$isdir[missing] <- FALSE
        info$mtime[missing] <- NA
        warning("cannot open file: missing target")
      }
      info
    }
    discover <- .discovery_with_file_info(target_file_info, "windows")
    if (case == "fallback") {
      expect_message(hits <- discover(fx$recipe, fx$data_dir), "matched via fallback")
    } else {
      hits <- discover(fx$recipe, fx$data_dir)
    }
    expect_identical(basename(hits[[1L]]$paths), primary_file, info = case)
    expect_null(hits[[1L]]$release_decision, info = case)
    expected_aux <- if (case == "auxiliary") "supplement.sav" else character()
    expect_identical(basename(hits[[1L]]$aux_paths), expected_aux, info = case)
  }
})

test_that("POSIX discovery retains its nondirectory predicate without timestamps", {
  fx <- .discovery_fixture("yy01a_current.sav", directories = "yy01a_directory.sav")
  original_file_info <- base::file.info
  no_timestamp_info <- function(..., extra_cols = TRUE) {
    info <- original_file_info(..., extra_cols = extra_cols)
    info$mtime[] <- NA
    info
  }
  discover <- .discovery_with_file_info(no_timestamp_info, "unix")
  hits <- discover(fx$recipe, fx$data_dir)
  expect_identical(basename(hits[[1L]]$paths), "yy01a_current.sav")
  expect_null(hits[[1L]]$release_decision)
})

test_that("empty and sidecar-only directories retain missing-file diagnostics", {
  for (sidecars in list(character(), c("yy01a_codebook.pdf", "yy01a_notes.txt"))) {
    fx <- .discovery_fixture(sidecars, directories = "yy01a_EN_1.0p.sav")
    expect_warning(hits <- lissr:::discover_wave_files(fx$recipe, fx$data_dir),
                   "no file found for wave")
    expect_length(hits, 0L)
  }
})

test_that("release ranking and its provenance consider only primary data files", {
  fx <- .discovery_fixture(
    c("yy01a_EN_1.0p.sav", "yy01a_EN_2.0p.sav", "yy01a_codebook_9.0_EN.pdf"),
    pin = "2.0p")
  expect_warning(hits <- lissr:::discover_wave_files(fx$recipe, fx$data_dir),
                 "using highest release version")
  expect_identical(basename(hits[[1L]]$paths), "yy01a_EN_2.0p.sav")
  expect_identical(hits[[1L]]$release_decision, list(
    selected = "yy01a_EN_2.0p.sav", ignored = "yy01a_EN_1.0p.sav",
    rule = "highest parsed release version"))
  expect_true(hits[[1L]]$release_ok)
})

test_that("tied and unrankable primary data candidates remain ambiguous", {
  for (data_files in list(
    c("yy01a_EN_1.0p.sav", "yy01a_EN_1.0p.csv"),
    c("yy01a_current.sav", "yy01a_previous.sav"))) {
    fx <- .discovery_fixture(c(data_files, "yy01a_codebook_9.0_EN.pdf"))
    expect_error(lissr:::discover_wave_files(fx$recipe, fx$data_dir),
                 "release versions cannot be ranked")
  }
})

test_that("one unversioned data candidate remains selectable", {
  fx <- .discovery_fixture(c("yy01a_current.csv", "yy01a_codebook.pdf"))
  hits <- lissr:::discover_wave_files(fx$recipe, fx$data_dir)
  expect_identical(basename(hits[[1L]]$paths), "yy01a_current.csv")
  expect_null(hits[[1L]]$release_decision)
})

test_that("auxiliary inputs stay separate from primary releases and codebooks", {
  fx <- .discovery_fixture(
    c("yy01a_EN_1.0p.sav", "yy01a_supplement.CSV", "yy01a_supplement.pdf"),
    aux = "yy01a_supplement.sav")
  hits <- lissr:::discover_wave_files(fx$recipe, fx$data_dir)
  expect_identical(basename(hits[[1L]]$paths), "yy01a_EN_1.0p.sav")
  expect_identical(basename(hits[[1L]]$aux_paths), "yy01a_supplement.CSV")
  expect_null(hits[[1L]]$release_decision)
})

test_that("independent auxiliary matching filters exact and extension fallback hits", {
  for (aux_file in c("supplement.sav", "supplement.CSV")) {
    fx <- .discovery_fixture(
      c("yy01a_EN_1.0p.sav", aux_file, "supplement.pdf"),
      directories = "supplement.zsav", pattern = "yy01a_EN_*",
      aux = "supplement.sav")
    hits <- lissr:::discover_wave_files(fx$recipe, fx$data_dir)
    expect_identical(basename(hits[[1L]]$paths), "yy01a_EN_1.0p.sav")
    expect_identical(basename(hits[[1L]]$aux_paths), aux_file)
  }
})

test_that("a directory cannot satisfy an exact auxiliary declaration", {
  fx <- .discovery_fixture(
    c("yy01a_EN_1.0p.sav", "supplement.csv", "supplement.pdf"),
    directories = "supplement.sav", aux = "supplement.sav")
  hits <- lissr:::discover_wave_files(fx$recipe, fx$data_dir)
  expect_identical(basename(hits[[1L]]$aux_paths), "supplement.csv")
})

test_that("missing auxiliary data still warns and auxiliary-only waves are skipped", {
  fx <- .discovery_fixture(c("yy01a_EN_1.0p.sav", "supplement.pdf"),
                           aux = "supplement.sav")
  expect_warning(hits <- lissr:::discover_wave_files(fx$recipe, fx$data_dir),
                 "declared aux file .* not found")
  expect_length(hits[[1L]]$aux_paths, 0L)

  only_aux <- .discovery_fixture(c("yy01a_supplement.sav", "yy01a_codebook.pdf"),
                                 aux = "yy01a_supplement.sav")
  expect_warning(hits <- lissr:::discover_wave_files(only_aux$recipe, only_aux$data_dir),
                 "only aux_files matched")
  expect_length(hits, 0L)
})

test_that("the cd10c pattern pin keeps its selected data release in mixed folders", {
  recipe <- yaml::read_yaml(system.file("recipes", "cd_merge_recipe.yml",
                                       package = "lissr", mustWork = TRUE))
  recipe$wave_index <- Filter(function(wave) identical(wave$id, "cd10c"),
                              recipe$wave_index)
  fx <- .discovery_fixture(c("cd10c_EN_1.0p.sav", "cd10c_EN_1.1p.sav",
                             "cd10c_EN_1.1p_codebook.pdf"))
  hits <- lissr:::discover_wave_files(recipe, fx$data_dir)
  expect_identical(basename(hits[[1L]]$paths), "cd10c_EN_1.1p.sav")
  expect_null(hits[[1L]]$release_decision)
})
