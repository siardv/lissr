.batch_fixture <- function(files = character(), directories = character()) {
  data_dir <- withr::local_tempdir("lissr_batch_",
                                  .local_envir = parent.frame())
  for (directory in directories) dir.create(file.path(data_dir, directory))
  if (length(files)) stopifnot(all(file.create(file.path(data_dir, files))))
  list(data_dir = data_dir, output_dir = file.path(data_dir, "output"))
}

.batch_recipe <- function(module, wave_id = paste0(module, "01a")) {
  list(
    meta = list(module = module, module_label = "batch fixture",
                schema_version = "1.0.0", recipe_version = "fixture",
                created = "fixture", source_spec = "fixture",
                covered_waves = list(wave_id)),
    global = list(id_variable = "nomem_encr", wave_variable = "wave_id",
                  year_variable = "wave_year", labelled_policy = "to_numeric",
                  missing_variable_policy = "warn_and_create_na",
                  strip_label_whitespace = TRUE),
    wave_index = list(list(id = wave_id, year = 2001L,
                          file_pattern = paste0(wave_id, "_*"))),
    logging = list(summary_artifact = TRUE)
  )
}

.batch_capture <- function(expr) {
  messages <- character()
  value <- withCallingHandlers(expr, message = function(condition) {
    messages <<- c(messages, conditionMessage(condition))
    invokeRestart("muffleMessage")
  })
  list(value = value, messages = messages)
}

.batch_discovered_result <- function(recipe, data_dir, output_dir, strict) {
  # keep fixture-only absent-wave warnings out of the batch-selection assertions
  hits <- suppressWarnings(suppressMessages(
    lissr:::discover_wave_files(recipe, data_dir)))
  if (!length(hits)) stop("fixture has no discoverable wave data")
  list(data = data.frame(module = recipe$meta$module),
       paths = unname(unlist(lapply(hits, function(hit) hit$paths))),
       data_dir = data_dir, output_dir = output_dir, strict = strict)
}

.batch_with_file_info <- function(file_info, os_type) {
  available <- lissr:::.available_builtin_recipes
  platform <- .Platform
  platform$OS.type <- os_type
  environment(available) <- list2env(
    list(file.info = file_info, .Platform = platform),
    parent = environment(available))
  available
}

test_that("automatic batches validate only modules with local data", {
  fx <- .batch_fixture(
    files = c("ch07a_EN_1.0p.sav", "cp/cp08a_EN_1.0p.sav",
              "CR08A_EN_2.0p.SAV", "cv08a_codebook.pdf", "ca_notes.dta",
              "unrelated.sav"),
    directories = c("cp", "ci08a_EN_1.0p.sav"))
  validated <- character()
  testthat::local_mocked_bindings(
    validate_recipe = function(recipe, path = "<unknown>") {
      validated <<- c(validated, path)
      invisible(TRUE)
    },
    merge_liss_module = .batch_discovered_result
  )
  expect_no_warning(run <- .batch_capture(lissr::merge_liss_modules(
    data_dir = fx$data_dir, output_dir = fx$output_dir)))
  expect_identical(names(run$value), c("ch", "cp", "cr"))
  expect_identical(basename(validated),
                   paste0(c("ch", "cp", "cr"), "_merge_recipe.yml"))
  expect_false(any(grepl("skipp", run$messages, ignore.case = TRUE)))
  expect_identical(run$value$cp$data_dir, file.path(fx$data_dir, "cp"))
  expect_identical(run$value$ch$data_dir, fx$data_dir)
  expect_identical(basename(run$value$cr$paths), "CR08A_EN_2.0p.SAV")
  expect_true(all(vapply(run$value, function(result)
    identical(result$output_dir, fx$output_dir), logical(1))))
  expect_true(all(vapply(run$value, function(result)
    identical(result$strict, FALSE), logical(1))))
})

test_that("omitted and NULL recipe paths select the same built-in recipes", {
  fx <- .batch_fixture("ch07a_EN_1.0p.sav")
  testthat::local_mocked_bindings(
    validate_recipe = function(...) invisible(TRUE),
    merge_liss_module = .batch_discovered_result
  )
  omitted <- suppressMessages(lissr::merge_liss_modules(data_dir = fx$data_dir))
  explicit_null <- suppressMessages(lissr::merge_liss_modules(
    recipe_paths = NULL, data_dir = fx$data_dir))
  expect_identical(explicit_null, omitted)
  expect_identical(names(omitted), "ch")
  expect_identical(omitted$ch$output_dir, ".")
  expect_null(formals(lissr::merge_liss_modules)$recipe_paths)
})

test_that("automatic discovery accepts each supported data extension", {
  testthat::local_mocked_bindings(
    validate_recipe = function(...) invisible(TRUE),
    merge_liss_module = .batch_discovered_result
  )
  for (extension in c("SAV", "ZSAV", "DTA", "CSV")) {
    data_file <- paste0("CH07A_EN_1.0p.", extension)
    fx <- .batch_fixture(c(data_file, "ch07a_codebook.pdf"))
    expect_no_warning(results <- suppressMessages(lissr::merge_liss_modules(
      data_dir = fx$data_dir)))
    expect_identical(names(results), "ch", info = extension)
    expect_identical(basename(results$ch$paths), data_file, info = extension)
  }
})

test_that("automatic discovery respects existing module-directory precedence", {
  fx <- .batch_fixture(c("ch07a_EN_1.0p.sav", "cr08a_EN_2.0p.sav"),
                       directories = "ch")
  testthat::local_mocked_bindings(
    validate_recipe = function(...) invisible(TRUE),
    merge_liss_module = .batch_discovered_result
  )
  run <- .batch_capture(lissr::merge_liss_modules(data_dir = fx$data_dir))
  expect_identical(names(run$value), "cr")
  expect_false(any(grepl("skipp", run$messages, ignore.case = TRUE)))
  stopifnot(file.create(file.path(fx$data_dir, "ch", "ch07a_EN_1.0p.sav")))
  results <- suppressMessages(lissr::merge_liss_modules(data_dir = fx$data_dir))
  expect_identical(names(results), c("ch", "cr"))
  expect_identical(results$ch$data_dir, file.path(fx$data_dir, "ch"))
  expect_identical(results$cr$data_dir, fx$data_dir)
})

test_that("automatic batches with no local data fail before loading recipes", {
  testthat::local_mocked_bindings(
    load_recipe = function(...) stop("an absent recipe was loaded")
  )
  for (files in list(character(),
                    c("ch07a_codebook.pdf", "ch07a_notes.txt", "ch_notes.dta",
                      "ch07afoo.sav", "ch_merged.sav", "unrelated.csv",
                      "xx01a_EN_1.0p.sav"))) {
    fx <- .batch_fixture(files, directories = "ch07a_EN_1.0p.sav")
    expect_error(suppressMessages(lissr::merge_liss_modules(
      data_dir = fx$data_dir)), "no data files found for any module")
  }
  fx <- .batch_fixture()
  expect_error(lissr::merge_liss_modules(
    data_dir = file.path(fx$data_dir, "missing")),
    "no data files found for any module")
})

test_that("automatic discovery retains Windows resolved-target filtering", {
  fx <- .batch_fixture(c("ch07a_EN_1.0p.sav", "cr08a_EN_2.0p.sav"),
                       directories = "cp08a_EN_1.0p.sav")
  original_file_info <- base::file.info
  unresolved_path <- file.path(fx$data_dir, "ch07a_EN_1.0p.sav")
  unresolved_file_info <- function(..., extra_cols = TRUE) {
    info <- original_file_info(..., extra_cols = extra_cols)
    unresolved <- rownames(info) == unresolved_path
    if (any(unresolved)) {
      info$isdir[unresolved] <- FALSE
      info$mtime[unresolved] <- NA
      warning("cannot open file: missing target")
    }
    info
  }
  windows_available <- .batch_with_file_info(unresolved_file_info, "windows")
  expect_no_warning(paths <- windows_available(fx$data_dir))
  expect_identical(basename(paths), "cr_merge_recipe.yml")
  posix_available <- .batch_with_file_info(unresolved_file_info, "unix")
  expect_no_warning(paths <- posix_available(fx$data_dir))
  expect_identical(basename(paths), c("ch_merge_recipe.yml", "cr_merge_recipe.yml"))
})

test_that("automatic discovery includes valid links but excludes missing and directory links", {
  fx <- .batch_fixture(c("source.sav", "missing.sav"), directories = "nested")
  targets <- file.path(fx$data_dir, c("source.sav", "missing.sav", "nested"))
  links <- file.path(fx$data_dir, c("ch07a_current.sav", "cp08a_missing.sav",
                                   "cr08a_directory.sav"))
  link_created <- rep(FALSE, length(links))
  remove_links <- function() {
    if (any(link_created[1:2])) {
      stopifnot(all(file.remove(links[1:2][link_created[1:2]])))
      link_created[1:2] <<- FALSE
    }
    if (link_created[[3L]]) {
      if (.Platform$OS.type == "windows") {
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
  if (!all(link_created)) skip("file symlinks unavailable")
  stopifnot(file.remove(targets[[2L]]))
  expect_no_warning(paths <- lissr:::.available_builtin_recipes(fx$data_dir))
  expect_identical(basename(paths), "ch_merge_recipe.yml")
  remove_links()
  expect_true(file.exists(targets[[1L]]))
  expect_true(dir.exists(targets[[3L]]))
})

test_that("explicit custom recipes preserve path, order and named or positional calls", {
  fx <- .batch_fixture(c("yy01a_current.sav", "zz01a_current.sav"))
  paths <- file.path(fx$data_dir, c("Custom_recipe.v1-zz.yml", "yy_custom.yml"))
  yaml::write_yaml(.batch_recipe("zz"), paths[[1L]])
  yaml::write_yaml(.batch_recipe("yy"), paths[[2L]])
  loaded <- character()
  original_load_recipe <- lissr:::load_recipe
  testthat::local_mocked_bindings(
    load_recipe = function(path) {
      loaded <<- c(loaded, path)
      original_load_recipe(path)
    },
    merge_liss_module = .batch_discovered_result
  )
  positional <- suppressMessages(lissr::merge_liss_modules(
    paths, fx$data_dir, fx$output_dir, strict = TRUE))
  named <- suppressMessages(lissr::merge_liss_modules(
    recipe_paths = paths, data_dir = fx$data_dir,
    output_dir = fx$output_dir, strict = TRUE))
  expect_identical(loaded, rep(paths, 2L))
  expect_identical(names(positional), c("zz", "yy"))
  expect_identical(named, positional)
  expect_true(all(vapply(named, function(result) result$strict, logical(1))))
  expect_true(all(vapply(named, function(result)
    identical(result$output_dir, fx$output_dir), logical(1))))
})

test_that("explicit recipes without data retain their validation and skipped notices", {
  fx <- .batch_fixture("yy01a_current.sav")
  paths <- file.path(fx$data_dir, c("yy_custom.yml", "zz_custom.yml"))
  yaml::write_yaml(.batch_recipe("yy"), paths[[1L]])
  yaml::write_yaml(.batch_recipe("zz"), paths[[2L]])
  validated <- character()
  original_validate_recipe <- lissr::validate_recipe
  testthat::local_mocked_bindings(
    validate_recipe = function(recipe, path = "<unknown>") {
      validated <<- c(validated, path)
      original_validate_recipe(recipe, path)
    },
    merge_liss_module = .batch_discovered_result
  )
  run <- .batch_capture(lissr::merge_liss_modules(paths, data_dir = fx$data_dir))
  expect_identical(validated, paths)
  expect_identical(names(run$value), "yy")
  expect_true(any(grepl("skipping.*zz", run$messages)))
  expect_true(any(grepl("skipped \\(no data\\): zz", run$messages)))
})

test_that("invalid explicit recipes abort before any module merge even without data", {
  fx <- .batch_fixture("yy01a_current.sav")
  paths <- file.path(fx$data_dir, c("yy_custom.yml", "invalid_zz.yml"))
  yaml::write_yaml(.batch_recipe("yy"), paths[[1L]])
  yaml::write_yaml(list(meta = list(module = "zz")), paths[[2L]])
  merged <- FALSE
  testthat::local_mocked_bindings(
    merge_liss_module = function(...) {
      merged <<- TRUE
      stop("invalid recipes must abort before merging")
    }
  )
  expect_error(suppressMessages(lissr::merge_liss_modules(
    paths, data_dir = fx$data_dir)), "schema violation")
  expect_false(merged)
})

test_that("automatic and explicit batches forward strict and continue after module errors", {
  fx <- .batch_fixture(c("ch07a_current.sav", "cp08a_current.sav"))
  paths <- vapply(c("ch", "cp"), function(module)
    system.file("recipes", paste0(module, "_merge_recipe.yml"),
                package = "lissr", mustWork = TRUE), character(1))
  testthat::local_mocked_bindings(
    validate_recipe = function(...) invisible(TRUE),
    merge_liss_module = function(recipe, data_dir, output_dir, strict) {
      if (!identical(strict, TRUE)) stop("strict was not forwarded")
      if (recipe$meta$module == "ch") stop("fixture merge failure")
      .batch_discovered_result(recipe, data_dir, output_dir, strict)
    }
  )
  for (recipe_paths in list(NULL, paths)) {
    run <- .batch_capture(lissr::merge_liss_modules(
      recipe_paths = recipe_paths, data_dir = fx$data_dir, strict = TRUE))
    expect_identical(names(run$value), c("ch", "cp"))
    expect_null(run$value$ch$data)
    expect_identical(run$value$ch$error, "fixture merge failure")
    expect_identical(run$value$cp$data$module, "cp")
    expect_true(run$value$cp$strict)
    expect_true(any(grepl("continuing with remaining modules", run$messages)))
    expect_true(any(grepl("1/2 module", run$messages)))
    expect_true(any(grepl("failed: ch", run$messages)))
  }
})

test_that("selected recipe and data warnings remain visible in both batch modes", {
  fx <- .batch_fixture("ch07a_current.sav")
  path <- system.file("recipes", "ch_merge_recipe.yml", package = "lissr",
                       mustWork = TRUE)
  validated <- character()
  testthat::local_mocked_bindings(
    validate_recipe = function(recipe, path = "<unknown>") {
      validated <<- c(validated, recipe$meta$module)
      warning("selected recipe warning", call. = FALSE)
      invisible(TRUE)
    },
    merge_liss_module = function(recipe, data_dir, output_dir, strict) {
      warning("selected data warning", call. = FALSE)
      .batch_discovered_result(recipe, data_dir, output_dir, strict)
    }
  )
  for (recipe_paths in list(NULL, path)) {
    warnings <- character()
    run <- withCallingHandlers(.batch_capture(lissr::merge_liss_modules(
      recipe_paths = recipe_paths, data_dir = fx$data_dir)),
      warning = function(condition) {
        warnings <<- c(warnings, conditionMessage(condition))
        invokeRestart("muffleWarning")
      })
    expect_identical(warnings, c("selected recipe warning", "selected data warning"))
    expect_identical(names(run$value), "ch")
  }
  expect_identical(validated, c("ch", "ch"))
})

test_that("automatic and explicitly selected recipes produce identical real merge data", {
  fx <- .batch_fixture()
  haven::write_sav(data.frame(nomem_encr = 1:3, ch07a005 = c(1, 2, 3)),
                   file.path(fx$data_dir, "ch07a_EN_1.0p.sav"))
  haven::write_sav(data.frame(nomem_encr = 1:3, cr08a001 = c(1, 2, 3)),
                   file.path(fx$data_dir, "cr08a_EN_2.0p.sav"))
  paths <- vapply(c("ch", "cr"), function(module)
    system.file("recipes", paste0(module, "_merge_recipe.yml"),
                package = "lissr", mustWork = TRUE), character(1))
  automatic_dir <- file.path(fx$data_dir, "automatic")
  explicit_dir <- file.path(fx$data_dir, "explicit")
  # tiny partial-wave fixtures deliberately exercise existing missing-wave warnings
  automatic <- suppressWarnings(suppressMessages(lissr::merge_liss_modules(
    data_dir = fx$data_dir, output_dir = automatic_dir)))
  explicit <- suppressWarnings(suppressMessages(lissr::merge_liss_modules(
    paths, data_dir = fx$data_dir, output_dir = explicit_dir)))
  expect_identical(names(automatic), c("ch", "cr"))
  expect_identical(names(explicit), names(automatic))
  for (module in names(automatic)) {
    expect_null(automatic[[module]]$error, info = module)
    expect_null(explicit[[module]]$error, info = module)
    expect_equal(nrow(automatic[[module]]$data), 3L, info = module)
    expect_identical(automatic[[module]]$data, explicit[[module]]$data, info = module)
    expect_identical(automatic[[module]]$validation,
                     explicit[[module]]$validation, info = module)
    expect_identical(automatic[[module]]$valid_for_analysis,
                     explicit[[module]]$valid_for_analysis, info = module)
    expect_identical(automatic[[module]]$provenance$inputs,
                     explicit[[module]]$provenance$inputs, info = module)
    expect_identical(automatic[[module]]$provenance$release_decisions,
                     explicit[[module]]$provenance$release_decisions, info = module)
    automatic_file <- file.path(automatic_dir, paste0(module, "_merged.sav"))
    explicit_file <- file.path(explicit_dir, paste0(module, "_merged.sav"))
    expect_true(file.exists(automatic_file), info = module)
    expect_true(file.exists(explicit_file), info = module)
    expect_identical(haven::read_sav(automatic_file), haven::read_sav(explicit_file),
                     info = module)
  }
})
