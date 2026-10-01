.test_gate_binary <- function(name) {
  suffix <- if (.Platform$OS.type == "windows") ".exe" else ""
  file.path(R.home("bin"), paste0(name, suffix))
}

test_that("the package test entrypoint rejects failures anywhere in raw results", {
  entrypoint <- normalizePath(testthat::test_path("..", "testthat.R"), mustWork = TRUE)
  root <- withr::local_tempdir("lissr_test_gate_")
  startup <- file.path(root, "empty_startup")
  writeLines(character(), startup)
  withr::local_envvar(c(R_TESTS = NA_character_, R_PROFILE = startup, R_PROFILE_USER = startup,
                        R_ENVIRON = startup, R_ENVIRON_USER = startup))
  package_dir <- file.path(root, "package")
  library_dir <- file.path(root, "library")
  dir.create(package_dir)
  dir.create(library_dir)
  # only the package name is needed to exercise the real installed-package runner
  writeLines(c(
    "Package: lissr", "Version: 0.0.0", "Title: Test Entrypoint Fixture",
    "Description: An empty package for isolated test entrypoint regressions.",
    "Authors@R: person('Test', 'Fixture', email = 'fixture@example.org', role = c('aut', 'cre'))",
    "License: MIT", "Config/testthat/edition: 3"
  ), file.path(package_dir, "DESCRIPTION"))
  writeLines(character(), file.path(package_dir, "NAMESPACE"))
  install_log <- file.path(root, "install.log")
  install_status <- system2(.test_gate_binary("R"), c(
    "CMD", "INSTALL", "--no-docs", "--no-html", "--no-multiarch",
    shQuote(paste0("--library=", library_dir)), shQuote(package_dir)
  ), stdout = install_log, stderr = install_log, timeout = 30)
  if (!identical(install_status, 0L)) {
    stop("test fixture installation failed:\n", paste(readLines(install_log), collapse = "\n"))
  }

  library_paths <- file.path(root, "library_paths.rds")
  saveRDS(.libPaths(), library_paths)
  runner <- file.path(root, "run_entrypoint.R")
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    ".libPaths(c(args[[1]], readRDS(args[[2]])))",
    "stopifnot(identical(normalizePath(find.package('lissr')),",
    "                    normalizePath(file.path(args[[1]], 'lissr'))))",
    "Sys.setenv(TESTTHAT_EDITION = '3', TESTTHAT_PARALLEL = 'FALSE')",
    "options(crayon.enabled = FALSE, cli.num_colors = 1L)",
    "setwd(args[[3]])",
    "source(args[[4]], echo = FALSE)",
    "cat('gate_entrypoint_completed\\n')"
  ), runner)

  cases <- list(
    pass = list(status = 0L, diagnostic = "gate_entrypoint_completed",
                code = "testthat::expect_true(TRUE)"),
    warning = list(status = 0L, diagnostic = "gate_entrypoint_completed",
                   code = "warning('gate_warning'); testthat::expect_true(TRUE)"),
    skip = list(status = 0L, diagnostic = "gate_skip",
                code = "testthat::skip('gate_skip')"),
    warning_then_skip = list(status = 0L, diagnostic = "gate_skip",
                            code = "warning('gate_warning'); testthat::skip('gate_skip')"),
    error = list(status = 1L, diagnostic = "gate_error",
                 code = "stop('gate_error')"),
    warning_then_error = list(status = 1L, diagnostic = "gate_error",
                             code = "warning('gate_warning'); stop('gate_error')"),
    error_then_cleanup_warning = list(status = 1L, diagnostic = "gate_error",
      code = "withr::defer(warning('gate_cleanup_warning')); stop('gate_error')"),
    error_then_cleanup_skip = list(status = 1L, diagnostic = "gate_cleanup_skip",
      code = "withr::defer(testthat::skip('gate_cleanup_skip')); stop('gate_error')"),
    failure = list(status = 1L, diagnostic = "gate_failure",
                   code = "testthat::expect_true(FALSE, info = 'gate_failure')"),
    failure_then_cleanup_warning = list(status = 1L, diagnostic = "gate_failure",
      code = paste("withr::defer(warning('gate_cleanup_warning'));",
                   "testthat::expect_true(FALSE, info = 'gate_failure')"))
  )
  for (name in names(cases)) {
    case <- cases[[name]]
    case_dir <- file.path(root, name)
    dir.create(file.path(case_dir, "testthat"), recursive = TRUE)
    writeLines(c("testthat::test_that('entrypoint gate fixture', {", case$code, "})"),
               file.path(case_dir, "testthat", "test-case.R"))
    output_path <- file.path(case_dir, "output.log")
    status <- system2(.test_gate_binary("Rscript"), c(
      "--vanilla", shQuote(runner), shQuote(library_dir), shQuote(library_paths),
      shQuote(case_dir), shQuote(entrypoint)
    ), stdout = output_path, stderr = output_path, timeout = 30)
    output <- paste(readLines(output_path, warn = FALSE), collapse = "\n")
    context <- paste(name, output, sep = "\n")
    expect_identical(status, case$status, info = context)
    expect_match(output, case$diagnostic, fixed = TRUE, info = context)
    # older check reporters omit the summary when there are no problems
    if (!name %in% c("pass", "skip")) {
      expect_match(output, "[ FAIL", fixed = TRUE, info = context)
    }
    if (grepl("warning", name)) expect_match(output, "WARN [1-9]", info = context)
    expect_identical(grepl("gate_entrypoint_completed", output, fixed = TRUE),
                     identical(case$status, 0L), info = context)
  }
})
