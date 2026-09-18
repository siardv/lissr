# ============================================================================
# stage 8 (v1.4 development): documentation drift regressions
# ============================================================================
# the vignettes carry factual claims about the package (module counts, wave
# counts, action vocabulary, recipe structure, flag-column names). these
# tests re-derive each claim from the code and the bundled recipes, so a
# vignette can no longer describe a package that does not exist. vignette
# sources are read from the installed inst/doc; README assertions run only
# in a source tree (they skip under R CMD check).

.doc_src <- function(f) {
  p <- system.file("doc", f, package = "lissr")
  if (!nzchar(p)) character(0) else readLines(p, warn = FALSE)
}

.all_doc_files <- function() {
  d <- system.file("doc", package = "lissr")
  if (!nzchar(d)) return(character(0))
  list.files(d, pattern = "\\.Rmd$")
}

.quiet_recipe <- function(mod) {
  suppressWarnings(suppressMessages(lissr::liss_recipe(mod)))
}

TEN_MODULES <- c("ch", "cv", "cd", "cf", "cw", "cp", "cs", "ci", "ca", "cr")

test_that("no vignette mislabels the Background Variables file as module (CA)", {
  files <- .all_doc_files()
  skip_if(length(files) == 0, "vignette sources not installed")
  for (f in files) {
    txt <- paste(.doc_src(f), collapse = "\n")
    expect_false(grepl("Background Variables (module )?\\(CA\\)", txt),
                 label = paste0(f, ": Background Variables labelled (CA)"))
    expect_false(grepl("variables module \\(CA\\)", txt, ignore.case = TRUE),
                 label = paste0(f, ": variables module (CA)"))
  }
})

test_that("no vignette claims eight core modules", {
  files <- .all_doc_files()
  skip_if(length(files) == 0, "vignette sources not installed")
  for (f in files) {
    txt <- paste(.doc_src(f), collapse = "\n")
    expect_false(grepl("eight core|eight modules", txt, ignore.case = TRUE),
                 label = paste0(f, ": eight-module claim"))
  }
})

test_that("batch-merge module lists cover all ten modules", {
  for (f in c("merge-workflow.Rmd", "multi-module-linkage.Rmd")) {
    lines <- .doc_src(f)
    skip_if(length(lines) == 0, "vignette sources not installed")
    txt <- paste(lines, collapse = "\n")
    # find every  <name> <- c("xx", ...)  module vector
    m <- regmatches(txt,
                    gregexpr("(all_)?modules <- c\\([^)]*\\)", txt))[[1]]
    m <- m[grepl('"ch"', m)]
    expect_gt(length(m), 0)
    checked <- 0L
    for (vec in m) {
      codes <- regmatches(vec, gregexpr('"[a-z]{2}"', vec))[[1]]
      codes <- gsub('"', "", codes)
      # deliberate small selections are fine; anything presented as a
      # full-module batch (8 or more codes) must be exactly the ten
      if (length(codes) >= 8) {
        expect_setequal(codes, TEN_MODULES)
        checked <- checked + 1L
      }
    }
    expect_gt(checked, 0)
  }
})

test_that("the custom-recipes action vocabulary listing matches the code", {
  lines <- .doc_src("custom-recipes.Rmd")
  skip_if(length(lines) == 0, "vignette sources not installed")
  out_lines <- lines[startsWith(lines, "#>")]
  doc_actions <- unique(unlist(regmatches(out_lines,
                                          gregexpr('"[a-z_0-9]+"', out_lines))))
  doc_actions <- gsub('"', "", doc_actions)
  expect_setequal(doc_actions, unique(unlist(lissr:::VALID_ACTIONS)))
})

test_that("the custom-recipes str() section counts match liss_recipe('ch')", {
  lines <- .doc_src("custom-recipes.Rmd")
  skip_if(length(lines) == 0, "vignette sources not installed")
  r <- .quiet_recipe("ch")
  hits <- regmatches(lines,
                     regexec("^#>  \\$ ([a-z_]+)\\s*:List of ([0-9]+)", lines))
  hits <- Filter(function(h) length(h) == 3, hits)
  expect_identical(length(hits), 10L)
  for (h in hits) {
    section <- h[[2]]
    claimed <- as.integer(h[[3]])
    expect_identical(claimed, length(r[[section]]),
                     label = paste0("str() count for ", section))
  }
})

test_that("boundary flags named in the longitudinal vignette exist in the ch recipe", {
  lines <- .doc_src("longitudinal-panel-analysis.Rmd")
  skip_if(length(lines) == 0, "vignette sources not installed")
  r <- .quiet_recipe("ch")
  declared <- unlist(lapply(r$boundary_rules, function(b) {
    c(b$flag_name, b$flag_column, b$flag_variable)
  }))
  out_lines <- lines[startsWith(lines, "#>")]
  mentioned <- unique(unlist(regmatches(
    out_lines,
    gregexpr("[A-Za-z0-9_]+_(era|period|present)\\b", out_lines))))
  expect_gt(length(mentioned), 0)
  expect_true(all(mentioned %in% declared),
              label = paste("fabricated flag name(s):",
                            paste(setdiff(mentioned, declared),
                                  collapse = ", ")))
})

test_that("wave-count claims in the vignettes match the recipes", {
  n_ch <- length(.quiet_recipe("ch")$wave_index)
  n_cw <- length(.quiet_recipe("cw")$wave_index)

  mm <- paste(.doc_src("multi-module-linkage.Rmd"), collapse = "\n")
  skip_if(!nzchar(mm), "vignette sources not installed")
  expect_match(mm, sprintf("Work module has %d waves", n_cw))
  expect_match(mm, sprintf("Health has %d", n_ch))

  lg <- paste(.doc_src("longitudinal-panel-analysis.Rmd"), collapse = "\n")
  expect_match(lg, sprintf("all %d waves", n_ch))
})

test_that("the documented schema version matches the packaged schema", {
  schema_path <- system.file("schema", "CANONICAL_SCHEMA.md",
                             package = "lissr")
  expect_true(nzchar(schema_path))
  head1 <- readLines(schema_path, n = 1)
  ver <- regmatches(head1, regexpr("v[0-9]+\\.[0-9]+\\.[0-9]+", head1))
  expect_identical(ver, "v1.1.0")
  mw <- paste(.doc_src("merge-workflow.Rmd"), collapse = "\n")
  skip_if(!nzchar(mw), "vignette sources not installed")
  expect_match(mw, sprintf("Canonical Schema %s", ver))
})

test_that("README covers all ten modules and every shipped vignette", {
  readme <- file.path(testthat::test_path("..", ".."), "README.md")
  skip_if_not(file.exists(readme), "source tree not available")
  txt <- paste(readLines(readme, warn = FALSE), collapse = "\n")
  for (code in TEN_MODULES) {
    expect_match(txt, paste0("\\b", code, "\\b"),
                 label = paste("module", code, "in README"))
  }
  for (f in .all_doc_files()) {
    slug <- sub("\\.Rmd$", "", f)
    expect_match(txt, slug, fixed = TRUE,
                 label = paste("vignette", slug, "linked in README"))
  }
})

test_that("every shipped vignette has rendered output and a complete index entry", {
  files <- .all_doc_files()
  expect_gt(length(files), 0L)
  for (file in files) {
    for (extension in c("html", "R")) {
      output <- sub("Rmd$", extension, file)
      expect_true(nzchar(system.file("doc", output, package = "lissr")),
                  label = paste("missing vignette output", output))
    }
  }
  index_path <- system.file("Meta", "vignette.rds", package = "lissr")
  if (!nzchar(index_path))
    index_path <- system.file("build", "vignette.rds", package = "lissr")
  expect_true(nzchar(index_path))
  if (nzchar(index_path)) {
    index <- readRDS(index_path)
    expect_setequal(index$File, files)
    expect_setequal(index$PDF, sub("Rmd$", "html", files))
    expect_setequal(index$R, sub("Rmd$", "R", files))
  }
})

test_that("the rendered schema vignette matches the packaged schema source", {
  html <- system.file("doc", "canonical-schema.html", package = "lissr")
  expect_true(nzchar(html))
  if (nzchar(html)) {
    schema <- system.file("schema", "CANONICAL_SCHEMA.md", package = "lissr")
    source <- readLines(schema, encoding = "UTF-8")
    schema_copy <- withr::local_tempfile()
    writeBin(charToRaw(enc2utf8(paste0(paste(source, collapse = "\n"), "\n"))), schema_copy)
    fingerprint <- paste0("<!-- schema-source-md5: ", unname(tools::md5sum(schema_copy)), " -->")
    expect_true(any(grepl(fingerprint, readLines(html, warn = FALSE), fixed = TRUE)))
    headings <- xml2::xml_text(xml2::xml_find_all(xml2::read_html(html), "//h5"))
    headings <- gsub("[[:space:]]+", " ", trimws(headings))
    contract_titles <- grep("^#### ", source, value = TRUE)
    contract_titles <- gsub("`", "", sub("^#### ", "", contract_titles), fixed = TRUE)
    expect_gt(length(contract_titles), 0L)
    expect_true(all(contract_titles %in% headings))
  }
})
