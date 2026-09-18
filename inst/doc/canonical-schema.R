## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----eval = FALSE-------------------------------------------------------------
# system.file("schema", "CANONICAL_SCHEMA.md", package = "lissr")

## ----schema-body, echo = FALSE, results = "asis"------------------------------
schema_path <- system.file("schema", "CANONICAL_SCHEMA.md", package = "lissr")
schema <- readLines(schema_path, warn = FALSE, encoding = "UTF-8")
# normalize line endings for a portable source fingerprint
schema_copy <- tempfile()
writeBin(charToRaw(enc2utf8(paste0(paste(schema, collapse = "\n"), "\n"))), schema_copy)
cat("<!-- schema-source-md5: ", unname(tools::md5sum(schema_copy)), " -->\n", sep = "")
unlink(schema_copy)
# demote headings one level so the vignette title stays the only h1
schema <- sub("^#", "##", schema)
cat(schema, sep = "\n")
