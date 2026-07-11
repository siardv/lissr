# stage7_live_check.R: live acceptance for the hardened download/auth layer
#
# runs the one stage-7 step that cannot run in a sandbox: a real login
# (with the emailed 2FA code), the two authentication probes, and a
# single-file download exercising the integrity paths. downloads ONE
# small file into a temporary directory and removes it afterwards.
#
# usage: from an INTERACTIVE R session started at the package root:
#   source("inst/scripts/verification/stage7_live_check.R")
#
# Rscript will not work: two-factor verification needs a live prompt,
# and the package (correctly) refuses to prompt in non-interactive
# sessions. this guard fires before any network request, so no 2FA
# email is triggered by a wrong invocation.
if (!interactive()) {
  stop(
    "this live check needs an interactive R session (the 2FA code is ",
    "typed at a prompt).\n  start R at the package root and run:\n  ",
    "source(\"inst/scripts/verification/stage7_live_check.R\")",
    call. = FALSE
  )
}

library(lissr)

.s7_ok <- character(0)
.s7_bad <- character(0)
note <- function(pass, label) {
  if (isTRUE(pass)) {
    .s7_ok <<- c(.s7_ok, label)
    cat("PASS:", label, "\n")
  } else {
    .s7_bad <<- c(.s7_bad, label)
    cat("FAIL:", label, "\n")
  }
}

# 1. reachability and login (keyring password + 2FA prompt)
note(liss_server_status(verbose = FALSE), "server reachable")
s <- liss_login()
note(!is.null(s), "login established a session")

if (is.null(s)) {
  cat("\ncannot continue without a session; fix the login first\n")
} else {
  # 2. the cheap probes: login-page fallback first (no blueprint cached
  # in a fresh session), then the HEAD probe once the blueprint exists;
  # both must leave the RNG untouched
  set.seed(1)
  seed0 <- .Random.seed
  note(isTRUE(liss_is_logged_in()), "login-page probe (no blueprint) is TRUE")
  note(identical(seed0, .Random.seed), "probe left the RNG state untouched")

  bp <- liss_blueprint()
  note(is.data.frame(bp) && nrow(bp) > 0, "blueprint scraped and cached")
  note(isTRUE(liss_is_logged_in()), "HEAD probe (with blueprint) is TRUE")

  # 3. single-file download into a temp dir: integrity, naming, re-run
  sav <- bp[bp$type == "spss" & !is.na(bp$path), ][1, ]
  dir <- file.path(tempdir(), "lissr_stage7_live")
  unlink(dir, recursive = TRUE)

  res1 <- liss_download(sav, .dir = dir, .unzip = TRUE)
  note(identical(res1$status, "ok"),
       paste0("single-file download ok (", sav$file, ")"))
  note(length(list.files(dir)) > 0, "downloaded content present on disk")

  res2 <- liss_download(sav, .dir = dir, .unzip = TRUE,
                        .skip_existing = TRUE)
  cat("  re-run status:", res2$status,
      "(skipped_existing when the listed name persists; ok when a zip was extracted and removed)\n")
  note(res2$status %in% c("skipped_existing", "ok"),
       "skip_existing re-run behaves")

  # 4. a bogus path must fail without writing a data file
  bogus <- sav
  bogus$file <- "lissr_bogus_probe.sav"
  bogus$path <- "/this-path-does-not-exist-12345"
  res3 <- liss_download(bogus, .dir = dir, .unzip = FALSE)
  cat("  bogus-path status:", res3$status, "\n")
  note(!identical(res3$status, "ok") &&
         !file.exists(file.path(dir, "lissr_bogus_probe.sav")),
       "bogus path fails without writing a file")
  if (identical(res3$status, "session_expired")) {
    cat("  note: the archive redirects unknown paths to the login page;\n",
        "  the batch-abort path was exercised instead of the HTTP-error path\n")
  }

  unlink(dir, recursive = TRUE)
}

cat("\n==== stage 7 live check:", length(.s7_ok), "passed,",
    length(.s7_bad), "failed ====\n")
if (length(.s7_bad) > 0) {
  cat("failed:", paste(.s7_bad, collapse = "; "), "\n")
}
