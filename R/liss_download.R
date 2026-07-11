#' download files from the LISS Data Archive
#'
#' downloads the files described in a selection tibble (typically produced
#' by [liss_select()]) to a local directory. requires an active session
#' established by [liss_login()].
#'
#' downloads stream to disk through the authenticated session with a
#' per-file timeout, and transient failures (network errors, HTTP 5xx)
#' are retried. HTTP error responses are never written to disk as data
#' files, ZIP archives are only removed after their extraction has been
#' verified, and the first session expiry aborts the remaining batch
#' (each remaining file is reported as `skipped_batch_aborted` instead
#' of failing one by one).
#'
#' @param .hosted a tibble with at least columns `module`, `wave`, `file`,
#'   and `path`, as returned by [liss_select()] or `get_hosted_files()`.
#'   if `NULL`, calls `get_hosted_files()` internally.
#' @param .dir character. local directory to save files to. created
#'   automatically if it does not exist.
#' @param .modules optional character or integer vector to filter by module.
#' @param .waves optional integer vector to filter by wave number.
#' @param .unzip logical. if `TRUE` (the default), ZIP files are extracted
#'   and the archive is removed once extraction is verified; a failed
#'   extraction keeps the archive and reports `unzip_failed_archive_kept`.
#' @param .skip_existing logical. if `TRUE`, files whose listed name is
#'   already present in `.dir` are skipped without a network request
#'   (status `skipped_existing`). the check uses the archive's listed
#'   file name, not the name a content-disposition header may assign.
#' @param .timeout numeric. per-file download timeout in seconds.
#' @param .retries integer. how many times a failed request (curl error
#'   or HTTP 5xx) is retried before giving up on that file.
#' @return a tibble of download results with columns `file` and `status`
#'   (invisibly). `status` is one of `"ok"`, `"skipped_existing"`,
#'   `"session_expired"`, `"skipped_batch_aborted"`, `"http_<code>"`,
#'   `"unzip_failed_archive_kept"`, or `"error: <message>"`.
#' @export
#' @examples
#' \dontrun{
#' liss_login()
#' selection <- liss_select()
#' liss_download(selection)
#' }
liss_download <- function(.hosted = NULL, .dir = "liss",
                          .modules = NULL, .waves = NULL, .unzip = TRUE,
                          .skip_existing = FALSE, .timeout = 300,
                          .retries = 2) {
  session <- .liss_get_session()

  hosted_supplied <- !is.null(.hosted)
  if (is.null(.hosted)) {
    .hosted <- get_hosted_files(.modules = .modules)
  }

  base_url <- "https://www.dataarchive.lissdata.nl"

  if (!is.null(.modules)) {
    .hosted <- dplyr::filter(
      .hosted,
      .data$module %in% .modules | .data$module_id %in% .modules
    )
  }
  if (!is.null(.waves)) {
    .hosted <- dplyr::filter(.hosted, .data$wave %in% .waves)
  }
  if (nrow(.hosted) == 0) {
    cli::cli_alert_warning("No files to download.")
    return(invisible(NULL))
  }

  # a full-archive download must be an explicit choice: when no selection
  # and no filters were given, confirm interactively (defaulting to NO)
  # and refuse outright in non-interactive sessions instead of silently
  # auto-confirming as before
  if (!hosted_supplied && is.null(.modules) && is.null(.waves)) {
    n_mods  <- dplyr::n_distinct(.hosted$module)
    n_waves <- dplyr::n_distinct(.hosted$wave)
    if (!interactive()) {
      cli::cli_abort(c(
        "refusing to download the full archive ({nrow(.hosted)} files) non-interactively",
        "i" = "supply {.arg .modules} and/or {.arg .waves}, or pass an explicit selection as {.arg .hosted}"
      ))
    }
    msg <- paste0(
      "No modules or waves specified. Download all ", nrow(.hosted),
      " files (", n_mods, " modules, ", n_waves, " waves)?"
    )
    if (!isTRUE(utils::askYesNo(msg, default = FALSE))) {
      cli::cli_alert_info("Download cancelled.")
      return(invisible(NULL))
    }
  }

  dir_path <- normalizePath(.dir, mustWork = FALSE)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)

  n_total <- nrow(.hosted)
  cli::cli_alert_info("Downloading {n_total} file(s) to {.path {dir_path}}")

  results <- vector("list", n_total)
  aborted <- FALSE

  for (i in seq_len(n_total)) {
    row <- .hosted[i, ]

    if (aborted) {
      results[[i]] <- tibble::tibble(file = row$file,
                                     status = "skipped_batch_aborted")
      next
    }

    if (isTRUE(.skip_existing) &&
        file.exists(file.path(dir_path, sanitize_filename(row$file)))) {
      cli::cli_alert_info("[{i}/{n_total}] {row$file}: already present, skipped")
      results[[i]] <- tibble::tibble(file = row$file,
                                     status = "skipped_existing")
      next
    }

    url <- paste0(base_url, row$path)
    tmp <- file.path(dir_path, paste0(".lissr_partial_", i))

    results[[i]] <- tryCatch({
      res <- .with_retries(
        function() .liss_fetch_file(session, url, tmp, .timeout),
        attempts = .retries + 1
      )

      if (grepl("login", res$url %||% "")) {
        # session expired: abort the whole batch rather than iterating
        # through every remaining file against a dead session
        aborted <- TRUE
        unlink(tmp)
        cli::cli_alert_danger(
          "Session expired at file {i}/{n_total}; aborting the remaining batch. Run {.fn liss_login} and retry."
        )
        tibble::tibble(file = row$file, status = "session_expired")
      } else if (is.numeric(res$status_code %||% NULL) &&
                 (res$status_code %||% 0) >= 400) {
        # an HTTP error body must never be written as a data file
        sc <- res$status_code
        unlink(tmp)
        cli::cli_alert_danger("[{i}/{n_total}] {row$file}: HTTP {sc}")
        tibble::tibble(file = row$file, status = paste0("http_", sc))
      } else {
        cd <- res$headers[["content-disposition"]]
        fname <- parse_content_disposition(cd, fallback = row$file)
        out_path <- file.path(dir_path, fname)
        file.rename(tmp, out_path)

        if (.unzip && grepl("\\.zip$", fname, ignore.case = TRUE)) {
          extracted <- .verify_unzip(out_path, dir_path)
          if (is.null(extracted)) {
            cli::cli_alert_warning(
              "[{i}/{n_total}] {fname}: extraction failed; archive kept at {.path {out_path}}"
            )
            tibble::tibble(file = fname, status = "unzip_failed_archive_kept")
          } else {
            unlink(out_path)
            cli::cli_alert_success(
              "[{i}/{n_total}] {row$module} w{row$wave}: {fname} (unzipped, {length(extracted)} file{?s})"
            )
            tibble::tibble(file = fname, status = "ok")
          }
        } else {
          cli::cli_alert_success(
            "[{i}/{n_total}] {row$module} w{row$wave}: {fname}"
          )
          tibble::tibble(file = fname, status = "ok")
        }
      }
    }, error = function(e) {
      unlink(tmp)
      cli::cli_alert_danger(
        "[{i}/{n_total}] {row$module} w{row$wave}: {conditionMessage(e)}"
      )
      tibble::tibble(file = row$file,
                     status = paste0("error: ", conditionMessage(e)))
    })
  }

  results <- dplyr::bind_rows(results)
  n_ok <- sum(results$status == "ok")
  n_skip <- sum(startsWith(results$status, "skipped"))
  n_fail <- n_total - n_ok - n_skip
  if (n_fail > 0) {
    cli::cli_alert_warning(
      "{n_ok}/{n_total} files downloaded, {n_skip} skipped, {n_fail} failed."
    )
  } else {
    cli::cli_alert_info("{n_ok}/{n_total} files downloaded successfully.")
  }
  invisible(results)
}
