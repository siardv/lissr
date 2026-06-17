#' download files from the LISS Data Archive
#'
#' downloads the files described in a selection tibble (typically produced
#' by [liss_select()]) to a local directory. requires an active session
#' established by [liss_login()].
#'
#' @param .hosted a tibble with at least columns `module`, `wave`, `file`,
#'   and `path`, as returned by [liss_select()] or `get_hosted_files()`.
#'   if `NULL`, calls `get_hosted_files()` internally.
#' @param .dir character. local directory to save files to. created
#'   automatically if it does not exist.
#' @param .modules optional character or integer vector to filter by module.
#' @param .waves optional integer vector to filter by wave number.
#' @param .unzip logical. if `TRUE` (the default), ZIP files are extracted
#'   and the archive is removed.
#' @return a tibble of download results with columns `file` and `status`
#'   (invisibly).
#' @export
#' @examples
#' \dontrun{
#' liss_login()
#' selection <- liss_select()
#' liss_download(selection)
#' }
liss_download <- function(.hosted = NULL, .dir = "liss",
                          .modules = NULL, .waves = NULL, .unzip = TRUE) {
  session <- .liss_get_session()

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

  if (is.null(.modules) && is.null(.waves)) {
    n_mods  <- dplyr::n_distinct(.hosted$module)
    n_waves <- dplyr::n_distinct(.hosted$wave)
    msg <- paste0(
      "No modules or waves specified. Download all ", nrow(.hosted),
      " files (", n_mods, " modules, ", n_waves, " waves)?"
    )
    if (!isTRUE(utils::askYesNo(msg))) {
      cli::cli_alert_info("Download cancelled.")
      return(invisible(NULL))
    }
  }

  dir_path <- normalizePath(.dir, mustWork = FALSE)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)

  cli::cli_alert_info("Downloading {nrow(.hosted)} file(s) to {.path {dir_path}}")

  results <- purrr::map_dfr(seq_len(nrow(.hosted)), function(i) {
    row <- .hosted[i, ]
    url <- paste0(base_url, row$path)

    tryCatch({
      res <- rvest::session_jump_to(session, url)

      if (grepl("login", res$url)) {
        cli::cli_alert_danger("Session expired at file {i}/{nrow(.hosted)}")
        return(tibble::tibble(file = row$file, status = "session_expired"))
      }

      cd <- res$response$headers$`content-disposition`
      fname <- if (!is.null(cd)) gsub("^.*?\"(.*?)\".*$", "\\1", cd) else row$file

      out_path <- file.path(dir_path, fname)
      writeBin(httr::content(res$response, as = "raw"), out_path)

      if (.unzip && grepl("\\.zip$", fname, ignore.case = TRUE)) {
        utils::unzip(out_path, exdir = dir_path, overwrite = TRUE)
        unlink(out_path)
        cli::cli_alert_success(
          "[{i}/{nrow(.hosted)}] {row$module} w{row$wave}: {fname} (unzipped)"
        )
      } else {
        cli::cli_alert_success(
          "[{i}/{nrow(.hosted)}] {row$module} w{row$wave}: {fname}"
        )
      }

      tibble::tibble(file = fname, status = "ok")
    }, error = function(e) {
      cli::cli_alert_danger(
        "[{i}/{nrow(.hosted)}] {row$module} w{row$wave}: {e$message}"
      )
      tibble::tibble(file = row$file, status = e$message)
    })
  })

  n_ok <- sum(results$status == "ok")
  cli::cli_alert_info("{n_ok}/{nrow(.hosted)} files downloaded successfully.")
  invisible(results)
}
