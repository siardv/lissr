#' check whether the current session is still authenticated
#'
#' probes the cached session cheaply: when a cached blueprint offers a
#' known protected file path, a HEAD request against it (deterministic
#' first entry, no body transfer) decides; otherwise the login page is
#' requested once and inspected. the probe never scrapes the archive,
#' never downloads a data file, and never touches the random number
#' generator (the previous implementation did all three). returns
#' `FALSE` if the session has expired or was never established.
#'
#' @return logical scalar.
#' @export
#' @examples
#' \dontrun{
#' liss_login()
#' liss_is_logged_in()
#' }
liss_is_logged_in <- function() {
  session <- tryCatch(.liss_get_session(), error = function(e) NULL)
  if (is.null(session) || !rvest::is.session(session)) return(FALSE)

  base_url <- "https://www.dataarchive.lissdata.nl"

  # preferred probe: HEAD a known protected file path from the cached
  # blueprint (never triggers a scrape when no blueprint is cached)
  if (exists("blueprint", envir = .liss_cache)) {
    bp <- .liss_cache$blueprint
    sav <- bp[bp$type == "spss" & !is.na(bp$path), , drop = FALSE]
    if (nrow(sav) > 0) {
      probe_url <- paste0(base_url, sav$path[[1]])
      res <- tryCatch(.liss_head(session, probe_url),
                      error = function(e) NULL)
      if (is.null(res)) return(FALSE)
      sc <- res$status_code %||% 0
      return(is.numeric(sc) && sc < 400 && !grepl("login", res$url %||% ""))
    }
  }

  # fallback probe: request the login page once. an authenticated
  # session is redirected away from it, or is served a page carrying a
  # logout link rather than a password field.
  res <- tryCatch(.liss_jump(session, paste0(base_url, "/login")),
                  error = function(e) NULL)
  if (is.null(res)) return(FALSE)
  if (!grepl("login", res$url %||% "")) return(TRUE)
  body <- tryCatch(rawToChar(res$response$content), error = function(e) "")
  has_logout <- grepl("logout", body, ignore.case = TRUE)
  has_password_field <- grepl("type=[\"']?password", body, ignore.case = TRUE)
  has_logout || !has_password_field
}
