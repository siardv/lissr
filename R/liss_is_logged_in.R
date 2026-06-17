#' check whether the current session is still authenticated
#'
#' verifies the cached session by attempting to download a random data file
#' from the archive. returns `FALSE` if the session has expired or was
#' never established.
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

  # get blueprint to find a downloadable sav file
  bp <- if (exists("blueprint", envir = .liss_cache)) {
    .liss_cache$blueprint
  } else {
    tryCatch(liss_blueprint(), error = function(e) NULL)
  }
  if (is.null(bp) || nrow(bp) == 0) return(FALSE)

  sav_files <- bp[bp$type == "spss" & !is.na(bp$path), ]
  if (nrow(sav_files) == 0) return(FALSE)

  # attempt to download a random sav file
  test_row <- sav_files[sample.int(nrow(sav_files), 1), ]
  test_url <- paste0("https://www.dataarchive.lissdata.nl", test_row$path)

  res <- tryCatch(
    rvest::session_jump_to(session, test_url),
    error = function(e) NULL
  )
  if (is.null(res)) return(FALSE)

  # redirected to login means session expired
  !grepl("login", res$url)
}
