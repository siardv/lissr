#' retrieve the active session (internal)
#'
#' called by all `liss_*` functions that require an authenticated session.
#' @return an [rvest::session] object
#' @noRd
.liss_get_session <- function() {
  if (!exists("session", envir = .liss_cache)) {
    stop("No active session. Run liss_login() first.", call. = FALSE)
  }
  .liss_cache$session
}

#' store a session after successful authentication (internal)
#'
#' @param session an [rvest::session] object
#' @param username character scalar
#' @noRd
.liss_set_session <- function(session, username) {
  .liss_cache$session    <- session
  .liss_cache$username   <- username
  .liss_cache$login_time <- Sys.time()
}
