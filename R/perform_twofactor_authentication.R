#' perform two-factor authentication (internal)
#'
#' prompts for a verification code and submits it to complete login.
#'
#' @param session an active [rvest::session] on the verification page
#' @return the authenticated session, or `NULL` on failure
#' @noRd
perform_twofactor_authentication <- function(session) {
  cli::cli_progress_step("Performing two-factor authentication")

  if (requireNamespace("svDialogs", quietly = TRUE)) {
    verification_code <- svDialogs::dlg_input(
      "Enter verification code (sent by email)"
    )$res
  } else {
    verification_code <- readline(
      prompt = "Enter verification code (sent by email): "
    )
  }

  filled_form <- rvest::html_form(session)[[2]] |>
    rvest::html_form_set(code = verification_code)
  authentication_response <- rvest::session_submit(session, filled_form)

  if (grepl("twofactor|login", authentication_response$url)) {
    cli::cli_alert_danger("Verification failed \u2014 check your credentials.")
  } else {
    cli::cli_alert_success("Verification successful \u2014 session started.")
  }
  return(authentication_response)
}
