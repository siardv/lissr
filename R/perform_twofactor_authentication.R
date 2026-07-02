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

  # pick the form that actually carries the verification-code field; fall back
  # to the historical second-form position only if none matches
  forms <- rvest::html_form(session)
  has_code <- vapply(forms, function(f) "code" %in% names(f$fields), logical(1))
  idx <- if (any(has_code)) which(has_code)[[1]] else min(2L, length(forms))
  filled_form <- rvest::html_form_set(forms[[idx]], code = verification_code)
  authentication_response <- rvest::session_submit(session, filled_form)

  if (grepl("twofactor|login", authentication_response$url)) {
    cli::cli_alert_danger("Verification failed: check your credentials.")
    # a failed verification must not be cached as an authenticated session
    return(NULL)
  }
  cli::cli_alert_success("Verification successful: session started.")
  authentication_response
}
