#' perform two-factor authentication (internal)
#'
#' prompts for a verification code and submits it to complete login.
#' the code is whitespace-normalized (codes pasted from email carry
#' spaces and newlines), a rejected code can be retried without
#' restarting the whole login, and non-interactive sessions abort with
#' a clear message instead of silently submitting an empty code.
#'
#' @param session an active [rvest::session] on the verification page
#' @param attempts integer. how many times the code prompt is offered.
#' @return the authenticated session, or `NULL` on failure
#' @noRd
perform_twofactor_authentication <- function(session, attempts = 3) {
  cli::cli_progress_step("Performing two-factor authentication")

  if (!interactive()) {
    cli::cli_abort(c(
      "two-factor verification requires an interactive session",
      "i" = "the verification code is sent by email and must be entered at a prompt"
    ))
  }

  current <- session
  for (attempt in seq_len(attempts)) {
    raw_code <- if (requireNamespace("svDialogs", quietly = TRUE)) {
      svDialogs::dlg_input("Enter verification code (sent by email)")$res
    } else {
      readline(prompt = "Enter verification code (sent by email): ")
    }
    code <- .normalize_2fa_code(raw_code)
    if (!nzchar(code)) {
      if (attempt < attempts) {
        cli::cli_alert_warning(
          "Empty verification code; try again ({attempt}/{attempts})."
        )
        next
      }
      cli::cli_alert_danger("No verification code provided.")
      return(NULL)
    }

    # pick the form that actually carries the verification-code field;
    # fall back to the historical second-form position only if none
    # matches, and diagnose a page-layout drift instead of erroring
    forms <- rvest::html_form(current)
    has_code <- vapply(forms, function(f) "code" %in% names(f$fields),
                       logical(1))
    if (length(forms) == 0 || (!any(has_code) && length(forms) < 2)) {
      cli::cli_alert_danger(
        "Could not find the verification form; the page layout may have changed."
      )
      return(NULL)
    }
    idx <- if (any(has_code)) which(has_code)[[1]] else min(2L, length(forms))
    filled_form <- rvest::html_form_set(forms[[idx]], code = code)
    response <- rvest::session_submit(current, filled_form)

    if (!grepl("twofactor|login", response$url)) {
      cli::cli_alert_success("Verification successful: session started.")
      return(response)
    }
    if (grepl("login", response$url) && !grepl("twofactor", response$url)) {
      # kicked back to the login page: the session is dead, a fresh code
      # cannot save it
      cli::cli_alert_danger(
        "Verification failed and the session returned to the login page; restart with {.fn liss_login}."
      )
      return(NULL)
    }
    if (attempt < attempts) {
      cli::cli_alert_warning(
        "Verification code rejected; try again ({attempt}/{attempts})."
      )
      current <- response
    }
  }
  cli::cli_alert_danger("Verification failed after {attempts} attempt{?s}.")
  NULL
}
