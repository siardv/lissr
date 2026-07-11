#' log in to the LISS Data Archive
#'
#' authenticates with username and password (retrieved from the system
#' keyring via [keyring::key_get()]) and handles two-factor verification.
#' stores the authenticated session in an internal cache for use by all
#' other `liss_*` functions.
#'
#' @param username character or numeric. the LISS archive username (typically
#'   a 5-digit number). if `NULL` (the default), looks up saved credentials
#'   via [keyring::key_list()]. when exactly one set of credentials is stored,
#'   that username is used automatically. if no credentials are saved, prompts
#'   interactively for username and password.
#' @return the authenticated [rvest::session] (invisibly), or `NULL` on
#'   failure.
#' @export
#' @examples
#' \dontrun{
#' # store credentials once (interactive prompt)
#' liss_store_credentials(username = 12345)
#'
#' # then log in — credentials are picked up automatically
#' liss_login()
#' }
liss_login <- function(username = NULL) {
  if (!is.null(username)) username <- as.character(username)
  if (!liss_server_status(verbose = FALSE)) {
    return(invisible(NULL))
  }

  saved_creds <- keyring::key_list("LISS_Data_Archive")
  creds_from_keyring <- FALSE

  if (is.null(username) || missing(username)) {
    if (nrow(saved_creds) == 1) {
      username <- saved_creds$username
      cli::cli_alert_info("Found saved credentials for user {dQuote({username})}.")
    } else if (nrow(saved_creds) > 1) {
      cli::cli_alert_danger(
        "Multiple saved credentials found. Please specify a username: {toString(saved_creds$username)}"
      )
      return(invisible(NULL))
    } else {
      # no saved credentials — prompt for username
      if (!interactive()) {
        cli::cli_abort("No saved credentials found. Run interactively or supply a username.")
      }
      username <- readline(prompt = "LISS Data Archive username: ")
      username <- trimws(username)
      if (nchar(username) == 0) {
        cli::cli_alert_danger("No username provided.")
        return(invisible(NULL))
      }
    }
  }

  cli::cli_progress_step("Initiating login process")

  has_key <- nrow(saved_creds) > 0 && username %in% saved_creds$username
  if (has_key) {
    password <- tryCatch(keyring::key_get("LISS_Data_Archive", username),
                         error = function(e) NULL)
    if (is.null(password) || !nzchar(password)) {
      cli::cli_alert_danger(
        "The stored password for {.val {username}} is empty or unreadable; re-store it with {.code liss_store_credentials(\"{username}\")}."
      )
      return(invisible(NULL))
    }
    cli::cli_alert_success("Using secure password from keychain.")
    creds_from_keyring <- TRUE
  } else {
    if (requireNamespace("askpass", quietly = TRUE)) {
      password <- askpass::askpass("Your LISS Data Archive password:")
    } else {
      password <- readline(prompt = "Your LISS Data Archive password: ")
    }
    if (is.null(password) || nchar(password) == 0) {
      cli::cli_alert_danger("No password provided.")
      return(invisible(NULL))
    }
  }

  login_url <- "https://www.dataarchive.lissdata.nl/login"
  session <- rvest::session(login_url)
  login_forms <- rvest::html_form(session)

  # guarded form discovery: a layout drift on the login page must fail
  # with a diagnosis, not a subscript-out-of-bounds error
  form_hits <- which(vapply(login_forms,
                            function(form) identical(form$action, login_url),
                            logical(1)))
  if (length(login_forms) == 0 || length(form_hits) == 0) {
    cli::cli_abort(c(
      "could not find a login form posting to {.url {login_url}}",
      "x" = "forms found on the page: {length(login_forms)}",
      "i" = "the archive's login page layout may have changed; please report this on the lissr issue tracker"
    ))
  }
  selected_login_form <- login_forms[[form_hits[[1]]]]

  fields_names <- names(selected_login_form$fields)
  user_field <- grep("user", fields_names, value = TRUE)
  pass_field <- grep("pass", fields_names, value = TRUE)
  if (length(user_field) != 1 || length(pass_field) != 1) {
    cli::cli_abort(c(
      "could not identify the username and password fields on the login form",
      "x" = "fields present: {paste(fields_names, collapse = ', ')}",
      "i" = "the archive's login page layout may have changed; please report this on the lissr issue tracker"
    ))
  }

  login_args <- stats::setNames(list(username, password),
                                c(user_field, pass_field))
  filled_login_form <- rvest::html_form_set(selected_login_form, !!!login_args)

  cli::cli_progress_step(
    msg = "Submitting login credentials",
    msg_failed = "Login failed \u2014 check your credentials."
  )
  initial_session <- rvest::session_submit(session, filled_login_form)

  if (!grepl("verify", initial_session$url)) {
    cli::cli_alert_danger(
      "Login failed for username {.val {username}}; the password is not echoed for security."
    )
    cli::cli_alert_info(
      "Check the credentials and, if they changed, re-store them with {.code liss_store_credentials(\"{username}\")}."
    )
    return(invisible(NULL))
  }

  auth_session <- perform_twofactor_authentication(session = initial_session)

  if (!is.null(auth_session) && rvest::is.session(auth_session)) {
    .liss_set_session(auth_session, username)

    # offer to save credentials if they were entered manually
    if (!creds_from_keyring && interactive()) {
      save_choice <- readline(
        prompt = "Save credentials to system keyring for future sessions? (y/n): "
      )
      if (tolower(trimws(save_choice)) %in% c("y", "yes")) {
        tryCatch({
          keyring::key_set_with_value(
            "LISS_Data_Archive",
            username = username,
            password = password
          )
          cli::cli_alert_success(
            "Credentials saved. Future calls to {.fn liss_login} will log in automatically."
          )
        }, error = function(e) {
          cli::cli_alert_warning(
            "Could not save credentials: {e$message}"
          )
          cli::cli_alert_info(
            "You can save them manually with {.code liss_store_credentials(\"{username}\")}"
          )
        })
      }
    }
  }

  invisible(auth_session)
}
