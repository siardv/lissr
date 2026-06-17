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

  if (!requireNamespace("keyring", quietly = TRUE)) {
    stop(
      "The 'keyring' package is required to retrieve stored credentials.\n",
      "Install it with: install.packages(\"keyring\")",
      call. = FALSE
    )
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
    password <- keyring::key_get("LISS_Data_Archive", username)
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

  masked_password <- paste0(
    substr(password, 1, 1),
    strrep("*", nchar(password) - 1)
  )

  login_url <- "https://www.dataarchive.lissdata.nl/login"
  session <- rvest::session(login_url)
  login_forms <- rvest::html_form(session)

  selected_login_form <- login_forms[[
    which(sapply(login_forms, function(form) form$action == login_url))
  ]]

  fields_names <- names(selected_login_form$fields)
  login_field_names <- sapply(
    c("user", "pass"),
    grep,
    x = fields_names, value = TRUE, USE.NAMES = FALSE
  )

  login_args <- setNames(list(username, password), login_field_names)
  filled_login_form <- rvest::html_form_set(selected_login_form, !!!login_args)

  cli::cli_progress_step(
    msg = "Submitting login credentials",
    msg_failed = "Login failed \u2014 check your credentials."
  )
  initial_session <- rvest::session_submit(session, filled_login_form)
  whitespace <- paste("\n", strrep(cli::style_hidden(cli::symbol$info), 2))

  if (!grepl("verify", initial_session$url)) {
    cli::cli_alert_info(c(
      "Credentials entered:",
      whitespace,
      "i" = "Username: {username}",
      whitespace,
      "i" = "Password: {masked_password}"
    ))
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
