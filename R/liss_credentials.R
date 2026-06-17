#' store LISS Data Archive credentials in the system keyring
#'
#' saves username and password to the operating system's credential store
#' (macOS Keychain, Windows Credential Store, or Linux Secret Service) so
#' that [liss_login()] can retrieve them automatically.
#'
#' @param username character or numeric. your LISS Data Archive username
#'   (typically a 5-digit number).
#' @param password character. if `NULL` (the default), prompts interactively
#'   via [keyring::key_set()]. supplying the password directly is discouraged
#'   outside of non-interactive environments because the value may be recorded
#'   in `.Rhistory`.
#' @return invisible `TRUE` on success.
#' @export
#' @examples
#' \dontrun{
#' # interactive (recommended) — prompts for password securely
#' liss_store_credentials(username = 12345)
#'
#' # non-interactive (CI / automated environments only)
#' liss_store_credentials(username = 12345, password = Sys.getenv("LISS_PASSWORD"))
#' }
liss_store_credentials <- function(username, password = NULL) {
  username <- as.character(username)
  if (!requireNamespace("keyring", quietly = TRUE)) {
    stop(
      "The 'keyring' package is required to store credentials securely.\n",
      "Install it with: install.packages(\"keyring\")",
      call. = FALSE
    )
  }

  if (is.null(password)) {
    # interactive prompt — password never visible in console history
    keyring::key_set("LISS_Data_Archive", username = username)
  } else {
    keyring::key_set_with_value(
      "LISS_Data_Archive",
      username = username,
      password = password
    )
  }
  cli::cli_alert_success("Credentials stored for {.val {username}}.")
  invisible(TRUE)
}

#' delete stored LISS Data Archive credentials
#'
#' removes the username and password previously saved with
#' [liss_store_credentials()] from the system keyring.
#'
#' @param username character or numeric. the username whose credentials
#'   should be removed.
#' @return invisible `TRUE` on success.
#' @export
#' @examples
#' \dontrun{
#' liss_delete_credentials(username = 12345)
#' }
liss_delete_credentials <- function(username) {
  username <- as.character(username)
  if (!requireNamespace("keyring", quietly = TRUE)) {
    stop(
      "The 'keyring' package is required to manage credentials.\n",
      "Install it with: install.packages(\"keyring\")",
      call. = FALSE
    )
  }
  keyring::key_delete("LISS_Data_Archive", username = username)
  cli::cli_alert_success("Credentials deleted for {.val {username}}.")
  invisible(TRUE)
}

#' list stored LISS Data Archive credentials
#'
#' returns a data frame of usernames with stored credentials.
#'
#' @return a data frame with columns `service` and `username`.
#' @export
#' @examples
#' \dontrun{
#' liss_list_credentials()
#' }
liss_list_credentials <- function() {
  if (!requireNamespace("keyring", quietly = TRUE)) {
    stop(
      "The 'keyring' package is required to manage credentials.\n",
      "Install it with: install.packages(\"keyring\")",
      call. = FALSE
    )
  }
  keyring::key_list("LISS_Data_Archive")
}
