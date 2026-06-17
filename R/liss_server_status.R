#' check LISS Data Archive availability
#'
#' sends a HEAD request to the LISS Data Archive website and reports
#' whether the server responds with HTTP 200.
#'
#' @param verbose logical. print status messages to the console?
#' @return invisible logical. `TRUE` if the website is accessible.
#' @export
#' @examples
#' \dontrun{
#' liss_server_status()
#' }
liss_server_status <- function(verbose = TRUE) {
  lissdata_url <- "https://www.dataarchive.lissdata.nl/"
  isitdownrightnow_url <- "https://www.isitdownrightnow.com/dataarchive.lissdata.nl.html"

  tryCatch(
    {
      response <- httr::HEAD(lissdata_url, httr::timeout(5))

      if (httr::status_code(response) == 200) {
        if (verbose) {
          cli::cli_alert_success("The LISS Data Archive is online and accessible.")
        }
        return(invisible(TRUE))
      } else {
        stop("Non-200 status code received")
      }
    },
    error = function(e) {
      site_status <- cli::style_hyperlink("isitdownrightnow.com", isitdownrightnow_url)
      site_name <- "LISS Data Archive"
      whitespace <- paste0("\n", cli::style_hidden(cli::symbol$cross))

      alert_message <- paste(
        "It looks like the", site_name, "is currently unavailable.",
        whitespace,
        "Visit", site_status, "for more information."
      )

      cli::cli_alert_danger(alert_message)
      return(invisible(FALSE))
    }
  )
}
