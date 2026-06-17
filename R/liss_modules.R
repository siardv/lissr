#' list available LISS panel modules
#'
#' returns a data frame of modules available in the LISS Data Archive. if
#' a blueprint has already been cached (via [liss_blueprint()]), the module
#' list is derived from the cache; otherwise the archive index page is
#' scraped directly.
#'
#' @param .details logical. if `TRUE`, includes file counts per type
#'   (requires a cached blueprint).
#' @return a tibble with columns `module`, `module_id`, and `waves`.
#' @export
#' @examples
#' \dontrun{
#' liss_modules()
#' liss_modules(.details = TRUE)
#' }
liss_modules <- function(.details = FALSE) {
  if (exists("blueprint", envir = .liss_cache)) {
    bp <- .liss_cache$blueprint
    overview <- bp %>%
      dplyr::group_by(.data$module, .data$module_id) %>%
      dplyr::summarise(
        waves     = dplyr::n_distinct(.data$wave),
        files     = dplyr::n(),
        spss      = sum(.data$type == "spss"),
        stata     = sum(.data$type == "stata"),
        codebooks = sum(.data$type == "codebook"),
        .groups   = "drop"
      ) %>%
      dplyr::arrange(.data$module)
    if (.details) return(overview)
    return(dplyr::select(overview, "module", "module_id", "waves"))
  }

  base_url <- "https://www.dataarchive.lissdata.nl"
  cli::cli_alert_info("Fetching module index...")
  index_page <- xml2::read_html(paste0(base_url, "/study-units/view/1"))
  mods <- purrr::map_dfr(c("#id1 > .card-body", "#id2 > .card-body"), function(sel) {
    links <- rvest::html_elements(rvest::html_element(index_page, sel), "a")
    tibble::tibble(
      module_id = rvest::html_attr(links, "href") %>%
        stringr::str_extract("[0-9]+") %>% as.integer(),
      module    = rvest::html_text2(links) %>%
        stringr::str_remove("^[0-9]+\\s") %>% stringr::str_squish()
    )
  })
  if (!.details) return(mods)

  cli::cli_alert_info("Fetching wave counts for {nrow(mods)} module(s)...")
  mods$waves <- purrr::map_int(mods$module_id, function(id) {
    page <- tryCatch(
      xml2::read_html(paste0(base_url, "/study-units/view/", id)),
      error = function(e) NULL
    )
    if (is.null(page)) return(0L)
    mes <- rvest::html_element(page, "#id_mes")
    if (is.na(mes)) return(0L)
    length(rvest::html_elements(mes, "a[href*='study-units/view']"))
  }, .progress = TRUE)

  mods %>%
    dplyr::select("module", "module_id", "waves") %>%
    dplyr::arrange(.data$module)
}
