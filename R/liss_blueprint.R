#' build a complete file inventory of the LISS Data Archive
#'
#' scrapes every module page to build a data frame listing all downloadable
#' files (SPSS, Stata, codebooks) across every wave. the result is cached
#' in memory so subsequent calls return instantly.
#'
#' @param refresh logical. if `TRUE`, re-scrapes the archive even when a
#'   cached blueprint exists.
#' @return a tibble with columns `module`, `module_id`, `wave`, `wave_id`,
#'   `type`, `name`, `file`, and `path`.
#' @export
#' @examples
#' \dontrun{
#' bp <- liss_blueprint()
#' bp
#' }
liss_blueprint <- function(refresh = FALSE) {
  if (!isTRUE(refresh) && exists("blueprint", envir = .liss_cache)) {
    ts <- .liss_cache$timestamp
    cli::cli_alert_info("Using cached blueprint ({ts})")
    return(.liss_cache$blueprint)
  }

  base_url <- "https://www.dataarchive.lissdata.nl"

  cli::cli_alert_info("Fetching module index...")
  index_page <- tryCatch(
    xml2::read_html(paste0(base_url, "/study-units/view/1")),
    error = function(e) NULL
  )
  if (is.null(index_page)) {
    cli::cli_abort(c(
      "could not fetch the archive's module index",
      "i" = "check the connection and {.fn liss_server_status}; nothing was cached"
    ))
  }
  fails <- new.env(parent = emptyenv())
  fails$modules <- 0L
  fails$waves <- 0L
  mods <- purrr::map_dfr(c("#id1 > .card-body", "#id2 > .card-body"), function(sel) {
    links <- rvest::html_elements(rvest::html_element(index_page, sel), "a")
    tibble::tibble(
      module_id = rvest::html_attr(links, "href") %>%
        stringr::str_extract("[0-9]+") %>% as.integer(),
      module    = rvest::html_text2(links) %>%
        stringr::str_remove("^[0-9]+\\s") %>% stringr::str_squish()
    )
  })

  if (nrow(mods) == 0) {
    cli::cli_abort(c(
      "the module index yielded no modules",
      "i" = "the archive's page layout may have changed; nothing was cached"
    ))
  }

  cli::cli_alert_info("Scanning {nrow(mods)} module(s) for waves and files...")

  blueprint <- purrr::map_dfr(seq_len(nrow(mods)), function(i) {
    mod <- mods[i, ]
    page <- tryCatch(
      xml2::read_html(paste0(base_url, "/study-units/view/", mod$module_id)),
      error = function(e) NULL
    )
    if (is.null(page)) {
      fails$modules <- fails$modules + 1L
      return(NULL)
    }

    mes <- rvest::html_element(page, "#id_mes")
    if (length(mes) == 0 || is.na(mes)) return(NULL)
    wave_links <- rvest::html_elements(mes, "a[href*='study-units/view']")
    if (length(wave_links) == 0) return(NULL)

    waves <- tibble::tibble(
      wave_id = rvest::html_attr(wave_links, "href") %>%
        stringr::str_extract("[0-9]+") %>% as.integer(),
      wave    = rvest::html_text2(wave_links) %>%
        stringr::str_extract("[0-9]+") %>% as.integer()
    )

    purrr::map_dfr(seq_len(nrow(waves)), function(j) {
      w <- waves[j, ]
      wp <- tryCatch(
        xml2::read_html(paste0(base_url, "/study-units/view/", w$wave_id)),
        error = function(e) NULL
      )
      if (is.null(wp)) {
        fails$waves <- fails$waves + 1L
        return(NULL)
      }
      dd <- rvest::html_element(wp, "#id_dd")
      if (length(dd) == 0 || is.na(dd)) return(NULL)
      rows <- rvest::html_elements(dd, ".row")
      if (length(rows) == 0) return(NULL)

      purrr::map_dfr(rows, function(r) {
        txt <- rvest::html_text2(r) %>%
          stringr::str_replace_all("[\\r\\n]|Note:.*?$", " ") %>%
          stringr::str_squish()
        link <- rvest::html_element(r, "a")
        href <- if (!is.na(link)) rvest::html_attr(link, "href") else NA_character_
        parts <- stringr::str_split(txt, " (?=[^ ]+$)", n = 2, simplify = TRUE)
        tibble::tibble(name = parts[1], file = parts[2], path = href)
      }) %>%
        dplyr::filter(!is.na(.data$file) & .data$file != "" & !is.na(.data$path)) %>%
        dplyr::mutate(
          module    = mod$module,
          module_id = mod$module_id,
          wave      = w$wave,
          wave_id   = w$wave_id,
          type      = dplyr::case_when(
            grepl("\\.sav$", .data$file, ignore.case = TRUE)  ~ "spss",
            grepl("\\.dta$", .data$file, ignore.case = TRUE)  ~ "stata",
            grepl("\\.pdf$", .data$file, ignore.case = TRUE)  ~ "codebook",
            TRUE                                               ~ "other"
          )
        )
    })
  }, .progress = TRUE) %>%
    dplyr::select("module", "module_id", "wave", "wave_id",
                  "type", "name", "file", "path") %>%
    dplyr::arrange(.data$module, .data$wave, .data$type)

  if (nrow(blueprint) == 0) {
    cli::cli_abort(c(
      "the archive scrape produced an empty blueprint; nothing was cached",
      "i" = "the page layout may have changed, or every page failed to load ({fails$modules} module page{?s}, {fails$waves} wave page{?s} failed)"
    ))
  }
  if (fails$modules > 0 || fails$waves > 0) {
    cli::cli_warn(c(
      "{fails$modules} module page{?s} and {fails$waves} wave page{?s} failed to load; the cached blueprint may be incomplete",
      "i" = "re-run {.code liss_blueprint(refresh = TRUE)} to retry"
    ))
  }

  .liss_cache$blueprint <- blueprint
  .liss_cache$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M")

  n_mods  <- dplyr::n_distinct(blueprint$module)
  n_waves <- dplyr::n_distinct(blueprint$wave)
  cli::cli_alert_success(
    "Blueprint cached: {nrow(blueprint)} files across {n_mods} modules and {n_waves} waves"
  )
  blueprint
}
