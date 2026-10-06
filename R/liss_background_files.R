# public archive I/O is separate so catalogue tests can run offline
.liss_archive_html <- function(url) xml2::read_html(url)

# background releases are listed directly on study 322, without wave pages
.liss_background_files <- function(page, module, module_id) {
  rows <- rvest::html_elements(page, "#id_dd .row")
  files <- purrr::map_dfr(rows, function(row) {
    links <- rvest::html_elements(row, "a[href^='/hosted-files/download/']")
    if (!length(links)) return(NULL)
    file <- rvest::html_text2(links) %>% trimws()
    description <- xml2::read_html(as.character(row))
    xml2::xml_remove(rvest::html_elements(description,
      "a[href^='/hosted-files/download/']"))
    name <- rvest::html_text2(description) %>% stringr::str_squish()
    tibble::tibble(
      name = name,
      file = file,
      path = rvest::html_attr(links, "href")
    )
  })
  if (!nrow(files)) {
    cli::cli_warn("No Background Variables file links found; the archive layout may have changed.")
    return(tibble::tibble(module = character(), module_id = integer(),
      wave = integer(), wave_id = integer(), type = character(),
      name = character(), file = character(), path = character()))
  }
  files <- dplyr::filter(files, grepl("^/hosted-files/download/[0-9]+$", .data$path))
  is_zip <- grepl("\\.zip$", files$file, ignore.case = TRUE)
  parts <- stringr::str_match(files$file,
    "(?i)^avars_([0-9]{6})_[a-z]{2}_[0-9]+[._][0-9]+p\\.zip$")
  month <- suppressWarnings(as.integer(parts[, 2]))
  valid <- !is.na(month) & month >= 100001L & month %% 100L %in% 1:12
  invalid <- is_zip & !valid
  if (any(invalid)) {
    cli::cli_warn("Background archive filenames without a valid YYYYMM were omitted: {paste(files$file[invalid], collapse = ', ')}")
  }
  if (!any(is_zip & valid)) {
    cli::cli_warn("No dated Background Variables ZIP releases found; the catalogue may be incomplete.")
  }
  files$wave <- ifelse(is_zip & valid, month, NA_integer_)
  files$type <- ifelse(is_zip, "archive", "codebook")
  files <- files[!invalid & (is_zip | grepl("\\.pdf$", files$file, ignore.case = TRUE)), ]
  dplyr::mutate(files, module = module, module_id = module_id,
                wave_id = module_id) %>%
    dplyr::select("module", "module_id", "wave", "wave_id",
                  "type", "name", "file", "path")
}
