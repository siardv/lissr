#' filter the blueprint for downloadable files (internal)
#'
#' @param .modules optional character or integer vector of module names or IDs
#'   to include. `NULL` (the default) returns all modules.
#' @param .types regex pattern for file extensions to include.
#' @return a filtered tibble from the cached blueprint.
#' @noRd
get_hosted_files <- function(.modules = NULL, .types = "\\.sav$") {
  force(.types)
  bp <- if (exists("blueprint", envir = .liss_cache)) {
    .liss_cache$blueprint
  } else {
    liss_blueprint()
  }

  result <- bp[grepl(.types, bp$file, ignore.case = TRUE), ]
  if (!is.null(.modules)) {
    result <- dplyr::filter(
      result,
      .data$module_id %in% .modules | .data$module %in% .modules
    )
  }
  result
}
