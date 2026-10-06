#' display a module-by-wave availability matrix
#'
#' prints a cross-tabulation showing which modules have data available
#' in which waves. uses the cached blueprint if available, otherwise
#' calls [liss_blueprint()] first.
#' Monthly Background Variables releases are omitted because YYYYMM
#' months are not core study wave numbers; inspect [liss_blueprint()]
#' for their availability instead.
#'
#' @return a data frame (invisibly) with modules as rows and waves as
#'   columns. available cells contain a multiplication sign (unicode
#'   U+00D7) and missing cells contain a middle dot (unicode U+00B7).
#' @export
#' @examples
#' \dontrun{
#' liss_wave_matrix()
#' }
liss_wave_matrix <- function() {
  if (exists("blueprint", envir = .liss_cache)) {
    bp <- .liss_cache$blueprint
  } else {
    bp <- liss_blueprint()
  }

  if (any(bp$module_id == 322L)) {
    cli::cli_alert_info("Background Variables uses monthly releases; inspect {.fn liss_blueprint} for YYYYMM availability.")
    bp <- dplyr::filter(bp, .data$module_id != 322L)
  }
  if (!nrow(bp)) {
    out <- data.frame()
    print(out)
    return(invisible(out))
  }
  presence <- bp %>%
    dplyr::distinct(.data$module, .data$wave) %>%
    dplyr::mutate(available = TRUE)

  all_waves <- sort(unique(bp$wave))
  all_mods  <- sort(unique(bp$module))

  grid <- tidyr::expand_grid(module = all_mods, wave = all_waves) %>%
    dplyr::left_join(presence, by = c("module", "wave")) %>%
    dplyr::mutate(
      symbol = dplyr::if_else(is.na(.data$available), "\u00b7", "\u00d7")
    ) %>%
    dplyr::select("module", "wave", "symbol") %>%
    tidyr::pivot_wider(
      names_from = "wave", values_from = "symbol",
      names_prefix = "w", names_sort = TRUE
    )

  out <- as.data.frame(grid)
  rownames(out) <- out$module
  out$module <- NULL
  print(out, row.names = TRUE)
  invisible(out)
}
