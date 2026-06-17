#' interactively select modules, waves, and file types
#'
#' presents a series of interactive menus to choose which modules, waves,
#' and file types to include in a download. the result can be passed
#' directly to [liss_download()].
#'
#' @return a tibble suitable for [liss_download()], or `NULL` if the user
#'   cancels at any step.
#' @export
#' @examples
#' \dontrun{
#' selection <- liss_select()
#' liss_download(selection)
#' }
liss_select <- function() {
  if (exists("blueprint", envir = .liss_cache)) {
    bp <- .liss_cache$blueprint
  } else {
    bp <- liss_blueprint()
  }

  # step 1: select modules
  all_mods <- sort(unique(bp$module))
  cli::cli_alert_info(
    "{length(all_mods)} module(s) available. Select one or more, or 0 to cancel."
  )
  sel_mods <- utils::select.list(all_mods, multiple = TRUE, title = "Select module(s)")
  if (length(sel_mods) == 0) {
    cli::cli_alert_info("No modules selected.")
    return(invisible(NULL))
  }

  # step 2: select waves
  bp_filtered <- dplyr::filter(bp, .data$module %in% sel_mods)
  available_waves <- sort(unique(bp_filtered$wave))
  cli::cli_alert_info("Available waves: {min(available_waves)}-{max(available_waves)}")
  wave_input <- readline("Enter waves (e.g., 1:5, 1,3,7, or 'all'): ")

  # strip surrounding quotes and whitespace so both all and 'all' work
  wave_clean <- gsub("^[\"']+|[\"']+$", "", trimws(wave_input))

  if (tolower(wave_clean) == "all") {
    sel_waves <- available_waves
  } else {
    sel_waves <- tryCatch(
      eval(parse(text = paste0("c(", wave_input, ")"))),
      error = function(e) {
        cli::cli_alert_danger("Could not parse wave input.")
        NULL
      }
    )
  }
  if (is.null(sel_waves) || length(sel_waves) == 0) {
    cli::cli_alert_info("No waves selected.")
    return(invisible(NULL))
  }
  sel_waves <- sort(unique(as.integer(sel_waves)))

  invalid <- setdiff(sel_waves, available_waves)
  if (length(invalid) > 0) {
    cli::cli_alert_warning("Waves not available: {paste(invalid, collapse = ', ')}")
    sel_waves <- intersect(sel_waves, available_waves)
  }

  # step 3: check coverage
  presence <- bp_filtered %>%
    dplyr::filter(.data$wave %in% sel_waves) %>%
    dplyr::distinct(.data$module, .data$wave)
  expected <- tidyr::expand_grid(module = sel_mods, wave = sel_waves)
  missing  <- dplyr::anti_join(expected, presence, by = c("module", "wave"))

  if (nrow(missing) > 0) {
    cli::cli_alert_warning("Some modules are not available in all selected waves:")
    missing_summary <- missing %>%
      dplyr::group_by(.data$module) %>%
      dplyr::summarise(
        waves = paste0("w", sort(.data$wave), collapse = ", "),
        .groups = "drop"
      )
    for (k in seq_len(nrow(missing_summary))) {
      mod_name  <- missing_summary$module[k]
      mod_waves <- missing_summary$waves[k]
      cli::cli_bullets(c("!" = "{mod_name}: missing {mod_waves}"))
    }
    if (!isTRUE(utils::askYesNo("Continue with available files only?"))) {
      cli::cli_alert_info("Selection cancelled.")
      return(invisible(NULL))
    }
  }

  # step 4: filter to selected modules and waves
  result <- bp_filtered %>%
    dplyr::filter(.data$wave %in% sel_waves) %>%
    dplyr::arrange(.data$module, .data$wave, .data$type)

  # step 5: select file types
  type_map <- c(
    "SPSS (.sav)"        = "\\.sav$",
    "Stata (.dta)"       = "\\.dta$",
    "Codebook (English)" = "EN.*\\.pdf$",
    "Codebook (Dutch)"   = "NL.*\\.pdf$"
  )
  available_types <- purrr::keep(
    type_map,
    function(p) any(grepl(p, result$file, ignore.case = TRUE))
  )
  if (length(available_types) == 0) {
    cli::cli_alert_warning("No recognized file types found in selection.")
    return(invisible(NULL))
  }

  cli::cli_alert_info("Select which file type(s) to include, or 0 to cancel.")
  sel_types <- utils::select.list(
    names(available_types), multiple = TRUE, title = "Select file type(s)"
  )
  if (length(sel_types) == 0) {
    cli::cli_alert_info("No file types selected.")
    return(invisible(NULL))
  }

  combined <- paste(unname(available_types[sel_types]), collapse = "|")
  result <- dplyr::filter(result, grepl(combined, .data$file, ignore.case = TRUE))

  if (nrow(result) == 0) {
    cli::cli_alert_warning("No files match the selected types.")
    return(invisible(NULL))
  }

  n_files <- nrow(result)
  n_mods  <- dplyr::n_distinct(result$module)
  n_waves <- dplyr::n_distinct(result$wave)
  types_str <- paste(sel_types, collapse = ", ")
  cli::cli_alert_success(
    "Selected {n_files} file(s) across {n_mods} module(s) and {n_waves} wave(s) [{types_str}]"
  )
  cli::cli_alert_info("Use {.code liss_download(selection)} to download.")

  result
}
