#' parse interactive wave input without eval() (internal)
#'
#' accepts comma-separated integers and a:b ranges (e.g. "1:5", "1,3,7",
#' "2:4, 9"); anything else returns NULL. replaces the former
#' eval(parse(text = ...)) on raw user input.
#' @noRd
parse_wave_input <- function(x) {
  x <- gsub("[[:space:]]", "", x %||% "")
  if (!nzchar(x)) return(integer(0))
  parts <- strsplit(x, ",", fixed = TRUE)[[1]]
  if (!length(parts) || !all(grepl("^[0-9]+(:[0-9]+)?$", parts))) return(NULL)
  out <- unlist(lapply(parts, function(p) {
    if (grepl(":", p, fixed = TRUE)) {
      ab <- as.integer(strsplit(p, ":", fixed = TRUE)[[1]])
      seq.int(ab[1], ab[2])
    } else {
      as.integer(p)
    }
  }))
  sort(unique(out))
}

# calendar ranges use successive months, including December to January
parse_month_input <- function(x) {
  x <- gsub("[[:space:]]", "", x %||% "")
  if (!nzchar(x)) return(integer(0))
  parts <- strsplit(x, ",", fixed = TRUE)[[1]]
  if (!length(parts) || !all(grepl("^[0-9]{6}(:[0-9]{6})?$", parts))) return(NULL)
  values <- unlist(strsplit(parts, ":", fixed = TRUE))
  values <- as.integer(values)
  if (any(values < 100001L | !values %% 100L %in% 1:12)) return(NULL)
  out <- unlist(lapply(parts, function(p) {
    endpoints <- as.integer(strsplit(p, ":", fixed = TRUE)[[1]])
    indices <- endpoints %/% 100L * 12L + endpoints %% 100L - 1L
    if (length(indices) == 2L) indices <- seq.int(indices[1], indices[2])
    indices %/% 12L * 100L + indices %% 12L + 1L
  }))
  sort(unique(as.integer(out)))
}

# interactive I/O is separate from the selection rules
.liss_select_list <- function(...) utils::select.list(...)
.liss_readline <- function(...) readline(...)

# background document language tokens must not match substrings in Dutch names
.liss_match_file_type <- function(pattern, files, module_id) {
  matches <- grepl(pattern, files, ignore.case = TRUE)
  background <- module_id == 322L
  if (pattern %in% c("EN.*\\.pdf$", "NL.*\\.pdf$")) {
    language <- if (pattern == "EN.*\\.pdf$") "EN" else "NL"
    matches[background] <- grepl(paste0("_", language, "([_. -].*)?\\.pdf$"),
      files[background], ignore.case = TRUE)
  }
  matches
}

.liss_select_periods <- function(available, monthly = FALSE) {
  available <- sort(unique(available[!is.na(available)]))
  if (!length(available)) {
    cli::cli_alert_warning("No dated releases available.")
    return(NULL)
  }
  if (monthly) {
    cli::cli_alert_info("Available Background Variables months: {paste(available, collapse = ', ')}")
    input <- .liss_readline("Enter months (YYYYMM, e.g., 202511, 202512:202601, or 'all'): ")
  } else {
    cli::cli_alert_info("Available waves: {min(available)}-{max(available)}")
    input <- .liss_readline("Enter waves (e.g., 1:5, 1,3,7, or 'all'): ")
  }
  clean <- gsub("^[\"']+|[\"']+$", "", trimws(input))
  if (tolower(clean) == "all") {
    selected <- available
  } else {
    selected <- if (monthly) parse_month_input(clean) else parse_wave_input(clean)
  }
  if (is.null(selected) || !length(selected)) {
    cli::cli_alert_warning("No valid {if (monthly) 'months' else 'waves'} selected.")
    return(NULL)
  }
  invalid <- setdiff(selected, available)
  if (length(invalid)) {
    cli::cli_alert_warning("{if (monthly) 'Months' else 'Waves'} not available: {paste(invalid, collapse = ', ')}")
  }
  selected <- intersect(selected, available)
  if (!length(selected)) return(NULL)
  sort(unique(as.integer(selected)))
}

#' interactively select modules, waves, and file types
#'
#' presents a series of interactive menus to choose which modules, waves,
#' and file types to include in a download. the result can be passed
#' directly to [liss_download()]. Background Variables months (YYYYMM)
#' are selected separately from core wave numbers. Its undated documents
#' remain available at the file-type step. ZIP archives are explicit choices;
#' all listed languages and release versions remain available, without
#' preferring a version or choosing a month automatically.
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
  sel_mods <- .liss_select_list(all_mods, multiple = TRUE, title = "Select module(s)")
  if (length(sel_mods) == 0) {
    cli::cli_alert_info("No modules selected.")
    return(invisible(NULL))
  }

  # step 2: select core waves and background months independently
  bp_filtered <- dplyr::filter(bp, .data$module %in% sel_mods)
  core <- dplyr::filter(bp_filtered, .data$module_id != 322L)
  background <- dplyr::filter(bp_filtered, .data$module_id == 322L)
  sel_waves <- integer(0)
  sel_months <- integer(0)
  if (nrow(core)) {
    sel_waves <- .liss_select_periods(core$wave)
    if (is.null(sel_waves)) return(invisible(NULL))
  }
  if (nrow(background)) {
    if (all(is.na(background$wave))) {
      cli::cli_alert_info("Only undated Background Variables documents are available.")
    } else {
      sel_months <- .liss_select_periods(background$wave, monthly = TRUE)
      if (is.null(sel_months)) return(invisible(NULL))
    }
  }

  # step 3: check coverage
  presence <- core %>%
    dplyr::filter(.data$wave %in% sel_waves) %>%
    dplyr::distinct(.data$module, .data$wave)
  expected <- tidyr::expand_grid(module = unique(core$module), wave = sel_waves)
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

  # step 4: undated background documents are optional at the type step
  result <- dplyr::bind_rows(
    dplyr::filter(core, .data$wave %in% sel_waves),
    dplyr::filter(background, .data$wave %in% sel_months | is.na(.data$wave))
  ) %>% dplyr::arrange(.data$module, .data$wave, .data$type)

  # step 5: select file types
  type_map <- c(
    "SPSS (.sav)"        = "\\.sav$",
    "Stata (.dta)"       = "\\.dta$",
    "Codebook (English)" = "EN.*\\.pdf$",
    "Codebook (Dutch)"   = "NL.*\\.pdf$"
  )
  if (nrow(background)) {
    type_map <- c(type_map, "ZIP archives (.zip)" = "\\.zip$",
                  "Documents (.pdf)" = "\\.pdf$")
  }
  available_types <- purrr::keep(
    type_map,
    function(p) any(.liss_match_file_type(p, result$file, result$module_id))
  )
  if (length(available_types) == 0) {
    cli::cli_alert_warning("No recognized file types found in selection.")
    return(invisible(NULL))
  }

  cli::cli_alert_info("Select which file type(s) to include, or 0 to cancel.")
  sel_types <- .liss_select_list(
    names(available_types), multiple = TRUE, title = "Select file type(s)"
  )
  if (length(sel_types) == 0) {
    cli::cli_alert_info("No file types selected.")
    return(invisible(NULL))
  }

  selected_files <- Reduce(`|`, lapply(unname(available_types[sel_types]),
    function(p) .liss_match_file_type(p, result$file, result$module_id)))
  result <- dplyr::filter(result, selected_files)

  if (nrow(result) == 0) {
    cli::cli_alert_warning("No files match the selected types.")
    return(invisible(NULL))
  }

  n_files <- nrow(result)
  n_mods  <- dplyr::n_distinct(result$module)
  n_waves <- dplyr::n_distinct(result$wave, na.rm = TRUE)
  types_str <- paste(sel_types, collapse = ", ")
  cli::cli_alert_success(
    "Selected {n_files} file(s) across {n_mods} module(s) and {n_waves} wave/month code(s) [{types_str}]"
  )
  cli::cli_alert_info("Use {.code liss_download(selection)} to download.")

  result
}
