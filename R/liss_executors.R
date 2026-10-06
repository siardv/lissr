# ============================================================================
# liss_executors.R, executor kernels for the cross-wave merge contract
# ============================================================================
# pure, side-effect-free computational kernels for the three payload families
# of the cross-wave merge: crosswalk + coverage check,
# the derived_variables superset aggregation, and the transform action.
#
# these are deliberately base-R and data-frame-agnostic so they are unit
# testable without haven, dplyr, or .sav data. the engine sources this file and
# the dispatch / merge_liss_module wire the kernels to resolved columns. column
# resolution (find_col / resolve_var_target), wave-aware source selection, and
# output writing live in the engine; the numeric contract lives here.

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- crosswalk ---------------------------------------------------

# map a source vector through a single from->to mapping; unmapped -> NA.
# `mapping` is a named list/vector whose names are source codes (as strings).
crosswalk_map <- function(x, mapping) {
  if (length(mapping) == 0) return(rep(NA_real_, length(x)))
  src  <- as.character(x)
  out  <- rep(NA_real_, length(x))
  hit  <- !is.na(src) & src %in% names(mapping)
  out[hit] <- as.numeric(unlist(mapping[src[hit]], use.names = FALSE))
  out
}

# character sibling of crosswalk_map for string-valued maps (cp DV08 long/short).
# unmapped and NA inputs resolve to NA_character_; names are source codes.
crosswalk_map_chr <- function(x, mapping) {
  if (length(mapping) == 0) return(rep(NA_character_, length(x)))
  src <- as.character(x)
  out <- rep(NA_character_, length(x))
  hit <- !is.na(src) & src %in% names(mapping)
  out[hit] <- as.character(unlist(mapping[src[hit]], use.names = FALSE))
  out
}

# multi-scheme crosswalk (cw pattern): per-row scheme picks the scheme_N mapping.
crosswalk_map_scheme <- function(x, scheme, crosswalk) {
  out <- rep(NA_real_, length(x))
  for (s in unique(scheme[!is.na(scheme)])) {
    m <- crosswalk[[paste0("scheme_", s)]]
    if (is.null(m)) next
    rows <- which(!is.na(scheme) & scheme == s)
    out[rows] <- crosswalk_map(x[rows], m)
  }
  out
}

# per-wave coverage check: excess = codes non-NA in source that went unmapped.
# excess > 0 is reported at severity error. same logic for label_to_string.
crosswalk_coverage <- function(x_source, x_mapped) {
  na_before <- sum(is.na(x_source))
  na_after  <- sum(is.na(x_mapped))
  unmapped  <- sort(unique(x_source[!is.na(x_source) & is.na(x_mapped)]))
  list(na_before      = na_before,
       na_after       = na_after,
       excess         = na_after - na_before,
       unmapped_codes = unmapped,
       severity       = if (na_after - na_before > 0) "error" else "ok")
}

# ---- derived_variables superset ----------------------------------

# aggregate a list of equal-length numeric source vectors by `method`.
# default missing_as_zero = FALSE -> na.rm = TRUE, all-NA row resolves to NA
# (matches the default). TRUE -> missing components contribute 0 (ca financial totals).
#
# KNOWN LIMITATION (contract II.2): under the numeric -7 scheme, -7 has already
# been recoded to NA before this runs, so missing_as_zero cannot distinguish
# structural from respondent NA. this kernel treats every NA identically; the
# limitation is a property of the numeric scheme, not something the kernel hides.
dv_aggregate <- function(sources_list, method = "sum", missing_as_zero = FALSE) {
  if (length(sources_list) == 0) return(numeric(0))
  M <- do.call(cbind, lapply(sources_list, as.numeric))
  res <- switch(method,
    "direct"   = M[, 1],
    "coalesce" = apply(M, 1, function(r) { v <- r[!is.na(r)]; if (length(v)) v[1] else NA_real_ }),
    "sum" = if (missing_as_zero) {
              Mz <- M; Mz[is.na(Mz)] <- 0; rowSums(Mz)
            } else {
              s <- rowSums(M, na.rm = TRUE)
              s[rowSums(!is.na(M)) == 0] <- NA_real_
              s
            },
    "max" = if (missing_as_zero) {
              Mz <- M; Mz[is.na(Mz)] <- 0; apply(Mz, 1, max)
            } else {
              apply(M, 1, function(r) { v <- r[!is.na(r)]; if (length(v)) max(v) else NA_real_ })
            },
    # presence flag: 1 where any source is non-NA (item was asked / answered),
    # 0 otherwise (routed out / not applicable). never NA, so missing_as_zero
    # does not apply.
    "presence" = as.numeric(rowSums(!is.na(M)) > 0),
    stop("unknown derived_variables method: ", method)
  )
  as.numeric(res)
}

# coerce a derived vector to its declared output_type (no DV is text
# unless explicitly character; totals/counts integer, amounts double).
dv_coerce_output <- function(x, output_type = NULL) {
  if (is.null(output_type)) return(x)
  switch(output_type,
    "integer"   = as.integer(round(x)),
    "double"    = as.numeric(x),
    "character" = as.character(x),
    x)
}

# valid_range is a post-CHECK, not a mutation: count out-of-range values.
range_check <- function(x, valid_range = NULL) {
  if (is.null(valid_range) || length(valid_range) != 2) return(0L)
  sum(!is.na(x) & (x < valid_range[[1]] | x > valid_range[[2]]))
}

# ---- transform ---------------------------------------------------

# per-wave scalar offset; op in subtract | add | identity. subtract is the
# legacy standalone action re-expressed here (one offset mechanism).
transform_apply <- function(x, op = "identity", value = NULL) {
  v <- if (is.null(value)) NA_real_ else as.numeric(value)
  switch(op,
    "subtract"   = x - v,
    "add"        = x + v,
    "int_divide" = x %/% v,
    "modulo"     = x %% v,
    "identity"   = x,
    stop("unknown transform op: ", op))
}

# ---- calendar year-month (yyyymm) --------------------------------

# strict parser behind the derive_fieldwork_month action. this is a date
# parser, not arithmetic: transform_apply() stays generic, and a value becomes
# a year-month only here, never by modulo or integer division alone.
#
# supported input: a numeric (double or integer), character or logical
# vector; a factor is read by its labels and a haven labelled vector by its
# values. anything else (a list, complex, raw, a date or other classed
# object, a matrix) is unsupported: nothing is coerced and every cell is NA.
# the parser sees a column as the caller holds it: text that an earlier step
# has already turned into numbers is judged as numbers (the
# derive_fieldwork_month action refuses such a source, see there).
#
# accepted forms: an integer-valued number, or text of exactly six digits
# (surrounding whitespace ignored). nothing is rounded, truncated, repaired
# or inferred from another column.
#
# each cell gets one class, first match wins:
#   source_missing    NA or NaN (blank text counts as missing)
#   declared_missing  a declared spss user-missing code (na_values) or a value
#                     inside a declared na_range
#   invalid           not a six-digit yyyymm with month 01-12
#   outside_window    a calendar yyyymm whose year lies outside `year_range`
#   valid             a calendar yyyymm inside the window
# calendar form and the year window are separate tests. the window is a
# plausibility policy, not part of calendar validity: it keeps code-like
# values that happen to carry a valid month (999912, 100001) out of a date
# column. it is fixed, deliberately wide, and not derived from any wave,
# wave_year or release.
#
# `na_range` is one c(low, high) pair or a list of such pairs. returns the
# parsed double vector (NA unless valid), the per-cell class and the class
# counts, which sum to length(x).
parse_yyyymm <- function(x, na_values = NULL, na_range = NULL,
                         year_range = c(1900, 2100)) {
  stopifnot(is.numeric(year_range), length(year_range) == 2L,
            !anyNA(year_range), all(is.finite(year_range)),
            all(year_range == floor(year_range)), year_range[[1]] <= year_range[[2]])
  classes <- c("valid", "source_missing", "declared_missing", "invalid",
               "outside_window", "unsupported")
  result <- function(value, class) {
    counts <- as.integer(table(factor(class, levels = classes)))
    names(counts) <- classes
    list(value = value, class = class, counts = counts)
  }
  if (is.factor(x)) x <- as.character(x)
  if (inherits(x, "haven_labelled")) x <- unclass(x)
  n <- length(x)
  supported <- is.atomic(x) && !is.object(x) && is.null(dim(x)) &&
    (is.numeric(x) || is.character(x) || is.logical(x))
  if (!supported) return(result(rep(NA_real_, n), rep("unsupported", n)))

  if (is.character(x)) {
    txt <- trimws(x)
    missing <- is.na(txt) | !nzchar(txt)
    integral <- !missing & grepl("^[0-9]{6}$", txt)
    num <- suppressWarnings(as.numeric(txt))
  } else {
    txt <- NULL
    num <- as.numeric(x)
    missing <- is.na(num)
    integral <- !missing & is.finite(num) & num == floor(num)
  }

  declared <- rep(FALSE, n)
  nav <- suppressWarnings(as.numeric(na_values))
  nav <- nav[!is.na(nav)]
  if (length(nav) > 0) declared <- declared | (!is.na(num) & num %in% nav)
  if (!is.null(txt) && length(na_values) > 0)
    declared <- declared | (txt %in% trimws(as.character(na_values)))
  for (r in (if (is.list(na_range)) na_range else list(na_range))) {
    r <- suppressWarnings(as.numeric(r))
    if (length(r) == 2L && !anyNA(r))
      declared <- declared | (!is.na(num) & num >= r[[1]] & num <= r[[2]])
  }

  # month and year are taken only from six-digit values, so no arithmetic is
  # done on huge or infinite numbers
  six <- integral & !is.na(num) & num >= 100001 & num <= 999912
  ym <- ifelse(six, num, 0)
  calendar <- six & ym %% 100 >= 1 & ym %% 100 <= 12
  in_window <- calendar & ym %/% 100 >= year_range[[1]] &
    ym %/% 100 <= year_range[[2]]
  class <- rep("invalid", n)
  class[calendar & !in_window] <- "outside_window"
  class[calendar & in_window] <- "valid"
  class[declared] <- "declared_missing"
  class[missing] <- "source_missing"
  value <- rep(NA_real_, n)
  ok <- class == "valid"
  value[ok] <- num[ok]
  result(value, class)
}

# spss user-missing declarations of a column: the live haven attributes and
# the stash that the to_numeric labelled policy leaves behind. both stores
# are read and their union applies; neither takes precedence, so a value
# declared missing in either one is missing.
yyyymm_declarations <- function(x) {
  list(
    na_values = c(attr(x, "_original_na_values", exact = TRUE),
                  attr(x, "na_values", exact = TRUE)),
    na_range = Filter(Negate(is.null),
                      list(attr(x, "_original_na_range", exact = TRUE),
                           attr(x, "na_range", exact = TRUE))))
}

# audit counts for a rule that replaces a target column by parsed
# year-months. the old column is read as it is, with its own declarations
# and without coercion: a cell is missing when it is NA or declared missing.
# a row is unchanged when old and new are both missing, or when the old cell
# is the same year-month in an accepted form; every other row is a change.
yyyymm_replacement_counts <- function(old, new) {
  n <- length(new)
  decl <- yyyymm_declarations(old)
  p <- parse_yyyymm(old, decl$na_values, decl$na_range)
  if (length(p$value) != n || p$counts[["unsupported"]] > 0L) {
    # an unsupported old column: only its own NA test is trusted
    old_missing <- tryCatch(is.na(old), error = function(e) NULL)
    if (!is.logical(old_missing) || length(old_missing) != n)
      old_missing <- rep(FALSE, n)
    old_value <- rep(NA_real_, n)
  } else {
    old_missing <- p$class %in% c("source_missing", "declared_missing")
    old_value <- p$value
  }
  same <- (old_missing & is.na(new)) |
    (!is.na(old_value) & !is.na(new) & old_value == new)
  kept <- if (is.list(old)) old[!old_missing] else as.character(old)[!old_missing]
  list(values_changed = sum(!same),
       values_lost = sum(!old_missing & is.na(new)),
       na_before = sum(old_missing),
       na_after = sum(is.na(new)),
       distinct_before = length(unique(kept)),
       distinct_after = length(unique(new[!is.na(new)])))
}
