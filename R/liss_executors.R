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
