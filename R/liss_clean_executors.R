# ============================================================================
# liss_clean_executors.R, kernels for the income-cleaning contract
# ============================================================================
# pure, side-effect-free computational kernels for the income-cleaning
# pipeline: magnitude and volatility signatures, robust univariate
# detectors and their consensus, candidate generation (temporal
# smoothing, donor matching, bound mapping) and candidate selection,
# and the income equivalisation scale.
#
# these are deliberately base-R and data-frame-agnostic so they are unit
# testable without haven, dplyr, or .sav data. the orchestrator
# (liss_clean_income.R) resolves columns, walks households, and writes
# the audit ledger; the numeric contract lives here.
#
# relies on `%||%` defined in liss_executors.R (same package namespace).

# ---- basic statistics --------------------------------------------------

# finite (non-NA, non-NaN, non-infinite) values of a numeric vector
finite_vals <- function(x) x[is.finite(x)]

# most frequent finite value; NA_real_ when none. first-seen wins ties,
# which keeps repeated runs deterministic.
stat_mode_num <- function(x) {
  x <- finite_vals(x)
  if (length(x) == 0) return(NA_real_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ---- magnitude and volatility signatures -------------------------------

# order-of-magnitude of each value, same length as the input. NA for
# non-finite or non-positive entries (the legacy implementation dropped
# those entries and returned a shorter vector, which misaligned the
# write-back whenever a household contained a zero income; returning a
# full-length vector removes that defect). mode "nearest" rounds a value
# up to the next power of ten when it exceeds cutoff * 10 times its
# floor magnitude (default: ratio above 7.5 rounds up).
power10_magnitude <- function(x, mode = c("nearest", "floor", "ceiling"),
                              cutoff = 0.75) {
  mode <- match.arg(mode)
  out <- rep(NA_real_, length(x))
  ok  <- is.finite(x) & x > 0
  if (!any(ok)) return(out)
  v <- x[ok]
  fl <- 10^trunc(log10(v))
  ce <- 10^ceiling(log10(v))
  out[ok] <- switch(mode,
    "floor"   = fl,
    "ceiling" = ce,
    "nearest" = ifelse(v / fl > cutoff * 10, ce, fl)
  )
  out
}

# local log-volatility of a series: for each finite value, the mean
# absolute log-ratio of consecutive finite pairs in a centered window of
# its immediate finite neighbours (previous and next finite value).
# positions with fewer than two finite values in the window get 0, as do
# non-finite positions. the window walks the NA-omitted series, so gaps
# do not dilute the signal. values are rounded to two significant
# digits, matching the thresholds the detection rules were tuned on.
local_log_volatility <- function(x) {
  out <- rep(0, length(x))
  fin <- which(is.finite(x))
  if (length(fin) < 2) return(out)
  v <- x[fin]
  n <- length(v)
  for (j in seq_len(n)) {
    lo <- max(1L, j - 1L)
    hi <- min(n, j + 1L)
    k <- v[lo:hi]
    if (length(k) < 2) next
    ratios <- log(k[-1] / k[-length(k)])
    ratios <- ratios[is.finite(ratios)]
    if (length(ratios) == 0) next
    out[fin[j]] <- signif(mean(abs(ratios)), 2)
  }
  out
}

# MAD-based modified z-score (Iglewicz & Hoaglin 1993; Leys et al.
# 2013). NA propagates; a zero MAD falls back to the SD, and a zero SD
# yields all-zero scores.
robust_zscore <- function(x) {
  med <- stats::median(x, na.rm = TRUE)
  mad_val <- stats::mad(x, constant = 1.4826, na.rm = TRUE)
  if (!is.finite(mad_val) || mad_val == 0) mad_val <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(mad_val) || mad_val == 0) return(rep(0, length(x)))
  (x - med) / mad_val
}

# ---- robust univariate detectors ---------------------------------------

# flag univariate outliers by tukey IQR fences, mean/SD z interval, or
# MAD interval. returns a full-length logical (FALSE at non-finite
# positions) carrying `bounds`, `method`, and `threshold` attributes.
# fewer than four finite values flags nothing.
detect_univariate_outliers <- function(x, method = c("iqr", "mad", "zscore"),
                                       threshold = NULL) {
  method <- match.arg(method)
  x_clean <- finite_vals(x)
  empty <- rep(FALSE, length(x))
  attr(empty, "bounds") <- c(lower = NA_real_, upper = NA_real_)
  attr(empty, "method") <- method
  if (length(x_clean) < 4) return(empty)

  if (is.null(threshold)) threshold <- if (method == "iqr") 1.5 else 2.5

  bounds <- switch(method,
    "iqr" = {
      q <- unname(stats::quantile(x_clean, c(0.25, 0.75)))
      iqr <- diff(q)
      c(lower = q[1] - threshold * iqr, upper = q[2] + threshold * iqr)
    },
    "zscore" = {
      m <- mean(x_clean)
      s <- stats::sd(x_clean)
      c(lower = m - threshold * s, upper = m + threshold * s)
    },
    "mad" = {
      med <- stats::median(x_clean)
      mad_val <- stats::mad(x_clean, constant = 1.4826)
      c(lower = med - threshold * mad_val, upper = med + threshold * mad_val)
    }
  )

  is_outlier <- is.finite(x) & (x < bounds["lower"] | x > bounds["upper"])
  attr(is_outlier, "bounds") <- bounds
  attr(is_outlier, "method") <- method
  attr(is_outlier, "threshold") <- threshold
  is_outlier
}

# run several detectors and combine: an observation is flagged when at
# least `consensus` methods agree (consensus = 1 is the union).
# `thresholds` optionally overrides the per-method threshold by name.
detect_outliers_consensus <- function(x, methods = c("iqr", "mad"),
                                      consensus = 1, thresholds = NULL) {
  results <- lapply(methods, function(m) {
    detect_univariate_outliers(x, method = m,
                               threshold = thresholds[[m]] %||% NULL)
  })
  names(results) <- methods
  counts <- Reduce(`+`, lapply(results, as.integer))
  list(
    outliers  = counts >= consensus,
    counts    = counts,
    by_method = results,
    bounds    = lapply(results, function(r) attr(r, "bounds"))
  )
}

# ---- candidate generators ----------------------------------------------

# weighted moving-average imputation of a single position, equivalent to
# imputeTS::na_ma at that position with the target masked. the window
# half-width `k` is widened until at least one finite neighbour enters
# it (or the series is exhausted). linear weighting gives offset d the
# weight k_eff + 1 - d; simple weighting is unweighted. returns
# NA_real_ when the rest of the series holds fewer than two finite
# values (too little support for a trend-based candidate).
wma_impute_at <- function(x, i, k = 2, weighting = c("linear", "simple")) {
  weighting <- match.arg(weighting)
  n <- length(x)
  if (i < 1 || i > n) return(NA_real_)
  x[i] <- NA_real_
  if (length(finite_vals(x)) < 2) return(NA_real_)

  k_eff <- max(1L, as.integer(k))
  repeat {
    offs <- setdiff(seq.int(-k_eff, k_eff), 0L)
    pos  <- i + offs
    keep <- pos >= 1 & pos <= n
    pos  <- pos[keep]
    offs <- offs[keep]
    fin  <- is.finite(x[pos])
    if (any(fin)) {
      pos  <- pos[fin]
      offs <- offs[fin]
      w <- if (weighting == "linear") k_eff + 1 - abs(offs) else rep(1, length(offs))
      return(sum(w * x[pos]) / sum(w))
    }
    if (k_eff >= n) return(NA_real_)
    k_eff <- k_eff + 1L
  }
}

# match a target row against a donor frame on a hierarchy of keys and
# aggregate the donor values. starting from all rows with a finite
# value, each key narrows the pool only when at least `min_donors`
# matches survive; keys with a missing target value are skipped. the
# target row itself is always excluded (the legacy implementation let a
# flagged value donate to itself). returns NA_real_ when no donor
# remains.
donor_pool_value <- function(target_row, data, value_col, key_cols,
                             aggregate = stats::median, min_donors = 1,
                             exclude_row = NA_integer_) {
  if (!value_col %in% names(data)) return(NA_real_)
  base <- which(is.finite(data[[value_col]]))
  if (!is.na(exclude_row)) base <- setdiff(base, exclude_row)
  if (length(base) < min_donors) return(NA_real_)

  key_cols <- intersect(key_cols, intersect(names(target_row), names(data)))
  for (key in key_cols) {
    tv <- target_row[[key]][1]
    if (is.na(tv)) next
    k <- which(!is.na(data[[key]]) & data[[key]] == tv)
    narrowed <- intersect(base, k)
    if (length(narrowed) >= min_donors) base <- narrowed
  }
  if (length(base) == 0) return(NA_real_)
  aggregate(data[[value_col]][base], na.rm = TRUE)
}

# expand bracket codes (1..K) to euro bounds. codes outside 1..K (and
# NA) map to NA on both sides.
category_bounds_from_codes <- function(codes, lower, upper) {
  codes <- as.numeric(codes)
  idx <- match(codes, seq_along(lower))
  list(lower = lower[idx], upper = upper[idx])
}

# ---- candidate filtering and selection ----------------------------------

# keep finite candidates inside [valid_min, valid_max], de-duplicating
# by value with the earliest-generated source winning (the deterministic
# tie-break the ruleset documents).
filter_candidates <- function(values, sources, valid_min, valid_max) {
  keep <- is.finite(values) & values >= valid_min & values <= valid_max
  values <- values[keep]
  sources <- sources[keep]
  dup <- duplicated(values)
  list(values = values[!dup], sources = sources[!dup])
}

# pick the candidate closest to the anchor; first minimum wins ties.
select_candidate <- function(values, sources, anchor) {
  if (length(values) == 0) {
    return(list(value = NA_real_, source = NA_character_,
                distance = NA_real_))
  }
  d <- abs(values - anchor)
  j <- which.min(d)
  list(value = values[j], source = sources[j], distance = d[j])
}

# relative deviation of a value from its category bounds, used to rank
# bound violations (largest deviation corrected first). values inside
# the bounds score 1; non-positive values score Inf.
bound_deviation_ratio <- function(val, min_b, max_b) {
  if (!is.finite(val) || val <= 0) return(Inf)
  if (is.finite(max_b) && val > max_b) return(val / max_b)
  if (is.finite(min_b) && min_b > 0 && val < min_b) return(min_b / val)
  1
}

# ---- income equivalisation ----------------------------------------------

# convert household income to a per-equivalent-adult scale.
#   weighted_sqrt : income / (adults + child_weight * children)^elasticity
#                   (the scale used by the source analysis pipelines,
#                   with child_weight 0.8 and elasticity 0.5)
#   oecd_modified : income / (1 + 0.5 * (adults - 1) + 0.3 * children)
#   sqrt          : income / sqrt(household_size)
# rows with household_size < 1, n_children < 0, n_children >=
# household_size (zero adults), or non-finite sizes yield NA: every
# scale presumes at least one adult, and a zero-adult composition can
# push the oecd_modified divisor below 1 (equivalised income above the
# household total). composition vectors must be length 1 or match the
# income length; silent recycling of intermediate lengths is an error.
equivalise_income_kernel <- function(income, household_size, n_children = 0,
                                     scale = c("weighted_sqrt",
                                               "oecd_modified", "sqrt"),
                                     child_weight = 0.8, elasticity = 0.5) {
  scale <- match.arg(scale)
  n <- length(income)
  for (nm in c("household_size", "n_children")) {
    len <- length(get(nm))
    if (!len %in% c(1L, n)) {
      stop("equivalise: ", nm, " has length ", len,
           "; must be 1 or length(income) = ", n, call. = FALSE)
    }
  }
  household_size <- rep_len(as.numeric(household_size), n)
  n_children <- rep_len(as.numeric(n_children), n)
  adults <- household_size - n_children

  bad <- !is.finite(household_size) | !is.finite(n_children) |
    household_size < 1 | n_children < 0 | adults < 1

  divisor <- switch(scale,
    "weighted_sqrt" = (adults + child_weight * n_children)^elasticity,
    "oecd_modified" = 1 + 0.5 * (adults - 1) + 0.3 * n_children,
    "sqrt"          = sqrt(household_size)
  )
  out <- as.numeric(income) / divisor
  out[bad] <- NA_real_
  out
}
