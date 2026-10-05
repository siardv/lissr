# ============================================================================
# residual user-missing sweep disclosure (NA_SWEEP details): exact grouping,
# exact report and JSONL tokens under a decimal-comma OutDec or LC_NUMERIC,
# untouched other log fields (class-json text included), zero-hit and integer
# controls, unchanged sweep data. expectations come from oracles that
# do not share the implementation: an identical()-based grouping loop, a
# per-cell predicate loop, tokens computed outside R (python "%.17g"), and
# jsonlite's C strtod for read-back. table() is never its own reference.
# ============================================================================

# ---- oracles and helpers -----------------------------------------------------

na_sweep_registry <- function(sets) {
  registry <- new.env(parent = emptyenv())
  for (s in sets) {
    registry[[s$col]] <- c(registry[[s$col]],
                           list(list(labels = NULL, na_values = s$na_values,
                                     na_range = s$na_range, wave = s$wave,
                                     vlab = NULL)))
  }
  registry
}

# per-cell restatement of the documented rule: declaring wave only (all rows
# for a set without wave), declared value or inside the inclusive range
oracle_hits <- function(x, waves, sets) {
  vapply(seq_along(x), function(i) {
    if (is.na(x[i])) return(FALSE)
    for (s in sets) {
      if (!is.na(s$wave) && !identical(waves[i], s$wave)) next
      if (any(x[i] == s$na_values)) return(TRUE)
      if (length(s$na_range) == 2 &&
          x[i] >= s$na_range[1] && x[i] <= s$na_range[2]) return(TRUE)
    }
    FALSE
  }, logical(1))
}

# grouping by identical() wave and == code in a plain loop (no table(),
# unique() or match()); rows sorted by wave, then code
oracle_details <- function(col, codes, waves) {
  key_w <- character(0)
  key_c <- numeric(0)
  n <- integer(0)
  for (i in seq_along(codes)) {
    k <- 0L
    for (j in seq_along(key_c)) {
      if (identical(key_w[j], waves[i]) && key_c[j] == codes[i]) {
        k <- j
        break
      }
    }
    if (k == 0L) {
      key_w <- c(key_w, waves[i])
      key_c <- c(key_c, as.double(codes[i]))
      n <- c(n, 1L)
    } else {
      n[k] <- n[k] + 1L
    }
  }
  o <- order(key_w, key_c, method = "radix")
  data.frame(column = rep(col, length(o)), wave = key_w[o], code = key_c[o],
             n = n[o], stringsAsFactors = FALSE)
}

empty_details <- function() {
  data.frame(column = character(0), wave = character(0), code = numeric(0),
             n = integer(0), stringsAsFactors = FALSE)
}

# jsonlite parses numbers with the C library's strtod (correctly rounded);
# as.numeric() is not the oracle because R's own decimal parser accumulates
# in long double, which has only double precision on some platforms
read_exact <- function(tokens) {
  as.double(jsonlite::fromJSON(paste0("[", paste(tokens, collapse = ","), "]")))
}

# "col: N cell(s) set to NA (w1: t x1, t x2; w2: t x3)" -> rows
report_sweep_rows <- function(report, col) {
  prefix <- paste0("^", col, ": [0-9]+ cell\\(s\\) set to NA \\(")
  line <- grep(prefix, report, value = TRUE)
  if (length(line) != 1L) return(NULL)
  body <- sub("\\)$", "", sub(prefix, "", line))
  do.call(rbind, lapply(strsplit(body, "; ", fixed = TRUE)[[1]], function(g) {
    items <- strsplit(sub("^[^:]*: ", "", g), ", ", fixed = TRUE)[[1]]
    data.frame(wave = sub(": .*$", "", g),
               token = sub(" x[0-9]+$", "", items),
               n = as.integer(sub("^.* x", "", items)),
               stringsAsFactors = FALSE)
  }))
}

# sweep, then the merge's NA_SWEEP log entry, JSONL writer and report writer
run_sweep <- function(merged, sets, cols, dir) {
  sw <- lissr:::sweep_user_missing(merged, na_sweep_registry(sets), cols)
  entry <- lissr:::make_log("NA_SWEEP", "*", paste0(length(cols), " col(s)"),
                            "sweep_user_missing", sw$swept,
                            values_changed = sw$swept)
  entry$details <- sw$details
  log_path <- file.path(dir, "log.jsonl")
  report_path <- file.path(dir, "report.txt")
  lissr:::write_jsonl(list(entry), log_path)
  lissr:::write_report(sw$data, list(), list(entry),
                       list(meta = list(module = "xx", recipe_version = "test")),
                       report_path, sweep = sw$details)
  list(sw = sw, log_line = readLines(log_path, warn = FALSE),
       report = readLines(report_path, warn = FALSE))
}

# expected: data.frame(wave, code, n, token) in contract order
expect_exact_disclosure <- function(out, col, expected) {
  d <- out$sw$details[out$sw$details$column == col, , drop = FALSE]
  expect_identical(d$wave, expected$wave)
  expect_identical(d$code, expected$code)
  expect_identical(d$n, expected$n)
  rows <- report_sweep_rows(out$report, col)
  expect_false(is.null(rows))
  expect_identical(rows$wave, ifelse(is.na(expected$wave), "NA", expected$wave))
  expect_identical(rows$token, expected$token)
  expect_identical(rows$n, expected$n)
  expect_identical(read_exact(rows$token), expected$code)
  valid <- isTRUE(jsonlite::validate(out$log_line))
  expect_true(valid)
  if (!valid) return(invisible(NULL))
  ent <- jsonlite::fromJSON(out$log_line, simplifyVector = FALSE)
  js <- Filter(function(r) identical(r$column, col), ent$details)
  expect_identical(vapply(js, function(r) as.double(r$code), numeric(1)),
                   expected$code)
  expect_identical(vapply(js, function(r) {
    if (is.null(r$wave)) NA_character_ else r$wave
  }, character(1)), expected$wave)
  expect_identical(vapply(js, function(r) as.integer(r$n), integer(1)),
                   expected$n)
  # the JSON number text is the report token itself
  expect_true(all(vapply(expected$token, function(tk) {
    grepl(paste0('"code":', tk, ","), out$log_line, fixed = TRUE)
  }, logical(1))))
}

new_dir <- function() {
  d <- tempfile("lissr_sweep_")
  dir.create(d)
  d
}

# run fun() with LC_NUMERIC = loc, then restore the caller's locale; NULL when
# the locale cannot be set or does not use a decimal comma. R warns whenever
# LC_NUMERIC leaves "C"; only Sys.setlocale()'s own warnings are muffled, and
# warnings raised by fun() are returned for the test to inspect
under_numeric_locale <- function(loc, fun) {
  quiet_set <- function(l) withCallingHandlers(Sys.setlocale("LC_NUMERIC", l),
    warning = function(w) invokeRestart("muffleWarning"))
  old <- Sys.getlocale("LC_NUMERIC")
  on.exit(quiet_set(old), add = TRUE)
  got <- quiet_set(loc)
  if (!nzchar(got) || !identical(Sys.localeconv()[["decimal_point"]], ","))
    return(NULL)
  raw_token <- sprintf("%.17g", 1.23456789)
  state_before <- list(Sys.getlocale("LC_NUMERIC"), getOption("OutDec"))
  warned <- character(0)
  value <- withCallingHandlers(fun(), warning = function(w) {
    warned <<- c(warned, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, locale = got, raw_token = raw_token, warnings = warned,
       state_before = state_before,
       state_after = list(Sys.getlocale("LC_NUMERIC"), getOption("OutDec")))
}

no_time <- function(l) sub('"timestamp":"[^"]*"', "", l)
sweep_lines <- function(r) grep("^s[0-9]+: |^Cells set to NA", r, value = TRUE)

# ---- exact grouping and tokens ---------------------------------------------

test_that("nearby doubles stay distinct in details, report and JSONL", {
  dir <- new_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  eps <- .Machine$double.eps
  x <- c(1, 1 + eps, 1 + 2 * eps, 1 + eps, 5)
  merged <- data.frame(nomem_encr = seq_along(x), wave_id = "xx01a", s323 = x,
                       stringsAsFactors = FALSE)
  sets <- list(list(col = "s323", wave = "xx01a", na_values = NULL,
                    na_range = c(1, 1 + 4 * eps)))
  out <- run_sweep(merged, sets, "s323", dir)
  expect_exact_disclosure(out, "s323", data.frame(
    wave = "xx01a", code = c(1, 1 + eps, 1 + 2 * eps), n = c(1L, 2L, 1L),
    token = c("1", "1.0000000000000002", "1.0000000000000004"),
    stringsAsFactors = FALSE))
  expect_identical(out$sw$data$s323, c(NA, NA, NA, NA, 5))
  expect_identical(out$sw$swept, 4L)
})

test_that("distinct fractional codes keep distinct exact tokens", {
  dir <- new_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  x <- c(1.23456789, 1.23456780, 1.23456789, 2.5)
  merged <- data.frame(nomem_encr = seq_along(x), wave_id = "xx01a", s323 = x,
                       stringsAsFactors = FALSE)
  sets <- list(list(col = "s323", wave = "xx01a",
                    na_values = c(1.23456789, 1.23456780), na_range = NULL))
  out <- run_sweep(merged, sets, "s323", dir)
  expect_exact_disclosure(out, "s323", data.frame(
    wave = "xx01a", code = c(1.23456780, 1.23456789), n = c(1L, 2L),
    token = c("1.2345678", "1.2345678899999999"), stringsAsFactors = FALSE))
  expect_identical(out$sw$data$s323, c(NA, NA, NA, 2.5))
})

test_that("large finite and extreme-magnitude codes round-trip exactly", {
  dir <- new_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  xmax <- .Machine$double.xmax
  codes <- c(-xmax, -99999999, 2^-1074, .Machine$double.xmin, 99999999,
             2^53, 2^53 + 2, 1e17, xmax)
  x <- c(codes, 99999999, 1500, NA)
  merged <- data.frame(nomem_encr = seq_along(x), wave_id = "xx01a", s323 = x,
                       stringsAsFactors = FALSE)
  sets <- list(list(col = "s323", wave = "xx01a", na_values = rev(codes),
                    na_range = NULL))
  out <- run_sweep(merged, sets, "s323", dir)
  expect_exact_disclosure(out, "s323", data.frame(
    wave = "xx01a", code = codes, n = c(1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L),
    token = c("-1.7976931348623157e+308", "-99999999", "4.9406564584124654e-324",
              "2.2250738585072014e-308", "99999999", "9007199254740992",
              "9007199254740994", "1e+17", "1.7976931348623157e+308"),
    stringsAsFactors = FALSE))
  expect_identical(out$sw$data$s323, c(rep(NA_real_, 10), 1500, NA))
  expect_identical(out$sw$swept, 10L)
})

test_that("declared ranges, overlaps and wave attribution follow the predicate", {
  dir <- new_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  above <- -9990 + 2^-30
  merged <- data.frame(
    nomem_encr = 1:9,
    wave_id = c(rep("xx01a", 6), rep("xx02b", 3)),
    s323 = c(-9999, -9995.5, -9990, above, -10000, 1500, -9999, -9995.5, 2000),
    s324 = c(-8, 3, -8, 4, -9, 5, -8, -8, 1),
    stringsAsFactors = FALSE)
  sets <- list(
    list(col = "s323", wave = "xx01a", na_values = -9999,
         na_range = c(-9999, -9990)),
    list(col = "s323", wave = "xx02b", na_values = -9995.5, na_range = NULL),
    list(col = "s324", wave = "xx01a", na_values = c(-8, -9), na_range = NULL))
  out <- run_sweep(merged, sets, c("s324", "s323"), dir)
  # rows follow the cols order, then wave, then ascending code
  expect_identical(out$sw$details$column, c("s324", "s324", rep("s323", 4)))
  expect_exact_disclosure(out, "s324", data.frame(
    wave = "xx01a", code = c(-9, -8), n = c(1L, 2L), token = c("-9", "-8"),
    stringsAsFactors = FALSE))
  expect_exact_disclosure(out, "s323", data.frame(
    wave = c("xx01a", "xx01a", "xx01a", "xx02b"),
    code = c(-9999, -9995.5, -9990, -9995.5), n = c(1L, 1L, 1L, 1L),
    token = c("-9999", "-9995.5", "-9990", "-9995.5"), stringsAsFactors = FALSE))
  # data equal the per-cell oracle: undeclared and out-of-range codes stay
  for (cl in c("s323", "s324")) {
    hits <- oracle_hits(merged[[cl]], merged$wave_id,
                        Filter(function(s) s$col == cl, sets))
    expected <- merged[[cl]]
    expected[hits] <- NA
    expect_identical(out$sw$data[[cl]], expected)
  }
  expect_identical(out$sw$data$s323[c(4, 5, 7)], c(above, -10000, -9999))
  expect_identical(out$sw$swept, 7L)
  expect_true("s323: 4 cell(s) set to NA (xx01a: -9999 x1, -9995.5 x1, -9990 x1; xx02b: -9995.5 x1)" %in%
                out$report)
  expect_true("Cells set to NA after the checks above: 7 (exact codes, '.' decimal mark)" %in%
                out$report)
})

test_that("a decimal-comma OutDec changes neither grouping nor tokens", {
  dir_dot <- new_dir()
  dir_comma <- new_dir()
  on.exit(unlink(c(dir_dot, dir_comma), recursive = TRUE), add = TRUE)
  eps <- .Machine$double.eps
  x <- c(1.23456789, 1.23456780, 1 + eps, 1, -9999, 1.23456789, 7.5)
  merged <- data.frame(nomem_encr = seq_along(x), wave_id = "xx01a", s323 = x,
                       stringsAsFactors = FALSE)
  sets <- list(list(col = "s323", wave = "xx01a",
                    na_values = c(1.23456789, 1.23456780, -9999),
                    na_range = c(1, 1 + 2 * eps)))
  ref <- run_sweep(merged, sets, "s323", dir_dot)
  withr::local_options(OutDec = ",")
  expect_no_warning(out <- run_sweep(merged, sets, "s323", dir_comma))
  expect_identical(out$sw, ref$sw)
  expect_exact_disclosure(out, "s323", data.frame(
    wave = "xx01a", code = c(-9999, 1, 1 + eps, 1.23456780, 1.23456789),
    n = c(1L, 1L, 1L, 1L, 2L),
    token = c("-9999", "1", "1.0000000000000002", "1.2345678",
              "1.2345678899999999"), stringsAsFactors = FALSE))
  expect_identical(sweep_lines(out$report), sweep_lines(ref$report))
  expect_identical(no_time(out$log_line), no_time(ref$log_line))
})

# a decimal-comma numeric locale changes what C-level sprintf() writes, which
# options(OutDec) does not; each locale is skipped explicitly where the system
# lacks it, so coverage needs a system that has at least one of them
for (loc in c("de_DE.UTF-8", "nl_NL.UTF-8", "fr_FR.UTF-8")) {
  test_that(paste0("LC_NUMERIC ", loc, " keeps exact decimal-point tokens and valid JSON"), {
    dir_c <- new_dir()
    dir_l <- new_dir()
    on.exit(unlink(c(dir_c, dir_l), recursive = TRUE), add = TRUE)
    eps <- .Machine$double.eps
    xmax <- .Machine$double.xmax
    codes <- c(-xmax, -9999, -9995.5, 2^-1074, .Machine$double.xmin, 1, 1 + eps,
               1 + 2 * eps, 1.23456780, 1.23456789, 99999999, 2^53 + 2, 1e17, xmax)
    x <- c(codes, 1.23456789, 1500, NA)
    merged <- data.frame(nomem_encr = seq_along(x), wave_id = "xx01a", s323 = x,
                         stringsAsFactors = FALSE)
    sets <- list(list(col = "s323", wave = "xx01a", na_values = codes,
                      na_range = NULL))
    ref <- run_sweep(merged, sets, "s323", dir_c)
    locale_before <- Sys.getlocale("LC_NUMERIC")
    res <- under_numeric_locale(loc, function() run_sweep(merged, sets, "s323", dir_l))
    expect_identical(Sys.getlocale("LC_NUMERIC"), locale_before)
    if (is.null(res))
      skip(paste0("numeric locale ", loc, " is unavailable or has no decimal comma"))
    # the locale was active: unrepaired %.17g output would carry a comma
    expect_identical(res$raw_token, "1,2345678899999999")
    expect_identical(res$warnings, character(0))
    # the writers left the caller's numeric locale and OutDec untouched
    expect_identical(res$state_after, res$state_before)
    out <- res$value
    expect_identical(out$sw, ref$sw)
    expect_exact_disclosure(out, "s323", data.frame(
      wave = "xx01a", code = codes, n = c(rep(1L, 9), 2L, rep(1L, 4)),
      token = c("-1.7976931348623157e+308", "-9999", "-9995.5",
                "4.9406564584124654e-324", "2.2250738585072014e-308", "1",
                "1.0000000000000002", "1.0000000000000004", "1.2345678",
                "1.2345678899999999", "99999999", "9007199254740994", "1e+17",
                "1.7976931348623157e+308"), stringsAsFactors = FALSE))
    expect_identical(no_time(out$log_line), no_time(ref$log_line))
    expect_identical(sweep_lines(out$report), sweep_lines(ref$report))
  })
}

# ---- controls ----------------------------------------------------------------

test_that("zero hits keep typed empty details, no report section, empty JSON array", {
  dir <- new_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  merged <- data.frame(nomem_encr = 1:3, wave_id = "xx01a",
                       s323 = c(1500, NA, 2000), stringsAsFactors = FALSE)
  sets <- list(list(col = "s323", wave = "xx01a", na_values = c(-9999, -9998),
                    na_range = NULL))
  out <- run_sweep(merged, sets, "s323", dir)
  expect_identical(out$sw$swept, 0L)
  expect_identical(out$sw$data, merged)
  expect_identical(out$sw$details, empty_details())
  expect_false(any(grepl("Residual User-Missing Sweep", out$report, fixed = TRUE)))
  expect_false(any(grepl("^s323: ", out$report)))
  expect_true(grepl('"details":[]', out$log_line, fixed = TRUE))
  expect_identical(jsonlite::fromJSON(out$log_line, simplifyVector = FALSE)$details,
                   list())
  # no candidate columns at all
  none <- lissr:::sweep_user_missing(merged, na_sweep_registry(sets), character(0))
  expect_identical(none$details, empty_details())
})

test_that("integer columns keep plain integer tokens and JSON numbers", {
  dir <- new_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  merged <- data.frame(nomem_encr = 1:5, wave_id = "xx01a",
                       s323 = c(-9999L, -9998L, -9999L, 7L, NA),
                       stringsAsFactors = FALSE)
  sets <- list(list(col = "s323", wave = "xx01a", na_values = c(-9999, -9998),
                    na_range = NULL))
  out <- run_sweep(merged, sets, "s323", dir)
  expect_exact_disclosure(out, "s323", data.frame(
    wave = "xx01a", code = c(-9999, -9998), n = c(2L, 1L),
    token = c("-9999", "-9998"), stringsAsFactors = FALSE))
  expect_type(out$sw$details$code, "double")
  expect_identical(out$sw$data$s323, c(NA, NA, NA, 7L, NA))
  expect_true("s323: 3 cell(s) set to NA (xx01a: -9999 x2, -9998 x1)" %in% out$report)
  expect_true(grepl('{"column":"s323","wave":"xx01a","code":-9999,"n":2}',
                    out$log_line, fixed = TRUE))
})

test_that("rows without a wave column report wave NA (JSON null)", {
  dir <- new_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  merged <- data.frame(nomem_encr = 1:3, s323 = c(-9, -9, 4))
  sets <- list(list(col = "s323", wave = NA_character_, na_values = -9,
                    na_range = NULL))
  out <- run_sweep(merged, sets, "s323", dir)
  expect_exact_disclosure(out, "s323", data.frame(
    wave = NA_character_, code = -9, n = 2L, token = "-9",
    stringsAsFactors = FALSE))
  expect_true(grepl('"wave":null', out$log_line, fixed = TRUE))
})

test_that("non-finite swept codes keep the JSONL line valid", {
  dir <- new_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  merged <- data.frame(nomem_encr = 1:3, wave_id = "xx01a",
                       s323 = c(-Inf, -9, 5), stringsAsFactors = FALSE)
  sets <- list(list(col = "s323", wave = "xx01a", na_values = NULL,
                    na_range = c(-Inf, -1)))
  out <- run_sweep(merged, sets, "s323", dir)
  expect_identical(out$sw$details$code, c(-Inf, -9))
  expect_true(jsonlite::validate(out$log_line))
  expect_identical(jsonlite::fromJSON(out$log_line)$details$code, c(-Inf, -9))
  expect_true("s323: 2 cell(s) set to NA (xx01a: -Inf x1, -9 x1)" %in% out$report)
})

# ---- other fields keep the existing serialization ---------------------------

test_that("class-json text in ordinary entries is written exactly as before", {
  dir <- new_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  entry <- list(rule_id = "OTHER",
                description = structure("{\"example\":true}", class = "json"),
                values_changed = 1.23456789, duration_ms = 12.345678)
  path <- file.path(dir, "log.jsonl")
  lissr:::write_jsonl(list(entry, entry), path)
  lines <- readLines(path, warn = FALSE)
  # the existing writer call is the reference: quoted json text, 4 digits
  expect_identical(lines, rep(as.character(jsonlite::toJSON(entry, auto_unbox = TRUE)), 2))
  expect_true(all(vapply(lines, jsonlite::validate, logical(1))))
  parsed <- jsonlite::fromJSON(lines[1], simplifyVector = FALSE)
  expect_identical(parsed$description, "{\"example\":true}")
  expect_identical(parsed$values_changed, 1.2346)
  expect_identical(parsed$duration_ms, 12.3457)
})

test_that("NA_SWEEP entries keep untouched fields, including class-json text", {
  dir <- new_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  merged <- data.frame(nomem_encr = 1:4, wave_id = "xx01a",
                       s323 = c(1.23456789, -9999, -9999, 1500),
                       stringsAsFactors = FALSE)
  sw <- lissr:::sweep_user_missing(merged, na_sweep_registry(list(
    list(col = "s323", wave = "xx01a", na_values = c(-9999, 1.23456789),
         na_range = NULL))), "s323")
  entry <- lissr:::make_log("NA_SWEEP", "*", "1 col(s)", "sweep_user_missing",
                            sw$swept, values_changed = sw$swept,
                            duration_ms = 12.345678)
  entry$note <- structure("[1,2]", class = "json")
  entry$details <- sw$details
  entry$trailer <- structure("{\"k\":1}", class = "json")
  # equals the writer's first placeholder, so the splice must pick another
  entry$decoy <- "lissr_na_sweep_details_0"
  path <- file.path(dir, "log.jsonl")
  withr::local_seed(20261005)
  seed_before <- get(".Random.seed", envir = globalenv())
  lissr:::write_jsonl(list(entry), path)
  expect_identical(get(".Random.seed", envir = globalenv()), seed_before)
  line <- readLines(path, warn = FALSE)
  expect_true(jsonlite::validate(line))
  # byte level: removing the details member leaves the existing call's text
  without <- entry
  without$details <- NULL
  expect_identical(sub(",\"details\":\\[.*\\],\"trailer\":", ",\"trailer\":", line),
                   as.character(jsonlite::toJSON(without, auto_unbox = TRUE)))
  parsed <- jsonlite::fromJSON(line, simplifyVector = FALSE)
  expect_identical(names(parsed), names(entry))
  expect_identical(parsed$note, "[1,2]")
  expect_identical(parsed$trailer, "{\"k\":1}")
  expect_identical(parsed$decoy, "lissr_na_sweep_details_0")
  expect_identical(parsed$duration_ms, 12.3457)
  expect_identical(vapply(parsed$details, function(r) as.double(r$code), numeric(1)),
                   c(-9999, 1.23456789))
  expect_identical(vapply(parsed$details, function(r) as.integer(r$n), integer(1)),
                   c(2L, 1L))
  expect_true(grepl("\"code\":1.2345678899999999,", line, fixed = TRUE))
})

test_that("randomized cases: data, totals, details and tokens match the oracles", {
  set.seed(20261004)
  eps <- .Machine$double.eps
  pool <- c(-9999, -9998, -9995.5, -9, -8, 0.1, 1, 1 + eps, 1 + 2 * eps,
            1.2345678, 1.23456789, 1500, 2000, 99999999, 2^53)
  waves_all <- c("xx01a", "xx02b", "xx03c")
  bad <- character(0)
  with_hits <- 0L
  dir <- new_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  for (trial in seq_len(200)) {
    nr <- sample(5:40, 1)
    w <- sample(waves_all[seq_len(sample(1:3, 1))], nr, replace = TRUE)
    x <- sample(c(pool, NA), nr, replace = TRUE)
    sets <- lapply(unique(w), function(wv) {
      nv <- sample(0:3, 1)
      list(col = "s323", wave = wv,
           na_values = if (nv > 0) sample(pool, nv) else NULL,
           na_range = if (stats::runif(1) < 0.5) sort(sample(pool, 2)) else NULL)
    })
    merged <- data.frame(nomem_encr = seq_len(nr), wave_id = w, s323 = x,
                         stringsAsFactors = FALSE)
    out <- run_sweep(merged, sets, "s323", dir)
    hits <- oracle_hits(x, w, sets)
    exp_x <- x
    exp_x[hits] <- NA
    exp_d <- if (any(hits)) oracle_details("s323", x[hits], w[hits]) else empty_details()
    ok <- identical(out$sw$data$s323, exp_x) &&
      identical(out$sw$swept, sum(hits)) &&
      identical(out$sw$details, exp_d)
    if (ok && nrow(exp_d) > 0) {
      with_hits <- with_hits + 1L
      rows <- report_sweep_rows(out$report, "s323")
      ent <- jsonlite::fromJSON(out$log_line, simplifyVector = FALSE)
      ok <- !is.null(rows) && identical(read_exact(rows$token), exp_d$code) &&
        identical(rows$n, exp_d$n) &&
        identical(vapply(ent$details, function(r) as.double(r$code), numeric(1)),
                  exp_d$code)
    }
    if (!ok) bad <- c(bad, as.character(trial))
  }
  expect_identical(bad, character(0))
  expect_gt(with_hits, 100L)
})

# ---- end-to-end merge ----------------------------------------------------------

sweep_fixture_recipe <- function() {
  list(
    meta = list(module = "xx", module_label = "Fixture", schema_version = "1.0.0",
                recipe_version = "t", created = "t", source_spec = "t",
                covered_waves = list("xx01a", "xx02b")),
    global = list(id_variable = "nomem_encr", wave_variable = "wave_id",
                  year_variable = "wave_year", labelled_policy = "to_numeric",
                  missing_variable_policy = "warn_and_create_na",
                  strip_label_whitespace = TRUE),
    wave_index = list(list(id = "xx01a", year = 2001, file_pattern = "xx01a_*"),
                      list(id = "xx02b", year = 2002, file_pattern = "xx02b_*")),
    logging = list(log_file = "xx_log.jsonl", report_file = "xx_report.txt",
                   summary_artifact = FALSE))
}

merge_capturing_warnings <- function(recipe, data_dir, out_dir) {
  warned <- character(0)
  res <- withCallingHandlers(
    suppressMessages(merge_liss_module(recipe, data_dir, out_dir)),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  list(res = res, warnings = warned,
       log = readLines(file.path(out_dir, "xx_log.jsonl"), warn = FALSE),
       report = readLines(file.path(out_dir, "xx_report.txt"), warn = FALSE))
}

test_that("merge_liss_module discloses swept codes in the log, JSONL and report", {
  skip_if_not_installed("haven")
  eps <- .Machine$double.eps
  base <- tempfile("lissr_sweep_merge_")
  data_dir <- file.path(base, "data")
  dir.create(data_dir, recursive = TRUE)
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  haven::write_sav(data.frame(
    nomem_encr = 1:4,
    xx01a323 = haven::labelled_spss(c(1500, -9999, -9998, -9999),
                                    labels = c(dk = -9999, refusal = -9998),
                                    na_values = c(-9999, -9998))),
    file.path(data_dir, "xx01a_EN_1.0p.sav"))
  haven::write_sav(data.frame(
    nomem_encr = 1:4,
    xx02b323 = haven::labelled_spss(c(2000, 1.23456789, 1.2345678, 1 + eps),
                                    na_range = c(1, 1.3))),
    file.path(data_dir, "xx02b_EN_1.0p.sav"))

  dot <- merge_capturing_warnings(sweep_fixture_recipe(), data_dir,
                                  file.path(base, "out_dot"))
  sweeps <- Filter(function(e) identical(e$rule_id, "NA_SWEEP"), dot$res$log)
  expect_length(sweeps, 1L)
  expected <- data.frame(column = "s323",
                         wave = c("xx01a", "xx01a", "xx02b", "xx02b", "xx02b"),
                         code = c(-9999, -9998, 1 + eps, 1.2345678, 1.23456789),
                         n = c(2L, 1L, 1L, 1L, 1L), stringsAsFactors = FALSE)
  expect_identical(sweeps[[1]]$details, expected)
  expect_identical(sweeps[[1]]$values_changed, 6L)
  # only the NA_SWEEP entry carries details; one JSONL line per log entry
  has_details <- vapply(dot$res$log, function(e) !is.null(e[["details"]]), logical(1))
  is_sweep <- vapply(dot$res$log, function(e) identical(e$rule_id, "NA_SWEEP"), logical(1))
  expect_identical(has_details, is_sweep)
  expect_length(dot$log, length(dot$res$log))
  expect_true(all(vapply(dot$log, jsonlite::validate, logical(1))))
  expect_identical(as.numeric(dot$res$data$s323), c(1500, NA, NA, NA, 2000, NA, NA, NA))

  sw_line <- grep('"rule_id":"NA_SWEEP"', dot$log, value = TRUE, fixed = TRUE)
  ent <- jsonlite::fromJSON(sw_line, simplifyVector = FALSE)
  expect_identical(vapply(ent$details, function(r) as.double(r$code), numeric(1)),
                   expected$code)
  expect_true(paste0("s323: 6 cell(s) set to NA (xx01a: -9999 x2, -9998 x1; ",
                     "xx02b: 1.0000000000000002 x1, 1.2345678 x1, ",
                     "1.2345678899999999 x1)") %in% dot$report)
  expect_gt(match("--- Residual User-Missing Sweep (after validation) ---", dot$report),
            match("--- Validation Summary ---", dot$report))

  # decimal comma: same data, details, JSONL details, sweep lines, warnings
  withr::local_options(OutDec = ",")
  comma <- merge_capturing_warnings(sweep_fixture_recipe(), data_dir,
                                    file.path(base, "out_comma"))
  expect_identical(comma$res$data, dot$res$data)
  comma_sweep <- Filter(function(e) identical(e$rule_id, "NA_SWEEP"), comma$res$log)
  expect_identical(comma_sweep[[1]]$details, expected)
  expect_identical(no_time(comma$log), no_time(dot$log))
  expect_identical(sweep_lines(comma$report), sweep_lines(dot$report))
  expect_identical(comma$warnings, dot$warnings)
})

test_that("a merge without swept cells logs empty details and no report section", {
  skip_if_not_installed("haven")
  base <- tempfile("lissr_sweep_merge0_")
  data_dir <- file.path(base, "data")
  dir.create(data_dir, recursive = TRUE)
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  # differing declarations block label restoration; no declared code occurs
  haven::write_sav(data.frame(
    nomem_encr = 1:3,
    xx01a323 = haven::labelled_spss(c(1500, 2000, 2500), labels = c(dk = -9999),
                                    na_values = -9999)),
    file.path(data_dir, "xx01a_EN_1.0p.sav"))
  haven::write_sav(data.frame(
    nomem_encr = 1:3,
    xx02b323 = haven::labelled_spss(c(10, 20, 30), na_values = -9998)),
    file.path(data_dir, "xx02b_EN_1.0p.sav"))
  out <- merge_capturing_warnings(sweep_fixture_recipe(), data_dir,
                                  file.path(base, "out"))
  sweeps <- Filter(function(e) identical(e$rule_id, "NA_SWEEP"), out$res$log)
  expect_length(sweeps, 1L)
  expect_identical(sweeps[[1]]$values_changed, 0L)
  expect_identical(sweeps[[1]]$details, empty_details())
  sw_line <- grep('"rule_id":"NA_SWEEP"', out$log, value = TRUE, fixed = TRUE)
  expect_true(grepl('"details":[]', sw_line, fixed = TRUE))
  expect_false(any(grepl("Residual User-Missing Sweep", out$report, fixed = TRUE)))
  expect_identical(as.numeric(out$res$data$s323), c(1500, 2000, 2500, 10, 20, 30))
})
