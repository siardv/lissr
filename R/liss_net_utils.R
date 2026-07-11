# ============================================================================
# liss_net_utils.R, hardened primitives for the download and auth layer
# ============================================================================
# pure helpers (content-disposition parsing, filename sanitation, 2FA code
# normalization, retry logic, verified unzip) plus the two thin shims that
# perform actual network I/O through a session's cookie handle. the shims
# are isolated here so offline tests can mock them and exercise every
# error path in liss_download() and liss_is_logged_in() without a network.

# extract a filename from a content-disposition header. supports, in
# precedence order, the RFC 5987 `filename*=` form (charset prefix
# stripped, percent-decoding applied), the quoted `filename="..."` form,
# and the unquoted token form (the shape the previous parser silently
# mangled into the whole header string). the result is always sanitized;
# a missing or unusable header yields the sanitized fallback.
parse_content_disposition <- function(cd, fallback) {
  if (is.null(cd) || length(cd) != 1 || is.na(cd) || !nzchar(cd)) {
    return(sanitize_filename(fallback, fallback))
  }
  cd <- as.character(cd)
  fname <- NULL

  m <- regmatches(cd, regexec("filename\\*\\s*=\\s*([^;]+)", cd,
                              ignore.case = TRUE))[[1]]
  if (length(m) == 2) {
    v <- trimws(m[[2]])
    v <- sub("^[^']*'[^']*'", "", v)  # charset'language' prefix
    v <- gsub('^"|"$', "", v)
    v <- tryCatch(utils::URLdecode(v), error = function(e) v)
    if (nzchar(v)) fname <- v
  }
  if (is.null(fname)) {
    m <- regmatches(cd, regexec('filename\\s*=\\s*"([^"]*)"', cd,
                                ignore.case = TRUE))[[1]]
    if (length(m) == 2 && nzchar(m[[2]])) fname <- m[[2]]
  }
  if (is.null(fname)) {
    m <- regmatches(cd, regexec("filename\\s*=\\s*([^;[:space:]]+)", cd,
                                ignore.case = TRUE))[[1]]
    if (length(m) == 2 && nzchar(m[[2]])) fname <- m[[2]]
  }
  sanitize_filename(fname %||% fallback, fallback)
}

# reduce a candidate filename to a safe basename: directory components
# (either separator) are stripped, traversal and dot-only names are
# rejected, control characters are removed, and filesystem-reserved
# characters are replaced. when nothing safe remains the (basename of
# the) fallback is used, with a hard default as the last resort.
sanitize_filename <- function(x, fallback = "download.bin") {
  clean <- function(v) {
    if (is.null(v) || length(v) != 1 || is.na(v)) return("")
    v <- basename(gsub("\\\\", "/", as.character(v)))
    v <- gsub("[[:cntrl:]]", "", v)
    v <- gsub('[<>:"/\\\\|?*]', "_", v)
    v <- trimws(v)
    if (!nzchar(v) || grepl("^\\.+$", v)) return("")
    v
  }
  out <- clean(x)
  if (!nzchar(out)) out <- clean(fallback)
  if (!nzchar(out)) out <- "download.bin"
  out
}

# normalize a two-factor verification code: strip every whitespace
# character (codes are pasted with spaces and trailing newlines). NULL
# and NA collapse to the empty string so callers test one condition.
.normalize_2fa_code <- function(x) {
  if (is.null(x) || length(x) != 1 || is.na(x)) return("")
  gsub("[[:space:]]", "", as.character(x))
}

# run fn() up to `attempts` times, retrying on curl-level errors (the
# network hiccup class) and on httr-like responses with a 5xx status
# (transient server failures). 4xx responses are returned immediately;
# they are deterministic and retrying them only hammers the server.
# waits `wait * attempt` seconds between attempts. returns the last
# response, or re-throws the last error when every attempt failed.
.with_retries <- function(fn, attempts = 3, wait = 1) {
  last_err <- NULL
  last_res <- NULL
  for (a in seq_len(attempts)) {
    res <- tryCatch(fn(), error = function(e) e)
    if (inherits(res, "error")) {
      last_err <- res
    } else {
      sc <- res$status_code %||% 0
      if (!(is.numeric(sc) && length(sc) == 1 && sc >= 500)) return(res)
      last_err <- NULL
      last_res <- res
    }
    if (a < attempts) Sys.sleep(wait * a)
  }
  if (!is.null(last_err)) stop(last_err)
  last_res
}

# extract a zip and verify the extraction: the extracted paths must be
# reported and exist on disk. returns the paths on success and NULL on
# any warning, error, or empty result, so the caller keeps the archive
# whenever extraction cannot be trusted (utils::unzip only WARNS on a
# corrupt archive, and the old code deleted the sole copy regardless).
.verify_unzip <- function(zip_path, exdir) {
  files <- tryCatch(
    utils::unzip(zip_path, exdir = exdir, overwrite = TRUE),
    warning = function(w) NULL,
    error = function(e) NULL
  )
  if (is.null(files) || !is.character(files) || length(files) == 0) {
    return(NULL)
  }
  if (!all(file.exists(files))) return(NULL)
  files
}

# fetch a url through the session's cookie handle, streaming the body
# to `dest` with a timeout (the previous path buffered whole files in
# memory and could hang forever). mirrors rvest's own request pattern:
# per-session config plus the shared curl handle that carries cookies.
.liss_fetch_file <- function(session, url, dest, timeout_s = 300) {
  httr::GET(url, session$config, handle = session$handle,
            httr::write_disk(dest, overwrite = TRUE),
            httr::timeout(timeout_s))
}

# HEAD a url through the session's cookie handle: the cheap
# authentication probe (no body is transferred).
.liss_head <- function(session, url, timeout_s = 20) {
  httr::HEAD(url, session$config, handle = session$handle,
             httr::timeout(timeout_s))
}

# jump the rvest session to a url; isolated for offline mocking.
.liss_jump <- function(session, url) {
  rvest::session_jump_to(session, url)
}
