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

# plan reported member paths without repairing unsafe or ambiguous names
.liss_zip_plan <- function(listing) {
  if (!is.data.frame(listing) || !all(c("Name", "Length") %in% names(listing)) ||
      !is.character(listing$Name) || !nrow(listing) || anyNA(listing$Name) ||
      any(!nzchar(listing$Name)) || anyDuplicated(listing$Name) ||
      !is.numeric(listing$Length) || anyNA(listing$Length) ||
      any(!is.finite(listing$Length) | listing$Length < 0 |
          listing$Length != trunc(listing$Length))) stop("invalid ZIP member listing")
  members <- listing$Name
  if (any(grepl("^/", members) | grepl("\\\\", members) |
          grepl('[<>:"|?*]', members) | grepl("[[:cntrl:]]", members)))
    stop("unsafe ZIP member path")
  directories <- endsWith(members, "/")
  parts <- lapply(strsplit(members, "/", fixed = TRUE), function(value) {
    if (any(value == "..")) stop("parent traversal in ZIP member")
    value <- value[!value %in% c("", ".")]
    if (any(grepl("[ .]$", value) |
            grepl("^(con|prn|aux|nul|com[1-9]|lpt[1-9])($|\\.)", value, ignore.case = TRUE)))
      stop("ambiguous ZIP path component")
    value
  })
  keys <- vapply(parts, paste, character(1), collapse = "/")
  if (any(!directories & !nzchar(keys)) || anyDuplicated(keys[nzchar(keys)]) ||
      any(directories & listing$Length != 0)) stop("ambiguous ZIP member layout")
  file_indices <- which(!directories)
  if (!length(file_indices)) stop("ZIP has no file members")
  file_keys <- keys[file_indices]
  for (value in parts) if (length(value) > 1L) {
    parents <- vapply(seq_len(length(value) - 1L), function(index)
      paste(value[seq_len(index)], collapse = "/"), character(1))
    if (any(parents %in% file_keys)) stop("ZIP file is also a parent directory")
  }
  list(members = members, parts = parts, directories = directories,
       sizes = listing$Length, file_indices = file_indices)
}

# check every existing component before creating or replacing a destination
.liss_zip_target <- function(root, parts, directory, blocked = character()) {
  current <- root
  prefix <- paste0(sub("/+$", "", root), "/")
  for (index in seq_along(parts)) {
    current <- file.path(current, parts[[index]])
    link <- Sys.readlink(current)
    if (!is.na(link) && nzchar(link)) stop("linked ZIP destination component")
    real <- if (file.exists(current) || dir.exists(current))
      normalizePath(current, winslash = "/", mustWork = TRUE) else current
    if (!startsWith(real, prefix)) stop("ZIP destination escaped its root")
    if (any(real == blocked | vapply(blocked, function(path)
      startsWith(real, paste0(path, "/")), logical(1)))) stop("ZIP destination overlaps protected input")
    if (file.exists(current) || dir.exists(current)) {
      needs_directory <- index < length(parts) || directory
      if (needs_directory && !dir.exists(current)) stop("ZIP directory conflicts with existing file")
      if (!needs_directory && (!utils::file_test("-f", current) ||
                              file.access(current, 2L) != 0L))
        stop("ZIP file conflicts with an unwritable or directory destination")
    }
  }
  current
}

# stream bytes into a flat private staging file; archive links are never recreated
.liss_zip_stream <- function(zip_path, member, destination, expected_size) {
  input <- unz(zip_path, member, open = "rb")
  on.exit(close(input), add = TRUE)
  output <- file(destination, open = "wb")
  on.exit(close(output), add = TRUE)
  remaining <- expected_size
  repeat {
    bytes <- readBin(input, what = "raw", n = as.integer(min(1048576, remaining + 1)))
    if (length(bytes) > remaining) stop("ZIP member exceeds its reported size")
    if (!length(bytes)) break
    writeBin(bytes, output)
    remaining <- remaining - length(bytes)
  }
  if (remaining != 0) stop("ZIP member is shorter than its reported size")
  invisible(NULL)
}

# keep promotion isolated so synthetic tests can exercise a failed rename
.liss_zip_move <- function(from, to) file.rename(from, to)

# validate and stage before promotion; NULL keeps the archive in the caller
# ordinary file replacements are not a transaction across the whole archive
.verify_unzip <- function(zip_path, exdir) {
  tryCatch({
    zip_path <- normalizePath(zip_path, winslash = "/", mustWork = TRUE)
    if (!utils::file_test("-f", zip_path)) stop("ZIP input is not a regular file")
    plan <- .liss_zip_plan(utils::unzip(zip_path, list = TRUE, unzip = "internal"))
    if (!dir.exists(exdir) && !dir.create(exdir, recursive = TRUE))
      stop("cannot create ZIP destination")
    root <- normalizePath(exdir, winslash = "/", mustWork = TRUE)
    stage <- tempfile(".lissr_unzip_", tmpdir = root)
    if (!dir.create(stage, mode = "0700")) stop("cannot create ZIP staging directory")
    on.exit(unlink(stage, recursive = TRUE, expand = FALSE), add = TRUE)
    layout <- file.path(stage, "layout")
    payloads <- file.path(stage, "payloads")
    if (!dir.create(layout) || !dir.create(payloads)) stop("cannot prepare ZIP staging")
    blocked <- c(normalizePath(stage, winslash = "/", mustWork = TRUE), zip_path)
    targets <- vapply(seq_along(plan$members), function(index)
      .liss_zip_target(root, plan$parts[[index]], plan$directories[[index]], blocked), character(1))

    # reserve protected names in the same filesystem alias checks as member paths
    reserved <- file.path(layout, basename(stage))
    root_prefix <- paste0(sub("/+$", "", root), "/")
    if (startsWith(zip_path, root_prefix))
      reserved <- c(reserved, file.path(layout, substring(zip_path, nchar(root_prefix) + 1L)))
    for (value in reserved) {
      if (!dir.exists(dirname(value)) && !dir.create(dirname(value), recursive = TRUE))
        stop("cannot reserve protected ZIP layout")
      if (!file.create(value)) stop("cannot reserve protected ZIP name")
    }

    # regular placeholder paths expose aliases on the destination filesystem
    for (index in seq_along(plan$members)) {
      value <- do.call(file.path, c(list(layout), as.list(plan$parts[[index]])))
      if (plan$directories[[index]]) {
        if (!dir.exists(value) && !dir.create(value, recursive = TRUE)) stop("invalid ZIP directory layout")
      } else {
        if (!dir.exists(dirname(value)) && !dir.create(dirname(value), recursive = TRUE))
          stop("invalid ZIP parent layout")
        if (file.exists(value) || dir.exists(value) || !file.create(value))
          stop("aliased ZIP file names")
      }
    }
    staged <- file.path(payloads, as.character(plan$file_indices))
    for (position in seq_along(plan$file_indices)) {
      index <- plan$file_indices[[position]]
      .liss_zip_stream(zip_path, plan$members[[index]], staged[[position]], plan$sizes[[index]])
      if (file.info(staged[[position]])$size != plan$sizes[[index]]) stop("ZIP staging size mismatch")
    }

    # check the full destination plan again before promoting any payload
    for (index in seq_along(plan$members))
      .liss_zip_target(root, plan$parts[[index]], plan$directories[[index]], blocked)
    for (index in seq_along(plan$members)) {
      directory <- if (plan$directories[[index]]) targets[[index]] else dirname(targets[[index]])
      if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE))
        stop("cannot create ZIP output directory")
      .liss_zip_target(root, plan$parts[[index]], plan$directories[[index]], blocked)
    }
    for (position in seq_along(plan$file_indices)) {
      index <- plan$file_indices[[position]]
      target <- .liss_zip_target(root, plan$parts[[index]], FALSE, blocked)
      if (file.exists(target) && !isTRUE(Sys.chmod(staged[[position]], file.info(target)$mode, use_umask = FALSE)))
        stop("cannot retain ZIP destination permissions")
      if (!isTRUE(.liss_zip_move(staged[[position]], target))) stop("cannot promote ZIP payload")
      .liss_zip_target(root, plan$parts[[index]], FALSE, blocked)
      if (file.info(target)$size != plan$sizes[[index]]) stop("ZIP output size mismatch")
    }
    file.path(path.expand(exdir), plan$members[plan$file_indices])
  }, warning = function(condition) NULL, error = function(condition) NULL)
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
