# the reference writer is part of the versioned byte-identity profile
.liss_identity_profile <- function() {
  list(algorithm = "sha256", serialization = "R_XDR_version_2",
    reference_writer = "4.6.1", profile_version = "1.0.0")
}

# only serialize-owned streams enter this helper; object bytes remain exact
.liss_identity_normalize <- function(bytes) {
  invalid <- function() rlang::abort("unsupported source identity serialization profile",
    class = "liss_serialized_identity_unsupported")
  if (!is.raw(bytes) || !is.null(attributes(bytes)) || length(bytes) < 18L ||
      !identical(bytes[1:2], charToRaw("X\n"))) invalid()
  header <- readBin(bytes[3:14], "integer", n = 3L, size = 4L, endian = "big")
  if (length(header) != 3L || anyNA(header) || !identical(header[[1L]], 2L) ||
      !identical(header[[3L]], 131840L) || header[[2L]] < header[[3L]]) invalid()
  bytes[7:10] <- as.raw(c(0L, 4L, 6L, 1L))
  bytes
}

.liss_identity_bytes <- function(value) {
  .liss_identity_normalize(serialize(value, NULL, ascii = FALSE, xdr = TRUE, version = 2L))
}

.liss_identity_hash <- function(value) {
  hash <- as.character(openssl::sha256(.liss_identity_bytes(value)))
  attributes(hash) <- NULL
  hash
}
