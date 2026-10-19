# independent reference-profile oracle for invented fixtures and resealing probes
.nr_identity_hash <- function(value) {
  bytes <- serialize(value, NULL, version = 2L)
  bytes[7:10] <- as.raw(c(0L, 4L, 6L, 1L))
  digest <- as.character(openssl::sha256(bytes))
  attributes(digest) <- NULL
  digest
}
