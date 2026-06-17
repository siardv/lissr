#' @keywords internal
#' @importFrom dplyr .data
#' @importFrom magrittr %>%
#' @importFrom stats setNames
#' @importFrom utils head
#' @seealso
#' The canonical recipe schema that every merge recipe must satisfy, shipped
#' with the package and locatable via
#' `system.file("schema", "CANONICAL_SCHEMA.md", package = "lissr")`.
#' Primary entry points: [merge_liss_module()], [load_recipe()], and
#' [validate_recipe()].
"_PACKAGE"

# shared in-memory cache for session, blueprint, and username.
# persists for the lifetime of the R session (or until the package
# namespace is unloaded).
.liss_cache <- new.env(parent = emptyenv())

# null-coalescing operator used throughout the package
`%||%` <- function(a, b) if (is.null(a)) b else a
