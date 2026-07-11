# package load hooks

# refresh the controlled action vocabulary from the INSTALLED registry at
# load time. the file on disk is authoritative for the running package;
# resolving it here (rather than at install/build time) guarantees the path
# points at this installation and never at a previously installed version.
.onLoad <- function(libname, pkgname) {
  path <- system.file("extdata", "action_vocabulary.yml", package = pkgname,
                      lib.loc = libname)
  if (nzchar(path) && file.exists(path)) {
    reg <- tryCatch(.load_action_registry(path), error = function(e) NULL)
    if (!is.null(reg)) {
      ns <- asNamespace(pkgname)
      assign("VALID_ACTIONS", reg$actions, envir = ns)
      assign(".ACTION_PAYLOADS", reg$payloads, envir = ns)
      assign(".ACTION_STATUS", reg$statuses, envir = ns)
    }
  }
  invisible()
}
