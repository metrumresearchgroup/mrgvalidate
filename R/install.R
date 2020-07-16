#' Install a GitHub package to a temporary location
#'
#' @inheritParams remotes::install_github
#'
#' @return the temporary directory, invisibly
#' @keywords internal
install_temp_github_package <- function(repo, ref) {
  dir <- tempdir()
  install_with_tests(
    repo,
    ref = ref,
    lib = dir,
    force = TRUE,
    quiet = TRUE
  )
  invisible(dir)
}

#' Install a GitHub package with tests
#'
#' This is a light wrapper around [remotes::install_github()].
#'
#' @inheritParams remotes::install_github
#' @param ... additional arguments passed to [remotes::install_github()]
#' @keywords internal
install_with_tests <- function(repo, ref = "HEAD", ...) {
  remotes::install_github(
    repo = repo,
    ref = ref,
    upgrade = "never",
    INSTALL_opts = c("--install-tests"),
    ...
  )
}
