#' Run tests for an installed package
#'
#' `package` must have been installed with `--install-tests`
#'
#' @inheritParams testthat::test_dir
#' @param unload `TRUE` to unload `pkg` after running its tests
#'
#' @return list
run_installed_tests <- function(package,
                                path = NULL,
                                reporter = testthat::ListReporter$new(),
                                unload = FALSE) {
  checkmate::assert_string(package)
  checkmate::assert_string(path, null.ok = TRUE)
  checkmate::assert_logical(unload, len = 1L)

  # TODO: update to use withr
  # if (unload) {
  #   on.exit(
  #     detach(
  #       paste0("package:", package),
  #       unload = TRUE,
  #       character.only = TRUE
  #     )
  #   )
  # }

  if (is.null(path)) {
    res <- testthat::test_package(
      package,
      reporter = reporter,
      stop_on_failure = FALSE
    )
  } else {
    require(package, character.only = TRUE)
    # this gives the tests access to the package namespace, which is useful if
    # the package defines constants (for example)
    env <- list2env(
      as.list(getNamespace(package), all.names = TRUE),
      parent = parent.env(getNamespace(package))
    )
    # TODO: add package argument once available in CRAN testthat
    res <- testthat::test_dir(path, reporter = reporter, env = env)
  }
  res
}
