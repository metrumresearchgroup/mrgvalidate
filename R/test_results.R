#' Get test results for a package
#'
#' @inheritParams remotes::install_github
#' @param dry_run `TRUE` to look for test results in `all_tests.csv`, `FALSE` to
#'   download the package and run the tests
#' @param ... additional arguments passed to [validate_tests()]
#'
#' @return the test results, as a tibble
#' @export
test_results <- function(dry_run = TRUE, repo = NULL, ref = NULL, ...) {
  checkmate::assert_logical(dry_run, len = 1L)
  checkmate::assert_string(repo, null.ok = TRUE)
  checkmate::assert_string(ref, null.ok = TRUE)

  tmp_lib <- tempdir()
  # TODO: consider replacing with withr::defer()
  on.exit(unlink(tmp_lib))

  if (dry_run) {
    test_df <- readr::read_csv(ALL_TESTS, col_types = readr::cols())
  } else {
    withr::local_libpaths(tmp_lib, action = "prefix")
    print(.libPaths())
    pkg <- install_with_tests(
      repo = repo,
      ref = ref,
      dependencies = FALSE,
      force = TRUE,
      # quiet = TRUE
    )

    print(fs::dir_ls(file.path(tmp_lib, pkg)))

    test_df <- validate_tests(
      pkg = pkg,
      path = tmp_lib,
      ...
    )
  }
  test_df
}
