context("Unit tests for validate-tests.R")

tmp_lib <- tempdir()
withr::defer(unlink(tmp_lib))

withr::local_libpaths(tmp_lib, action = "prefix")
pkg <- install_with_tests(
  repo = paste(c(ORG, REPO), collapse = "/"),
  ref = TAG,
  force = TRUE,
  quiet = TRUE
)

test_that("validate_tests returns expected df", {
  # TODO: need a teardown function that uninstalls a package, if we installed it
  on.exit({ cleanup() })
  cleanup()

  test_df <- validate_tests(
    pkg = pkg,
    path = tmp_lib,
    out_file = NULL,
    return_df = TRUE
  )

  expect_equal(nrow(test_df), TEST_DF_ROWS)
  expect_equal(ncol(test_df), TEST_DF_COLS)
  expect_equal(sum(test_df$failed), 0)
})


test_that("validate_tests returns expected df with extra tests", {
  on.exit({ cleanup() })
  cleanup()

  test_df <- validate_tests(
    pkg = pkg,
    path = tmp_lib,
    out_file = NULL,
    return_df = TRUE,
    extra_test_dirs = EXTRA_TESTS
  )

  expect_equal(nrow(test_df), TEST_DF_ROWS_EXTRA_TESTS)
  expect_equal(ncol(test_df), TEST_DF_COLS)
  expect_equal(sum(test_df$failed), 0)
})
