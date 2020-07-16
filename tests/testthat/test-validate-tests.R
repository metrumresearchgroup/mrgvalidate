context("Unit tests for validate-tests.R")

test_that("validate_tests returns expected df", {
  # TODO: need a teardown function that uninstalls a package, if we installed it
  on.exit({ cleanup() })
  cleanup()

  withr::local_libpaths(TEMP_LIB, action = "prefix")
  test_df <- validate_tests(
    pkg = REPO,
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

  withr::local_libpaths(TEMP_LIB, action = "prefix")
  test_df <- validate_tests(
    pkg = REPO,
    path = TEMP_LIB,
    out_file = NULL,
    return_df = TRUE,
    extra_test_dirs = EXTRA_TESTS
  )

  expect_equal(nrow(test_df), TEST_DF_ROWS_EXTRA_TESTS)
  expect_equal(ncol(test_df), TEST_DF_COLS)
  expect_equal(sum(test_df$failed), 0)
})
