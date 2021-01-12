context("Unit tests for validate-tests.R")

# clone into a temporary directory
tmp_dir <- withr::local_tempdir()
commit_hash <- pull_tagged_repo(
  org = ORG,
  repo = REPO,
  tag = TAG,
  domain = DOMAIN,
  dest_dir = tmp_dir,
  overwrite = FALSE
)

test_that("validate_tests returns expected df", {
  test_df <- validate_tests(
    pkg = REPO,
    root_dir = tmp_dir,
    out_file = NULL,
    return_df = TRUE
  )

  expect_equal(nrow(test_df), TEST_DF_ROWS)
  expect_equal(ncol(test_df), TEST_DF_COLS)
  expect_equal(sum(test_df$failed), 0)
})


test_that("validate_tests returns expected df with extra tests", {
  test_df <- validate_tests(
    pkg = REPO,
    root_dir = tmp_dir,
    out_file = NULL,
    return_df = TRUE,
    extra_test_dirs = EXTRA_TESTS
  )

  expect_equal(nrow(test_df), TEST_DF_ROWS_EXTRA_TESTS)
  expect_equal(ncol(test_df), TEST_DF_COLS)
  expect_equal(sum(test_df$failed), 0)
})
