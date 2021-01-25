context("Unit tests for validate-tests.R")


test_that("validate_tests returns expected df", {
  on.exit({ cleanup() })
  cleanup()

  # get repo
  commit_hash <- pull_tagged_repo(org = ORG, repo = REPO, tag = TAG, domain = DOMAIN)

  # run tests
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

  # get repo
  commit_hash <- pull_tagged_repo(org = ORG, repo = REPO, tag = TAG, domain = DOMAIN)

  # run tests
  test_df <- validate_tests(
    pkg = REPO,
    out_file = NULL,
    return_df = TRUE,
    extra_test_dirs = EXTRA_TESTS
  )

  expect_equal(nrow(test_df), TEST_DF_ROWS_EXTRA_TESTS)
  expect_equal(ncol(test_df), TEST_DF_COLS)
  expect_equal(sum(test_df$failed), 0)
})
