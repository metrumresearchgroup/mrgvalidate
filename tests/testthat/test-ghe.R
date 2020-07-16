context("Optionally test hitting GHE")

if (Sys.getenv("MRGVALIDATE_TEST_GHE") != "true") {
  skip("Only run GHE tests when MRGVALIDATE_TEST_GHE set to true")
}

test_that("get_issues() pull from GHE", {
  release_issues <- get_issues(org = GHE_ORG, repo = GHE_REPO, mile = GHE_MILESTONE, domain = GHE_DOMAIN)
  expect_equal(nrow(release_issues), STORIES_DF_ROWS_GHE)
})
