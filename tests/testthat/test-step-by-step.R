context("Test step-by-step functions")

test_that("no docs exist at the beginning", {
  expect_false(fs::file_exists(ALL_TESTS))
  expect_false(fs::file_exists(paste0(tools::file_path_sans_ext(REQ_FILE), ".docx")))
  expect_false(fs::file_exists(paste0(tools::file_path_sans_ext(VAL_FILE), ".docx")))
  expect_false(fs::file_exists(paste0(tools::file_path_sans_ext(MAT_FILE), ".docx")))
  expect_false(fs::file_exists(REQ_FILE))
  expect_false(fs::file_exists(VAL_FILE))
  expect_false(fs::file_exists(MAT_FILE))
  expect_false(fs::file_exists(STORY_RDS))
})

test_that("pull_tagged_repo() gets clones and gets commit hash", {
  commit_hash <- pull_tagged_repo(org = ORG, repo = REPO, tag = TAG, domain = DOMAIN)
  expect_identical(commit_hash, COMMIT_REF)
})

test_that("validate_tests() writes csv results", {
  validate_tests(pkg = REPO)
  expect_true(fs::file_exists(ALL_TESTS))

  test_df <- readr::read_csv(ALL_TESTS, col_types = readr::cols())
  expect_equal(nrow(test_df), TEST_DF_ROWS)
  expect_equal(ncol(test_df), TEST_DF_COLS)
  expect_equal(sum(test_df$failed), 0)

})

test_that("write_validation_testing() dry_run renders", {

  write_validation_testing(
    org = ORG,
    repo = REPO,
    version = TAG,
    dry_run = TRUE
  )

  expect_true(fs::file_exists(paste0(tools::file_path_sans_ext(VAL_FILE), ".docx")))
  expect_true(fs::file_exists(VAL_FILE))

  val_text <- readr::read_file(VAL_FILE)
  expect_true(str_detect(val_text, VAL_TITLE))
  expect_true(str_detect(val_text, VAL_BOILER))
})


test_that("get_issues() and process_stories() pull from github", {
  release_issues <- get_issues(org = ORG, repo = REPO, mile = MILESTONE, domain = DOMAIN)
  stories_df <- process_stories(release_issues, org = ORG, repo = REPO, domain = DOMAIN)

  expect_equal(nrow(stories_df), STORIES_DF_ROWS)
  expect_equal(ncol(stories_df), STORIES_DF_COLS)

  saveRDS(stories_df, STORY_RDS)
})

test_that("write_requirements() renders", {
  stories_df <- readRDS(STORY_RDS)

  write_requirements(
    df = stories_df,
    pkg = REPO,
    version = TAG
  )

  expect_true(fs::file_exists(paste0(tools::file_path_sans_ext(REQ_FILE), ".docx")))
  expect_true(fs::file_exists(REQ_FILE))

  req_text <- readr::read_file(REQ_FILE)
  expect_true(str_detect(req_text, REQ_TITLE))
  expect_true(str_detect(req_text, REQ_BOILER))
})

test_that("write_traceability_matrix() renders", {
  stories_df <- readRDS(STORY_RDS)

  write_traceability_matrix(
    df = stories_df,
    pkg = REPO,
    version = TAG
  )

  expect_true(fs::file_exists(paste0(tools::file_path_sans_ext(MAT_FILE), ".docx")))
  expect_true(fs::file_exists(MAT_FILE))

  mat_text <- readr::read_file(MAT_FILE)
  expect_true(str_detect(mat_text, MAT_TITLE))
  expect_true(str_detect(mat_text, MAT_BOILER))
})

cleanup()
