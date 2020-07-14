context("Test validation testing function")

library(stringr)
source("data/contants_for_testing.R")

test_that("write_validation_testing() renders", {
  skip("foo")
  on.exit({ cleanup() })
  cleanup()

  write_validation_testing(
    org = ORG,
    repo = REPO,
    version = TAG,
    domain = DOMAIN
  )

  expect_true(fs::file_exists(ALL_TESTS))
  expect_true(fs::file_exists(paste0(tools::file_path_sans_ext(VAL_FILE), ".docx")))
  expect_true(fs::file_exists(VAL_FILE))

  val_text <- readr::read_file(VAL_FILE)
  expect_true(str_detect(val_text, VAL_TITLE))
  expect_true(str_detect(val_text, VAL_BOILER))

})
