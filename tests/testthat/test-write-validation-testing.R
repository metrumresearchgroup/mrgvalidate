context("Test validation testing function")

test_that("write_validation_testing() renders", {
  tmp_dir <- withr::local_tempdir()
  withr::local_dir(tmp_dir)

  write_validation_testing(
    org = ORG,
    repo = REPO,
    version = TAG,
    domain = DOMAIN
  )

  expect_true(fs::file_exists(ALL_TESTS))
  expect_true(fs::file_exists(fs::path_ext_set(VAL_FILE, "docx")))
  expect_true(fs::file_exists(VAL_FILE))

  val_text <- readr::read_file(VAL_FILE)
  expect_true(stringr::str_detect(val_text, VAL_TITLE))
  expect_true(stringr::str_detect(val_text, VAL_BOILER))

})
