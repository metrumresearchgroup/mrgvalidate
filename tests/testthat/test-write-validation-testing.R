context("Test validation testing function")

test_that("write_validation_testing() renders", {
  on.exit({ cleanup() })
  cleanup()

  # withr::local_libpaths(TEMP_LIB, action = "prefix")
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
  expect_true(stringr::str_detect(val_text, VAL_TITLE))
  expect_true(stringr::str_detect(val_text, VAL_BOILER))

})
