context("Test full wrapper")

library(stringr)

test_that("generate_docs() renders", {
  tmp_dir <- withr::local_tempdir()
  # default is to write output to the working directory
  withr::local_dir(tmp_dir)

  generate_docs(
    org = ORG,
    repo = REPO,
    milestone = MILESTONE,
    version = TAG,
    domain = DOMAIN
  )

  # check that files exist
  expect_true(fs::file_exists(ALL_TESTS))
  expect_true(fs::file_exists(fs::path_ext_set(REQ_FILE, "docx")))
  expect_true(fs::file_exists(fs::path_ext_set(VAL_FILE, "docx")))
  expect_true(fs::file_exists(fs::path_ext_set(MAT_FILE, "docx")))
  expect_true(fs::file_exists(REQ_FILE))
  expect_true(fs::file_exists(VAL_FILE))
  expect_true(fs::file_exists(MAT_FILE))

  # check that the markdown looks right
  req_text <- readr::read_file(REQ_FILE)
  expect_true(str_detect(req_text, REQ_TITLE))
  expect_true(str_detect(req_text, REQ_BOILER))

  val_text <- readr::read_file(VAL_FILE)
  expect_true(str_detect(val_text, VAL_TITLE))
  expect_true(str_detect(val_text, VAL_BOILER))

  mat_text <- readr::read_file(MAT_FILE)
  expect_true(str_detect(mat_text, MAT_TITLE))
  expect_true(str_detect(mat_text, MAT_BOILER))

})

test_that("generate_docs() renders into output_dir", {
  tmp_dir <- withr::local_tempdir()

  generate_docs(
    org = ORG,
    repo = REPO,
    milestone = MILESTONE,
    version = TAG,
    domain = DOMAIN,
    output_dir = tmp_dir
  )

  # check that files exist
  withr::local_dir(tmp_dir)

  expect_true(fs::file_exists(ALL_TESTS))
  expect_true(fs::file_exists(fs::path_ext_set(REQ_FILE, "docx")))
  expect_true(fs::file_exists(fs::path_ext_set(VAL_FILE, "docx")))
  expect_true(fs::file_exists(fs::path_ext_set(MAT_FILE, "docx")))
  expect_true(fs::file_exists(REQ_FILE))
  expect_true(fs::file_exists(VAL_FILE))
  expect_true(fs::file_exists(MAT_FILE))

  # check that the markdown looks right
  req_text <- readr::read_file(REQ_FILE)
  expect_true(str_detect(req_text, REQ_TITLE))
  expect_true(str_detect(req_text, REQ_BOILER))

  val_text <- readr::read_file(VAL_FILE)
  expect_true(str_detect(val_text, VAL_TITLE))
  expect_true(str_detect(val_text, VAL_BOILER))

  mat_text <- readr::read_file(MAT_FILE)
  expect_true(str_detect(mat_text, MAT_TITLE))
  expect_true(str_detect(mat_text, MAT_BOILER))

})
