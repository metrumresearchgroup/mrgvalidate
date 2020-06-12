context("Test full wrapper")

library(stringr)
source("data/contants_for_testing.R")

test_that("generate_docs() renders", {
  on.exit({ cleanup() })

  mrgvalidate::generate_docs(
    org = ORG,
    repo = REPO,
    milestone = MILESTONE,
    version = TAG,
    domain = DOMAIN
  )

  # check that files exist
  expect_true(fs::file_exists(ALL_TESTS))
  expect_true(fs::file_exists(paste0(tools::file_path_sans_ext(REQ_FILE), ".docx")))
  expect_true(fs::file_exists(paste0(tools::file_path_sans_ext(VAL_FILE), ".docx")))
  expect_true(fs::file_exists(paste0(tools::file_path_sans_ext(MAT_FILE), ".docx")))
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
  on.exit({ cleanup() })
  cleanup()
  fs::dir_create(OUTPUT_DIR)

  mrgvalidate::generate_docs(
    org = ORG,
    repo = REPO,
    milestone = MILESTONE,
    version = TAG,
    domain = DOMAIN,
    output_dir = OUTPUT_DIR
  )

  # check that files exist
  expect_true(fs::file_exists(file.path(OUTPUT_DIR, ALL_TESTS)))
  expect_true(fs::file_exists(file.path(OUTPUT_DIR, paste0(tools::file_path_sans_ext(REQ_FILE), ".docx"))))
  expect_true(fs::file_exists(file.path(OUTPUT_DIR, paste0(tools::file_path_sans_ext(VAL_FILE), ".docx"))))
  expect_true(fs::file_exists(file.path(OUTPUT_DIR, paste0(tools::file_path_sans_ext(MAT_FILE), ".docx"))))
  expect_true(fs::file_exists(file.path(OUTPUT_DIR, REQ_FILE)))
  expect_true(fs::file_exists(file.path(OUTPUT_DIR, VAL_FILE)))
  expect_true(fs::file_exists(file.path(OUTPUT_DIR, MAT_FILE)))

  # check that the markdown looks right
  req_text <- readr::read_file(file.path(OUTPUT_DIR, REQ_FILE))
  expect_true(str_detect(req_text, REQ_TITLE))
  expect_true(str_detect(req_text, REQ_BOILER))

  val_text <- readr::read_file(file.path(OUTPUT_DIR, VAL_FILE))
  expect_true(str_detect(val_text, VAL_TITLE))
  expect_true(str_detect(val_text, VAL_BOILER))

  mat_text <- readr::read_file(file.path(OUTPUT_DIR, MAT_FILE))
  expect_true(str_detect(mat_text, MAT_TITLE))
  expect_true(str_detect(mat_text, MAT_BOILER))

})
