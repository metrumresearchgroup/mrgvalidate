context("Test full wrapper")

library(stringr)

test_that("create_validation_docs() renders", {

  abort("THESE TESTS ARENT IMPLEMENTED YET")

  # set up clean docs output dir
  output_dir <- file.path(tempdir(), "mrgvalidate-create-validation-docs")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit({ fs::dir_delete(output_dir) })

  specs <- data.frame() ### NEED TO ADD A TEST DF FOR THIS
                          # (pull some subset of the googlesheet and write to inst?)
  roles <- data.frame()   # same as ^

  mrgvalidate::create_validation_docs(
    product_name = "Metworx TEST",
    version = "vFAKE",
    specs,
    auto_test_dir = system.file("validation-results-sample", package = "mrgvalidate"),
    man_test_dir = system.file("manual-tests-sample", package = "mrgvalidate"), ##### NEED TO ADD SOME SAMPLES HERE
    roles = roles
  )

  # check that files exist
  expect_true(fs::file_exists(file.path(output_dir, paste0(tools::file_path_sans_ext(REQ_FILE), ".docx"))))
  expect_true(fs::file_exists(file.path(output_dir, paste0(tools::file_path_sans_ext(VAL_FILE), ".docx"))))
  expect_true(fs::file_exists(file.path(output_dir, paste0(tools::file_path_sans_ext(MAT_FILE), ".docx"))))
  expect_true(fs::file_exists(file.path(output_dir, REQ_FILE)))
  expect_true(fs::file_exists(file.path(output_dir, VAL_FILE)))
  expect_true(fs::file_exists(file.path(output_dir, MAT_FILE)))

  # check that the markdown looks right
  # DO WE WANT TO HAVE GOLDEN FILES THAT WE CHECK AGAINST OR IS THIS ENOUGH?
  req_text <- readr::read_file(file.path(output_dir, REQ_FILE))
  expect_true(str_detect(req_text, REQ_TITLE))
  expect_true(str_detect(req_text, REQ_BOILER))

  val_text <- readr::read_file(file.path(output_dir, VAL_FILE))
  expect_true(str_detect(val_text, VAL_TITLE))
  expect_true(str_detect(val_text, VAL_BOILER))

  mat_text <- readr::read_file(file.path(output_dir, MAT_FILE))
  expect_true(str_detect(mat_text, MAT_TITLE))
  expect_true(str_detect(mat_text, MAT_BOILER))

})
