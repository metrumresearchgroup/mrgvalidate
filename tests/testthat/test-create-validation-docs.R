context("Test full wrapper")

library(stringr)

test_that("create_validation_docs() renders markdown", {
  # set up clean docs output dir
  output_dir <- file.path(tempdir(), "mrgvalidate-create-validation-docs")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit({ fs::dir_delete(output_dir) })

  mrgvalidate::create_validation_docs(
    product_name = "Metworx TEST",
    version = "vFAKE",
    specs = readRDS(file.path(TEST_INPUTS_DIR, "specs.RDS")),
    auto_test_dir = file.path(TEST_INPUTS_DIR, "validation-results-sample"),
    man_test_dir = file.path(TEST_INPUTS_DIR, "manual-tests-sample"),
    roles = readr::read_csv(file.path(TEST_INPUTS_DIR, "roles.csv"), col_types = "cc"),
    output_dir = output_dir
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


test_that("create_validation_docs() returns data df", {
  # set up clean docs output dir
  output_dir <- file.path(tempdir(), "mrgvalidate-create-validation-docs")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit({ fs::dir_delete(output_dir) })

  res_df <- mrgvalidate::create_validation_docs(
    product_name = "Metworx TEST",
    version = "vFAKE",
    specs = readRDS(file.path(TEST_INPUTS_DIR, "specs.RDS")),
    auto_test_dir = file.path(TEST_INPUTS_DIR, "validation-results-sample"),
    man_test_dir = file.path(TEST_INPUTS_DIR, "manual-tests-sample"),
    roles = readr::read_csv(file.path(TEST_INPUTS_DIR, "roles.csv"), col_types = "cc"),
    output_dir = output_dir # CHANGE TO write = FALSE
  )

  expect_true(nrow(res_df) > 1)
  expect_equal(names(res_df), CREATE_OUT_DF_NAMES)

  purrr::walk(names(res_df)[1:ncol(res_df)-1], function(.n) {
    expect_true(inherits(res_df[[.n]], "character"))
    expect_true(all(purrr::map_lgl(res_df[[.n]], ~!is.null(.x))))
    expect_true(all(purrr::map_lgl(res_df[[.n]], ~nchar(.x) > 1)))
  })
  expect_true(inherits(res_df$tests, "list"))
  expect_true(all(purrr::map_lgl(res_df$tests, ~inherits(.x, "tbl_df"))))

})
