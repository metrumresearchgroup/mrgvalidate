
library(stringr)
library(officer)

test_that("create_validation_templates() renders package templates [VAL-TMPL-001]", {
  # set up clean docs output dir
  output_dir <- file.path(tempdir(), "mrgvalidate-create-templates")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit({ fs::dir_delete(output_dir) })

  product_name <- "package"

  create_validation_templates(
    type = product_name,
    output_dir = output_dir,
    new_folder = "package_templates",
    cleanup_rmd = FALSE
  )

  full_output_dir <- file.path(output_dir, "package_templates")
  # check that files exist
  check_files(product_name, full_output_dir)


  # check that the markdown looks right
  boiler_text <- get_boiler_text("package")

  val_plan_text <- readr::read_file(file.path(full_output_dir, rename_val_file(VAL_PLAN_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$VAL_PLAN_BOILER, val_plan_text, fixed = TRUE))

  test_plan_text <- readr::read_file(file.path(full_output_dir, rename_val_file(TEST_PLAN_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$TEST_PLAN_BOILER, test_plan_text, fixed = TRUE))

  test_results_text <- readr::read_file(file.path(full_output_dir, rename_val_file(TEST_RESULTS_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$TEST_RESULTS_BOILER, test_results_text, fixed = TRUE))

  req_text <- readr::read_file(file.path(full_output_dir, rename_val_file(REQ_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$REQ_BOILER, req_text, fixed = TRUE))

  mat_text <- readr::read_file(file.path(full_output_dir, rename_val_file(MAT_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$MAT_BOILER, mat_text, fixed = TRUE))

  val_sum_text <- readr::read_file(file.path(full_output_dir, rename_val_file(VAL_SUM_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$VAL_SUM_BOILER, val_sum_text, fixed = TRUE))

  rls_notes_text <- readr::read_file(file.path(full_output_dir, rename_val_file(RLS_NOTES_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$RLS_NOTES_BOILER, rls_notes_text, fixed = TRUE))

})


test_that("create_validation_templates() renders metworx templates [VAL-TMPL-002]", {
  # set up clean docs output dir
  output_dir <- file.path(tempdir(), "mrgvalidate-create-templates")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit({ fs::dir_delete(output_dir) })

  product_name <- "metworx"

  create_validation_templates(
    type = product_name,
    output_dir = output_dir,
    new_folder = "metworx_templates",
    cleanup_rmd = FALSE
  )

  full_output_dir <- file.path(output_dir, "metworx_templates")
  # check that files exist
  check_files(product_name, full_output_dir)


  # check that the markdown looks right
  boiler_text <- get_boiler_text("metworx")

  val_plan_text <- readr::read_file(file.path(full_output_dir, rename_val_file(VAL_PLAN_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$VAL_PLAN_BOILER, val_plan_text, fixed = TRUE))

  test_plan_text <- readr::read_file(file.path(full_output_dir, rename_val_file(TEST_PLAN_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$TEST_PLAN_BOILER, test_plan_text, fixed = TRUE))

  test_results_text <- readr::read_file(file.path(full_output_dir, rename_val_file(TEST_RESULTS_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$TEST_RESULTS_BOILER, test_results_text, fixed = TRUE))

  req_text <- readr::read_file(file.path(full_output_dir, rename_val_file(REQ_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$REQ_BOILER, req_text, fixed = TRUE))

  mat_text <- readr::read_file(file.path(full_output_dir, rename_val_file(MAT_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$MAT_BOILER, mat_text, fixed = TRUE))

  val_sum_text <- readr::read_file(file.path(full_output_dir, rename_val_file(VAL_SUM_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$VAL_SUM_BOILER, val_sum_text, fixed = TRUE))

  rls_notes_text <- readr::read_file(file.path(full_output_dir, rename_val_file(RLS_NOTES_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$RLS_NOTES_BOILER, rls_notes_text, fixed = TRUE))

})
