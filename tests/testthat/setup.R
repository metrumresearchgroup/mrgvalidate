###########################
# constants for test input
###########################


TEST_INPUTS_DIR <- system.file("test-inputs", package = "mrgvalidate")
CREATE_OUT_DF_NAMES <- c(
  "StoryId",
  "StoryName",
  "StoryDescription",
  "ProductRisk",
  "RequirementId",
  "RequirementDescription",
  "date",
  "test_type",
  "tests"
)

rename_val_file <- function(out_file, product_name, ext = "docx"){
  paste0(tools::file_path_sans_ext(out_file),"-", product_name, ".", ext)
}

check_files <- function(product_name, output_dir){
  expect_true(fs::file_exists(file.path(output_dir, rename_val_file(VAL_PLAN_FILE, product_name))))
  expect_true(fs::file_exists(file.path(output_dir, rename_val_file(TEST_PLAN_FILE, product_name))))
  expect_true(fs::file_exists(file.path(output_dir, rename_val_file(TEST_RESULTS_FILE, product_name))))
  expect_true(fs::file_exists(file.path(output_dir, rename_val_file(MAT_FILE, product_name))))
  expect_true(fs::file_exists(file.path(output_dir, rename_val_file(REQ_FILE, product_name))))
  expect_true(fs::file_exists(file.path(output_dir, rename_val_file(VAL_SUM_FILE, product_name))))
  expect_true(fs::file_exists(file.path(output_dir, rename_val_file(RLS_NOTES_FILE, product_name))))


  expect_true(fs::file_exists(file.path(output_dir, rename_val_file(VAL_PLAN_FILE, product_name, "Rmd"))))
  expect_true(fs::file_exists(file.path(output_dir, rename_val_file(TEST_PLAN_FILE, product_name, "Rmd"))))
  expect_true(fs::file_exists(file.path(output_dir, rename_val_file(TEST_RESULTS_FILE, product_name, "Rmd"))))
  expect_true(fs::file_exists(file.path(output_dir, rename_val_file(MAT_FILE, product_name, "Rmd"))))
  expect_true(fs::file_exists(file.path(output_dir, rename_val_file(REQ_FILE, product_name, "Rmd"))))
  expect_true(fs::file_exists(file.path(output_dir, rename_val_file(VAL_SUM_FILE, product_name, "Rmd"))))
  expect_true(fs::file_exists(file.path(output_dir, rename_val_file(RLS_NOTES_FILE, product_name, "Rmd"))))
}

get_boiler_text <- function(type){
  VAL_PLAN_BOILER <- get_template("validation_plan", type = type) %>% readLines() %>% paste(collapse = "\n")
  TEST_PLAN_BOILER <- get_template("testing_plan", type = type) %>% readLines() %>% paste(collapse = "\n")
  TEST_RESULTS_BOILER <- get_template("testing_results", type = type) %>% readLines() %>% paste(collapse = "\n")
  MAT_BOILER <- get_template("traceability_matrix", type = type) %>% readLines() %>% paste(collapse = "\n")
  REQ_BOILER <- get_template("requirements_specification", type = type) %>% readLines() %>% paste(collapse = "\n")
  VAL_SUM_BOILER <- get_template("validation_summary", type = type) %>% readLines() %>% paste(collapse = "\n")
  RLS_NOTES_BOILER <- get_template("release_notes", type = type) %>% readLines() %>% paste(collapse = "\n")
  return(
    list(
      VAL_PLAN_BOILER = VAL_PLAN_BOILER,
      TEST_PLAN_BOILER = TEST_PLAN_BOILER,
      TEST_RESULTS_BOILER = TEST_RESULTS_BOILER,
      MAT_BOILER = MAT_BOILER,
      REQ_BOILER = REQ_BOILER,
      VAL_SUM_BOILER = VAL_SUM_BOILER,
      RLS_NOTES_BOILER = RLS_NOTES_BOILER
    )
  )
}
