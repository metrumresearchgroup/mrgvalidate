library(stringr)

test_that("create_validation_docs() renders markdown", {
  # set up clean docs output dir
  output_dir <- file.path(tempdir(), "mrgvalidate-create-validation-docs")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit({ fs::dir_delete(output_dir) })

  specs <- readRDS(file.path(TEST_INPUTS_DIR, "specs.RDS"))
  mrgvalidate::create_validation_docs(
    product_name = "Metworx TEST",
    version = "vFAKE",
    specs = specs,
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

  # get TestId's we expect to see
  test_ids <- c(
    read_csv_test_results(file.path(TEST_INPUTS_DIR, "validation-results-sample"))$results %>% pull(TestId),
    read_manual_test_results(file.path(TEST_INPUTS_DIR, "manual-tests-sample")) %>% pull(TestId)
  ) %>% unique()


  # check that the markdown looks right
  # DO WE WANT TO HAVE GOLDEN FILES THAT WE CHECK AGAINST OR IS THIS ENOUGH?
  req_text <- readr::read_file(file.path(output_dir, REQ_FILE))
  expect_true(str_detect(req_text, REQ_TITLE))
  expect_true(str_detect(req_text, REQ_BOILER))
  expect_true(str_detect(req_text, REQ_TITLE))
  expect_true(all(str_detect(req_text, specs$RequirementId)))

  val_text <- readr::read_file(file.path(output_dir, VAL_FILE))
  expect_true(str_detect(val_text, VAL_TITLE))
  expect_true(str_detect(val_text, VAL_BOILER))
  expect_true(all(str_detect(val_text, test_ids)))

  mat_text <- readr::read_file(file.path(output_dir, MAT_FILE))
  expect_true(str_detect(mat_text, MAT_TITLE))
  expect_true(str_detect(mat_text, MAT_BOILER))
  expect_true(all(str_detect(mat_text, test_ids)))

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
    output_dir = output_dir,
    write = FALSE
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

test_that("create_validation_docs() works with no requirements", {
  # set up clean docs output dir
  output_dir <- file.path(tempdir(), "mrgvalidate-create-validation-docs")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit({ fs::dir_delete(output_dir) })

  res_df <- mrgvalidate::create_validation_docs(
    product_name = "Metworx TEST",
    version = "vFAKE",
    specs = readRDS(file.path(TEST_INPUTS_DIR, "specs.RDS")) %>% select(-RequirementId, -RequirementDescription),
    auto_test_dir = file.path(TEST_INPUTS_DIR, "validation-results-sample"),
    man_test_dir = file.path(TEST_INPUTS_DIR, "manual-tests-sample"),
    roles = readr::read_csv(file.path(TEST_INPUTS_DIR, "roles.csv"), col_types = "cc"),
    output_dir = output_dir,
  )

  expect_true(nrow(res_df) > 1)
  expect_equal(
    names(res_df),
    stringr::str_subset(CREATE_OUT_DF_NAMES, "RequirementId|RequirementDescription", negate = TRUE)
  )

  purrr::walk(names(res_df)[1:ncol(res_df)-1], function(.n) {
    expect_true(inherits(res_df[[.n]], "character"))
    expect_true(all(purrr::map_lgl(res_df[[.n]], ~!is.null(.x))))
    expect_true(all(purrr::map_lgl(res_df[[.n]], ~nchar(.x) > 1)))
  })
  expect_true(inherits(res_df$tests, "list"))
  expect_true(all(purrr::map_lgl(res_df$tests, ~inherits(.x, "tbl_df"))))

  req_text <- readr::read_file(file.path(output_dir, REQ_FILE))
  # Summary of requirements is dropped if there are no requirements.
  expect_false(str_detect(req_text, "Summary"))
})

test_that("create_validation_docs() can auto-assign test IDs", {
  output_dir <- file.path(tempdir(), "mrgvalidate-create-validation-docs")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit(fs::dir_delete(output_dir))

  specs <- tibble::tribble(
    ~StoryId, ~StoryName, ~StoryDescription, ~ProductRisk, ~TestIds,
    "st001", "sn1", "story one", "low", c("name one", "name two"),
    "st002", "sn2", "story two", "low", "name three")

  auto_test_dir <- file.path(output_dir, "auto-tests")
  fs::dir_create(auto_test_dir)
  readr::write_csv(
    tibble::tribble(
      ~TestName, ~TestId, ~passed, ~failed,
      "name one", "name one", 1, 0,
      "name two", "name two", 1, 0,
      "name three", "name three", 1, 0),
    file.path(auto_test_dir, "justname.csv"))

  fs::file_copy(
    file.path(TEST_INPUTS_DIR, "validation-results-sample",
              "util-results.json"),
    file.path(auto_test_dir, "justname.json"))

  expect_warning(
    res_df <- create_validation_docs(
      product_name = "Metworx TEST",
      version = "vFAKE",
      specs = specs,
      auto_test_dir = auto_test_dir,
      output_dir = output_dir),
    "temporary kludge")

  expected_ids <- c("METW-1", "METW-2", "METW-3")
  expect_setequal(
    res_df %>% unnest(tests) %>% pull(TestId) %>% unique(),
    expected_ids)

  for (.f in c(REQ_FILE, VAL_FILE, MAT_FILE)) {
    text <- readr::read_file(file.path(output_dir, REQ_FILE))
    expect_true(all(str_detect(text, expected_ids)),
                label = paste0("IDs in ", .f))
  }
})
