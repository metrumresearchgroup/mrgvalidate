test_that("read_csv_test_results() can read test results", {
  output_dir <- file.path(TEST_INPUTS_DIR, "validation-results-sample")
  tres <- read_csv_test_results(output_dir)

  df <- tres$results
  expect_setequal(names(df),
                  c("result_file", "TestName",
                    "passed", "failed", "TestId"))

  info <- tres$info
  files <- unique(df$result_file)
  expect_setequal(files, c("util-results", "vscode-julia-results"))
  expect_setequal(files, names(info))

  expect_setequal(names(info[[1]]), c("date", "executor", "info"))
  expect_true(stringr::str_detect(info[[1]]$date,
                                  "^[0-9]+\\-[0-9]+\\-[0-9]+"))
})

test_that("read_csv_test_results() gives helpful error if no CSVs are found", {
  withr::with_tempdir({
    expect_error(read_csv_test_results(getwd()),
                 class = "mrgvalidate_input_error")
  })
})

test_that("read_csv_test_results() errors if JSON sidecar is missing", {
  withr::with_tempdir({
    file.create("dummy.csv")
    expect_error(read_csv_test_results(getwd()),
                 class = "mrgvalidate_missing_result_info")
  })
})

test_that("read_manual_test_results() works correctly", {
  output_dir <- file.path(TEST_INPUTS_DIR, "manual-tests-sample")
  res_df <- read_manual_test_results(output_dir)

  expect_setequal(names(res_df),
                  c("TestId", "content",
                    "TestName", "date"))

  purrr::walk(names(res_df), function(.n) {
    expect_true(inherits(res_df[[.n]], "character"))
    expect_true(all(purrr::map_lgl(res_df[[.n]], ~!is.null(.x))))
    expect_true(all(purrr::map_lgl(res_df[[.n]], ~nchar(.x) > 1)))
  })
})

test_that("read_manual_test_results() gives helpful error on empty input", {
  withr::with_tempdir({
    expect_error(read_manual_test_results(getwd()),
                 class = "mrgvalidate_input_error")
  })
})

test_that("read_manual_test_results() errors out if missing required attributes", {
  output_dir <- file.path(TEST_INPUTS_DIR, "manual-tests-sample")
  test_dir <- file.path(tempdir(), "mrgvalidate-read-results")
  on.exit(fs::dir_delete(test_dir))
  fs::dir_copy(output_dir, test_dir)

  # Remove executor from a test
  test1_dir <- list.files(test_dir)[1]
  test1_files <- list.files(file.path(test_dir, test1_dir))
  test1_md_file <- test1_files[grep(".md", test1_files)]
  test1_path <- file.path(test_dir, test1_dir, test1_md_file)
  test1 <- readLines(test1_path) %>%
    stringr::str_replace("\\* executor: Seth Green", "")
  writeLines(test1, test1_path)

  expect_error(
    res_df <- read_manual_test_results(test_dir),
    "The test ids MAN-FAKE-001, are missing the following attributes respectively"
  )

})
