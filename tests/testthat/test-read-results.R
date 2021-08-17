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

  expect_setequal(names(info[[1]]), c("date", "info"))
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
