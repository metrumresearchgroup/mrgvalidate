test_that("read_csv_test_results() can read test results", {
  output_dir <- system.file("validation-results-sample",
                            package = "mrgvalidate")
  tres <- read_csv_test_results(output_dir)

  df <- tres$results
  expect_setequal(names(df),
                  c("result_file", "test_name",
                    "passed", "failed", "skipped", "test_tag"))

  info <- tres$info
  files <- unique(df$result_file)
  expect_setequal(files, c("util-results", "vscode-julia-results"))
  expect_setequal(files, names(info))

  expect_setequal(names(info[[1]]), c("date", "info"))
  expect_true(stringr::str_detect(info[[1]]$date,
                                  "^[0-9]+\\-[0-9]+\\-[0-9]+"))
})

test_that("read_csv_test_results() errors if JSON sidecar is missing", {
  withr::with_tempdir({
    file.create("dummy.csv")
    expect_error(read_csv_test_results(getwd()),
                 class = "mrgvalidate_missing_result_info")
  })
})
