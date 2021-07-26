test_that("get_sys_info() returns date and system info", {
  res <- get_sys_info()
  expect_true(stringr::str_detect(res$date, "[0-9]+\\-[0-9]+\\-[0-9]+"))
  expect_equal(names(res$info$sys), c("sysname", "version", "release", "machine"))
})

test_that("get_sys_info() writes to json", {
  tmp_file <- tempfile()
  res <- get_sys_info(out_path = tmp_file)
  withr::defer(unlink(tmp_file))

  expect_true(fs::file_exists(tmp_file))

  written_res <- jsonlite::fromJSON(tmp_file)
  expect_true(stringr::str_detect(written_res$date, "[0-9]+\\-[0-9]+\\-[0-9]+"))
  expect_equal(names(written_res$info$sys), c("sysname", "version", "release", "machine"))
})

test_that("get_sys_info() captures environment variables", {
  res <- get_sys_info(env_vars = c("USER", "FAKE_VAR"))

  expect_true(length(res$info$env_vars$USER) > 0)
  expect_equal(res$info$env_vars$FAKE_VAR, "")
})

test_that("get_sys_info() captures sessionInfo", {
  res <- get_sys_info(session = TRUE)

  expect_true(any(stringr::str_detect(res$info$session, "R version")))
  expect_true(any(stringr::str_detect(res$info$session, "testthat")))
})
