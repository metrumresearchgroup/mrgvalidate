
TEST_RESULTS <- tibble::tribble(
  ~TestId, ~TestName, ~passed, ~failed,
  "FOO-BAR-001", "t1", 2, 0,
  "FOO-BAR-002", "t2", 1, 0,
  "FOO-BAR-003", "t3", 1, 0)

# Note: This function assumes that the caller cleans up (e.g., by calling this
# within withr::with_tempdir()).
setup_test_results <- function() {
  readr::write_csv(TEST_RESULTS, "t.csv")
  fs::file_copy(
    file.path(TEST_INPUTS_DIR, "validation-results-sample", "vscode-julia-results.json"),
    "t.json",
    overwrite = TRUE
  )
}

SPECS <- tibble::tribble(
  ~StoryId, ~StoryDescription, ~RequirementId, ~RequirementDescription, ~TestIds,
  "st001", "story one", "req001", "req one", c("FOO-BAR-001", "FOO-BAR-003"),
  "st002", "story two", "req002", "req two", "FOO-OTHER-002",
  "st003", "story three", "req003", "req three", NA
)

test_that("check_test_input() filters NA IDs", {
  input <- TEST_RESULTS %>%
    dplyr::add_row(TestId = NA, TestName = "t4", passed = 1, failed = 0)
  expect_warning(res <- check_test_input(input),
                 "Dropping 1 test\\(s) with no ID")
  expect_equal(res, TEST_RESULTS)
})

test_that("check_test_input() aborts on repeated IDs", {
  input <- TEST_RESULTS %>%
    dplyr::add_row(TestId = "FOO-BAR-001", TestName = "t4",
                   passed = 1, failed = 0)
  expect_error(res <- check_test_input(input),
               class = "mrgvalidate_input_error")
})

test_that("find_missing() returns missing pieces and prints messages", {
  withr::with_tempdir({
    setup_test_results()
    expect_warning(
      res_list <- create_test_framework(product_name = "product",
                                        specs = SPECS,
                                        auto_test_dir = getwd()),
      "not mentioned in `specs`")
    dd <- res_list$dd
    expect_message(
      res_missing <- find_missing(dd),
      "2 missing piece\\(s\\) found\\. Check results")
    expect_equal(length(res_missing), 3)

    dd_good <- filter(dd, StoryId == "st001")
    expect_message(
      res_no_missing <- find_missing(dd_good),
      "No missing pieces found")
    expect_equal(length(res_no_missing), 3)

    # find_missing() will bypass find_reqs_without_stories() if called with
    # input that doesn't have requirement columns.
    dd_good_no_reqs <- select(dd_good, -RequirementId, -RequirementDescription)
    expect_message(
      res_no_missing_no_reqs <- find_missing(dd_good_no_reqs),
      "No missing pieces found")
    expect_equal(length(res_no_missing_no_reqs), 3)
    expect_equal(res_no_missing_no_reqs[[3]], tibble::tibble())
  })
})

test_that("find_tests_without_reqs() returns tests without reqs", {
  withr::with_tempdir({
    setup_test_results()

    expect_warning(
      res_list <- create_test_framework(product_name = "product",
                                        specs = SPECS,
                                        auto_test_dir = getwd()),
      "not mentioned in `specs`")
    dd <- res_list$dd

    expected <- tibble::tribble(
      ~TestId, ~TestName,
      "FOO-BAR-002", "t2")

    expect_equal(
      find_tests_without_reqs(dd),
      expected)

    # Stories are used if there are no requirement columns.
    expect_equal(
      find_tests_without_reqs(
        select(dd, -RequirementId, -RequirementDescription)),
      expected)
  })
})

test_that("find_reqs_with_missing_tests() returns reqs without tests", {
  withr::with_tempdir({
    setup_test_results()
    expect_warning(
      res_list <- create_test_framework(product_name = "product",
                                        specs = SPECS,
                                        auto_test_dir = getwd()),
      "not mentioned in `specs`")
    dd <- res_list$dd
    expect_equal(
      find_reqs_with_missing_tests(dd),
      tibble::tribble(
        ~RequirementId, ~RequirementDescription, ~TestId,
        "req002", "req two", "FOO-OTHER-002"))

    # Stories are used if there are no requirement columns.
    expect_equal(
      find_reqs_with_missing_tests(
        select(dd, -RequirementId, -RequirementDescription)),
      tibble::tribble(
        ~StoryId, ~StoryDescription, ~TestId,
        "st002", "story two", "FOO-OTHER-002"))
  })
})

test_that("find_reqs_without_stories() returns reqs without stories", {
  expect_equal(
    find_reqs_without_stories(
      dplyr::mutate(SPECS,
                    StoryId = ifelse(RequirementId == "req002", NA, StoryId))),
    tibble::tribble(
      ~RequirementId, ~RequirementDescription,
      "req002", "req two"))

  expect_equal(
    find_reqs_without_stories(
      tibble::tribble(
        ~StoryId, ~RequirementId, ~RequirementDescription,
        "st001", "req001", "req one")),
    tibble::tibble(RequirementId = character(0),
                   RequirementDescription = character(0)))
})

test_that("find_reqs_without_stories() errors if input lacks req columns", {
  expect_error(
    find_reqs_without_stories(
      select(SPECS, -RequirementId, -RequirementDescription),
    class = "mrgvalidate_input_error"))
})
