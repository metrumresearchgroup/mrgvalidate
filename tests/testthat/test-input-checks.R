
TEST_RESULTS <- tibble::tribble(
  ~TestId, ~TestName, ~passed, ~failed,
  "FOO-BAR-001", "t1", 2, 0,
  "FOO-BAR-002", "t2", 1, 0,
  "FOO-BAR-003", "t3", 1, 0,
  NA, "t4", 3, 0)

# Note: This function assumes that the caller cleans up (e.g., by calling this
# within withr::with_tempdir()).
setup_test_results <- function() {
  readr::write_csv(TEST_RESULTS, "t.csv")
  get_sys_info(out_path = "t.json")
}

SPECS <- tibble::tribble(
  ~StoryId, ~RequirementId, ~RequirementDescription, ~TestIds,
  "st001", "req001", "req one", c("FOO-BAR-001", "FOO-BAR-003"),
  "st002", "req002", "req two", "FOO-OTHER-002",
  "st003", "req003", "req three", NA
)

test_that("find_missing() returns missing pieces and prints messages", {
  withr::with_tempdir({
    setup_test_results()
    dd <- create_validation_docs("product", "1.0", SPECS, getwd(),
                                 write = FALSE)
    expect_message(
      res_missing <- find_missing(dd),
      "2 missing piece\\(s\\) found\\. Check results")
    expect_equal(length(res_missing), 3)

    dd_good <- filter(dd, StoryId == "st001")
    expect_message(
      res_no_missing <- find_missing(dd_good),
      "No missing pieces found")
    expect_equal(length(res_no_missing), 3)
  })
})

test_that("find_tests_without_reqs() returns tests without reqs", {
  withr::with_tempdir({
    setup_test_results()
    dd <- create_validation_docs("product", "1.0", SPECS, getwd(),
                                 write = FALSE)
    expect_equal(
      find_tests_without_reqs(dd),
      tibble::tribble(
        ~TestId, ~TestName,
        "FOO-BAR-002", "t2"))
  })
})

test_that("find_reqs_with_missing_tests() returns reqs without tests", {
  withr::with_tempdir({
    setup_test_results()
    dd <- create_validation_docs("product", "1.0", SPECS, getwd(),
                                 write = FALSE)
    expect_equal(
      find_reqs_with_missing_tests(dd),
      tibble::tribble(
        ~RequirementId, ~RequirementDescription, ~TestId,
        "req002", "req two", "FOO-OTHER-002"))
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
