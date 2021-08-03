
setup_test_results <- function() {
  test_results <- tibble::tribble(
    ~TestId, ~TestName, ~passed, ~failed,
    "FOO-BAR-001", "t1", 2, 0,
    "FOO-BAR-002", "t2", 1, 0,
    "FOO-BAR-003", "t3", 1, 0,
    NA, "t4", 3, 0)
  readr::write_csv(test_results, "t.csv")
  get_sys_info(out_path = "t.json")
}

SPECS <- tibble::tribble(
  ~StoryId, ~RequirementId, ~RequirementDescription, ~TestIds,
  "st001", "req001", "req one", c("FOO-BAR-001", "FOO-BAR-003"),
  "st002", "req002", "req two", "FOO-OTHER-002",
  "st003", "req003", "req three", NA
)

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

test_that("find_reqs_without_tests() returns reqs without tests", {
  withr::with_tempdir({
    setup_test_results()
    dd <- create_validation_docs("product", "1.0", SPECS, getwd(),
                                 write = FALSE)
    expect_equal(
      find_reqs_without_tests(dd),
      tibble::tribble(
        ~RequirementId, ~RequirementDescription, ~TestId,
        "req002", "req two", "FOO-OTHER-002"))
  })
})

test_that("find_reqs_without_stories() returns reqs without stories", {
  expect_equal(
    find_reqs_without_stories(
      tibble::tribble(
        ~StoryId, ~RequirementId, ~RequirementDescription,
        "st001", "req001", "req one",
        NA, "req002", "req two",
        "st003", "req003", "req three")),
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
