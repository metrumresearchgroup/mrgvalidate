
test_that("parse_test_id() returns NA if there is no ID", {
  cases <- list(
    # input expected
    c("", NA),
    c("foo", NA),
    c("FOO-BAR-001", NA),
    c("[FOO-001]", NA)
  )
  for (case in cases) {
    expect_equal(parse_test_id(case[[1]]), case[[2]])
  }
})

test_that("parse_test_id() can parse IDs", {
  cases <- list(
    # input expected
    c("[FOO-BAR-001]", "FOO-BAR-001"),
    c("[F-B-0]", "F-B-0"),
    c("a [FOO-BAR-001] b", "FOO-BAR-001"),
    c("[FOO-BAR-001] b", "FOO-BAR-001"),
    c("a [FOO-BAR-001]", "FOO-BAR-001"),
    c("a[FOO-BAR-001]b", "FOO-BAR-001")
  )
  for (case in cases) {
    expect_equal(parse_test_id(case[[1]]), case[[2]])
  }
})

test_that("parse_test_id() extracts only the first match", {
  expect_equal(parse_test_id("[FOO-BAR-001] [FOO-BAR-002] "),
               "FOO-BAR-001")
})
