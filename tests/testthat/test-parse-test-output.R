
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

test_that("strip_test_id() returns string without a bracketed test ID as is", {
  cases <- list(
    # input expected
    c("", ""),
    c("foo", "foo"),
    c("FOO-BAR-001", "FOO-BAR-001"),
    c("[FOO-001]", "[FOO-001]")
  )
  for (case in cases) {
    expect_equal(strip_test_id(case[[1]], "FOO-BAR-001"),
                 case[[2]])
  }
})

test_that("strip_test_id() strips only specified test ID", {
  expect_equal(strip_test_id("[DIFF-ID-002]",
                             "FOO-BAR-001"),
               "[DIFF-ID-002]")
  expect_equal(strip_test_id("[DIFF-ID-002][FOO-BAR-001]",
                             "FOO-BAR-001"),
               "[DIFF-ID-002]")
})

test_that("strip_test_id() strips only the first match", {
  expect_equal(strip_test_id("a[FOO-BAR-001]b[FOO-BAR-001]",
                             "FOO-BAR-001"),
               "ab[FOO-BAR-001]")
})

test_that("strip_test_id() collapses spaces", {
  cases <- list(
    # input expected
    c("[FOO-001]", "[FOO-001]"),
    c("[DIFF-ID-002]", "[DIFF-ID-002]"),
    c("a [FOO-BAR-001] b", "a b"),
    c("a  [FOO-BAR-001] b", "a b"),
    c("[FOO-BAR-001] b", "b"),
    c("a [FOO-BAR-001]", "a"),
    c("a[FOO-BAR-001]b", "ab"),
    c("  [FOO-BAR-001] desc", "desc")
  )
  for (case in cases) {
    expect_equal(strip_test_id(case[[1]], "FOO-BAR-001"),
                 case[[2]])
  }
})
