
test_that("get_reference_docx() returns default if file doesn't exist", {
  withr::with_tempdir({
    # Always "default" if style_dir is NULL.
    expect_equal(get_reference_docx("doesn't matter", NULL), "default")

    expect_equal(get_reference_docx("i-do-not-exist", getwd()), "default")
  })
})
