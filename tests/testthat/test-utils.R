
test_that("get_reference_docx() returns default if file doesn't exist", {
  withr::with_tempdir({
    # Always "default" if style_dir is NULL.
    expect_equal(get_reference_docx("doesn't matter", NULL), "default")

    expect_equal(get_reference_docx("i-do-not-exist", getwd()), "default")
  })
})

test_that("get_reference_docx() returns an absolute path", {
  withr::with_tempdir({
    file.create("base.docx")
    expect_true(fs::is_absolute_path(get_reference_docx("base.md", ".")))
  })
})
