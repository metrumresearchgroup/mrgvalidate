
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


test_that("extract_bug_section() works correctly", {
  release_notes_file <- file.path(TEST_INPUTS_DIR, "release_notes_sample.md")
  release_notes <- readLines(release_notes_file)

  bugs_actual <- extract_bug_section(release_notes)
  expect_equal(sum(stringr::str_count(bugs_actual, "(?i)bug(s)?")), 1)

  release_notes <- release_notes[1:13]
  expect_warning(
    bugs <- extract_bug_section(release_notes),
    "No bug section found"
  )
  expect_equal(bugs, "No bugs addressed in this release.")

  bugs_fake <- str_replace(bugs_actual, "Bug fixes", "bugs addressed")
  release_notes <- c(release_notes, bugs_actual, bugs_fake)

  expect_warning(
    bugs <- extract_bug_section(release_notes),
    "Found multiple potential Bugs sections"
  )
  expect_equal(sum(stringr::str_count(bugs, "(?i)bug(s)?")), 1)
})
