library(stringr)
library(officer)

product_name <- "package_TEST"

test_that("create_package_docs() renders markdown", {
  # set up clean docs output dir
  output_dir <- file.path(tempdir(), "mrgvalidate-create-validation-docs")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit({ fs::dir_delete(output_dir) })

  specs <- readRDS(file.path(TEST_INPUTS_DIR, "specs.RDS")) %>% filter(!grepl("MAN", TestIds))
  specs$StoryDescription[1] <- "story desc line 2\nLINE2!!"

  # Test that repeating TestIDs in traceability matrix are removed
  specs$TestIds <- purrr::modify_if(
    specs$TestIds,
    specs$RequirementId == "VSC-R002",
    ~ c(.x, "MXI-VSC-001"))

  mrgvalidate::create_package_docs(
    product_name = product_name,
    version = "vFAKE",
    repo_url = "git@github.com:metrumresearchgroup/mrgvalidate.git",
    specs = specs,
    release_notes_file = file.path(TEST_INPUTS_DIR, "release_notes_sample.md"),
    auto_test_dir = file.path(TEST_INPUTS_DIR, "validation-results-sample"),
    output_dir = output_dir,
    write = TRUE,
    cleanup_rmd = FALSE
  )

  # check that files exist
  check_files(product_name, output_dir)

  # get TestId's we expect to see
  test_ids <- c(
    read_csv_test_results(file.path(TEST_INPUTS_DIR, "validation-results-sample"))$results %>% pull(TestId)
  ) %>% unique()


  # check that the markdown looks right
  boiler_text <- get_boiler_text("package")

  val_plan_text <- readr::read_file(file.path(output_dir, rename_val_file(VAL_PLAN_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$VAL_PLAN_BOILER, val_plan_text, fixed = TRUE))

  test_plan_text <- readr::read_file(file.path(output_dir, rename_val_file(TEST_PLAN_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$TEST_PLAN_BOILER, test_plan_text, fixed = TRUE))

  test_results_text <- readr::read_file(file.path(output_dir, rename_val_file(TEST_RESULTS_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$TEST_RESULTS_BOILER, test_results_text, fixed = TRUE))

  req_text <- readr::read_file(file.path(output_dir, rename_val_file(REQ_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$REQ_BOILER, req_text, fixed = TRUE))
  # Read in docx to search for requirements
  req_text <- read_docx(file.path(output_dir, rename_val_file(REQ_FILE, product_name)))
  req_text <- docx_summary(req_text) %>% filter(content_type == "paragraph")
  for(i in seq_along(specs$RequirementId)){
    expect_true(any(grepl(specs$RequirementId[i], req_text$text)))
  }

  mat_text <- readr::read_file(file.path(output_dir, rename_val_file(MAT_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$MAT_BOILER, mat_text, fixed = TRUE))
  mat_text <- read_docx(file.path(output_dir, rename_val_file(MAT_FILE, product_name)))
  mat_text <- docx_summary(mat_text) %>% filter(content_type == "table cell")
  for(i in seq_along(test_ids)){
    expect_true(any(grepl(test_ids[i], mat_text$text)))
  }
  expect_false(any(str_detect(mat_text$text, "LINE2!!")))
  expect_equal(sum(str_count(mat_text$text, "MXI-VSC-002")), 1)

  val_sum_text <- readr::read_file(file.path(output_dir, rename_val_file(VAL_SUM_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$VAL_SUM_BOILER, val_sum_text, fixed = TRUE))

  rls_notes_text <- readr::read_file(file.path(output_dir, rename_val_file(RLS_NOTES_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$RLS_NOTES_BOILER, rls_notes_text, fixed = TRUE))

})


test_that("create_package_docs() returns data df", {
  # set up clean docs output dir
  output_dir <- file.path(tempdir(), "mrgvalidate-create-validation-docs")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit({ fs::dir_delete(output_dir) })

  specs <- readRDS(file.path(TEST_INPUTS_DIR, "specs.RDS")) %>% filter(!grepl("MAN", TestIds))

  res_df <- mrgvalidate::create_package_docs(
    product_name = product_name,
    version = "vFAKE",
    repo_url = "git@github.com:metrumresearchgroup/mrgvalidate.git",
    specs = specs,
    release_notes_file = file.path(TEST_INPUTS_DIR, "release_notes_sample.md"),
    auto_test_dir = file.path(TEST_INPUTS_DIR, "validation-results-sample"),
    output_dir = output_dir,
    write = FALSE,
    cleanup_rmd = FALSE
  )

  expect_true(nrow(res_df) > 1)
  expect_equal(names(res_df), CREATE_OUT_DF_NAMES)

  purrr::walk(names(res_df)[1:ncol(res_df)-1], function(.n) {
    expect_true(inherits(res_df[[.n]], "character"))
    expect_true(all(purrr::map_lgl(res_df[[.n]], ~!is.null(.x))))
    expect_true(all(purrr::map_lgl(res_df[[.n]], ~nchar(.x) > 1)))
  })
  expect_true(inherits(res_df$tests, "list"))
  expect_true(all(purrr::map_lgl(res_df$tests, ~inherits(.x, "tbl_df"))))

})

test_that("create_package_docs() drops missing test types", {
  output_dir <- file.path(tempdir(), "mrgvalidate-create-validation-docs")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit(fs::dir_delete(output_dir))

  specs <- readRDS(file.path(TEST_INPUTS_DIR, "specs.RDS"))

  outdir_auto <- file.path(output_dir, "auto")
  fs::dir_create(outdir_auto)

  expect_warning({
    res_df = create_package_docs(
      product_name = product_name,
      version = "vFAKE",
      repo_url = "git@github.com:metrumresearchgroup/mrgvalidate.git",
      specs = specs,
      release_notes_file = file.path(TEST_INPUTS_DIR, "release_notes_sample.md"),
      auto_test_dir = file.path(TEST_INPUTS_DIR, "validation-results-sample"),
      output_dir = outdir_auto,
      write = FALSE,
      cleanup_rmd = FALSE) %>% capture.output() # suppress print()
  },
    "Required links between tests and/or requirements are missing. Returning missing information", fixed = TRUE)
})

test_that("create_package_docs() works with no requirements", {
  # set up clean docs output dir
  output_dir <- file.path(tempdir(), "mrgvalidate-create-validation-docs")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit({ fs::dir_delete(output_dir) })

  specs <- readRDS(file.path(TEST_INPUTS_DIR, "specs.RDS")) %>% filter(!grepl("MAN", TestIds))

  res_df <- mrgvalidate::create_package_docs(
    product_name = product_name,
    version = "vFAKE",
    repo_url = "git@github.com:metrumresearchgroup/mrgvalidate.git",
    release_notes_file = file.path(TEST_INPUTS_DIR, "release_notes_sample.md"),
    specs = specs %>% select(-RequirementId, -RequirementDescription),
    auto_test_dir = file.path(TEST_INPUTS_DIR, "validation-results-sample"),
    output_dir = output_dir,
    cleanup_rmd = FALSE
  )

  expect_true(nrow(res_df) > 1)
  expect_equal(
    names(res_df),
    stringr::str_subset(CREATE_OUT_DF_NAMES, "RequirementId|RequirementDescription", negate = TRUE)
  )

  purrr::walk(names(res_df)[1:ncol(res_df)-1], function(.n) {
    expect_true(inherits(res_df[[.n]], "character"))
    expect_true(all(purrr::map_lgl(res_df[[.n]], ~!is.null(.x))))
    expect_true(all(purrr::map_lgl(res_df[[.n]], ~nchar(.x) > 1)))
  })
  expect_true(inherits(res_df$tests, "list"))
  expect_true(all(purrr::map_lgl(res_df$tests, ~inherits(.x, "tbl_df"))))

  # Summary of requirements is dropped if there are no requirements.
  req_text <- read_docx(file.path(output_dir, rename_val_file(REQ_FILE, product_name)))
  req_text <- docx_summary(req_text) %>% filter(content_type == "paragraph")
  # 'Requirements Specification' is the title, so skip to the first occurrence of Product Risk
  req_text <- req_text$text[grep("Product risk", req_text$text)[1]:length(req_text$text)]
  expect_false(any(str_detect(req_text, "Requirements")))
})

test_that("create_package_docs() can auto-assign test IDs", {
  output_dir <- file.path(tempdir(), "mrgvalidate-create-validation-docs")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit(fs::dir_delete(output_dir))

  specs <- tibble::tribble(
    ~StoryId, ~StoryName, ~StoryDescription, ~ProductRisk, ~TestIds,
    "st001", "sn1", "story one", "low", c("name one", "name two"),
    "st002", "sn2", "story two", "low", "name three")

  auto_test_dir <- file.path(output_dir, "auto-tests")
  fs::dir_create(auto_test_dir)
  readr::write_csv(
    tibble::tribble(
      ~TestName, ~TestId, ~passed, ~failed,
      "name one", "name one", 1, 0,
      "name two", "name two", 1, 0,
      "name three", "name three", 1, 0),
    file.path(auto_test_dir, "justname.csv"))

  fs::file_copy(
    file.path(TEST_INPUTS_DIR, "validation-results-sample",
              "util-results.json"),
    file.path(auto_test_dir, "justname.json"))

  expect_warning(
    res_df <- create_package_docs(
      product_name = product_name,
      version = "vFAKE",
      repo_url = "git@github.com:metrumresearchgroup/mrgvalidate.git",
      release_notes_file = file.path(TEST_INPUTS_DIR, "release_notes_sample.md"),
      specs = specs,
      auto_test_dir = auto_test_dir,
      output_dir = output_dir,
      cleanup_rmd = FALSE),
    "temporary kludge")

  expected_ids <- c("PACK-1", "PACK-2", "PACK-3")
  expect_setequal(
    res_df %>% unnest(tests) %>% pull(TestId) %>% unique(),
    expected_ids)

 # Make sure IDs are in relevant docs
  for (.f in c(REQ_FILE, TEST_PLAN_FILE, TEST_RESULTS_FILE, MAT_FILE)) {
    text <- read_docx(file.path(output_dir, rename_val_file(.f, product_name))) %>% docx_summary()
    text <- paste(text$text, collapse = "\n")
      expect_true(all(str_detect(text, expected_ids)),
                  label = paste0("IDs in ", .f))
  }
})

test_that("create_package_docs() drops orphan test IDs from testing docs", {
  output_dir <- file.path(tempdir(), "mrgvalidate-create-validation-docs")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit(fs::dir_delete(output_dir))

  specs <- tibble::tribble(
    ~StoryId, ~StoryName, ~StoryDescription, ~ProductRisk, ~TestIds,
    "st001", "sn1", "story one", "low", "t001")

  auto_test_dir <- file.path(output_dir, "auto-tests")
  fs::dir_create(auto_test_dir)
  readr::write_csv(
    tibble::tribble(
      ~TestName, ~TestId, ~passed, ~failed,
      "name one", "t001", 1, 0,
      "name two", "t002", 1, 0),
    file.path(auto_test_dir, "t.csv"))

  fs::file_copy(
    file.path(TEST_INPUTS_DIR, "validation-results-sample",
              "util-results.json"),
    file.path(auto_test_dir, "t.json"))

  expect_warning(
    res_df <- create_package_docs(
      product_name = product_name,
      version = "vFAKE",
      repo_url = "git@github.com:metrumresearchgroup/mrgvalidate.git",
      release_notes_file = file.path(TEST_INPUTS_DIR, "release_notes_sample.md"),
      specs = specs,
      auto_test_dir = auto_test_dir,
      output_dir = output_dir,
      cleanup_rmd = FALSE),
    "not mentioned in `specs`")

  test_text <- read_docx(file.path(output_dir, rename_val_file(TEST_RESULTS_FILE, product_name))) %>% docx_summary() %>%
    filter(content_type == "table cell") %>% pull(text) %>% paste(collapse = " ")
  expect_true(str_detect(test_text, "t001"))
  expect_false(str_detect(test_text, "t002"))
})

test_that("create_package_docs() works if passed style_dir and output_dir", {
  withr::with_tempdir({
    # Set up a directory of reference .docx files.
    fs::dir_create("style-refs")
    system2(rmarkdown::pandoc_exec(),
            c("-o", file.path("style-refs", "ref.docx"),
              "--print-default-data-file=reference.docx"))

    base_names <- c(VAL_PLAN_FILE, TEST_PLAN_FILE, TEST_RESULTS_FILE, MAT_FILE, REQ_FILE, VAL_SUM_FILE, RLS_NOTES_FILE)
    docxs <- rename_val_file(base_names, product_name)
    for (.f in docxs) {
      fs::file_copy(file.path("style-refs", "ref.docx"),
                    file.path("style-refs", .f))
    }

    specs <- readRDS(file.path(TEST_INPUTS_DIR, "specs.RDS")) %>% filter(!grepl("MAN", TestIds))

    # Regression case: if the path handling of style_dir isn't handled
    # correctly, the underlying pandoc call will error here. TODO: Is this true?? Pretty sure if it cant find it, it will just use the default. Dont think this test works properly
    mrgvalidate::create_package_docs(
      product_name = product_name,
      version = "vFAKE",
      repo_url = "git@github.com:metrumresearchgroup/mrgvalidate.git",
      release_notes_file = file.path(TEST_INPUTS_DIR, "release_notes_sample.md"),
      specs = specs,
      auto_test_dir = file.path(TEST_INPUTS_DIR, "validation-results-sample"),
      output_dir = "output",
      style_dir = "style-refs"
    )

    for (.f in docxs) {
      expect_true(fs::file_exists(file.path("output", .f)))
    }
  })
})


test_that("create_package_docs() changes test plan automated test section based on language", {
  # set up clean docs output dir
  output_dir <- file.path(tempdir(), "mrgvalidate-create-validation-docs")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit({ fs::dir_delete(output_dir) })

  specs <- readRDS(file.path(TEST_INPUTS_DIR, "specs.RDS")) %>% filter(!grepl("MAN", TestIds))

  res_df <- mrgvalidate::create_package_docs(
    product_name = product_name,
    version = "vFAKE",
    language = "Go",
    repo_url = "git@github.com:metrumresearchgroup/mrgvalidate.git",
    release_notes_file = file.path(TEST_INPUTS_DIR, "release_notes_sample.md"),
    specs = specs %>% select(-RequirementId, -RequirementDescription),
    auto_test_dir = file.path(TEST_INPUTS_DIR, "validation-results-sample"),
    output_dir = output_dir,
    cleanup_rmd = TRUE
  )

  test_plan_boiler_plate <- auto_testing_text("Go", return_text = TRUE) %>% str_trim() %>%
    str_replace_all("`", "")

  test_plan_text <- read_docx(file.path(output_dir, rename_val_file(TEST_PLAN_FILE, product_name)))
  test_plan_text <- docx_summary(test_plan_text) %>% filter(content_type == "paragraph")
  test_plan_text <- test_plan_text$text %>% str_replace_all("’","'")

  expect_true(any(str_detect(test_plan_text, test_plan_boiler_plate)))


  res_df <- mrgvalidate::create_package_docs(
    product_name = product_name,
    version = "vFAKE",
    language = "R",
    repo_url = "git@github.com:metrumresearchgroup/mrgvalidate.git",
    release_notes_file = file.path(TEST_INPUTS_DIR, "release_notes_sample.md"),
    specs = specs %>% select(-RequirementId, -RequirementDescription),
    auto_test_dir = file.path(TEST_INPUTS_DIR, "validation-results-sample"),
    output_dir = output_dir,
    cleanup_rmd = TRUE
  )

  test_plan_boiler_plate <- auto_testing_text("R", return_text = TRUE) %>% str_trim() %>%
    str_replace_all("`", "")

  test_plan_text <- read_docx(file.path(output_dir, rename_val_file(TEST_PLAN_FILE, product_name)))
  test_plan_text <- docx_summary(test_plan_text) %>% filter(content_type == "paragraph")
  test_plan_text <- test_plan_text$text %>% str_replace_all("’","'")

  expect_true(any(str_detect(test_plan_text, test_plan_boiler_plate)))
})



test_that("create_package_docs() works with manual tests", {
  # set up clean docs output dir
  output_dir <- file.path(tempdir(), "mrgvalidate-create-validation-docs")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit({ fs::dir_delete(output_dir) })

  specs <- readRDS(file.path(TEST_INPUTS_DIR, "specs.RDS"))
  specs$StoryDescription[1] <- "story desc line 2\nLINE2!!"

  # Test that repeating TestIDs in traceability matrix are removed
  specs$TestIds <- purrr::modify_if(
    specs$TestIds,
    specs$RequirementId == "VSC-R002",
    ~ c(.x, "MXI-VSC-001"))

  mrgvalidate::create_package_docs(
    product_name = product_name,
    version = "vFAKE",
    repo_url = "git@github.com:metrumresearchgroup/mrgvalidate.git",
    specs = specs,
    release_notes_file = file.path(TEST_INPUTS_DIR, "release_notes_sample.md"),
    auto_test_dir = file.path(TEST_INPUTS_DIR, "validation-results-sample"),
    man_test_dir = file.path(TEST_INPUTS_DIR, "manual-tests-sample"),
    output_dir = output_dir,
    write = TRUE,
    cleanup_rmd = FALSE
  )

  # check that files exist
  check_files(product_name, output_dir)

  # get TestId's we expect to see
  test_ids <- c(
    read_csv_test_results(file.path(TEST_INPUTS_DIR, "validation-results-sample"))$results %>% pull(TestId),
    read_manual_test_results(file.path(TEST_INPUTS_DIR, "manual-tests-sample")) %>% pull(TestId)
  ) %>% unique()


  # check that the markdown looks right
  boiler_text <- get_boiler_text("package")

  val_plan_text <- readr::read_file(file.path(output_dir, rename_val_file(VAL_PLAN_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$VAL_PLAN_BOILER, val_plan_text, fixed = TRUE))

  test_plan_text <- readr::read_file(file.path(output_dir, rename_val_file(TEST_PLAN_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$TEST_PLAN_BOILER, test_plan_text, fixed = TRUE))

  test_results_text <- readr::read_file(file.path(output_dir, rename_val_file(TEST_RESULTS_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$TEST_RESULTS_BOILER, test_results_text, fixed = TRUE))

  req_text <- readr::read_file(file.path(output_dir, rename_val_file(REQ_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$REQ_BOILER, req_text, fixed = TRUE))
  # Read in docx to search for requirements
  req_text <- read_docx(file.path(output_dir, rename_val_file(REQ_FILE, product_name)))
  req_text <- docx_summary(req_text) %>% filter(content_type == "paragraph")
  for(i in seq_along(specs$RequirementId)){
    expect_true(any(grepl(specs$RequirementId[i], req_text$text)))
  }

  mat_text <- readr::read_file(file.path(output_dir, rename_val_file(MAT_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$MAT_BOILER, mat_text, fixed = TRUE))
  mat_text <- read_docx(file.path(output_dir, rename_val_file(MAT_FILE, product_name)))
  mat_text <- docx_summary(mat_text) %>% filter(content_type == "table cell")
  for(i in seq_along(test_ids)){
    expect_true(any(grepl(test_ids[i], mat_text$text)))
  }
  expect_false(any(str_detect(mat_text$text, "LINE2!!")))
  expect_equal(sum(str_count(mat_text$text, "MXI-VSC-002")), 1)

  val_sum_text <- readr::read_file(file.path(output_dir, rename_val_file(VAL_SUM_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$VAL_SUM_BOILER, val_sum_text, fixed = TRUE))

  rls_notes_text <- readr::read_file(file.path(output_dir, rename_val_file(RLS_NOTES_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$RLS_NOTES_BOILER, rls_notes_text, fixed = TRUE))

})
