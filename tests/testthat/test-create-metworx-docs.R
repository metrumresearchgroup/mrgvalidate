library(stringr)

test_that("create_metworx_docs() renders markdown", {
  # set up clean docs output dir
  output_dir <- file.path(tempdir(), "mrgvalidate-create-validation-docs")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit({ fs::dir_delete(output_dir) })

  specs <- readRDS(file.path(TEST_INPUTS_DIR, "specs.RDS"))
  specs$StoryDescription[1] <- "story desc line 2\nLINE2!!"
  product_name <- "metworx_TEST"

  mrgvalidate::create_metworx_docs(
    product_name = product_name,
    version = "vFAKE",
    specs = specs,
    auto_test_dir = file.path(TEST_INPUTS_DIR, "validation-results-sample"),
    man_test_dir = file.path(TEST_INPUTS_DIR, "manual-tests-sample"),
    roles = readr::read_csv(file.path(TEST_INPUTS_DIR, "roles.csv"), col_types = "cc"),
    output_dir = output_dir,
    cleanup_rmd = FALSE
  )

  # check that files exist
  check_files(product_name)

  # get TestId's we expect to see
  test_ids <- c(
    read_csv_test_results(file.path(TEST_INPUTS_DIR, "validation-results-sample"))$results %>% pull(TestId),
    read_manual_test_results(file.path(TEST_INPUTS_DIR, "manual-tests-sample")) %>% pull(TestId)
  ) %>% unique()


  # check that the markdown looks right
  boiler_text <- get_boiler_text("metworx")

  val_plan_text <- readr::read_file(file.path(output_dir, rename_val_file(VAL_PLAN_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$VAL_PLAN_BOILER, val_plan_text, fixed = TRUE))

  test_plan_text <- readr::read_file(file.path(output_dir, rename_val_file(TEST_PLAN_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$TEST_PLAN_BOILER, test_plan_text, fixed = TRUE))

  test_results_text <- readr::read_file(file.path(output_dir, rename_val_file(TEST_RESULTS_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$TEST_RESULTS_BOILER, test_results_text, fixed = TRUE))

  req_text <- readr::read_file(file.path(output_dir, rename_val_file(REQ_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$REQ_BOILER, req_text, fixed = TRUE))
  # expect_true(all(str_detect(req_text, specs$RequirementId))) # Cant inspect data in parameterized RMD - thoughts?

  mat_text <- readr::read_file(file.path(output_dir, rename_val_file(MAT_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$MAT_BOILER, mat_text, fixed = TRUE))
  # expect_true(all(str_detect(mat_text, test_ids))) # Cant inspect data in parameterized RMD - thoughts?
  # expect_false(str_detect(mat_text, "LINE2!!"))

  val_sum_text <- readr::read_file(file.path(output_dir, rename_val_file(VAL_SUM_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$VAL_SUM_BOILER, val_sum_text, fixed = TRUE))

  rls_notes_text <- readr::read_file(file.path(output_dir, rename_val_file(RLS_NOTES_FILE, product_name, "Rmd")))
  expect_true(grepl(boiler_text$RLS_NOTES_BOILER, rls_notes_text, fixed = TRUE))
})


test_that("create_validation_docs() returns data df", {
  # set up clean docs output dir
  output_dir <- file.path(tempdir(), "mrgvalidate-create-validation-docs")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit({ fs::dir_delete(output_dir) })

  res_df <- mrgvalidate::create_validation_docs(
    product_name = "Metworx TEST",
    version = "vFAKE",
    specs = readRDS(file.path(TEST_INPUTS_DIR, "specs.RDS")),
    auto_test_dir = file.path(TEST_INPUTS_DIR, "validation-results-sample"),
    man_test_dir = file.path(TEST_INPUTS_DIR, "manual-tests-sample"),
    roles = readr::read_csv(file.path(TEST_INPUTS_DIR, "roles.csv"), col_types = "cc"),
    output_dir = output_dir,
    write = FALSE
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

test_that("create_validation_docs() drops missing test types", {
  output_dir <- file.path(tempdir(), "mrgvalidate-create-validation-docs")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit(fs::dir_delete(output_dir))

  specs <- readRDS(file.path(TEST_INPUTS_DIR, "specs.RDS"))

  outdir_auto <- file.path(output_dir, "auto")
  fs::dir_create(outdir_auto)

  create_validation_docs(
    product_name = "Metworx TEST",
    version = "vFAKE",
    specs = specs,
    auto_test_dir = file.path(TEST_INPUTS_DIR, "validation-results-sample"),
    output_dir = outdir_auto)

  val_text_auto <- readr::read_file(file.path(outdir_auto, VAL_FILE))
  expect_true(str_detect(val_text_auto, "# Automated Test Results"))
  expect_false(str_detect(val_text_auto, "# Manual Test Results"))

  outdir_man <- file.path(output_dir, "man")
  fs::dir_create(outdir_man)

  create_validation_docs(
    product_name = "Metworx TEST",
    version = "vFAKE",
    specs = specs,
    man_test_dir = file.path(TEST_INPUTS_DIR, "manual-tests-sample"),
    output_dir = outdir_man)

  val_text_man <- readr::read_file(file.path(outdir_man, VAL_FILE))
  expect_false(str_detect(val_text_man, "# Automated Test Results"))
  expect_true(str_detect(val_text_man, "# Manual Test Results"))
})

test_that("create_validation_docs() works with no requirements", {
  # set up clean docs output dir
  output_dir <- file.path(tempdir(), "mrgvalidate-create-validation-docs")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit({ fs::dir_delete(output_dir) })

  res_df <- mrgvalidate::create_validation_docs(
    product_name = "Metworx TEST",
    version = "vFAKE",
    specs = readRDS(file.path(TEST_INPUTS_DIR, "specs.RDS")) %>% select(-RequirementId, -RequirementDescription),
    auto_test_dir = file.path(TEST_INPUTS_DIR, "validation-results-sample"),
    man_test_dir = file.path(TEST_INPUTS_DIR, "manual-tests-sample"),
    roles = readr::read_csv(file.path(TEST_INPUTS_DIR, "roles.csv"), col_types = "cc"),
    output_dir = output_dir,
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

  req_text <- readr::read_file(file.path(output_dir, REQ_FILE))
  # Summary of requirements is dropped if there are no requirements.
  expect_false(str_detect(req_text, "Summary"))
})

test_that("create_validation_docs() can auto-assign test IDs", {
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
    res_df <- create_validation_docs(
      product_name = "Metworx TEST",
      version = "vFAKE",
      specs = specs,
      auto_test_dir = auto_test_dir,
      output_dir = output_dir),
    "temporary kludge")

  expected_ids <- c("METW-1", "METW-2", "METW-3")
  expect_setequal(
    res_df %>% unnest(tests) %>% pull(TestId) %>% unique(),
    expected_ids)

  for (.f in c(REQ_FILE, VAL_FILE, MAT_FILE)) {
    text <- readr::read_file(file.path(output_dir, REQ_FILE))
    expect_true(all(str_detect(text, expected_ids)),
                label = paste0("IDs in ", .f))
  }
})

test_that("create_validation_docs() drops orphan test IDs from testing docs", {
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
    res_df <- create_validation_docs(
      product_name = "Metworx TEST",
      version = "vFAKE",
      specs = specs,
      auto_test_dir = auto_test_dir,
      output_dir = output_dir),
    "not mentioned in `specs`")

  text <- readr::read_file(file.path(output_dir, VAL_FILE))
  expect_true(str_detect(text, "t001"))
  expect_false(str_detect(text, "t002"))
})

test_that("create_validation_docs() works if passed style_dir and output_dir", {
  withr::with_tempdir({
    # Set up a directory of reference .docx files.
    fs::dir_create("style-refs")
    system2(rmarkdown::pandoc_exec(),
            c("-o", file.path("style-refs", "ref.docx"),
              "--print-default-data-file=reference.docx"))

    base_names <- tools::file_path_sans_ext(c(REQ_FILE, VAL_FILE, MAT_FILE))
    docxs <- paste0(base_names, ".docx")
    for (.f in docxs) {
      fs::file_copy(file.path("style-refs", "ref.docx"),
                    file.path("style-refs", .f))
    }

    specs <- readRDS(file.path(TEST_INPUTS_DIR, "specs.RDS"))

    # Regression case: if the path handling of style_dir isn't handled
    # correctly, the underlying pandoc call will error here.
    mrgvalidate::create_validation_docs(
      product_name = "Metworx TEST",
      version = "vFAKE",
      specs = specs,
      auto_test_dir = file.path(TEST_INPUTS_DIR, "validation-results-sample"),
      man_test_dir = file.path(TEST_INPUTS_DIR, "manual-tests-sample"),
      output_dir = "output",
      style_dir = "style-refs"
    )
    for (.f in docxs) {
      expect_true(fs::file_exists(file.path("output", .f)))
    }
  })
})
