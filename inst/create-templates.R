#' @param template logical. If `TRUE` generate a template instead.
#'
#'
create_validation_templates <- function(style_dir,
                                        type,
                                        output_dir = system.file("template_renders", package = "mrgvalidate")
){

  release_notes <- glue("
## New Features


## Updates to existing functionality


## Bug fixes
")

  auto_info <- list(
    `[Automated Testing Tool]` = list(
      date = "[Time stamp]",
      executor = "[Name]",
      info = list(
        env_vars = list(
          AMI_NAME = "[AMI ID if applicable]",
          METWORX_VERSION = "[Workflow Blueprint version if applicable]",
          COMMIT_HASH = "[Commit Identifier]",
          REPO = "[Repo URL]"
        )
      )
    )
  )


  # Validation Plan
  template <- get_template("validation_plan", type = type)
  out_file <- file.path(output_dir, VAL_PLAN_FILE)
  fs::file_copy(template, out_file, overwrite = TRUE)

  rmarkdown::render(
    out_file,
    params = list(
      product_name = "{Release Name}",
      version = "",
      release_notes = release_notes,
      repo = auto_info$template$info$env_vars$REPO
    ),
    output_format = rmarkdown::word_document(
      reference_docx = get_reference_docx(out_file, style_dir)),
    output_dir = dirname(out_file),
    quiet = TRUE
  )


  # Testing Plan
  template <- get_template("testing_plan", type = type)
  out_file <- file.path(output_dir, TEST_PLAN_FILE)
  fs::file_copy(template, out_file, overwrite = TRUE)

  rmarkdown::render(
    out_file,
    params = list(
      product_name = "{Release Name}",
      version = "",
      auto_tests = "{Insert table with Test Name and Test IDs for each test}",
      man_tests = "{Insert tests with Test Name, description and Test IDs for each test}",
      template = TRUE
    ),
    output_format = rmarkdown::word_document(
      reference_docx = get_reference_docx(out_file, style_dir)),
    output_dir = dirname(out_file),
    quiet = TRUE
  )



  # Testing Results
  template <- get_template("testing_results", type = type)
  out_file <- file.path(output_dir, TEST_RESULTS_FILE)
  fs::file_copy(template, out_file, overwrite = TRUE)

  val_info <- format_val_info(auto_info)

  rmarkdown::render(
    out_file,
    params = list(
      product_name = "{Release Name}",
      version = "",
      val_info = val_info,
      auto_tests = auto_tests,
      man_tests = man_tests
    ),
    output_format = rmarkdown::word_document(
      reference_docx = get_reference_docx(out_file, style_dir)),
    output_dir = dirname(out_file),
    quiet = TRUE
  )

  make_testing_results(
    product = "{Release Name}",
    version = "",
    test_data$tests,
    auto_info,
    style_dir = style_dir,
    out_file = TEST_RESULTS_FILE,
    output_dir = output_dir,
    type = type,
    word_document = TRUE
  )

  # Traceability Matrix
  make_traceability_matrix(
    test_data$dd,
    product = "{Release Name}",
    version = "",
    style_dir = style_dir,
    out_file = MAT_FILE,
    output_dir = output_dir,
    type = type,
    word_document = TRUE
  )

  # Requirements Specification
  make_requirements(
    test_data$dd,
    product = "{Release Name}",
    version = "",
    roles = NULL,
    style_dir = style_dir,
    out_file = REQ_FILE,
    output_dir = output_dir,
    type = type,
    word_document = TRUE
  )

  # Validation Summary Report
  make_validation_summary(
    product = "{Release Name}",
    version = "",
    release_notes = release_notes,
    style_dir = style_dir,
    out_file = VAL_SUM_FILE,
    output_dir = output_dir,
    type = type,
    word_document = TRUE
  )

  cleanup_template_rmds()
}

cleanup_template_rmds <- function(output_dir = system.file("template_renders", package = "mrgvalidate"),
                                  file_names = c(VAL_PLAN_FILE, TEST_PLAN_FILE, TEST_RESULTS_FILE,
                                                 MAT_FILE, REQ_FILE, VAL_SUM_FILE, RLS_NOTES_FILE)
){
  for(i in seq_along(file_names)){
    file.i <- file.path(output_dir,file_names[i])
    if(fs::file_exists(file)) fs::file_delete(file)
  }

}

### Create template

