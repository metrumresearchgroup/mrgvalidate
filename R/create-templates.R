
#' Create template docs for packages or metworx
#'
#' @param type the type of doc you want to render ("package" or "metworx")
#' @param output_dir Directory to write the output documents to. Defaults to `system.file(package = "mrgvalidate")`.
#' @param new_folder Folder within `output_dir` to render docs.
#'        Mainly useful when generating docs to a system directory that doesn't already exist
#' @param style_dir Directory to check for a docx style reference
#' @param cleanup_rmd Whether to delete the copied RMD's after the word documents are generated.
#'  Defaults to `TRUE`.
#'
#' @details
#'
#' ## Developer Notes:
#' - **Note:** This function does **not** call the internal `make_` functions responsible for creating the validation
#' docs under normal circumstances.
#'
#' - `system.file()` wont register nested directories in the file path if the folder doesn't exist. In other words,
#' `output_dir = system.file("template_renders", package = "mrgvalidate")` would return `""`, and the function would fail
#' (because `"template_renders"` does not exist within `inst`). Use the `new_folder` argument when rendering to a
#' new folder within `inst`.
#'
#' @export
create_validation_templates <- function(type = c("package", "metworx"),
                                        output_dir = system.file(package = "mrgvalidate"),
                                        new_folder = "template_renders",
                                        style_dir = NULL,
                                        cleanup_rmd = TRUE
){

  type <- match.arg(type)

  append <- type # used for file naming
  # system.file wont register `template_renders` in file path if the folder doesn't exist
  # Set to there in function instead of argument
  if(!is.null(new_folder)){
    assert_character(new_folder)
    output_dir <- file.path(output_dir, new_folder)
  }

  if(!fs::dir_exists(output_dir)) fs::dir_create(output_dir)

  # Template Data -----------------------------------------------------------

  TEST_INPUTS_DIR <- system.file("test-inputs", package = "mrgvalidate")
  roles <- if(type == "metworx"){
    readr::read_csv(file.path(TEST_INPUTS_DIR, "roles.csv"), col_types = "cc")
  }else{
    NULL
  }

  release_notes <- glue("
## New Features
 - {New Features} {(PR#)}

## Updates to existing functionality
 - {Function updates} {(PR#)}

## Bug fixes
 - {Bugs addressed} {(PR#)}
", .open = "{{") %>% toString() %>% strsplit("\n") %>% unlist()

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

  val_info <- format_val_info(auto_info)
  auto_tests <- list(tibble::tibble(`Test ID` = rep("Test ID", 2), `Test name` = rep("Test name", 2),
                            Passed = rep("[Number of passes]", 2), Failed = rep("[Number of fails]", 2)))
  auto_tests_plan <- map(auto_tests, ~ {.x %>% select(-c("Passed", "Failed"))})

  man_tests <- if(type == "metworx"){
    glue("
## [Manual Test Identifier]: [Manual Test Name]

[Test Description]

### Run Details

 - Executor: [Name]
 - Blueprint configuration: [Name if applicable]
 - Date: [Date]

[Screenshots as needed]

[Description of process for each screenshot as needed]

")
  }else{
    NULL
  }

  mat_out <- tibble::tibble(`User Story ID` = "[STORY-ID]", `User Story` = "As a [role], I want [functionality] so [value driver]",
                    `Test ID` = paste(rep("[Test Identifier]", 4), collapse = ", "))

  spec_chunks <- list(
    test = glue("
## User Story: [User Story Name and Identifier]

As a [role], I want [functionality] so that [value driver].

**Product risk:** Low/Medium/High

**Requirements**

 - [Requirement Identifier]: [Requirement Name]
 - [Requirement Identifier]: [Requirement Name]
 - [Requirement Identifier]: [Requirement Name]
 - [Requirement Identifier]: [Requirement Name]
 - [Requirement Identifier]: [Requirement Name]

**Tests**

"))

  test_chunks <- list(
    tibble::tibble("Test ID" = rep("[Test Identifier]", 5), "Test Name" = rep("[Test Name]", 5)) %>%
      as.data.frame()
  )

  # Validation Plan ---------------------------------------------------------


  template <- get_template("validation_plan", type = type)
  reference_docx <- get_reference_docx(file.path(output_dir, VAL_PLAN_FILE), style_dir)
  out_file <- format_rmd_name(output_dir, VAL_PLAN_FILE, append = append)
  fs::file_copy(template, out_file, overwrite = TRUE)

  rmarkdown::render(
    out_file,
    params = list(
      release_notes = release_notes,
      repo = auto_info[[1]]$info$env_vars$REPO
    ),
    output_format = rmarkdown::word_document(
      reference_docx = reference_docx),
    output_dir = dirname(out_file),
    quiet = TRUE
  )


  # Testing Plan ------------------------------------------------------------


  template <- get_template("testing_plan", type = type)
  reference_docx <- get_reference_docx(file.path(output_dir, TEST_PLAN_FILE), style_dir)
  out_file <- format_rmd_name(output_dir, TEST_PLAN_FILE, append = append)
  fs::file_copy(template, out_file, overwrite = TRUE)

  rmarkdown::render(
    out_file,
    params = list(
      auto_tests = auto_tests_plan,
      man_tests = "{Insert tests with Test Name, description and Test IDs for each test}"
    ),
    output_format = rmarkdown::word_document(
      reference_docx = reference_docx),
    output_dir = dirname(out_file),
    quiet = TRUE
  )


  # Testing Results ---------------------------------------------------------


  template <- get_template("testing_results", type = type)
  reference_docx <- get_reference_docx(file.path(output_dir, TEST_RESULTS_FILE), style_dir)
  out_file <- format_rmd_name(output_dir, TEST_RESULTS_FILE, append = append)
  fs::file_copy(template, out_file, overwrite = TRUE)

  rmarkdown::render(
    out_file,
    params = list(
      val_info = val_info,
      auto_tests = auto_tests,
      man_tests = list(man_tests)
    ),
    output_format = rmarkdown::word_document(
      reference_docx = reference_docx),
    output_dir = dirname(out_file),
    quiet = TRUE
  )


  # Traceability Matrix -----------------------------------------------------


  template <- get_template("traceability_matrix", type = type)
  reference_docx <- get_reference_docx(file.path(output_dir, MAT_FILE), style_dir)
  out_file <- format_rmd_name(output_dir, MAT_FILE, append = append)
  fs::file_copy(template, out_file, overwrite = TRUE)

  rmarkdown::render(
    out_file,
    params = list(
      matrix = mat_out
    ),
    output_format = rmarkdown::word_document(
      reference_docx = reference_docx),
    output_dir = dirname(out_file),
    quiet = TRUE
  )


  # Requirements Specification ----------------------------------------------


  template <- get_template("requirements_specification", type = type)
  reference_docx <- get_reference_docx(file.path(output_dir, REQ_FILE), style_dir)
  out_file <- format_rmd_name(output_dir, REQ_FILE, append = append)
  fs::file_copy(template, out_file, overwrite = TRUE)

  rmarkdown::render(
    out_file,
    params = list(
      roles = roles,
      spec_chunks = spec_chunks,
      test_chunks = test_chunks
    ),
    output_format = rmarkdown::word_document(
      reference_docx = reference_docx),
    output_dir = dirname(out_file),
    quiet = TRUE
  )


  # Validation Summary Report -----------------------------------------------


  template <- get_template("validation_summary", type = type)
  reference_docx <- get_reference_docx(file.path(output_dir, VAL_SUM_FILE), style_dir)
  out_file <- format_rmd_name(output_dir, VAL_SUM_FILE, append = append)
  fs::file_copy(template, out_file, overwrite = TRUE)

  rmarkdown::render(
    out_file,
    params = list(
      bugs = extract_bug_section(release_notes)
    ),
    output_format = rmarkdown::word_document(
      reference_docx = reference_docx),
    output_dir = dirname(out_file),
    quiet = TRUE
  )


  # Release Notes -----------------------------------------------------------


  template <- get_template("release_notes", type = type)
  reference_docx <- get_reference_docx(file.path(output_dir, RLS_NOTES_FILE), style_dir)
  out_file <- format_rmd_name(output_dir, RLS_NOTES_FILE, append = append)
  fs::file_copy(template, out_file, overwrite = TRUE)


  rmarkdown::render(
    out_file,
    params = list(
      release_notes = release_notes
    ),
    output_format = rmarkdown::word_document(
      reference_docx = reference_docx),
    output_dir = dirname(out_file),
    quiet = TRUE
  )


  ## cleanup RMDs
  if(cleanup_rmd){
    cleanup_rmds(output_dir = output_dir, append = append)
  }
}


