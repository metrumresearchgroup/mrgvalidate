

#' Build the Testing Plan document and write it to output file(s)
#'
#' This takes the input from automated and/or manual tests and writes them to a
#' `.docx` file.
#' @importFrom rmarkdown render
#' @importFrom fs file_copy
#' @importFrom stringr str_trim
#' @importFrom stats setNames
#' @param product_name The name of the product you are validating, to be included in the output document.
#' @param version The version number of the product you are validating, to be included in the output document.
#' @param language Denotes the language the package was coded in. Either 'R' or 'Go'.
#'        Dictates boiler plate text in the generated validation docs.
#' @param tests Tibble containing all test results, FORMAT: CREATED ON LINE 59
#'   OF `generate-docs.R` in [create_package_docs()] or [create_metworx_docs()].
#' @param auto_info A named list containing the test suite information pulled
#'   from the `.json` files found in `auto_test_dir`, one element per `.json`
#'   (named with the filename _without_ extension). **Same note as `tests` about
#'   exporting and specs.**
#' @param style_dir Directory to check for a docx style reference that has the
#'   same base name as `out_file`.
#' @param out_file Filename to write markdown file out to. Any extension will be ignored and replaced with .Rmd
#' @param output_dir Directory to write the output documents to. Defaults to working directory.
#' @param type the type of doc you want to render ("package" or "metworx")
#' @param word_document Logical scaler indicating whether to render a docx document
#' @keywords internal
make_testing_plan <- function(
  product_name,
  version,
  language,
  tests,
  auto_info,
  style_dir = NULL,
  out_file = TEST_PLAN_FILE,
  output_dir = getwd(),
  type = "package",
  word_document = TRUE
){

  # Setup
  template <- get_template("testing_plan", type = type)
  reference_docx <- get_reference_docx(file.path(output_dir, out_file), style_dir) # set this before appending out_file
  out_file <- format_rmd_name(output_dir, out_file, append = product_name)
  fs::file_copy(template, out_file, overwrite = TRUE)

  auto_tests <- filter(tests, .data$test_type == "automatic")
  auto_tests <- map(names(auto_info), ~ {
    .suite <- auto_info[[.x]]
    # filter to relevant tests
    auto_tests <- auto_tests %>%
      filter(.data$result_file == .x) %>%
      select(
        `Test ID` = .data$TestId,
        `Test name` = .data$TestName
      )
  }) %>% setNames(names(auto_info))


  # write manual test outputs
  man_tests <- filter(tests, .data$test_type == "manual")
  if (nrow(man_tests) != 0) {
    man_tests <- pull(man_tests, .data$man_test_content) %>%
      # Remove Results from manual tests (everything after run details)
      # might want a tryCatch here? (unsure if they're always in that format)
      # revisit this for updated manual sections
      map(~ {
        .x <- gsub("### Run Details.*", "",.x)
        str_trim(.x)
      }) %>%
      map(~ glue("\n{.x}\n\n"))
  }

  if (isTRUE(word_document)) {
    message("  Rendering markdown to docx...")
    rmarkdown::render(
      out_file,
      params = list(
        product_name = product_name,
        version = version,
        language = language,
        auto_tests = auto_tests,
        man_tests = man_tests
      ),
      output_format = rmarkdown::word_document(
        reference_docx = reference_docx),
      output_dir = dirname(out_file),
      quiet = TRUE
    )
    message("  Finished rendering")
  }
}

#' Format automatic test plan into flextable in for word doc rendering
#'
#' @param test_df dataframe of automatic tests
#' @param template logical (T/F). If `TRUE` spit out text instead of formatting a dataframe.
#'
#' @importFrom flextable flextable autofit theme_box
#' @importFrom knitr knit_print
#'
#' @keywords internal
format_auto_test_plan <- function(test_df){
  for(i in seq_along(test_df)){
    tab <- test_df[[i]] %>% flextable_word()
    cat(knit_print(tab))
  }
}


#' Format manual test plan for word doc rendering
#'
#' @param man_tests list of manual tests
#'
#' @details this also includes text, as we do not want to render this section in the absence of manual tests
#'
#' @keywords internal
format_man_test_plan <- function(man_tests){
  if(is.null(man_tests)){
    cat(NULL)
  }else{
    if(length(man_tests) > 0){
      man_test_str <-
        "\n
## Manual Testing
\n
Manual tests will be conducted through following a manually written test script which outlines test instructions. Manual tests will use screenshots as test evidence.
\n

### Manual Test Design
\n
Manual tests are designed as follows:
\n

#### Set Up
\n
Describe the actions the test administrator must take prior to executing the test, whether it be adding a test file/script to the system, entering a software license, etc.
\n

#### Procedure
\n
Step-by-step instructions for executing the test.
\n

#### Expected Results
\n
Examples of the evidence users which confirm expected system behavior. A one sentence  summary of the evidence accompanies all screenshots to confirm testing evidence for Validation Reviewers.
\n

### Manual Tests
\n
The following manual tests will be conducted:
\n"

      cat(man_test_str)
      for(i in seq_along(man_tests)){
        cat("\n***\n\n")
        cat(man_tests[[i]])
      }
    }
  }
}


#' Generate automated testing boiler plate text for the testing plan
#'
#'
#' @param language Denotes the language the package was coded in. Either 'R' or 'Go'.
#'        Dictates boiler plate text in the generated validation docs.
#' @keywords internal
auto_testing_text <- function(language = c("R", "Go")){

  language <- match.arg(language)

  text <- if(language == "R"){
    paste("Testing in both development and validation phases proceeded using a standardized,
automated unit testing framework via the testthat R package. Tests were written in testthat format
and saved to R source files located within the package repository. Testing was executed using the
`testthat::test_check` function which ran all tests in every test file in the test directory. Each
test was run in a clean R environment. The test_check function returns a matrix of result data,
with one line for each test and the corresponding result. The test matrix was retained and saved as
an artifact from the validation testing. Tests relevant to the user stories covered by this change
request were extracted from the larger test matrix to create the traceability matrix connecting the
user story with the test result.")
  }else{
    paste("Testing in both development and validation phases proceeded using a standardized, automated
unit testing framework via Go's standard testing framework. Tests were written in Go and saved to `.go`
source files with the suffix `_test` located within the repository. Testing was executed using the
`go test ./...` command which ran all tests in every test file in the repository. The test results are
captured as a `.json` file and saved as an artifact from the validation testing. Tests relevant to the
user stories covered by this change request were extracted from the larger test matrix to create the
traceability matrix connecting the user story with the test result.")
  }

  text <- glue("{gsub('\n',' ', text)}")
  cat("\n")
  cat(text)
}
