

#' Build the Testing Plan document and write it to output file(s)
#'
#' This takes the input from automated and/or manual tests and writes them to a
#' `.docx` file.
#' @importFrom rmarkdown render
#' @importFrom fs file_copy
#' @importFrom stringr str_trim
#' @importFrom stats setNames
#' @param product The name of the product you are validating, to be included in the output document.
#' @param version The version number of the product you are validating, to be included in the output document.
#' @param tests Tibble containing all test results, FORMAT: CREATED ON LINE 59
#'   OF `generate-docs.R` in [create_validation_docs()].
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
  product,
  version,
  tests,
  auto_info,
  style_dir = NULL,
  out_file = TEST_PLAN_FILE,
  output_dir = getwd(),
  type = "package",
  word_document = TRUE
){

  template <- get_template("testing_plan", type = type)

  if (!fs::dir_exists(output_dir)) fs::dir_create(output_dir)
  out_file <- file.path(output_dir, out_file)
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
        product_name = product,
        version = version,
        auto_tests = auto_tests,
        man_tests = man_tests
      ),
      output_format = rmarkdown::word_document(
        reference_docx = get_reference_docx(out_file, style_dir)),
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
format_auto_test_plan <- function(test_df, template = FALSE){
  if(template){
    cat("\n")
    cat(test_df)
  }else{
    for(i in seq_along(test_df)){
      tab <- as.data.frame(test_df[[i]]) %>% flextable(theme_fun = theme_box) %>%
        flextable_word()
      cat(knit_print(tab))
    }
  }
}


#' Format manual test plan into flextable in for word doc rendering
#'
#' @param test_df dataframe of manual tests
#' @param template logical (T/F). If `TRUE` spit out text instead of formatting a dataframe.
#'
#' @details this also includes text, as we do not want to render this section in the absence of manual tests
#'
#' @keywords internal
format_man_test_plan <- function(man_tests, template = FALSE){
  if(is.null(man_tests)){
    cat(NULL)
  }else{
    if(nrow(man_tests) > 0){
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
      cat("\n")
      if(template){
        cat(man_tests)
      }else{
        for(i in seq_along(man_tests)){
          cat(man_tests[[i]])
        }
      }
    }
  }
}
