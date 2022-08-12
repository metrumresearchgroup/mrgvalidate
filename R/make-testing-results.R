

#' Build the Testing Results document and write it to output file(s)
#'
#' This takes the input from automated and/or manual tests and writes them to a
#' `.docx` file.
#' @importFrom rmarkdown render
#' @importFrom fs file_copy
#' @importFrom stats setNames
#' @param product_name The name of the product you are validating, to be included in the output document.
#' @param version The version number of the product you are validating, to be included in the output document.
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
make_testing_results <- function(
  product_name,
  version,
  tests,
  auto_info,
  style_dir = NULL,
  out_file = TEST_RESULTS_FILE,
  output_dir = getwd(),
  type = "package",
  word_document = TRUE
){

  # Setup
  template <- get_template("testing_results", type = type)
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
        `Test name` = .data$TestName,
        .data$passed,
        .data$failed
      )
  }) %>% setNames(names(auto_info))

  val_info <- format_val_info(auto_info)

  # write manual test outputs
  man_tests <- filter(tests, .data$test_type == "manual")
  if (nrow(man_tests) != 0) {
    # cat(file = out_file,  "\n# Manual Test Results\n", append = TRUE)
    # This needs to be revisited when we have data to test
    # (definitely in wrong format, but im not sure what .data$man_test_content looks like)
    # was:  walk(~ cat(file = out_file,  glue("\n{.x}\n\n"), append = TRUE))
    man_tests <- pull(man_tests, .data$man_test_content) %>%
      map(~ glue("\n{.x}\n\n"))
  }else{
    man_tests <- NULL
  }

  if (isTRUE(word_document)) {
    message("  Rendering markdown to docx...")
    rmarkdown::render(
      out_file,
      params = list(
        product_name = product_name,
        version = version,
        val_info = val_info,
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


#' Format manual test results for word doc rendering
#'
#' @param man_tests list of manual tests
#'
#' @details this also includes text, as we do not want to render this section in the absence of manual tests
#'
#' @keywords internal
format_man_test_results <- function(man_tests){
  if(is.null(man_tests)){
    cat(NULL)
  }else{
    if(length(man_tests) > 0){
      man_test_str <-
        "\n
## Manual Test Results
\n
"
      cat(man_test_str)
      for(i in seq_along(man_tests)){
        cat("\n***\n\n")
        cat(man_tests[[i]])
      }
    }
  }
}

#' Format automatic test plan into flextable in for word doc rendering
#'
#' @param auto_tests dataframe of automatic tests
#'
#' @importFrom flextable flextable autofit theme_vanilla
#' @importFrom knitr knit_print
#'
#' @keywords internal
format_auto_tests_results <- function(auto_tests, val_info){
  if(is.null(auto_tests)){
    cat(NULL)
  }else{
    if(length(auto_tests) > 0){
      auto_test_str <-
        "\n
## Automated Test Results
\n
"
      cat(auto_test_str)
      for(i in seq_along(auto_tests)){
        tab <- auto_tests[[i]] %>%
          flextable_word(column_shrink = "Test name")
        cat(val_info[[i]])
        cat("\n")
        cat(knit_print(tab))
      }
    }
  }
}

#' Format info on automatic tests
#'
#' @param auto_info named list
#'
#' @keywords internal
format_val_info <- function(auto_info){
  val_info <- map(names(auto_info), ~ {
    .suite <- auto_info[[.x]]
    executor <- .suite$executor
    if(is.null(executor)){
      stop(glue("No test executor was found for {.x}. This is required."))
    }
    glue('

### {.x}

**Date Run:** {.suite$date}

**Executor:** {.suite$executor}

{print_info_list(.suite$info)}

')
  })
  return(val_info)
}
