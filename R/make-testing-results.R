

#' Build the Testing Results document and write it to output file(s)
#'
#' This takes the input from automated and/or manual tests and writes them to a
#' `.docx` file.
#' @importFrom rmarkdown render
#' @importFrom fs file_copy
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
#' @param word_document Logical scaler indicating whether to render a docx document
#' @keywords internal
make_testing_results <- function(
  product,
  version,
  tests,
  auto_info,
  style_dir = NULL,
  out_file = "testing-results.Rmd",
  output_dir = getwd(),
  type = "package",
  word_document = TRUE
){

  template <- get_template("testing_results", type = type)

  if (!fs::dir_exists(output_dir)) fs::dir_create(output_dir)
  out_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(out_file), ".Rmd"))
  fs::file_copy(template, out_file)

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

  val_info <- map(names(auto_info), ~ {
    .suite <- auto_info[[.x]]
    glue('

### {.x}

**Date Run:** {.suite$date}

**Executor:** {.suite$executor}

{print_info_list(.suite$info)}

')
  })

  # write manual test outputs
  man_tests <- filter(tests, .data$test_type == "manual")
  if (nrow(man_tests) != 0) {
    # cat(file = out_file,  "\n# Manual Test Results\n", append = TRUE)
    # This needs to be revisited when we have data to test
    # (definitely in wrong format, but im not sure what .data$man_test_content looks like)
    # was:  walk(~ cat(file = out_file,  glue("\n{.x}\n\n"), append = TRUE))
    man_tests <- pull(man_tests, .data$man_test_content) %>%
      map(~ glue("\n{.x}\n\n"))
  }

  if (isTRUE(word_document)) {
    message("  Rendering markdown to docx...")
    rmarkdown::render(
      out_file,
      params = list(
        product_name = product,
        version = version,
        val_info = val_info,
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


#' Format manual test plan into flextable in for word doc rendering
#'
#' @param test_df dataframe of manual tests
#'
#' @details this also includes text, as we do not want to render this section in the absence of manual tests
#'
#' @keywords internal
format_man_test_results <- function(man_tests){
  if(is.null(man_tests)){
    cat(NULL)
  }else{
    if(nrow(man_tests) > 0){
      man_test_str <-
        "\n
## Manual Test Results
\n
"
      cat(man_test_str)
      for(i in seq_along(man_tests)){
        cat("\n")
        cat(man_tests[[i]])
      }
    }
  }
}

#' Format automatic test plan into flextable in for word doc rendering
#'
#' @param test_df dataframe of automatic tests
#'
#' @importFrom flextable flextable autofit theme_vanilla
#' @importFrom knitr knit_print
#'
#' @keywords internal
format_auto_tests_results <- function(test_df, val_info){

  for(i in seq_along(test_df)){
    tab <- test_df[[i]] %>% flextable() %>%
      flextable_word(column_shrink = "Test name")
    cat(val_info[[i]])
    cat("\n")
    cat(knit_print(tab))
  }
}

