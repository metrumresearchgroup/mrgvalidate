

#' Build the Testing Plan document and write it to output file(s)
#'
#' This takes the input from automated and/or manual tests and writes them to a
#' `.docx` file.
#' @importFrom rmarkdown render
#' @importFrom fs file_copy
#' @importFrom flextable flextable autofit theme_box
#' @importFrom knitr knit_print
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
make_testing_plan <- function(
  product,
  version,
  tests,
  auto_info,
  style_dir = NULL,
  out_file = "testing-plan.Rmd",
  output_dir = getwd(),
  type = "package",
  word_document = TRUE
){

  template <- get_template("testing_plan", type = type)

  if (!fs::dir_exists(output_dir)) fs::dir_create(output_dir)
  out_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(out_file), ".Rmd"))
  fs::file_copy(template, out_file)

  auto_tests <- filter(tests, .data$test_type == "automatic")

  test_df <- map(names(auto_info), ~ {
    .suite <- auto_info[[.x]]
    # filter to relevant tests
    test_df <- auto_tests %>%
      filter(.data$result_file == .x) %>%
      select(
        `Test ID` = .data$TestId,
        `Test name` = .data$TestName,
      )
  }) %>% setNames(names(auto_info))

  if (isTRUE(word_document)) {
    message("  Rendering markdown to docx...")
    rmarkdown::render(
      out_file,
      params = list(
        product_name = product,
        version = version,
        tests = test_df
      ),
      output_format = rmarkdown::word_document(
        reference_docx = get_reference_docx(out_file, style_dir)),
      output_dir = dirname(out_file),
      quiet = TRUE
    )
    message("  Finished rendering")
  }
}
