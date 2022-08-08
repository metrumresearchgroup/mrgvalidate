#' @describeIn create_validation_docs Create validation docs for packages
#'
#' @param product_name The product being validated.
#' @param version The version number of the product.
#' @param repo_url
#' @param specs tibble of stories and requirements. See [input_formats].
#' @param release_notes_file file path to a formatted markdown doc of release notes.
#' @param auto_test_dir,man_test_dir path to directories containing automatic
#'   and manual test output files. See [input_formats].
#' @param style_dir Directory that has style references for the generated docx
#'   files. When generating each output file, Pandoc will be instructed to use
#'   the reference file from this directory that has the same base name (if it
#'   exists).
#' @param output_dir Directory to write the output documents to. Defaults to
#'   working directory.
#' @param write Whether to create the output docs. Setting this to `FALSE` is
#'   useful when you're just interested in the return value.
#'
#'
#' @return In addition to creating the validation docs, a tibble that joins the
#'   tests with `specs` is returned invisibly.
#' @importFrom dplyr bind_rows filter full_join mutate pull recode rename select
#' @importFrom purrr map_chr
#' @importFrom stringr str_pad
#' @importFrom tidyr nest unnest
#' @importFrom rlang .data
#'
#' @export
create_package_docs <- function
(
  product_name,
  version,
  repo_url,
  specs,
  release_notes_file,
  auto_test_dir = NULL,
  man_test_dir = NULL,
  style_dir = NULL,
  output_dir = getwd(),
  write = TRUE
) {


  test_data <- create_test_framework(product_name = product_name,
                                     specs = specs,
                                     auto_test_dir = auto_test_dir,
                                     man_test_dir = man_test_dir,
                                     type = "package")

  # Read in NEWS.md for release notes - change this
  release_notes <- release_notes_file %>% readLines()


  if (isTRUE(write)) {

    # Validation Plan
    make_validation_plan(
      product_name,
      version,
      repo_url = repo_url,
      release_notes = release_notes,
      auto_info = test_data$auto_info,
      style_dir = style_dir,
      out_file = VAL_PLAN_FILE,
      output_dir = output_dir,
      type = "package",
      word_document = TRUE
    )


    # Testing Plan
    make_testing_plan(
      product_name,
      version,
      test_data$tests,
      test_data$auto_info,
      style_dir = style_dir,
      out_file = TEST_PLAN_FILE,
      output_dir = output_dir,
      type = "package",
      word_document = TRUE
    )

    # Testing Results
    make_testing_results(
      product_name,
      version,
      test_data$tests,
      test_data$auto_info,
      style_dir = style_dir,
      out_file = TEST_RESULTS_FILE,
      output_dir = output_dir,
      type = "package",
      word_document = TRUE
    )

    # Traceability Matrix
    make_traceability_matrix(
      test_data$dd,
      product_name,
      version,
      style_dir = style_dir,
      out_file = MAT_FILE,
      output_dir = output_dir,
      type = "package",
      word_document = TRUE
    )

    # Requirements Specification
    make_requirements(
      test_data$dd,
      product_name,
      version,
      roles = NULL,
      style_dir = style_dir,
      out_file = REQ_FILE,
      output_dir = output_dir,
      type = "package",
      word_document = TRUE
    )

    # Validation Summary Report
    make_validation_summary(
      product_name,
      version,
      release_notes = release_notes,
      style_dir = style_dir,
      out_file = VAL_SUM_FILE,
      output_dir = output_dir,
      type = "package",
      word_document = TRUE
    )
  }

  return(invisible(dd))
}
