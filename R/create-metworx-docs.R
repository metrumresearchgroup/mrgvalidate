#' @param product_name The product being validated.
#' @param version The version number of the product.
#' @param specs tibble of stories and requirements. See [input_formats].
#' @param release_notes_file file path to a formatted markdown doc of release notes.
#' @param auto_test_dir,man_test_dir path to directories containing automatic
#'   and manual test output files. See [input_formats].
#' @param roles A data frame of user roles that, if specified, is inserted into
#'   the requirements document.
#' @param style_dir Directory that has style references for the generated docx
#'   files. When generating each output file, Pandoc will be instructed to use
#'   the reference file from this directory that has the same base name (if it
#'   exists).
#' @param output_dir Directory to write the output documents to. Defaults to
#'   working directory.
#' @param write Whether to create the output docs. Setting this to `FALSE` is
#'   useful when you're just interested in the return value.
#' @param cleanup_rmd Whether to delete the copied RMD's after the word documents are generated.
#'  Defaults to `TRUE`.
#'
#' @describeIn create_package_docs Create validation docs for metworx
#'
#' @importFrom dplyr bind_rows filter full_join mutate pull recode rename select
#' @importFrom purrr map_chr
#' @importFrom stringr str_pad
#' @importFrom tidyr nest unnest
#' @importFrom rlang .data
#'
#' @export
create_metworx_docs <- function
(
  product_name,
  version,
  specs,
  release_notes_file = NULL,
  auto_test_dir = NULL,
  man_test_dir = NULL,
  roles = NULL,
  style_dir = NULL,
  output_dir = getwd(),
  write = TRUE,
  cleanup_rmd = TRUE
) {

  test_data <- create_test_framework(product_name = product_name,
                                     specs = specs,
                                     auto_test_dir = auto_test_dir,
                                     man_test_dir = man_test_dir,
                                     type = "metworx")


  # Error out here and call find_missing
  input <- check_input(test_data$dd)

  if(input$missing){
    write <- FALSE
    print(input$missing_data)
  }

  # Need some handling for release notes
  # possibly a dataframe or list object
  if(!is.null(release_notes_file)){
    assert_true(fs::file_exists(release_notes_file))
    # likely need a separate function for metworx (package handling placeholder)
    release_notes <- release_notes_file %>% readLines()
  }else{
    release_notes <- NULL
  }


  if (isTRUE(write)) {

    # Validation Plan
    make_validation_plan(
      product_name = product_name,
      version = version,
      release_notes = release_notes,
      auto_info = test_data$auto_info,
      style_dir = style_dir,
      out_file = VAL_PLAN_FILE,
      output_dir = output_dir,
      type = "metworx",
      word_document = TRUE
    )

    # Testing Plan
    make_testing_plan(
      product_name = product_name,
      version = version,
      language = NULL,
      test_data$tests,
      test_data$auto_info,
      style_dir = style_dir,
      out_file = TEST_PLAN_FILE,
      output_dir = output_dir,
      type = "metworx",
      word_document = TRUE
    )

    # Testing Results
    make_testing_results(
      product_name = product_name,
      version = version,
      test_data$tests,
      test_data$auto_info,
      style_dir = style_dir,
      out_file = TEST_RESULTS_FILE,
      output_dir = output_dir,
      type = "metworx",
      word_document = TRUE
    )

    # Traceability Matrix
    make_traceability_matrix(
      product_name = product_name,
      version = version,
      df = test_data$dd,
      style_dir = style_dir,
      out_file = MAT_FILE,
      output_dir = output_dir,
      type = "metworx",
      word_document = TRUE
    )

    # Requirements Specification
    make_requirements(
      product_name = product_name,
      version = version,
      df = test_data$dd,
      roles = roles,
      style_dir = style_dir,
      out_file = REQ_FILE,
      output_dir = output_dir,
      type = "metworx",
      word_document = TRUE
    )

    # Validation Summary Report
    make_validation_summary(
      product_name = product_name,
      version = version,
      release_notes = release_notes,
      style_dir = style_dir,
      out_file = VAL_SUM_FILE,
      output_dir = output_dir,
      type = "metworx",
      word_document = TRUE
    )

    # Release Notes
    make_release_notes(
      product_name = product_name,
      version = version,
      release_notes = release_notes,
      style_dir = style_dir,
      out_file = RLS_NOTES_FILE,
      output_dir = output_dir,
      type = "metworx",
      word_document = TRUE
    )
  }

  if(cleanup_rmd){
    cleanup_rmds(output_dir = output_dir, append = product_name)
  }

  dd <- test_data$dd

  return(invisible(dd))
}
