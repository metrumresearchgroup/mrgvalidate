
#' Build the Requirements Specification and write it to output file(s)
#' @importFrom purrr map walk
#' @importFrom dplyr distinct filter group_rows slice
#' @importFrom glue glue
#' @importFrom knitr kable
#' @importFrom rmarkdown render
#' @importFrom fs dir_exists dir_create
#' @importFrom rlang .data
#' @param product_name The product you are validating, to be included in the output document.
#' @param version The version number of the product you are validating, to be included in the output document.
#' @param df Tibble containing stories, requirements, and tests. Created in
#'   [create_test_framework()].
#' @param roles A data frame of user roles. If specified, this will be
#'   inserted as a table under a "User Roles" section.
#' @param style_dir Directory to check for a docx style reference that has the
#'   same base name as `out_file`.
#' @param out_file filename to write markdown file out to. Any extension will be ignored and replaced with .md
#' @param output_dir Directory to write the output documents to. Defaults to working directory.
#' @param word_document Logical scaler indicating whether to render a docx document
#' @keywords internal
make_requirements <- function(
  product_name,
  version,
  df,
  roles = NULL,
  style_dir = NULL,
  out_file = REQ_FILE,
  output_dir = getwd(),
  type = "package",
  word_document = TRUE
) {

  # Setup
  template <- get_template("requirements_specification", type = type)
  reference_docx <- get_reference_docx(file.path(output_dir, out_file), style_dir) # set this before appending out_file
  out_file <- format_rmd_name(output_dir, out_file, append = product_name)
  fs::file_copy(template, out_file, overwrite = TRUE)

  df_story <- df %>%
    filter(!is.na(.data$StoryId)) %>%
    unnest(.data$tests) %>%
    distinct(.data$StoryId, .data$TestId, .keep_all = TRUE)

  tests_by_story <- df_story %>%
    group_by(.data$StoryId) %>%
    group_rows() %>%
    map(~ df_story[.x, ])

  # Format stories
  spec_chunks <- map(tests_by_story, format_spec)
  test_chunks <- map(tests_by_story, format_req_tests)

  # collapse stories
  spec_chunks <- map(spec_chunks, function(x) {
    paste(x, collapse = "\n")
  })

  if (isTRUE(word_document)) {
    message("  Rendering markdown to docx...")
    rmarkdown::render(
      out_file,
      params = list(
        product_name = product_name,
        version = version,
        roles = roles,
        spec_chunks = spec_chunks,
        test_chunks = test_chunks
      ),
      output_format = rmarkdown::word_document(
        reference_docx = reference_docx),
      output_dir = dirname(out_file),
      quiet = TRUE
    )
    message("  Finished rendering")
  }

}

#' Format roles in requirements specification
#'
#' @param roles A data frame of user roles. If specified, this will be
#'   inserted as a table under a "User Roles" section.
#'
#' @keywords internal
format_roles <- function(roles){
  if (!is.null(roles)) {
    cat("## User Roles\n", kable(roles), "\n", sep = "\n")
  }
}


#' Format requirements in rmarkdown
#'
#' separate stories by divider (hr)
#'
#' @param spec_chunks list of requirements, organized by story
#' @param test_chunks list of tests, formatted as flextables, organized by story
#' @param word_doc logical. Sets horizontal separator format
#'
#' @importFrom flextable fontsize
#' @keywords internal
format_requirements <- function(spec_chunks, test_chunks, word_doc = TRUE){
  sep <- ifelse(word_doc, "\n***\n", "\n<hr>\n")
  cat(sep)
  for(i in seq_along(spec_chunks)){
    tab <- test_chunks[[i]] %>%
      flextable_word(pg_width = 6, column_width = c("Test ID" = 1.5)) %>%
      fontsize(size = 9, part = "body")
    cat(spec_chunks[[i]])
    cat(knit_print(tab))
    cat(sep)
  }
}
