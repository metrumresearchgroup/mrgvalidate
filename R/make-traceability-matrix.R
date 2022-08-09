
#' Build the Traceability Matrix and write it to output file(s)
#' @importFrom purrr map walk
#' @importFrom dplyr arrange first group_by select summarise
#' @importFrom knitr kable
#' @importFrom tidyr unnest
#' @importFrom glue glue
#' @importFrom rmarkdown render
#' @importFrom rlang .data
#' @importFrom stringr str_extract str_trim
#' @importFrom fs dir_exists dir_create
#' @param product_name The name of the product you are validating, to be included in the output document.
#' @param version The version number of the product you are validating, to be included in the output document.
#' @param df Tibble containing stories, requirements, and tests. Created in
#'   [create_test_framework()].
#' @param style_dir Directory to check for a docx style reference that has the
#'   same base name as `out_file`.
#' @param out_file filename to write markdown file out to. Any extension will be ignored and replaced with .md
#' @param output_dir Directory to write the output documents to. Defaults to working directory.
#' @param type the type of doc you want to render ("package" or "metworx")
#' @param word_document Logical scaler indicating whether to render a docx document
#' @keywords internal
make_traceability_matrix <- function(
  product_name,
  version,
  df,
  style_dir = NULL,
  out_file = MAT_FILE,
  output_dir = getwd(),
  type = "package",
  word_document = TRUE
) {

  # Setup
  template <- get_template("traceability_matrix", type = type)
  reference_docx <- get_reference_docx(file.path(output_dir, out_file), style_dir) # set this before appending out_file
  out_file <- format_rmd_name(output_dir, out_file, append = product_name)
  fs::file_copy(template, out_file, overwrite = TRUE)

  mat <- df %>%
    filter(!is.na(.data$StoryId)) %>%
    unnest(cols = c(.data$tests)) %>%
    filter(!is.na(.data$passed))

  mat <- if ("RequirementId" %in% names(mat)) {
    arrange(mat, .data$StoryId, .data$RequirementId, .data$TestId)
  } else {
    arrange(mat, .data$StoryId, .data$TestId)
  }

  mat <- group_by(mat, .data$StoryId) %>%
    summarise(test_ids = paste0(sort(unique(.data$TestId)), collapse = " "),
              description = first(.data$StoryDescription)) %>%
    mutate(description = glue("{str_extract(str_trim(.data$description), '^.+')} ({.data$StoryId})"))

  mat_out <- select(
    mat,
    `User Story` = .data$description,
    `Test ID` = .data$test_ids,
  )

  if (isTRUE(word_document)) {
    message("  Rendering markdown to docx...")
    rmarkdown::render(
      out_file,
      params = list(
        product_name = product_name,
        version = version,
        matrix = mat_out
      ),
      output_format = rmarkdown::word_document(
        reference_docx = reference_docx),
      output_dir = dirname(out_file),
      quiet = TRUE
    )
    message("  Finished rendering")
  }

}


