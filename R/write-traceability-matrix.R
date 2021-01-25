
#' Build the Traceability Matrix and write it to a output files
#' @importFrom purrr map walk
#' @importFrom dplyr slice select mutate group_by ungroup n
#' @importFrom knitr kable
#' @importFrom tidyr unnest
#' @importFrom glue glue
#' @importFrom rmarkdown render
#' @importFrom rlang .data
#' @importFrom fs dir_exists dir_create
#' @param df Tibble output from [process_stories()].
#' @param pkg The name of the package you are validating, to be included in the output document.
#' @param version The version number of the package you are validating, to be included in the output document.
#' @param out_file filename to write markdown file out to. Any extension will be ignored and replaced with .md
#' @param output_dir Directory to write the output documents to. Defaults to working directory.
#' @param word_document Logical scaler indicating whether to render a docx document
#' @export
write_traceability_matrix <- function(
  df,
  pkg,
  version,
  out_file = MAT_FILE,
  output_dir = getwd(),
  word_document = TRUE
) {
  if (!fs::dir_exists(output_dir)) fs::dir_create(output_dir)
  out_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(out_file), ".md"))

  mat_boiler <- glue('
# Traceability Matrix: {pkg} {version}

## Scope

This traceability matrix links product risk, test names, and test results to
specific user stories for the proposed software release. User stories, including
requirements and test specifications are listed in the Requirements Specification
and Validation Plan.

')

  mat <- select(df,-.data$story) %>% unnest(cols = c(.data$tests)) %>% mutate(date = Sys.Date())
  mat <- select(mat, .data$title, .data$issue, .data$risk, .data$test_name, .data$number, .data$failed, .data$date)
  mat <- mutate(mat, date= format(.data$date, "%Y-%m-%d"))
  mat <- mutate(mat, issue_title = paste(.data$issue, .data$title))

  mat <-
    group_by(mat, .data$issue) %>%
    mutate(issue_title = c(.data$issue_title[1], rep("", n()-1))) %>%
    ungroup()

  mat <- mutate(mat, pass = paste0( (.data$number-.data$failed), " of ", .data$number))

  mat_out <- select(
    mat,
    `issue title` = .data$issue_title,
    .data$risk,
    `test name` = .data$test_name,
    .data$pass,
    `date run` = .data$date
  )

  cat(file = out_file,  mat_boiler,"\n")
  tab <- knitr::kable(mat_out)
  cat(file = out_file, tab, sep = "\n", append = TRUE)
  message(glue("Finished writing to {out_file}"))

  if (isTRUE(word_document)) {
    message("  Rendering markdown to docx...")
    rmarkdown::render(
      out_file,
      output_format = "word_document",
      output_dir = dirname(out_file),
      quiet = TRUE
    )
    message("  Finished rendering")
  }

}


