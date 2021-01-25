
#' Build the Requirements Specification and write it to a markdown file
#' @importFrom purrr map walk
#' @importFrom dplyr slice
#' @importFrom glue glue
#' @importFrom rmarkdown render
#' @importFrom fs dir_exists dir_create
#' @param df Tibble output from [process_stories()].
#' @param pkg The name of the package you are validating, to be included in the output document.
#' @param version The version number of the package you are validating, to be included in the output document.
#' @param out_file filename to write markdown file out to. Any extension will be ignored and replaced with .md
#' @param output_dir Directory to write the output documents to. Defaults to working directory.
#' @param word_document Logical scaler indicating whether to render a docx document
#' @export
write_requirements <- function(
  df,
  pkg,
  version,
  out_file = REQ_FILE,
  output_dir = getwd(),
  word_document = TRUE
) {
  if (!fs::dir_exists(output_dir)) fs::dir_create(output_dir)
  out_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(out_file), ".md"))

  req_boiler <- glue('
# Requirements Specification: {pkg} {version}

## Scope

The purpose of this document is to define specific criteria for each testing
task.  Testing shall be conducted in accordance to the requirements within this
document. The Requirement Specifications ensure that each requirement is tested.


')

  cat(file=out_file, req_boiler, "\n")

  # write markdown file
  spec_chunks <- map(seq(nrow(df)), ~format_spec(slice(df,.x)))
  walk(spec_chunks, function(x) {
    cat(file=out_file, x, "\n<hr>\n", append=TRUE,sep="\n")
  })
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
