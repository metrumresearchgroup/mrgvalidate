

#' Build the Release Notes document and write it to output file(s)
#'
#' @importFrom rmarkdown render
#' @importFrom fs file_copy
#' @param product_name The name of the product you are validating, to be included in the output document.
#' @param version The version number of the product you are validating, to be included in the output document.
#' @param repo_url Character string denoting the url of repository.
#' @param release_notes list of release notes, formatted for rmarkdown.
#' @param style_dir Directory to check for a docx style reference that has the
#'   same base name as `out_file`.
#' @param out_file Filename to write markdown file out to. Any extension will be ignored and replaced with .Rmd
#' @param output_dir Directory to write the output documents to. Defaults to working directory.
#' @param type the type of doc you want to render ("package" or "metworx")
#' @param word_document Logical scaler indicating whether to render a docx document
#' @keywords internal
make_release_notes <- function(
  product_name,
  version,
  repo_url = NULL,
  release_notes = NULL,
  style_dir = NULL,
  out_file = RLS_NOTES_FILE,
  output_dir = getwd(),
  type = "package",
  word_document = TRUE
){

  if(type == "package"){
    assert_string(repo_url, null.ok = FALSE)
  }

  # Setup
  template <- get_template("release_notes", type = type)
  reference_docx <- get_reference_docx(file.path(output_dir, out_file), style_dir) # set this before appending out_file
  out_file <- format_rmd_name(output_dir, out_file, append = product_name)
  fs::file_copy(template, out_file, overwrite = TRUE)

  if (isTRUE(word_document)) {
    message("  Rendering markdown to docx...")
    rmarkdown::render(
      out_file,
      params = list(
        product_name = product_name,
        version = version,
        repo = glue("`{repo_url}`"),
        release_notes = release_notes
      ),
      output_format = rmarkdown::word_document(
        reference_docx = reference_docx),
      output_dir = dirname(out_file),
      quiet = TRUE
    )
    message("  Finished rendering")
  }
}

