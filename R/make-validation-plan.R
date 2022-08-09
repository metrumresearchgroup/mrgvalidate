

#' Build the Validation Plan document and write it to output file(s)
#'
#' @importFrom rmarkdown render
#' @importFrom fs file_copy
#' @importFrom purrr map_chr
#' @param product The name of the product you are validating, to be included in the output document.
#' @param version The version number of the product you are validating, to be included in the output document.
#' @param repo_url Character string denoting the url of repository.
#' @param release_notes list of release notes, formatted for rmarkdown.
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
make_validation_plan <- function(
  product,
  version,
  repo_url = NULL,
  release_notes = NULL,
  auto_info = NULL,
  style_dir = NULL,
  out_file = VAL_PLAN_FILE,
  output_dir = getwd(),
  type = "package",
  word_document = TRUE
){

  if(type == "package"){
    checkmate::assert_string(repo_url, null.ok = FALSE)
  }

  template <- get_template("validation_plan", type = type)

  if (!fs::dir_exists(output_dir)) fs::dir_create(output_dir)
  out_file <- file.path(output_dir, out_file)
  fs::file_copy(template, out_file, overwrite = TRUE)

  if (isTRUE(word_document)) {
    message("  Rendering markdown to docx...")
    rmarkdown::render(
      out_file,
      params = list(
        product_name = product,
        version = version,
        release_notes = release_notes,
        repo = glue("`{repo_url}`")
      ),
      output_format = rmarkdown::word_document(
        reference_docx = get_reference_docx(out_file, style_dir)),
      output_dir = dirname(out_file),
      quiet = TRUE
    )
    message("  Finished rendering")
  }
}


#' Format Release Notes
#'
#' Format `release_notes` to have evenly spaced headers with no white space
#'
#' @param release_notes a list or character vector of release_notes
#'
#' @details
#' Note: `release_notes` *cannot* be a glue object for formatting changes to work
#'
#' @keywords internal
format_release_changes <- function(release_notes = NULL){

  if(!is.list(release_notes)){
    release_notes <- list(release_notes)
  }
  release_text <- map(release_notes, ~{
    glue("{paste(.x, collapse = '\n') %>% str_trim()}")
  })

  for(i in seq_along(release_text)){
    cat(release_text[[i]])
    cat("\n\n\n")
  }
}
