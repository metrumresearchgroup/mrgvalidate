

#' Build the Validation Plan document and write it to output file(s)
#'
#' @importFrom rmarkdown render
#' @importFrom fs file_copy
#' @param product The name of the product you are validating, to be included in the output document.
#' @param version The version number of the product you are validating, to be included in the output document.
#' @param style_dir Directory to check for a docx style reference that has the
#'   same base name as `out_file`.
#' @param out_file Filename to write markdown file out to. Any extension will be ignored and replaced with .Rmd
#' @param output_dir Directory to write the output documents to. Defaults to working directory.
#' @param word_document Logical scaler indicating whether to render a docx document
#' @keywords internal
make_validation_plan <- function(
  product,
  version,
  release_notes = NULL,
  style_dir = NULL,
  out_file = VAL_PLAN_FILE,
  output_dir = getwd(),
  type = "package",
  word_document = TRUE
){

  template <- get_template("validation_plan", type = type)

  if (!fs::dir_exists(output_dir)) fs::dir_create(output_dir)
  out_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(out_file), ".Rmd"))
  fs::file_copy(template, out_file)

  if (isTRUE(word_document)) {
    message("  Rendering markdown to docx...")
    rmarkdown::render(
      out_file,
      params = list(
        product_name = product,
        version = version,
        release_notes = release_notes
      ),
      output_format = rmarkdown::word_document(
        reference_docx = get_reference_docx(out_file, style_dir)),
      output_dir = dirname(out_file),
      quiet = TRUE
    )
    message("  Finished rendering")
  }
}


#' format function changes from release_notes
#'
#' @param release_notes a list of release_notes
#'
#' @keywords internal
format_release_changes <- function(release_notes = NULL){
  # some boiler plate
  # This cant be fully done right now, as we need to know what the format of `release_notes` will be

  # Determine variables below -from- `release_notes`
  new_functionality <- c("blah_function()", "important_function()") # release_notes$new_functionality
  function_updates <- c("hello()", "goodbye()")  # release_notes$function_updates
  functions_removed <- c("fetch_stapler()", "put_in_fridge()")  # release_notes$functions_removed

  # hopefully we can get bugs in here too (`release_notes$bugs`)

  release_text <- glue("
New User Story features in this release include:\n
{paste0('- ',new_functionality, collapse = '\n')}
\n
Additionally the Release includes updated:\n
{paste0('- ',function_updates, collapse = '\n')}
\n
The following items (if any) have been removed:\n
{paste0('- ',functions_removed, collapse = '\n')}
")
  cat(release_text)
}
