
#####################
# formatting helpers
#####################

#' format a story for inclusion in output documents
#' @importFrom dplyr arrange distinct pull select
#' @importFrom knitr kable
#' @importFrom rlang .data
#' @importFrom stringr str_squish
#' @param x A single row from the stories df in [write_requirements()]
#' @keywords internal
format_spec <- function(x) {
  header <- paste0("## User Story: ", x$StoryId[[1]], " ", x$StoryName[[1]], "\n")
  bod <- gsub("\r", "", x$StoryDescription[[1]])
  risk <- gsub("risk: ", "", x$ProductRisk[[1]])

  if (all(c("RequirementId", "RequirementDescription") %in% names(x))) {
    reqs <- x %>%
      arrange(.data$RequirementId) %>%
      distinct(.data$RequirementId, .keep_all = TRUE)
    reqs <- paste0("- ", reqs$RequirementId,
                   ": ", str_squish(reqs$RequirementDescription), "\n")
  } else {
    reqs <- NULL
  }

  tst <- x %>%
    select(`Test ID` = .data$TestId, `Test name` = .data$TestName)
  tst_tab <- knitr::kable(tst, format="markdown")
  c(header,
    bod, "\n\n",
    "**Product risk**: ", risk, "\n\n",
    if(is.null(reqs)) "" else c("**Summary**\n", reqs, "\n\n"),
    "**Tests**\n\n", tst_tab)
}

#' Return reference docx file whose name matches current output file.
#'
#' @param out_file,style_dir The output file and reference directory passed to a
#'   `write_*` function (e.g., [write_requirements()]).
#' @return The full path of the reference docx within `style_dir` with the same
#'   name as `out_file`; if such a file doesn't exist, return the default value
#'   `reference_docx` value of [rmarkdown::word_document()].
#' @keywords internal
get_reference_docx <- function(out_file, style_dir) {
  ref <- "default"
  if (!is.null(style_dir)) {
    refdocx <- fs::path_abs(file.path(
      style_dir,
      paste0(tools::file_path_sans_ext(basename(out_file)),
             ".docx")))
    if (file.exists(refdocx)) {
      ref <- refdocx
    }
  }
  return(ref)
}

#' fetch template from package
#' @param template string matching which template you want to render
#' @param type the type of doc you want to render ("package" or "metworx")
#'
#' @export
get_template <- function(
  template = c("validation_plan", "testing_plan", "testing_results", "traceability_matrix", "requirements_specification", "validation_summary_report"),
  type = c("package", "metworx")
){
  template <- match.arg(template)
  type <- match.arg(type)
  template_file <- system.file(
    file.path("templates", type, paste0(template,"_",type,".Rmd")),
    package = "mrgvalidate")
  return(template_file)

}

#' Autofit flextables in word
#'
#' `flextable` by default will make the tables as wide as possible in word. This function will correct the `autofit()` feature and make the contents fit.
#'
#' @param tab a flextable object
#' @param pg_width width (in inches) of word document
#' @param column_shrink character string. If specified, shrink this column before fitting to word document
#'
#' @importFrom flextable flextable_dim width
#' @importFrom checkmate assert_true assert_character
#' @keywords internal
flextable_word <- function(tab, pg_width = 7, column_shrink = NULL){

  tab_out <- tab %>% theme_vanilla() %>% autofit()

  if(!is.null(column_shrink)){
    assert_character(column_shrink)
    assert_true(column_shrink %in% names(tab$body$dataset))
    tab_out <- width(tab_out, glue("{column_shrink}"), width = 2.5)
  }

  tab_out <- width(tab_out, width = dim(tab_out)$widths*pg_width /(flextable_dim(tab_out)$widths))
  return(tab_out)
}
