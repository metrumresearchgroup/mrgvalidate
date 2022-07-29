
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
  template = c("validation_plan", "testing_plan"),
  type = c("package", "metworx")
){
  template <- match.arg(template)
  type <- match.arg(type)
  template_file <- system.file(
    file.path("templates", type, paste0(template,"_",type,".Rmd")),
    package = "mrgvalidate")
  return(template_file)

}
