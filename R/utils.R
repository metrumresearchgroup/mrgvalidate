
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
    if(is.null(reqs)) "" else c("**Requirements**\n", reqs, "\n\n"),
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


#' Format an info JSON suite element for printing
#' @importFrom purrr map
#' @importFrom glue glue
#' @keywords internal
print_info_list <- function(.l) {
  if (is.null(.l)) return("")

  map(names(.l), ~ {
    .v <- .l[[.x]]
    if (inherits(.v, "list")) {
      return(print_info_list(.v)) # recursive y'all
    }

    if (length(.v) > 1) {
      .v <- paste(.v, collapse = "; ")
    }

    return(glue("**{.x}:** {.v}"))
  }) %>%
    unlist() %>%
    paste(collapse = "\n\n")
}


#' fetch template from package
#' @param template string matching which template you want to render
#' @param type the type of doc you want to render ("package" or "metworx")
#'
#' @export
get_template <- function(
  template = c("validation_plan", "testing_plan", "testing_results", "traceability_matrix",
               "requirements_specification", "validation_summary", "release_notes"),
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

  tab_out <- tab %>% as.data.frame() %>% flextable() %>%
    theme_vanilla() %>% autofit()

  if(!is.null(column_shrink)){
    assert_character(column_shrink)
    assert_true(column_shrink %in% names(tab))
    tab_out <- width(tab_out, glue("{column_shrink}"), width = 2.5)
  }

  tab_out <- width(tab_out, width = dim(tab_out)$widths*pg_width /(flextable_dim(tab_out)$widths))
  return(tab_out)
}

#' Make signature page
#'
#' @details
#' The template word docs were modified so that Heading 9 has a bottom border, which functions as a custom signature line
#'
#' @keywords internal
make_signature_line <- function(){
  sig_str <- glue('

<br>

## Signature Page:

<br>

<br>

<br>

#########
**Authored by:**

<br>

<br>

<br>

<br>

<br>

#########
**Reviewed by:**

<br>

<br>

<br>

<br>

<br>

#########
**Quality Assurance Approved by:**
')
  cat(sig_str)
}


#' Extract bugs section from release notes character vector
#' @param notes_lines release notes character vector
#' @keywords internal
extract_bug_section <- function(notes_lines) {
  # find beginning of bugs section
  bug_line <- which(stringr::str_detect(notes_lines, stringr::regex("^#+.+[Bb]ug.+", multiline = TRUE)))
  if (length(bug_line) == 0) {
    return("No bugs addressed in this release.")
  } else if (length(bug_line) > 1) {
    warning(paste(
      glue::glue("Found multiple potential Bugs sections in {release_notes_file}:"),
      paste(notes_lines[bug_line], collapse = "\n"),
      "Using first section and ignoring subsequent sections.",
      sep = "\n"
    ))
  }

  # find end of bugs section
  bug_heading <- unlist(stringr::str_split(notes_lines[bug_line], "\\b"))[1] %>%
    stringr::str_trim()

  end_of_bugs <- NULL
  for (.i in (bug_line + 1):length(notes_lines)) {
    if (grepl(bug_heading, notes_lines[.i])) end_of_bugs <- .i-1
  }
  if (is.null(end_of_bugs)) end_of_bugs <- length(notes_lines)

  return(notes_lines[bug_line:end_of_bugs])
}

#' Rename template
#'
#' @param output_dir directory for `out_file` to be copied to
#' @param out_file name of file, including extension. Note: this is not a file path.
#' @param append character string to append to out_file. Will be separated by '-'.
#'
#' @keywords internal
format_rmd_name <- function(output_dir, out_file, append = NULL){
  if (!fs::dir_exists(output_dir)) fs::dir_create(output_dir)

  if(!is.null(append)){
    assert_character(append)
    out_file <- paste0(tools::file_path_sans_ext(out_file),"-",append, ".Rmd")
  }

  out_file <- file.path(output_dir, out_file)

  return(out_file)
}


#' Delete copied RMD's used to render word documents
#'
#' @param output_dir directory where word documents will be generated
#' @param file_names file names of every validation plan
#' @param append package or 'metworx' appended to file name
#'
#' @keywords internal
cleanup_rmds <- function(output_dir,
                         file_names = c(VAL_PLAN_FILE, TEST_PLAN_FILE, TEST_RESULTS_FILE,
                                        MAT_FILE, REQ_FILE, VAL_SUM_FILE, RLS_NOTES_FILE),
                         append = NULL
){
  for(i in seq_along(file_names)){
    file.i <- format_rmd_name(output_dir, file_names[i], append)
    if(fs::file_exists(file.i)) fs::file_delete(file.i)
  }

}
