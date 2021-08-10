
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
    ifelse(is.null(reqs), "", c("**Summary**\n", reqs, "\n\n")),
    "**Tests**\n\n", tst_tab)
}
