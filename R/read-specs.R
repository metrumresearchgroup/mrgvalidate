
#' Read requirements from a Google Sheet.
#' @param ss,sheet Sheet identifiers passed [googlesheets4::read_sheet()].
#' @param req_id_col,req_description_col,test_ids_col Names to remap to
#'   RequirementId, RequirementDescription, and TestIds columns.
#' @return Tibble with the above columns.
#' @export
read_requirements_gsheet <- function
(
  ss, sheet = NULL,
  req_id_col = "RequirementId",
  req_description_col = "RequirementDescription",
  test_ids_col = "TestIds"
) {
  dd <- googlesheets4::read_sheet(ss = ss, sheet = sheet)
  dd %>%
    rename(RequirementId = !!req_id_col,
           RequirementDescription = !!req_description_col,
           TestIds = !!test_ids_col) %>%
    select(RequirementId, RequirementDescription, TestIds) %>%
    mutate(TestIds = stringr::str_split(TestIds, "[, ]+"))
}


#' Read stories from a Google Sheet.
#' @param ss,sheet Sheet identifiers passed [googlesheets4::read_sheet()].
#' @param story_id_col,story_name_col,story_description_col,risk_col,req_ids_col
#'   Names to remap to StoryId, StoryName, StoryDescription, ProductRisk, and
#'   RequirementIds columns.
#' @return Tibble with the above columns.
#' @export
read_stories_gsheet <- function
(
  ss, sheet = NULL,
  story_id_col = "StoryId",
  story_name_col = "StoryName",
  story_description_col = "StoryDescription",
  risk_col = "ProductRisk",
  req_ids_col = "RequirementIds"
) {
  dd <- googlesheets4::read_sheet(ss = ss, sheet = sheet)
  dd %>%
    rename(StoryId = !!story_id_col,
           StoryName = !!story_name_col,
           StoryDescription = !!story_description_col,
           ProductRisk = !!risk_col,
           RequirementIds = !!req_ids_col) %>%
    select(StoryId, StoryName, StoryDescription,
           ProductRisk, RequirementIds) %>%
    mutate(RequirementIds = stringr::str_split(RequirementIds, "[, ]+"))
}
