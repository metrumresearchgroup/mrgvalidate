
#' Read requirements and stories from Google Sheets.
#'
#' @details
#' The stories sheet must have the following columns:
#' * StoryId (character scalar)
#' * StoryName (character scalar)
#' * StoryDescription (character scalar)
#' * ProductRisk (character scalar)
#' * RequirementIds (character vector)
#'
#' The requirements sheet must have the following columns:
#' * RequirementId (character scalar)
#' * RequirementDescription (character scalar)
#' * TestIds (character vector)
#'
#' @param ss_stories,ss_req,sheet_stories,sheet_req Sheet identifiers for the
#'   stories and requirements passed along as the `ss` and `sheet` arguments to
#'   [googlesheets4::read_sheet()].
#' @return Tibble joining requirements and stories by RequirementId.
#' @export
read_spec_gsheets <- function
(
  ss_stories, ss_req,
  sheet_stories = NULL, sheet_req = NULL
) {
  stories <- read_stories_gsheet(ss = ss_stories, sheet = sheet_stories)
  reqs <- read_requirements_gsheet(ss = ss_req, sheet = sheet_req)
  return(merge_requirements_and_stories(stories, reqs))
}

#' Join the stories and requirements by requirement ID.
#' @param stories Data frame of stories, where the RequirementIds column has a
#'   list of requirement IDs associated with each story.
#' @param reqs Data frame of requirements, with each row identified by a unique
#'   requirement ID.
#' @importFrom dplyr full_join
#' @importFrom tidyr unnest
#' @keywords internal
merge_requirements_and_stories <- function(stories, reqs) {
  stories_flat <- stories %>%
    unnest("RequirementIds") %>%
    rename(RequirementId = RequirementIds)
  return(full_join(stories_flat, reqs, by = "RequirementId"))
}

#' Read requirements from a Google Sheet.
#' @param ss,sheet Sheet identifiers passed [googlesheets4::read_sheet()].
#' @param req_id_col,req_description_col,test_ids_col Names to remap to
#'   RequirementId, RequirementDescription, and TestIds columns.
#' @return Tibble with the above columns.
#' @importFrom dplyr rename select mutate
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
#' @importFrom dplyr rename select mutate
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
