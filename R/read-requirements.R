
#' Read requirements from a Google Sheet.
#' @param ss,sheet Sheet identifiers passed [googlesheets4::read_sheet()].
#' @param req_id_col,req_description_col,test_ids_col Names to remap to
#'   requirement ID, description, and test IDs.
#' @return Tibble with these columns: RequirementId, RequirementDescription, and
#'   TestIds
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
