
#' Read requirements from a Google Sheet.
#' @param ss,sheet Sheet identifiers passed [googlesheets4::read_sheet()].
#' @param req_id_col,req_description_col,test_ids_col Names to remap to
#'   requirement ID, description, and test IDs.
#' @return Tibble with these columns: req_id, req_description, and test_ids
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
    rename(req_id = !!req_id_col, req_description = !!req_description_col,
           test_ids = !!test_ids_col) %>%
    select(req_id, req_description, test_ids) %>%
    mutate(test_ids = stringr::str_split(test_ids, "[, ]+"))
}
