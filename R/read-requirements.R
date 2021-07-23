
#' Read requirements from a Google Sheet.
#' @param ss,sheet Sheet identifiers passed [googlesheets4::read_sheet()].
#' @param title_col,story_col,risk,test_ids_col Names to remap to title, story,
#'   risk, and test_ids columns.
#' @return Tibble with these columns: title, story, risk, and test_ids
#' @export
read_requirements_gsheet <- function
(
  ss, sheet = NULL,
  title_col = "title", story_col = "story",
  test_ids_col = "test_id", risk_col = "risk"
) {
  dd <- googlesheets4::read_sheet(ss = ss, sheet = sheet)
  dd %>%
    rename(title = !!title_col, story = !!story_col,
           test_ids = !!test_ids_col, risk = !!risk_col) %>%
    select(title, story, risk, test_ids) %>%
    mutate(test_ids = stringr::str_split(test_ids, "[, ]+"))
}
