
#' Find missing input pieces.
#'
#' find_tests_without_reqs()` returns IDs from the test results that are not
#' associated with a requirement. `find_reqs_with_missing_tests()` goes in the
#' reverse direction, returning requirements with test IDs that aren't found in
#' the test results. `find_reqs_without_stories()` returns requirements that are
#' not linked to a story.
#'
#' @param merged_inputs Tibble with stories, requirements and tests, as returned
#'   by [create_validation_docs()].
#' @return Tibble with the missing items.
#' @seealso [create_validation_docs()], [input_formats]
#' @importFrom dplyr arrange filter select
#' @importFrom rlang .data
#' @importFrom tidyr unnest
#' @export
find_tests_without_reqs <- function(merged_inputs) {
  merged_inputs %>%
    unnest(.data$tests) %>%
    filter(!is.na(.data$TestId)) %>%
    filter(is.na((.data$RequirementId))) %>%
    select(.data$TestId, .data$TestName) %>%
    arrange(.data$TestId)
}

#' @rdname find_tests_without_reqs
#' @importFrom dplyr arrange filter select
#' @importFrom rlang .data
#' @importFrom tidyr unnest
#' @export
find_reqs_with_missing_tests <- function(merged_inputs) {
  merged_inputs %>%
    unnest(.data$tests) %>%
    # Checking that just TestId is NA isn't sufficient because TestId comes from
    # the join of the requirements and test results.
    filter(is.na(.data$passed)) %>%
    filter(!is.na(.data$TestId)) %>%
    select(.data$RequirementId, .data$RequirementDescription, .data$TestId) %>%
    arrange(.data$RequirementId, .data$TestId)
}

#' @rdname find_tests_without_reqs
#' @importFrom dplyr arrange filter select
#' @importFrom rlang .data
#' @export
find_reqs_without_stories <- function(merged_inputs) {
  merged_inputs %>%
    filter(!is.na(.data$RequirementId)) %>%
    filter(is.na(.data$StoryId)) %>%
    select(.data$RequirementId, .data$RequirementDescription) %>%
    arrange(.data$RequirementId)
}
