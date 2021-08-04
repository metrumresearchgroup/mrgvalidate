
#' Find missing input pieces.
#'
#' @description
#'
#' `find_tests_without_reqs()` returns IDs from the test results that are not
#' associated with a requirement. `find_reqs_with_missing_tests()` goes in the
#' reverse direction, returning requirements with test IDs that aren't found in
#' the test results. `find_reqs_without_stories()` returns requirements that are
#' not linked to a story.
#'
#' `check_inputs()` is a convenience wrapper around all of the above functions.
#' It returns their results in a list and also displays messages about the
#' results.
#'
#' @param merged_inputs Tibble with stories, requirements and tests, as returned
#'   by [create_validation_docs()].
#' @return Tibble with the missing items, or, in the case of `check_inputs()`, a
#'   list of tibbles.
#' @seealso [create_validation_docs()], [input_formats]
#' @importFrom purrr map_int
check_inputs <- function(merged_inputs) {
  res <- list(
    find_tests_without_reqs = find_tests_without_reqs(merged_inputs),
    find_reqs_with_missing_tests = find_reqs_with_missing_tests(merged_inputs),
    find_reqs_without_stories = find_reqs_without_stories(merged_inputs))

  n_flagged  <- sum(purrr::map_int(res, nrow))
  if (n_flagged == 0) {
    message("No missing pieces found")
  } else {
    message(glue("{n_flagged} missing piece(s) found. Check results"))
  }
  return(res)
}

#' @rdname check_inputs
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

#' @rdname check_inputs
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

#' @rdname check_inputs
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
