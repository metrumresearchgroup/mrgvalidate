
#' Check input test data.
#'
#' * Warn about rows without test IDs and filter them out.
#' * Abort if non-NA IDs are not unique across rows.
#'
#' @param tests Tibble with TestId column
#' @return Tibble with NA tests IDs removed.
#' @seealso [create_validation_docs()], [input_formats]
#' @importFrom dplyr filter
#' @importFrom rlang .data
check_test_input <- function(tests) {
  na_test_ids <- sum(is.na(tests$TestId))
  if (na_test_ids > 0) {
    warning(glue("Dropping {na_test_ids} test(s) with no ID"))
    tests <- filter(tests, !is.na(.data$TestId))
  }

  dups <- tests$TestId[duplicated(tests$TestId)]
  if (length(dups) > 0) {
    abort(sprintf("Test input has test IDs repeated across rows: %s",
                  paste0(dups, collapse = ", ")),
          "mrgvalidate_input_error")
  }
  return(tests)
}

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
#' `find_missing()` is a convenience wrapper around all of the above functions.
#' It returns their results in a list and also displays messages about the
#' results.
#'
#' @details
#'
#' The requirement columns (RequirementId and RequirementDescription) are
#' optional (see [input_formats]). If not present, `find_tests_without_reqs()`
#' and `find_reqs_with_missing_tests()` use stories in place of requirements.
#' `find_reqs_without_stories()`, on the other hand, errors if the columns
#' aren't present because it can't do anything sensible without them.
#'
#' @param merged_inputs Tibble with stories, requirements and tests, as returned
#'   by [create_validation_docs()].
#' @return Tibble with the missing items, or, in the case of `find_missing()`, a
#'   list of tibbles.
#' @seealso [create_validation_docs()], [input_formats]
#' @importFrom purrr map_int
#' @importFrom tibble tibble
#' @export
find_missing <- function(merged_inputs) {
  if (has_req_cols(merged_inputs)) {
    req_no_stories <- find_reqs_without_stories(merged_inputs)
  } else {
    req_no_stories <- tibble()
  }

  res <- list(
    find_tests_without_reqs = find_tests_without_reqs(merged_inputs),
    find_reqs_with_missing_tests = find_reqs_with_missing_tests(merged_inputs),
    find_reqs_without_stories = req_no_stories)

  n_flagged  <- sum(purrr::map_int(res, nrow))
  if (n_flagged == 0) {
    message("No missing pieces found")
  } else {
    message(glue("{n_flagged} missing piece(s) found. Check results"))
  }
  return(res)
}

#' @rdname find_missing
#' @importFrom dplyr arrange filter select
#' @importFrom rlang .data
#' @importFrom tidyr unnest
#' @export
find_tests_without_reqs <- function(merged_inputs) {
  if (has_req_cols(merged_inputs)) {
    id_col <- "RequirementId"
  } else {
    id_col <- "StoryId"
  }

  merged_inputs %>%
    unnest(.data$tests) %>%
    filter(!is.na(.data$TestId)) %>%
    filter(is.na((.data[[id_col]]))) %>%
    select(.data$TestId, .data$TestName) %>%
    arrange(.data$TestId)
}

#' @rdname find_missing
#' @importFrom dplyr arrange filter select
#' @importFrom rlang .data
#' @importFrom tidyr unnest
#' @export
find_reqs_with_missing_tests <- function(merged_inputs) {
  if (has_req_cols(merged_inputs)) {
    id_col <- "RequirementId"
    desc_col <- "RequirementDescription"
  } else {
    id_col <- "StoryId"
    desc_col <- "StoryDescription"
  }

  merged_inputs %>%
    unnest(.data$tests) %>%
    # Checking that just TestId is NA isn't sufficient because TestId comes from
    # the join of the requirements and test results.
    filter(is.na(.data$passed)) %>%
    filter(!is.na(.data$TestId)) %>%
    select(.data[[id_col]], .data[[desc_col]], .data$TestId) %>%
    arrange(.data[[id_col]], .data$TestId)
}

#' @rdname find_missing
#' @importFrom dplyr arrange filter select
#' @importFrom rlang .data
#' @export
find_reqs_without_stories <- function(merged_inputs) {
  if (!has_req_cols(merged_inputs)) {
    abort(
      "merged_inputs lacks RequirementId and RequirementDescription columns",
      "mrgvalidate_input_error")
  }

  merged_inputs %>%
    filter(!is.na(.data$RequirementId)) %>%
    filter(is.na(.data$StoryId)) %>%
    select(.data$RequirementId, .data$RequirementDescription) %>%
    arrange(.data$RequirementId)
}

#' Return TRUE if `merged_inputs` has requirement columns.
#'
#' @keywords internal
has_req_cols <- function(merged_inputs) {
  req_cols <- c("RequirementId", "RequirementDescription")
  return(all(req_cols %in% names(merged_inputs)))
}
