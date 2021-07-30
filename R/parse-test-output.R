#' Parse testthat output
#'
#' @param result List output as reported by [testthat::ListReporter]
#' @return A tibble formatted according to [input_formats]
#' @seealso [input_formats], [create_validation_docs()]
#' @importFrom purrr map_chr map_lgl map_dfr
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace fixed
#' @importFrom rlang .data
#' @export
parse_testthat_list_reporter <- function(result) {
  map_dfr(result, function(.r) {
    .t <- unique(map_chr(.r$results, ~ .x$test))
    if (length(.t) > 1) {
      abort(paste("DEV ERROR: parsed more than one test name from results:", paste(.t, collapse = ", ")))
    }
    tibble::tibble(TestName = .t,
                   passed = sum(map_lgl(.r$results, ~ inherits(.x, "expectation_success"))),
                   failed = sum(map_lgl(.r$results, ~ inherits(.x,
                                                               c("expectation_failure",
                                                                 "expectation_error",
                                                                 "expectation_skip"))))
    ) %>%
      mutate(
        TestId = parse_test_id(.data$TestName),
        TestName = strip_test_id(.data$TestName, .data$TestId)
      )
  })
}

#' Extract test ID from a string.
#' @importFrom stringr str_match
parse_test_id <- function(string) {
  str_match(string, "\\[([A-Z]+-[A-Z]+-[0-9]+)\\]") %>%
    dplyr::nth(2)
}


#' Return a string without the embedded test ID.
#'
#' Note that any white space in the string will be collapsed to a single
#' character.
#' @importFrom stringr regex str_replace str_squish
strip_test_id <- function(string, id) {
  string %>%
    str_replace(fixed(paste0("[", id, "]")), "") %>%
    str_squish
}
