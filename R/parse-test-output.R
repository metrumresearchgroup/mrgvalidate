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
        # TODO: It probably makes sense to replace flanking spaces here too.
        TestName = str_replace(.data$TestName,
                                fixed(paste0("[", .data$TestId, "]")),
                                "")
      )
  })
}

#' @importFrom stringr str_match
parse_test_id <- function(string) {
  # TODO: we should probably unit test this
  # * the weird Julia case
  # * the nodejs case
  str_match(string, "\\[([A-Z]+-[A-Z]+-[0-9]+(?:-[A-Za-z_0-9]+)?)\\]") %>%
    dplyr::nth(2)
}
