#' @importFrom purrr map_chr map_lgl map_dfr
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace fixed
#' @export
parse_testthat_list_reporter <- function(result) {
  map_dfr(result, function(.r) {
    .t <- unique(map_chr(.r$results, ~ .x$test))
    if (length(.t) > 1) {
      abort(paste("DEV ERROR: parsed more than one test name from results:", paste(.t, collapse = ", ")))
    }
    tibble::tibble(test_name = .t,
                   passed = sum(map_lgl(.r$results, ~ inherits(.x, "expectation_success"))),
                   failed = sum(map_lgl(.r$results, ~ inherits(.x,
                                                               c("expectation_failure",
                                                                 "expectation_error",
                                                                 "expectation_skip"))))
    ) %>%
      mutate(
        # TODO: Call this TestId for consistency with requirements input?
        test_tag = parse_test_tag(test_name),
        # TODO: It probably makes sense to replace flanking spaces here too.
        test_name = str_replace(test_name, fixed(paste0("[", test_tag, "]")), "")
      )
  })
}

#' @importFrom stringr str_match
parse_test_tag <- function(test_name) {
  # TODO: we should probably unit test this
  # * the weird Julia case
  # * the nodejs case
  str_match(test_name, "\\[([A-Z]+-[A-Z]+-[0-9]+(?:-[A-Za-z_0-9]+)?)\\]") %>%
    dplyr::nth(2)
}
