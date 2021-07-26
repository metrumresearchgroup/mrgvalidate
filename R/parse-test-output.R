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
                   # TODO: Collapse these into one variable (e.g., "result")?
                   success = all(map_lgl(.r$results, ~ inherits(.x, "expectation_success"))),
                   fail = any(map_lgl(.r$results, ~ inherits(.x, "expectation_failure"))),
                   skip = any(map_lgl(.r$results, ~ inherits(.x, "expectation_skip")))
    ) %>%
      mutate(
        # TODO: Call this test_id for consistency with requirements input?
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
  #
  # TODO: the modifier matches the current spec but is going to cause problems
  # with metworxinspector's test_config_pkgs(). That programatically tacks on
  # package names, which may include characters (especially [.-_]).
  str_match(test_name, "\\[([A-Z]+-[A-Z]+-[0-9]+(?:-[A-Za-z0-9]+)?)\\]") %>%
    dplyr::nth(2)
}
