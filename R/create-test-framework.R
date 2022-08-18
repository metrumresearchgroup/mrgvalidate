
#' Format stories, tests, and other relevant information into usable objects
#'
#' @inheritParams create_metworx_docs
#'
#' @keywords internal
create_test_framework <- function(
  product_name,
  specs,
  auto_test_dir = NULL,
  man_test_dir = NULL,
  type = "package"
){

  if (is.null(auto_test_dir) && is.null(man_test_dir)) {
    abort("Must specify `auto_test_dir` or `man_test_dir`",
          "mrgvalidate_input_error")
  }

  # Merge automatic and manual tests into the same format so that downstream
  # write_* don't need to worry about the distinction.
  results <- vector(mode = "list", length = 2)
  if (!is.null(auto_test_dir)) {
    auto_res <- read_csv_test_results(auto_test_dir)
    results[[1]] <- auto_res$results %>%
      mutate(date = map_chr(.data$result_file, ~ auto_res$info[[.x]]$date)) %>%
      mutate(test_type = "automatic", man_test_content = NA)
    auto_info <- auto_res$info
    rm(auto_res)
  } else {
    # if no auto tests, set info to NULL.
    #   manual tests don't have an info object because info is contained in content
    auto_info <- NULL
  }

  if (!is.null(man_test_dir)) {
    results[[2]] <- read_manual_test_results(man_test_dir) %>%
      mutate(test_type = "manual",
             result_file = basename(man_test_dir),
             # For manual test, being merged into the main line is the
             # indication that it passed, and everything is taken as one
             # "assertion".
             passed = 1L,
             failed = 0L) %>%
      rename(man_test_content = .data$content)
  }

  tests <- check_test_input(bind_rows(results))

  dd <- specs %>%
    unnest(.data$TestIds) %>%
    rename(TestId = .data$TestIds)  %>%
    full_join(tests, by = "TestId")

  # Kludge to support legacy tests/specs without IDs.
  if (all(tests$TestId == tests$TestName)) {
    # TODO: Consider using lifecycle here.
    warning("Automatically generating IDs because test IDs match test names.
This is a temporary kludge to make mrgvalidate >= 1.0 work with old style
issues/tests. For new tests, please assign test IDs. ")
    prefix <- substr(toupper(product_name), 1, min(4, nchar(product_name)))
    ntests <- nrow(tests)
    id_map <- paste0(
      prefix, "-",
      str_pad(1:ntests, nchar(toString(ntests)), pad = "0"))
    names(id_map) <- tests$TestId

    tests <- mutate(tests, TestId = recode(.data$TestId, !!!id_map))
    dd <- mutate(dd, TestId = recode(.data$TestId, !!!id_map))
  }
  # End of kludge.

  dd <- nest(dd,
             tests = c(.data$TestId, .data$TestName,
                       .data$passed, .data$failed, .data$man_test_content,
                       .data$result_file))

  tests <- filter_unlinked_tests(dd, tests)

  return(
    list(
      dd = dd,
      tests = tests,
      auto_info = auto_info
    )
  )
}



#' Check input for missing links between tests, requirements, and stories
#'
#' @param dd Tibble containing stories, requirements, and tests. Created in
#'   [create_test_framework()].
#'
#' @export
check_input <- function(dd){

  missing_data <- find_missing(dd) %>% suppressMessages()
  missing <- purrr::map_lgl(missing_data, ~ nrow(.x) > 0)
  if(any(missing[-1])){
    warning("Required links between tests and/or requirements are missing. Returning missing information. Docs will not be generated")
    return(
      list(
        missing_data = missing_data[missing],
        missing = TRUE
      )
    )
  }else{
    return(
      list(
        missing_data = tibble(),
        missing = FALSE
      )
    )
  }
}


#' Remove unliked tests
#'
#' @details
#' [make_testing_results()] takes the `tests` tibble directly. Drop the test
#' IDs that aren't linked to `specs` because those IDs won't make it into the
#' other docs.
#'
#' @inheritParams check_input
#' @param tests Tibble containing all test results, FORMAT: CREATED ON LINE 59
#'   OF `generate-docs.R` in [create_package_docs()] or [create_metworx_docs()].
#'
#' @keywords internal
filter_unlinked_tests <- function(dd, tests){

  testids_linked <- dd %>% unnest(tests) %>%
    filter(!is.na(.data$StoryId), !is.na(.data$TestId)) %>%
    pull(.data$TestId) %>%
    unique()
  tests_is_linked <- tests$TestId %in% testids_linked
  n_unlinked <- sum(!tests_is_linked)

  if (n_unlinked > 0) {
    warning(glue("Dropping {n_unlinked} test(s) not mentioned in `specs`.
     Call find_tests_without_reqs() with the returned data frame to see them."))
    tests <- tests[tests_is_linked, ]
  }
  return(tests)
}
