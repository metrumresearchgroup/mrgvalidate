
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

  # make_testing_results() takes the `tests` tibble directly. Drop the test
  # IDs that aren't linked to `specs` because those IDs won't make it into the
  # other docs.
  testids_linked <- dd %>%
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

  if(any(is.na(dd$TestName)) | any(is.na(dd$passed))){
    missing_tests <- dd %>% filter(is.na(dd$TestName) | is.na(dd$passed))
    warning(glue("Dropping {nrow(missing_tests)} test(s) referenced in requirements, but not found in test outputs: {paste(missing_tests$TestId, collapse = ', ')}"))
    dd <- dd %>% filter(!is.na(.data$passed))
  }


  dd <- nest(dd,
             tests = c(.data$TestId, .data$TestName,
                       .data$passed, .data$failed, .data$man_test_content,
                       .data$result_file))


  return(
    list(
      dd = dd,
      tests = tests,
      auto_info = auto_info
    )
  )
}
