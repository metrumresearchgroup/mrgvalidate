#' Create validation docs
#'
#' This function is the main entry point for creating validation docs.
#' @param product_name The product being validated.
#' @param version The version number of the product.
#' @param specs tibble of stories and requirements. See [input_formats].
#' @param auto_test_dir,man_test_dir path to directories containing automatic
#'   and manual test output files. See [input_formats].
#' @param roles A data frame of user roles that, if specified, is inserted into
#'   the requirements document.
#' @param output_dir Directory to write the output documents to. Defaults to
#'   working directory.
#' @importFrom dplyr bind_rows full_join mutate rename
#' @importFrom purrr map_chr
#' @importFrom tidyr nest unnest
#' @importFrom rlang .data
#' @export
create_validation_docs <- function
(
  product_name, version, specs,
  auto_test_dir = NULL, man_test_dir = NULL, roles = NULL,
  output_dir = getwd()
) {

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

  tests <- bind_rows(results)
  dd <- specs %>%
    unnest(.data$TestIds) %>%
    rename(TestId = .data$TestIds)  %>%
    full_join(tests, by = "TestId") %>%
    nest(tests = c(.data$TestId, .data$TestName,
                   .data$passed, .data$failed, .data$man_test_content,
                   .data$result_file))

  write_requirements(
    dd,
    product_name,
    version,
    roles = roles,
    out_file = REQ_FILE,
    output_dir = output_dir,
    word_document = TRUE
  )

  write_traceability_matrix(
    dd,
    product_name,
    version,
    out_file = MAT_FILE,
    output_dir = output_dir,
    word_document = TRUE
  )

  write_validation_testing(
    product_name,
    version,
    tests,
    auto_info,
    out_file = VAL_FILE,
    output_dir = output_dir,
    word_document = TRUE
  )

  return(invisible(dd))
}
