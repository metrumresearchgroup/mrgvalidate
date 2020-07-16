
#' Run all tests for specified package, roll up successes and failures, and
#' write to csv file
#' @importFrom dplyr group_by summarize bind_rows
#' @importFrom purrr map_df map
#' @importFrom rlang .data
#' @importFrom fs dir_exists dir_create
#' @param pkg The name of the package you are validating, to be included in the
#'   output document.
#' @param path Package installation path, used with `extra_test_dirs`
#' @param out_file File path to write out the test results to. Any extension
#'   will be ignored and replaced with .csv
#' @param output_dir Directory to write the output documents to. Defaults to
#'   working directory.
#' @param return_df Boolean indicating whether to return the tibble that is
#'   written to `out_file`. Defaults to FALSE and returns nothing.
#' @param extra_test_dirs Character vector of paths (relative to package root
#'   dir) to directories that contain additional tests to run
#' @export
validate_tests <- function(pkg,
                           path = NULL,
                           out_file = ALL_TESTS,
                           output_dir = getwd(),
                           return_df = FALSE,
                           extra_test_dirs = NULL) {
  checkmate::assert_logical(return_df, len = 1L)

  test_list <- run_installed_tests(pkg)

  test_df <- purrr::map_dfr(
    test_list,
    parse_test_output,
    require_context = FALSE
  )

  if (!is.null(extra_test_dirs)) {
    extra_tests_df <-
      extra_test_dirs %>%
      purrr::map(
        ~ run_installed_tests(pkg, path = file.path(path, pkg, .))
      ) %>%
      purrr::flatten() %>%
      purrr::map_dfr(parse_test_output, require_context = FALSE)

    test_df <- bind_rows(test_df, extra_tests_df)
  }

  results <-
    test_df %>%
    dplyr::group_by(file, .data$context, .data$tests) %>%
    dplyr::summarize(
      nb = n(),
      passed = sum(.data$success),
      failed = sum(!.data$success)
    )

  if (sum(results$failed) > 0) {
    warning(glue("`validate_tests(pkg = '{pkg}', root_dir = '{path}')` had {sum(results$failed)} failing tests."))
  }

  if (!is.null(out_file)) {
    if (!fs::dir_exists(output_dir)) fs::dir_create(output_dir)
    out_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(out_file), ".csv"))
    readr::write_csv(results, path = out_file)
  }

  if (return_df) {
    return(results)
  }
}

#' @importFrom tibble tibble
#' @importFrom purrr map_chr map_lgl
parse_test_output <- function(result, require_context = TRUE) {
  if (require_context && is.null(result$context)) {
    stop("no context specified in file: ", result$file)
  }
  out <- tibble::tibble(
    context = dplyr::if_else(is.null(result$context), NA_character_, result$context),
    file = result$file,
    tests = purrr::map_chr(result$results, ~ .x$test),
    success = purrr::map_lgl(result$results, ~ inherits(.x, "expectation_success")),
    res_msg = purrr::map_chr(result$results, ~ paste(class(.x), collapse = ", "))
  )

  if (!all(map_lgl(result$results, ~ inherits(.x, "expectation_success")))) {
    loser_msg <- result$results[[which(!map_lgl(result$results, ~ inherits(.x, "expectation_success")))]]
    print(paste(result$file, "--", result$test, "--\n", paste(loser_msg, collapse = "\n")))
  }
  return(out)
}
