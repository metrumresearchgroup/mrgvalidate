
#' Run all tests for specified package, roll up successes and failures, and write to csv file
#' @importFrom dplyr group_by summarize bind_rows
#' @importFrom purrr map_df map
#' @importFrom rlang .data
#' @importFrom fs dir_exists dir_create
#' @param pkg The name of the package you are validating, to be included in the output document.
#' @param root_dir The directory path to where the package has been cloned. `file.path(root_dir, pkg)` should lead to the cloned repo that will be tested.
#' @param out_file File path to write out the test results to. Any extension will be ignored and replaced with .csv
#' @param output_dir Directory to write the output documents to. Defaults to working directory.
#' @param return_df Boolean indicating whether to return the tibble that is written to `out_file`. Defaults to FALSE and returns nothing.
#' @param extra_test_dirs Character vector of paths (relative to package root dir) to directories that contain additional tests to run
#' @export
validate_tests <- function(
  pkg,
  root_dir = tempdir(),
  out_file = ALL_TESTS,
  output_dir = getwd(),
  return_df = FALSE,
  extra_test_dirs = NULL
) {


  test_df <- purrr::map_df(test_list, parse_test_output)
  test_list <- run_installed_tests(pkg)

  if (!is.null(extra_test_dirs)) {
    extra_df_list <- map(extra_test_dirs, function(.t) {
      return(purrr::map_df(.tl, parse_test_output))
    })

    test_df <- bind_rows(test_df, extra_df_list)
        ~ run_installed_tests(pkg, path = file.path(path, pkg, .))
  }

  results <- test_df %>% group_by(file, .data$context, .data$tests) %>%
    summarize(nb = n(),
              passed = sum(.data$success),
              failed = sum(!.data$success)
    )

  if (sum(results$failed) > 0) {
    warning(glue("`validate_tests(pkg = '{pkg}', root_dir = '{root_dir}')` had {sum(results$failed)} failing tests."))
  }

  if (!is.null(out_file)) {
    if (!fs::dir_exists(output_dir)) fs::dir_create(output_dir)
    out_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(out_file), ".csv"))
    readr::write_csv(results, path=out_file)
  }

  ##########
  # print(results[results$failed > 0, c("file", "tests", "failed")])
  ##########

  if (isTRUE(return_df)) {
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
