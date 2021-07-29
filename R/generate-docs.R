#' Create validation docs
#'
#' This function is the main entry point for creating validation docs.
#' @param product_name The product being validated.
#' @param version The version number of the product.
#' @param specs tibble of requirements in the format returned by
#'   [read_spec_gsheets()].
#' @param auto_test_dir,man_test_dir path to directories containing automatic
#'   and manual test output files. See [input_formats].
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
  auto_test_dir = NULL, man_test_dir = NULL,
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
      mutate(test_type = "automatic", man_test_content = NA) %>%
      # TODO: Change something upstream to make test_tag/TestId consistent.
      rename(TestId = .data$test_tag)
  }

  if (!is.null(man_test_dir)) {
    results[[2]] <- read_manual_test_results(man_test_dir) %>%
      mutate(test_type = "manual",
             result_file = NA,
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
    nest(tests = c(.data$TestId, .data$test_name,
                   .data$passed, .data$failed, .data$man_test_content,
                   .data$result_file))

  write_requirements(
    dd,
    product_name,
    version,
    out_file = REQ_FILE,
    output_dir = output_dir,
    word_document = TRUE
  )

  return(dd)
}

#' Wrapper to generate all three documents, using defaults for output paths
#' @importFrom purrr map walk
#' @importFrom dplyr slice
#' @importFrom glue glue
#' @importFrom rmarkdown render
#' @param org Github organization that the repo is under
#' @param repo The name of the repo for the package you are validating
#' @param milestone The name of the milestone associated with the release you are validating. All issues tied to this milestone with be pulled.
#' @param version The version number of the package you are validating. This must correspond to a tag in the repo.
#' @param domain Domain where repo lives. Either "github.com" or "ghe.metrumrg.com", defaulting to "github.com"
#' @param extra_test_dirs Character vector of paths (relative to package root dir) to directories that contain additional tests to run
#' @param output_dir Directory to write the output documents to. Defaults to working directory.
#' @export
generate_docs <- function(
  org,
  repo,
  milestone,
  version,
  domain = VALID_DOMAINS,
  extra_test_dirs = NULL,
  output_dir = getwd()
) {

  domain <- match.arg(domain)

  # run tests and write validation doc
  write_validation_testing(
    org,
    repo,
    version,
    domain = domain,
    extra_test_dirs = extra_test_dirs,
    out_file = VAL_FILE,
    output_dir = output_dir,
    word_document = TRUE,
    dry_run = FALSE
  )

  # get issues from Github
  release_issues <- get_issues(org, repo, milestone, domain = domain)
  df <- process_stories(
    release_issues,
    org,
    repo,
    domain = domain,
    test_path = file.path(output_dir, ALL_TESTS),
    add_risk = TRUE
  )

  # write other two docs from issues df
  write_requirements(
    df,
    repo,
    version,
    out_file = REQ_FILE,
    output_dir = output_dir,
    word_document = TRUE
  )

  write_traceability_matrix(
    df,
    repo,
    version,
    out_file = MAT_FILE,
    output_dir = output_dir,
    word_document = TRUE
  )

}


