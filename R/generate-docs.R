#' Create validation docs
#'
#' This function is the main entry point for creating validation docs.
#' @param requirements tibble of requirements in the format returned by
#'   [read_requirements_github()] or [read_requirements_gsheet()]. It must have
#'   the following columns: title, story, risk, and test_ids.
#' @param test_output_dir path to a directory containing appropriately formatted
#'   test output files, such the output produced by writing
#'   [parse_testthat_list_reporter()] to a CSV. All CSV files in the directory
#'   will be combined to construct the results.
#' @param output_dir Directory to write the output documents to. Defaults to
#'   working directory.
#' @importFrom dplyr full_join rename
#' @importFrom purrr map reduce
#' @importFrom readr read_csv
#' @importFrom stringr str_replace fixed
#' @importFrom tibble add_column
#' @importFrom tidyr unnest
#' @export
create_validation_docs <- function
(
  requirements, test_output_dir, output_dir = getwd()
) {

  req_flat <- requirements %>%
    unnest(test_ids) %>%
    rename(test_id = test_ids)

  test_results <- map(
    list.files(test_output_dir, pattern = "\\.csv$", full.names = TRUE),
    ~{
      read_csv(.x, show_col_types = FALSE) %>%
        add_column(
          result_file = str_replace(basename(.x), fixed(".csv"), ""),
          .before = TRUE)
    }) %>%
    reduce(rbind)

  # TODO: Change something upstream to make test_tag/test_id consistent.
  dd <- full_join(test_results, req_flat, by = c("test_tag" = "test_id"))

  # TODO: call write_* functions. They need to be adjusted.

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


