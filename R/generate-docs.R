#' Create validation docs
#'
#' This function is the main entry point for creating validation docs.
#' @param specs tibble of requirements in the format returned by
#'   [read_spec_gsheets()].
#' @param test_output_dir path to direction containing a test output files. See
#'   [input-formats].
#' @param output_dir Directory to write the output documents to. Defaults to
#'   working directory.
#' @importFrom dplyr full_join rename
#' @importFrom tidyr unnest
#' @export
create_validation_docs <- function
(
  requirements, test_output_dir, output_dir = getwd()
) {

  req_flat <- requirements %>%
    unnest(TestIds) %>%
    rename(TestId = TestIds)

  tres <- read_csv_test_results(test_output_dir)

  # TODO: Change something upstream to make test_tag/TestId consistent.
  dd <- full_join(tres$results, req_flat,
                  suffix = c("", ".requirements"),
                  by = c("test_tag" = "TestId")) %>%
    nest(tests = c(result_file, test_name, passed, failed, test_tag))

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


