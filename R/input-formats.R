#' mrgvalidate test result input
#'
#' In order to format validation docs, mrgvalidate must collect various pieces
#' of information from the caller. This information can be broken down into two
#' categories: information about the requirements and information about the
#' executed tests. Below is a description of how information about the executed
#' tests is expected to be formatted.
#'
#' @details
#'
#' For automated tests, a directory should contain set of CSVs with the test
#' results. A CSV file must have the following columns:
#'
#' * test_name: a description of the test
#'
#' * passed, failed: a count of passed and failed assertions for the test
#'
#' * test_tag: a unique identifier of the test. This field is used to map tests
#' to requirements.
#'
#' For `testthat` tests, the [parse_testthat_list_reporter()] helper can be used
#' to convert [testthat::ListReporter] results into the above format.
#'
#' Alongside each CSV, there must be a JSON file with the same base name. This
#' file includes information about the run itself (e.g., the date it was
#' executed and system details). At the minimum, it must have a "date" field.
#' Any key-value pairs that are specified under the top-level "info" field will
#' be rendered in the validation docs.
#'
#' ```
#' {
#'     "date": "2021-07-26 12:20:10 EDT",
#'     "info":
#'     {
#'         "commit": "78f70b9297a7f8b0f2ec2a18d17a9cc6722359c8"
#'     }
#' }
#' ```
#'
#' @name input-formats
NULL

# TODO: Document requirements format here too?
# TODO: Explain why there are multiple CSVs.
# TODO: Manual tests.