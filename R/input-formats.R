#' mrgvalidate test result input
#'
#' In order to format validation docs, mrgvalidate must collect various pieces
#' of information from the caller. This information can be broken down into two
#' categories: information about the requirements and information about the
#' executed tests. Below is a description of how this data is expected to be
#' formatted. **Note: there a companion package `mrgvalprep` containing
#' functions to preprocess common inputs (R's `testthat`, Googlesheets, etc.)
#' into the format required by `mrgvalidate`.
#'
#' @details
#'
#' ## Stories and requirements
#'
#' A tibble or data.frame containing the stories, and optionally requirements,
#' for the software being validated. Will be passed to `specs` argument of
#' [create_package_docs()] or [create_metworx_docs()]. Must have the following columns:
#'
#' * StoryId: unique identifier for the story
#'
#' * StoryName: short name of story (ideally 5-8 words max, but no hard limit)
#'
#' * StoryDescription: body of the story ("As a user,..."). This will be dumped
#' as is into the requirements document. The text up to the first new line will
#' be used the value of the "User Story" column in the traceability matrix.
#'
#' * ProductRisk: specify the risk to the product of this story (typically
#' "Low", "Medium", "High", etc.)
#'
#' * RequirementId (Optional): unique identifier of technical requirement
#' associated with story. If included, there can be >1 requirement per story,
#' each with its own row in the table, in which case the first four columns will
#' be repeated for each requirement.
#'
#' * RequirementDescription (Optional): plain text description of the
#' requirement
#'
#' * TestIds: unique identifiers for tests that validate this requirement or
#' story. This field is used to map stories/requirements to tests.
#'
#' The `mrgvalprep` packages has several helpers for importing stories into this
#' format. See `mrgvalprep::read_spec_gsheets()` and
#' `mrgvalprep::parse_github_issues()` for examples.
#'
#' ## Automated tests
#'
#' For automated tests, a directory containing any number of CSVs with the test
#' results. The path will be passed to `auto_test_dir` argument of
#' [create_package_docs()] or [create_metworx_docs()]. A CSV file must have the following columns:
#'
#' * TestName: a description of the test
#'
#' * passed, failed: a count of passed and failed assertions for the test
#'
#' * TestId: a unique identifier of the test. This field is used to map tests
#' to stories/requirements.
#'
#' For `testthat` tests, the `mrgvalprep::parse_testthat_list_reporter()` helper can be used
#' to convert [testthat::ListReporter] results into the above format.
#'
#' Note that if TestName and TestId columns are identical,
#' [create_package_docs()] / [create_metworx_docs()] will auto-generate IDs. This is a temporary kludge
#' to support legacy tests and GitHub issues that don't use test IDs.
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
#' The `mrgvalprep::get_sys_info()` helper can be used to capture various system details and
#' write them to a JSON file.
#'
#' ## Manual tests
#'
#' For manual tests, a directory containing test subdirectories named by
#' test ID, one subdirectory per manual test. For example:
#'
#' ```
#' |-- MAN-ACC-001
#' |   |-- assets_MAN-ACC-001
#' |   |   `-- SSH-Access-1.png
#' |   `-- test.md
#' |-- MAN-ACC-002
#' |   |-- assets_MAN-ACC-002
#' |   |   |-- Guacamole-UI-1.png
#' |   |   `-- Guacamole-UI-2.png
#' |   `-- test.md
#' ```
#'
#' Each subdirectory must adhere to the following:
#'
#' * The subdirectory (and therefore the test ID) _must_ begin with `MAN-`.
#'
#' * It must contain a file called `test.md` which contains the user story, etc.
#'
#' * If there are any images or other content linked in the `test.md` file,
#' those files _must_ be in a subdirectory (in the same directory as the relevant `test.md`)
#' named `assets_[test ID]`.
#'
#' * The `test.md` file must have the following structure:
#'
#'   * A subheading starting with `## [test ID]: [test name]` at the top. The "test name"
#'   will be parsed through into the validation docs.
#'
#'   * A date that the test was run, on a line beginning with `* date: [date the test was run]`
#'
#'   * Any other content you want parsed through into the Validation Testing document. For example,
#'   test description, run details, test results, links to images, etc.
#'
#'
#' The path will be passed to `man_test_dir` argument of
#' [create_metworx_docs()].
#'
#' @seealso `mrgvalprep::parse_testthat_list_reporter()`,
#'   `mrgvalprep::get_sys_info()`, `mrgvalprep::read_spec_gsheets()`,
#'   `mrgvalprep::parse_github_issues()`
#'
#' @name input_formats
NULL

# TODO: Explain why there are multiple CSVs.
# TODO: Manual tests.
