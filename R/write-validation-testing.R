
#' Build the Validation Testing document and write it to a markdown file
#'
#' This takes the input from automated and/or manual tests and writes them to a
#' `.md` file (and optionally `.docx` file as well). The automated tests will be
#' written as tables of results, one per result file (`.csv` file found in
#' `auto_test_dir` passed to [create_validation_docs()]). The manual tests will
#' have their `content` sections rendered sequentially below the automated
#' tests.
#' @importFrom purrr walk
#' @importFrom knitr kable
#' @importFrom dplyr filter select summarise pull
#' @importFrom glue glue
#' @importFrom rmarkdown render
#' @importFrom rlang .data
#' @importFrom fs dir_exists dir_create
#' @param product The name of the product you are validating, to be included in the output document.
#' @param version The version number of the product you are validating, to be included in the output document.
#' @param tests Tibble containing all test results, FORMAT: CREATED ON LINE 59
#'   OF `generate-docs.R` in [create_validation_docs()]. ** TODO: If we're gonna
#'   export this function we're gonna need to spell out what this tibble
#'   actually needs to look like so that someone could theoretically constuct
#'   it. We may just wanna make these `write_*` functions private though. When
#'   would someone need to call them?**
#' @param auto_info A named list containing the test suite information pulled
#'   from the `.json` files found in `auto_test_dir`, one element per `.json`
#'   (named with the filename _without_ extension). **Same note as `tests` about
#'   exporting and specs.**
#' @param out_file Filename to write markdown file out to. Any extension will be ignored and replaced with .md
#' @param output_dir Directory to write the output documents to. Defaults to working directory.
#' @param word_document Logical scaler indicating whether to render a docx document
#' @export
write_validation_testing <- function(
  product,
  version,
  tests,
  auto_info,
  out_file = VAL_FILE,
  output_dir = getwd(),
  word_document = TRUE
) {
  if (!fs::dir_exists(output_dir)) fs::dir_create(output_dir)
  out_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(out_file), ".md"))

  na_test_ids <- sum(is.na(tests$TestId))
  if (na_test_ids > 0) {
    warning(glue("Dropping {na_test_ids} tests with no ID"))
    tests <- filter(tests, !is.na(.data$TestId))
  }

  # write to top section to file
  val_boiler <- glue('
---
title: ""
output:
  word_document: default
  pdf_document:
    latex_engine: xelatex
number_sections: yes
---

# Validation Testing: {product} {version}

## Scope

The purpose of this Validation Testing document is to define the conditions for
test execution and present the test results. All tests are specified and linked
to release candidate user stories as numbered issues in the Requirements
Specification document.

')
  cat(file = out_file,  val_boiler,"\n")

  # add automated test outputs
  cat(file = out_file,  "\n# Automated Test Results\n", append = TRUE)

  walk(names(auto_info), ~ {
    .suite <- auto_info[[.x]]

    val_info <- glue('

### {.x}

**Date Run:** {.suite$date}

{print_info_list(.suite$info)}

')

    # filter to relevant tests
    test_df <- tests %>%
      filter(.data$result_file == .x) %>%
      select(
        `test ID` = .data$TestId,
        `test name` = .data$test_name,
        .data$passed,
        .data$failed
      )

    # write to file
    cat(file = out_file,  val_info, sep = "\n", append = TRUE)
    cat(file = out_file, knitr::kable(test_df), sep = "\n", append = TRUE)
  })

  # write manual test outputs
  cat(file = out_file,  "\n# Manual Test Results\n", append = TRUE)

  tests %>%
    filter(.data$test_type == "manual") %>%
    pull(.data$man_test_content) %>%
    walk(~ cat(file = out_file,  glue("\n{.x}\n\n"), append = TRUE))

  message(glue("Finished writing to {out_file}"))

  if (isTRUE(word_document)) {
    message("  Rendering markdown to docx...")
    rmarkdown::render(
      out_file,
      output_format = "word_document",
      output_dir = dirname(out_file),
      quiet = TRUE
    )
    message("  Finished rendering")
  }
}

#' Format an info JSON suite element for printing
#' @importFrom purrr map
#' @importFrom glue glue
#' @keywords internal
print_info_list <- function(.l) {
  if (is.null(.l)) return("")

  map(names(.l), ~ {
    .v <- .l[[.x]]
    if (inherits(.v, "list")) {
      return(print_info_list(.v)) # recursive y'all
    }

    if (length(.v) > 1) {
      .v <- paste(.v, collapse = "; ")
    }

    return(glue("**{.x}:** {.v}"))
  }) %>%
    unlist() %>%
    paste(collapse = "\n\n")
}






##############################
# RETAINED TO BE REFACTORED
# FOR BACKWARDS COMPATABILITY
##############################

#' Build the Validation Testing document and write it to a markdown file
#'
#' This function will pull
#' @importFrom purrr map walk
#' @importFrom knitr kable
#' @importFrom dplyr slice n_distinct summarise
#' @importFrom glue glue
#' @importFrom rmarkdown render
#' @importFrom rlang .data
#' @importFrom devtools session_info
#' @importFrom fs dir_exists dir_create
#' @param org Github organization that the repo is under
#' @param repo The name of the repo for the package you are validating
#' @param version The version number of the package you are validating. This must correspond to a tag in the repo.
#' @param domain Domain where repo lives. Either "github.com" or "ghe.metrumrg.com", defaulting to "github.com"
#' @param extra_test_dirs Character vector of paths (relative to package root dir) to directories that contain additional tests to run
#' @param out_file filename to write markdown file out to. Any extension will be ignored and replaced with .md
#' @param output_dir Directory to write the output documents to. Defaults to working directory.
#' @param word_document Logical scaler indicating whether to render a docx document
#' @param dry_run Boolean indicating whether to clone repo and run tests (if FALSE) or skip those steps and look for test results in "all_tests.csv" instead (if TRUE). FALSE by default.
#' @export
write_validation_testing_ORIG <- function(
  org,
  repo,
  version,
  domain = VALID_DOMAINS,
  extra_test_dirs = NULL,
  out_file = VAL_FILE,
  output_dir = getwd(),
  word_document = TRUE,
  dry_run = FALSE
) {
  if (!fs::dir_exists(output_dir)) fs::dir_create(output_dir)
  out_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(out_file), ".md"))
  root_dir <- tempdir()
  domain <- match.arg(domain)

  val_boiler <- glue('
---
title: ""
output:
  word_document: default
  pdf_document:
    latex_engine: xelatex
number_sections: yes
---

# Validation Testing: {repo} {version}

## Scope

The purpose of this Validation Testing document is to define the conditions for
test execution. All tests are specified and linked to release candidate user
stories as numbered issues in the Requirements Specification-Validation Plan
document.

----------------

## Test locations

Tests are in the following location

1. `tests/testthat`

')

  # pull requested tag from repo
  if (isTRUE(dry_run)) {
    commit_hash <- get_commit_hash(root_dir, repo)
    date_stamp <- paste("DRYRUN --", Sys.time())
  } else {
    message(glue("Pulling repo {domain}/{org}/{repo}..."))
    commit_hash <- pull_tagged_repo(org = org, repo = repo, tag = version, dest_dir = root_dir, domain = domain)
    date_stamp <- Sys.time()
  }

  val_candidate <- glue('
## Test candidate

Testing {org}/{repo}

Commit: {commit_hash}

Date: {date_stamp}

')

  val_tests <- '

## Test details

Full report of all tests run.
'

  # run test_check
  if (isTRUE(dry_run)) {
    test_df <- readr::read_csv(ALL_TESTS, col_types = readr::cols())
  } else {
    test_df <- validate_tests(
      pkg = repo,
      root_dir = root_dir,
      out_file = ALL_TESTS,
      output_dir = output_dir,
      return_df = TRUE,
      extra_test_dirs = extra_test_dirs
    )
  }

  val_summary <- '

## Comprehensive summary

Summarizes the number of contexts, tests, and expectations and counts number
of tests and test failures.
'

  sum_df <- test_df %>%
    ungroup %>%
    summarise(
      contexts = n_distinct(.data$context),
      tests = n_distinct(.data$tests),
      assertions = sum(.data$nb),
      failed = sum(.data$failed)
    )

  session_info <- devtools::session_info()
  val_session <- glue('

## Session

Testing session information is captured.

```
{paste(session_info, collapse = "\n")}
```
')

  # write to file
  cat(file = out_file,  val_boiler,"\n")

  cat(file = out_file,  val_candidate,"\n", append = TRUE)

  cat(file = out_file,  val_summary,"\n", append = TRUE)
  tab <- knitr::kable(sum_df)
  cat(file = out_file, tab, sep = "\n", append = TRUE)

  cat(file = out_file,  val_tests,"\n", append = TRUE)
  tab <- knitr::kable(test_df)
  cat(file = out_file, tab, sep = "\n", append = TRUE)

  cat(file = out_file,  val_session,"\n", append = TRUE)

  message(glue("Finished writing to {out_file}"))

  if (isTRUE(word_document)) {
    message("  Rendering markdown to docx...")
    rmarkdown::render(
      out_file,
      output_format = "word_document",
      output_dir = dirname(out_file),
      quiet = TRUE
    )
    message("  Finished rendering")
  }
}
