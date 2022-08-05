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
#' @param style_dir Directory that has style references for the generated docx
#'   files. When generating each output file, Pandoc will be instructed to use
#'   the reference file from this directory that has the same base name (if it
#'   exists).
#' @param output_dir Directory to write the output documents to. Defaults to
#'   working directory.
#' @param type the type of doc you want to render ("package" or "metworx")
#' @param write Whether to create the output docs. Setting this to `FALSE` is
#'   useful when you're just interested in the return value.
#' @return In addition to creating the validation docs, a tibble that joins the
#'   tests with `specs` is returned invisibly.
#' @importFrom dplyr bind_rows filter full_join mutate pull recode rename select
#' @importFrom purrr map_chr
#' @importFrom stringr str_pad
#' @importFrom tidyr nest unnest
#' @importFrom rlang .data
#' @export
create_validation_docs <- function
(
  product_name, version, specs,
  auto_test_dir = NULL, man_test_dir = NULL, roles = NULL,
  style_dir = NULL,
  output_dir = getwd(),
  type = "package",
  write = TRUE
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

  # write_validation_testing() takes the `tests` tibble directly. Drop the test
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

  dd <- nest(dd,
             tests = c(.data$TestId, .data$TestName,
                       .data$passed, .data$failed, .data$man_test_content,
                       .data$result_file))

  # Read in NEWS.md for release notes
  release_notes <- file.path(auto_test_dir, "NEWS.md") %>% readLines() %>%
    parse_release_notes(product_name, version)


  if (isTRUE(write)) {
    # write_requirements(
    #   dd,
    #   product_name,
    #   version,
    #   roles = roles,
    #   style_dir = style_dir,
    #   out_file = REQ_FILE,
    #   output_dir = output_dir,
    #   word_document = TRUE
    # )
    #
    # write_traceability_matrix(
    #   dd,
    #   product_name,
    #   version,
    #   style_dir = style_dir,
    #   out_file = MAT_FILE,
    #   output_dir = output_dir,
    #   word_document = TRUE
    # )
    #
    # write_validation_testing(
    #   product_name,
    #   version,
    #   tests,
    #   auto_info,
    #   style_dir = style_dir,
    #   out_file = VAL_FILE,
    #   output_dir = output_dir,
    #   word_document = TRUE
    # )

    # Validation Plan
    make_validation_plan(
      product_name,
      version,
      release_notes = release_notes,
      style_dir = style_dir,
      out_file = VAL_PLAN_FILE,
      output_dir = output_dir,
      type = type,
      word_document = TRUE
    )


    # Testing Plan
    make_testing_plan(
      product_name,
      version,
      tests,
      auto_info,
      style_dir = style_dir,
      out_file = TEST_PLAN_FILE,
      output_dir = output_dir,
      type = type,
      word_document = TRUE
    )

    # Testing Results
    make_testing_results(
      product_name,
      version,
      tests,
      auto_info,
      style_dir = style_dir,
      out_file = TEST_RESULTS_FILE,
      output_dir = output_dir,
      type = type,
      word_document = TRUE
    )

    # Traceability Matrix
    make_traceability_matrix(
      dd,
      product_name,
      version,
      style_dir = style_dir,
      out_file = MAT_FILE,
      output_dir = output_dir,
      type = type,
      word_document = TRUE
    )

    # Requirements Specification
    make_requirements(
      dd,
      product_name,
      version,
      roles = NULL,
      style_dir = style_dir,
      out_file = REQ_FILE,
      output_dir = output_dir,
      type = type,
      word_document = TRUE
    )

    # Validation Summary Report
    make_validation_summary(
      product_name,
      version,
      release_notes = NULL,
      style_dir = style_dir,
      out_file = VAL_SUM_FILE,
      output_dir = output_dir,
      type = type,
      word_document = TRUE
    )
  }

  return(invisible(dd))
}
