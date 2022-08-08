#' Create validation docs
#'
#' This function is the main entry point for creating validation docs.
#'
#' @param type character string denoting which type of docs you want to generate. Either "package" or "metworx"
#'
#' @details
#' For packages, release notes will come from the corresponding `NEWS.md` document.
#' This should be copied into `auto_test_dir` in the build script before creating the docs via `create_validation_docs` or `create_package_docs`:
#' `fs::file_copy("NEWS.md", file.path(auto_test_dir, "NEWS.md"))`
#'
#' For metworx, a formatted markdown file path must be provided via the `release_notes_file` argument.
#'
#' Make sure sections are separated by `Heading 2 (##)` in both cases.
#'
#' @importFrom dplyr bind_rows filter full_join mutate pull recode rename select
#' @importFrom purrr map_chr
#' @importFrom stringr str_pad
#' @importFrom tidyr nest unnest
#' @importFrom rlang .data
#'
#' @export
create_validation_docs <- function
(
  product_name,
  version,
  specs,
  auto_test_dir = NULL,
  man_test_dir = NULL,
  release_notes_file = NULL,
  roles = NULL,
  style_dir = NULL,
  output_dir = getwd(),
  type = c("package", "metworx"),
  write = TRUE
) {

  type <- match.arg(type)
  checkmate::assert_logical(template)
  checkmate::assert_logical(write)

  switch (type,
    package = create_package_docs(product_name, version, repo_url = "blah", specs, auto_test_dir, man_test_dir,
                                  style_dir, output_dir, write),
    metworx = create_metworx_docs(product_name, version, specs, auto_test_dir, man_test_dir,
                                  release_notes_file, roles, style_dir, output_dir, write)
  )
}


#' Format stories, tests, and other relevant information into usable objects
#'
#' @inheritParams create_validation_docs
#'
#' @keywords internal
create_test_framework <- function(
  product_name,
  specs,
  auto_test_dir = NULL,
  man_test_dir = NULL,
  type = "package"
){

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
    auto_info <- get_repo_vars(auto_res$info, type)
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
  return(
    list(
      dd = dd,
      tests = tests,
      auto_info = auto_info
         )
  )
}

#' Get repo URL and commit hash
#'
#' @param auto_info named list
#' @param type either "package" or "metworx"
#'
#' @details
#' Environment variables not set, but included in `mrgvalprep::get_sys_info`, will show up as an empty character string ('')
#' If these variables are not set or included in `mrgvalprep::get_sys_info`, values will show up as NULL.
#' This function accounts for both cases, and will only fetch these values if not specified.
#' This method was chosen to preserve existing functionality of validation build scripts.
#'
#' @keywords internal
get_repo_vars <- function(auto_info, type){
  if(type == "package"){
    repo_info <- map(auto_info, ~{
      env_vars <- .x$info$env_vars

      if(is.null(env_vars$COMMIT_HASH)) env_vars$COMMIT_HASH <- ""
      if(is.null(env_vars$REPO)) env_vars$REPO <- ""

      if(env_vars$COMMIT_HASH == ""){
        env_vars$COMMIT_HASH <- system("git rev-parse HEAD", intern=TRUE)
      }
      if(env_vars$REPO == ""){
        env_vars$REPO <- system("git config --get remote.origin.url", intern=TRUE)
      }
      .x$info$env_vars <- env_vars
      .x
    })
  }else if(type == "metworx"){
    # We may eventually want to collect different information for metworx
    # For now, dont overwrite
    repo_info <- auto_info
  }

  return(repo_info)
}

