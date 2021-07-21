#' Run and summarize tests
#'
#' `pkg` will be built and installed (along with any missing dependencies) to a
#' temporary library. Then we run all tests for `pkg`, roll up successes and
#' failures, and write to a CSV file.
#'
#' @details
#'
#' The package found at `file.path(root_dir, pkg)` will be built and installed
#' into a tempory library that is appended to the _beginning_ of your
#' `.libPaths()`, using `devtools::install(..., build = TRUE, upgrade =
#' "never")`. This will also install (into the temporary library) any
#' dependencies from the package that are _not_ found in your `.libPaths()`.
#'
#' @importFrom dplyr group_by summarize bind_rows
#' @importFrom purrr map_df map
#' @importFrom rlang .data
#' @importFrom fs dir_exists dir_create
#'
#' @param pkg The name of the package you are validating, to be included in the
#'   output document.
#' @param root_dir The directory path to where the package has been cloned.
#'   `file.path(root_dir, pkg)` should lead to the cloned repo that will be
#'   tested.
#' @param out_file File path to write out the test results to. Any extension
#'   will be ignored and replaced with .csv
#' @param output_dir Directory to write the output documents to. Defaults to
#'   working directory.
#' @param return_df Boolean indicating whether to return the tibble that is
#'   written to `out_file`. Defaults to FALSE and returns nothing.
#' @param extra_test_dirs Character vector of paths (relative to package root
#'   dir) to directories that contain additional tests to run
#' @export
validate_tests <- function(
  pkg,
  root_dir = tempdir(),
  out_file = ALL_TESTS,
  output_dir = getwd(),
  return_df = FALSE,
  extra_test_dirs = NULL
) {

  tmp_lib <- withr::local_tempdir()
  withr::local_libpaths(tmp_lib, "prefix")
  devtools::install(
    file.path(root_dir, pkg),
    build = TRUE,
    quiet = TRUE,
    upgrade = "never"
  )

  test_list <- run_tests(pkg = pkg, root_dir = root_dir)

  test_df <- parse_testthat_list_reporter(test_list)

  if (!is.null(extra_test_dirs)) {
    extra_df_list <- map(extra_test_dirs, function(.t) {
      .tl <- run_tests(pkg = pkg, test_path = .t, root_dir = root_dir)
      return(parse_testthat_list_reporter(.tl))
    })

    test_df <- bind_rows(test_df, extra_df_list)
  }

  if (sum(test_df$fail) > 0) {
    warning(glue("`validate_tests(pkg = '{pkg}', root_dir = '{root_dir}')` had {sum(test_df$fail)} failing tests."))
  }

  if (!is.null(out_file)) {
    if (!fs::dir_exists(output_dir)) fs::dir_create(output_dir)
    out_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(out_file), ".csv"))
    readr::write_csv(test_df, out_file)
  }

  if (isTRUE(return_df)) {
    return(test_df)
  }
}

#' Clones the specified repo at the specified tag to disk
#' @importFrom glue glue
#' @importFrom processx run
#' @param org Github organization that the repo is under
#' @param repo The name of the repo for the package you are validating
#' @param tag The tag to pull from the repo. When this function is called internally, this is assumed to be the same as the version you are testing, though it can be any valid tag.
#' @param domain Domain where repo lives. Either "github.com" or "ghe.metrumrg.com", defaulting to "github.com"
#' @param dest_dir File path for directory to clone repo into. Defaults to [tempdir()]
#' @param overwrite Boolean indicating whether to overwrite `file.path(dest_dir, repo)` if something already exists there. TRUE by default.
#' @export
pull_tagged_repo <- function(
  org,
  repo,
  tag,
  domain = VALID_DOMAINS,
  dest_dir = tempdir(),
  overwrite = TRUE
) {
  if (isTRUE(overwrite)) {
    if (fs::dir_exists(file.path(dest_dir, repo))) fs::dir_delete(file.path(dest_dir, repo))
  }

  domain <- match.arg(domain)

  clone_string <- as.character(glue("git@{domain}:{org}/{repo}.git"))
  cmd_args <- c("clone", "--branch", tag, clone_string, "--depth=1")

  message(glue("Getting repo with `git {paste(cmd_args, collapse = ' ')}`"))
  proc <- processx::run(
    command = "git",
    args = cmd_args,
    wd = dest_dir
  )

  if (proc$status != "0") {
    stop(glue("Failed to clone {repo}\nCALL:\n  git {paste(cmd_args, collapse = ' ')}\nERROR:\n  {paste(proc$stderr, collapse = '  \n')}\n"))
  }

  # extract commit hash to return
  commit_hash <- get_commit_hash(dest_dir, repo)
  return(commit_hash)
}


#' Test a source package
#'
#' This is an internal function called by [validate_tests()]
#'
#' @param pkg Name of the package to test; should be installed to a library in
#'   [.libPaths()].
#' @param test_path Directory containing tests, where [testthat::test_dir()]
#'   will be run.
#' @param root_dir The directory path to where the package is (i.e. where the
#'   repo has been cloned). `file.path(root_dir, pkg, test_path)` should lead to
#'   the directory that will be tested.
#' @keywords internal
run_tests <- function(pkg, test_path = "tests/testthat", root_dir = tempdir()) {
  stopifnot(requireNamespace(pkg))
  message(glue("run_tests() on {root_dir}/{pkg}/{test_path}"))

  results_list <- callr::r(
    function(root_dir, pkg, test_path, setup_package_env) {
      withr::local_dir(file.path(root_dir, pkg))
      require(pkg, character.only = TRUE)

      # load package environment
      env <- setup_package_env(pkg, test_path)

      # run tests
      results_list <- testthat::test_dir(
        path = test_path,
        reporter = testthat::ListReporter$new(),
        env = env,
        filter = NULL,
        stop_on_failure = FALSE,
        stop_on_warning = FALSE
      )

      return(results_list)
    },
    args = list(
      root_dir = root_dir,
      pkg = pkg,
      test_path = test_path,
      setup_package_env = setup_package_env
    )
  )
  return(results_list)
}

#' Helper for setting up package environment for testing
#'
#' These are all things that were copied out of internal functions called by testthat::test_check to set up the package environment.
#' They are necessary because testthat::test_dir does NOT do this, which causes some tests to fail.
#' Specifically, this code came mostly from testthat:::test_package_dir and testthat:::test_pkg_env and some from test_check itself.
#' @param package the package name
#' @param test_path path to folder with tests in it
setup_package_env <- function(package, test_path) {

  if (!utils::file_test("-d", test_path)) {
    stop("No tests found for ", package, call. = FALSE)
  }

  env <-   list2env(
    as.list(getNamespace(package), all.names = TRUE),
    parent = parent.env(getNamespace(package))
  )

  withr::local_options(list(topLevelEnvironment = env))

  withr::local_envvar(list(
    TESTTHAT_PKG = package,
    TESTTHAT_DIR = fs::path_abs(".") # we're in the root dir because of withr::with_dir above
  ))

  return(env)
}

#' @importFrom glue trim
get_commit_hash <- function(root_dir, repo) {
  proc <- processx::run(command = "git", args = c("rev-parse", "HEAD"), wd = file.path(root_dir, repo))
  if (proc$status != "0") {
    stop(glue("Failed to get commit hash from {file.path(root_dir, repo)}\nCALL:\n  git {paste(cmd_args, collapse = ' ')}\nERROR:\n  {paste(proc$stderr, collapse = '  \n')}\n"))
  }
  return(trim(proc$stdout))
}
