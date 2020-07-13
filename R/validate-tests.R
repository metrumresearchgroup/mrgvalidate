
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

  test_list <- run_tests(pkg = pkg, root_dir = root_dir)

  test_df <- purrr::map_df(test_list, parse_test_output)

  if (!is.null(extra_test_dirs)) {
    extra_df_list <- map(extra_test_dirs, function(.t) {
      .tl <- run_tests(pkg = pkg, test_path = .t, root_dir = root_dir, build_package = FALSE)
      return(purrr::map_df(.tl, parse_test_output))
    })

    test_df <- bind_rows(test_df, extra_df_list)
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

#' Clones the specified repo at the specified tag to disk
#' @importFrom glue glue
#' @importFrom processx run
#' @param org Github organization that the repo is under
#' @param repo The name of the repo for the package you are validating
#' @param tag The tag to pull from the repo. When this function is called internally, this is assumed to be the same as the version you are testing, though it can be any valid tag.
#' @param domain Domain where repo lives. Either "github.com" or "ghe.metrumrg.com", defaulting to "github.com"
#' @param dest_dir File path for directory to clone repo into. Defaults to `tempdir()`
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

  if (domain == "github.com") {
    clone_string <- as.character(glue("git://github.com/{org}/{repo}.git"))
  } else {
    # need to use SSH for GHE
    clone_string <- as.character(glue("git@{domain}:{org}/{repo}.git"))
  }

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


#' Run test_check on package directory
#'
#' Starts a fresh R session with callr, then builds the package from within the repo, and runs test_check on the fresh build.
#' If `build_package = FALSE` this assumes that the package being tested has already been built and
#' installed at `file.path(root_dir, "mrgvalidate_lib")`, and it attempts to load the package from there.
#' This should be used exclusively for running extra test in other directories, because the intitial run should always be on a fresh install.
#' @importFrom testthat test_check ListReporter
#' @importFrom callr r
#' @importFrom withr with_dir
#' @importFrom devtools build
#' @importFrom utils install.packages
#' @importFrom fs dir_create dir_exists dir_delete
#' @param pkg name of the package to test
#' @param test_path Directory containing tests, where `testthat::test_check()` will be run. Defaults to "tests".
#' @param root_dir The directory path to where the package is (i.e. where the repo has been cloned). `file.path(root_dir, pkg, test_path)` should lead to the directory that will be tested.
#' @param build_package Boolean indicating whether to build and install the package to `file.path(root_dir, "mrgvalidate_lib")`
#' @export
run_tests <- function(pkg, test_path = "tests/testthat", root_dir = tempdir(), build_package = TRUE) {
  message(glue("run_tests() on {root_dir}/{pkg}/{test_path}"))

  # Run build and tests in new R session using callr::r()
  results_list <- callr::r(
    function(root_dir, pkg, test_path, build_package, setup_package_env) {
      # withr::with_dir(file.path(root_dir, pkg) ,{
      #
      #   # create temp folder to install into
      #   tmp_lib <- file.path(root_dir, "mrgvalidate_lib")
      #
      #   if (isTRUE(build_package)) {
      #     if(fs::dir_exists(tmp_lib)) fs::dir_delete(tmp_lib)
      #     fs::dir_create(tmp_lib)
      #
      #     # build and install
      #     source_path <- devtools::build()
      #     install.packages(source_path, lib = tmp_lib, repos = NULL)
      #
      #   }
      #
      #   # load package from temp folder
      #   require(pkg, lib.loc = tmp_lib, character.only = TRUE)
      #
      #   # load package environment
      #   env <- setup_package_env(pkg, test_path)
      #
      #   # run tests
      #   results_list <- testthat::test_dir(
      #     path = test_path,
      #     reporter = testthat::ListReporter$new(),
      #     env = env,
      #     filter = NULL,
      #     stop_on_failure = FALSE,
      #     stop_on_warning = FALSE,
      #     wrap = TRUE
      #   )
      #
      # })
      tmp_lib <- file.path(root_dir, "mrgvalidate_lib")
      fs::dir_create(tmp_lib)
      target_pkg <- file.path(root_dir, pkg)

      withr::with_libpaths(
        tmp_lib,
        {
          devtools::install(
            pkg = target_pkg,
            build = build_package,
            upgrade = "never",
            # quiet = TRUE
          )
          devtools::test(
            pkg = target_pkg,
            reporter = testthat::ListReporter$new()
          )
        },
        action = "prefix"
      )




      # devtools::install_deps(
      #   pkg = target_pkg,
      #   upgrade = "never",
      #   quiet = TRUE,
      #   lib = tmp_lib
      # )
    },
    args = list( # this is how you pass things into the callr::r() session
      root_dir = root_dir,
      pkg = pkg,
      test_path = test_path,
      build_package = build_package,
      setup_package_env = setup_package_env
    )
  )
  return (results_list)
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


#' @importFrom tibble tibble
#' @importFrom purrr map_chr map_lgl
parse_test_output <- function(result) {
  if (is.null(result$context)) {
    stop("no context specified in file: ", result$file)
  }
  out <- tibble::tibble(context = result$context,
                 file = result$file,
                 tests = map_chr(result$results, ~ .x$test),
                 success = map_lgl(result$results, ~ inherits(.x, "expectation_success")),
                 res_msg = map_chr(result$results, ~ paste(class(.x), collapse = ", "))
  )

  if (!all(map_lgl(result$results, ~ inherits(.x, "expectation_success")))) {
    #browser()
    #str(result)
    loser_msg <- result$results[[which(!map_lgl(result$results, ~ inherits(.x, "expectation_success")))]]
    print(paste(result$file, "--", result$test, "--\n", paste(loser_msg, collapse = "\n")))
  }
  return(out)
}

#' @importFrom glue trim
get_commit_hash <- function(root_dir, repo) {
  proc <- processx::run(command = "git", args = c("rev-parse", "HEAD"), wd = file.path(root_dir, repo))
  if (proc$status != "0") {
    stop(glue("Failed to get commit hash from {file.path(root_dir, repo)}\nCALL:\n  git {paste(cmd_args, collapse = ' ')}\nERROR:\n  {paste(proc$stderr, collapse = '  \n')}\n"))
  }
  return(trim(proc$stdout))
}
