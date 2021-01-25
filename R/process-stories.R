
#' Process issues from Github into a tibble of properly formatted stories
#' @importFrom tidyr unnest nest
#' @importFrom dplyr select mutate left_join
#' @importFrom rlang .data
#' @param release_issues Tibble output from [get_issues()] or [ghpm::get_issues()] containing all the issues you want to format.
#' @param org Github organization that the repo is under
#' @param repo The name of the repo for the package you are validating
#' @param domain Domain where repo lives. Either "github.com" or "ghe.metrumrg.com", defaulting to "github.com"
#' @param test_path File path to read test information from. Defaults to "all_tests.csv". If NULL is passed, no test information will be included.
#' @param add_risk Boolean for whether to pull the risk field from issues as well. Defaults to TRUE.
#' @export
process_stories <- function(
  release_issues,
  org,
  repo,
  domain = VALID_DOMAINS,
  test_path = ALL_TESTS,
  add_risk = TRUE
) {
  domain <- match.arg(domain)

  dd <- select(release_issues, .data$issue, .data$title, .data$body)

  df <- mutate(dd, tab = map(.data$body,proc_issue))

  df <- unnest(df, cols = c(.data$tab)) %>% mutate(body = NULL)

  # load test results, if requested
  if (!is.null(test_path)) {
    tst <- read_test_df(test_path)
    df <- left_join(df,tst,by=c("test_name", "test_file"))

    if(any(is.na(df$number))) {
      # check if any of them explictly have no tests
      missing_tests <- filter(df, is.na(.data$number) & .data$test_name != NO_TESTS_STRING)

      if (nrow(missing_tests) > 0) {
        warning(paste(
          glue("process_stories() tibble has {nrow(missing_tests)} rows with NA's for tests. The following tests were not found in {test_path}"),
          paste(paste(missing_tests$title, missing_tests$test_name, sep = " -- "), collapse = "\n"),
          sep = "\n"
        ))
      }
    }
  }

  if (isTRUE(add_risk)) {
    risk <- get_risk(org, repo, domain)
    df <- left_join(df, risk)

    if(any(is.na(df$risk))) {
      missing_risk <- unique(paste(df$issue[is.na(df$risk)], df$title[is.na(df$risk)], sep = " -- "))
      warning(paste(
        glue("process_stories() tibble has {length(missing_risk)} issues with risk missing:"),
        paste(missing_risk, collapse = "\n"),
        sep = "\n"
      ))
    }
  }

  df <- nest(
    df,
    tests = c(.data$test_file, .data$context, .data$test_name, .data$number, .data$passed, .data$failed)
  )

  return(df)
}


#' Get tibble of issues associated with a specific milestone
#' @importFrom dplyr filter
#' @importFrom ghpm api_url
#' @param org Github organization that the repo is under
#' @param repo The name of the repo for the package you are validating
#' @param mile The name of the milestone associated with the release you are validating. All issues tied to this milestone with be pulled.
#' @param domain Domain where repo lives. Either "github.com" or "ghe.metrumrg.com", defaulting to "github.com"
#' @importFrom rlang .data
#' @export
get_issues <- function(org, repo, mile, domain = VALID_DOMAINS) {
  domain <- match.arg(domain)
  if (domain == "github.com") {
    domain <- paste0("api.", domain)
  }
  pkg_issues <- ghpm::get_issues(org, repo, api_url(hostname = domain))
  release_issues <- pkg_issues %>% filter(.data$milestone == mile)

  return(release_issues)
}


#' Get risk field for each issue
#' @param org Github organization that the repo is under
#' @param repo The name of the repo for the package you are validating
#' @param domain Domain where repo lives. Either "github.com" or "ghe.metrumrg.com", defaulting to "github.com"
#' @importFrom dplyr filter select mutate
#' @importFrom ghpm api_url
#' @importFrom rlang .data
#' @export
get_risk <- function(org, repo, domain = VALID_DOMAINS) {
  domain <- match.arg(domain)
  if (domain == "github.com") {
    domain <- paste0("api.", domain)
  }
  issue_lab <- ghpm::get_issue_labels(org, repo, api_url(hostname = domain))
  issue_lab <- filter(issue_lab, grepl("risk", .data$label, fixed = TRUE))
  risk <- select(issue_lab, .data$issue, risk = .data$label)
  risk <- mutate(risk, risk = sub("risk: ", "", risk, fixed = TRUE))
  return(risk)
}
