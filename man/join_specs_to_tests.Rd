% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-test-framework.R
\name{join_specs_to_tests}
\alias{join_specs_to_tests}
\title{Format stories, tests, and requirements into dataframe}
\usage{
join_specs_to_tests(specs, auto_test_dir = NULL, man_test_dir = NULL)
}
\arguments{
\item{specs}{tibble of stories and requirements. See \link{input_formats}.}

\item{auto_test_dir}{Path to directory containing \code{.csv} and \code{.json} files with
results from automated tests. See \link{input_formats}.}

\item{man_test_dir}{Path to directory containing \code{.md} files and screenshots for
results from manual tests. See \link{input_formats}.}
}
\description{
Wrapper for \code{create_test_framework()}
}
