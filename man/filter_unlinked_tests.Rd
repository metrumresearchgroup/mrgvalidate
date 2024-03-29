% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-test-framework.R
\name{filter_unlinked_tests}
\alias{filter_unlinked_tests}
\title{Remove unliked tests}
\usage{
filter_unlinked_tests(dd, tests)
}
\arguments{
\item{dd}{Tibble containing stories, requirements, and tests. Created in
\code{\link[=create_test_framework]{create_test_framework()}}.}

\item{tests}{Tibble containing all test results, FORMAT: CREATED ON LINE 59
OF \code{generate-docs.R} in \code{\link[=create_package_docs]{create_package_docs()}} or \code{\link[=create_metworx_docs]{create_metworx_docs()}}.}
}
\description{
Remove unliked tests
}
\details{
\code{\link[=make_testing_results]{make_testing_results()}} takes the \code{tests} tibble directly. Drop the test
IDs that aren't linked to \code{specs} because those IDs won't make it into the
other docs.
}
\keyword{internal}
