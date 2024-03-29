% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make-testing-results.R
\name{make_testing_results}
\alias{make_testing_results}
\title{Build the Testing Results document and write it to output file(s)}
\usage{
make_testing_results(
  product_name,
  version,
  tests,
  auto_info,
  style_dir = NULL,
  out_file = TEST_RESULTS_FILE,
  output_dir = getwd(),
  type = "package",
  word_document = TRUE
)
}
\arguments{
\item{product_name}{The name of the product you are validating, to be included in the output document.}

\item{version}{The version number of the product you are validating, to be included in the output document.}

\item{tests}{Tibble containing all test results, FORMAT: CREATED ON LINE 59
OF \code{generate-docs.R} in \code{\link[=create_package_docs]{create_package_docs()}} or \code{\link[=create_metworx_docs]{create_metworx_docs()}}.}

\item{auto_info}{A named list containing the test suite information pulled
from the \code{.json} files found in \code{auto_test_dir}, one element per \code{.json}
(named with the filename \emph{without} extension). \strong{Same note as \code{tests} about
exporting and specs.}}

\item{style_dir}{Directory to check for a docx style reference that has the
same base name as \code{out_file}.}

\item{out_file}{Filename to write markdown file out to. Any extension will be ignored and replaced with .Rmd}

\item{output_dir}{Directory to write the output documents to. Defaults to working directory.}

\item{type}{the type of doc you want to render ("package" or "metworx")}

\item{word_document}{Logical scaler indicating whether to render a docx document}
}
\description{
This takes the input from automated and/or manual tests and writes them to a
\code{.docx} file.
}
\keyword{internal}
