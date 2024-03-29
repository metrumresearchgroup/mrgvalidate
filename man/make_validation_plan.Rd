% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make-validation-plan.R
\name{make_validation_plan}
\alias{make_validation_plan}
\title{Build the Validation Plan document and write it to output file(s)}
\usage{
make_validation_plan(
  product_name,
  version,
  repo_url = NULL,
  release_notes = NULL,
  auto_info = NULL,
  style_dir = NULL,
  out_file = VAL_PLAN_FILE,
  output_dir = getwd(),
  type = "package",
  word_document = TRUE
)
}
\arguments{
\item{product_name}{The name of the product you are validating, to be included in the output document.}

\item{version}{The version number of the product you are validating, to be included in the output document.}

\item{repo_url}{Character string denoting the url of repository.}

\item{release_notes}{list of release notes, formatted for rmarkdown.}

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
Build the Validation Plan document and write it to output file(s)
}
\keyword{internal}
