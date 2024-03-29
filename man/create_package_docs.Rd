% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-package-docs.R
\name{create_package_docs}
\alias{create_package_docs}
\title{Create validation docs}
\usage{
create_package_docs(
  product_name,
  version,
  language = c("R", "Go"),
  repo_url,
  specs,
  release_notes_file,
  auto_test_dir = NULL,
  man_test_dir = NULL,
  style_dir = NULL,
  output_dir = getwd(),
  write = TRUE,
  cleanup_rmd = TRUE
)
}
\arguments{
\item{product_name}{The product being validated.}

\item{version}{The version number of the product.}

\item{language}{Denotes the language the package was coded in. Either 'R' or 'Go'.
Dictates boiler plate text in the generated validation docs.}

\item{repo_url}{Character string denoting the url of repository.}

\item{specs}{tibble of stories and requirements. See \link{input_formats}.}

\item{release_notes_file}{file path to a formatted markdown doc of release notes.
This should contain two top-level headers for "Changes and New Features" and
"Bug Fixes". For packages, this can typically be extracted from the relevant
entry in the \code{NEWS.md} file.}

\item{auto_test_dir}{Path to directory containing \code{.csv} and \code{.json} files with
results from automated tests. See \link{input_formats}.}

\item{man_test_dir}{Path to directory containing \code{.md} files and screenshots for
results from manual tests. See \link{input_formats}.}

\item{style_dir}{Directory that has style references for the generated docx
files. When generating each output file, Pandoc will be instructed to use
the reference file from this directory that has the same base name (if it
exists).}

\item{output_dir}{Directory to write the output documents to. Defaults to
working directory.}

\item{write}{Whether to create the output docs. Setting this to \code{FALSE} is
useful when you're just interested in the return value.}

\item{cleanup_rmd}{Whether to delete the copied RMD's after the word documents are generated.
Defaults to \code{TRUE}.}
}
\value{
In addition to creating the validation docs, a tibble that joins the
tests with \code{specs} is returned invisibly.
}
\description{
This function is the entry point for creating validation docs with the new
7-document format for packages.
}
\seealso{
\code{\link[=create_metworx_docs]{create_metworx_docs()}}
}
