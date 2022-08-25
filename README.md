# mrgvalidate
The purpose of the `mrgvalidate` package is to generate 7 specific documents that are necessary for the software validation process at Metrum Research Group. Those documents are:

* release-notes.docx
* validation-plan.docx
* validation-testing.docx
* requirements-specification.docx
* traceability-matrix.docx
* testing-results.docx
* validation-summary.docx

## Installation

Install from source by [pulling the tarball](https://github.com/metrumresearchgroup/mrgvalidate/releases) or use:

```
devtools::install_github("metrumresearchgroup/mrgvalidate")
```

# Generating the docs

By default, these calls (`create_package_docs()` and `create_metworx_docs()`) will write `.docx` files for all six documents into your working directory. Parameterized `.Rmd` files are first copied over, but are removed at the end (you can override this with the argument `cleanup_rmd = FALSE`). If all input data conforms to what is described in [`?mrgvalidate::input_formats`](https://metrumresearchgroup.github.io/mrgvalidate/reference/input_formats.html), you should only have to write a single a line.

## Usage: Packages

```
create_package_docs(
  product_name = "Fake Product", 
  version = "vFake", 
  repo_url = "git@github.com:org/package.git",
  specs = spec_df,                # a tibble containing stories and requirements to validate
  release_notes_file = "NEWS.md", # file path to a formatted markdown doc of release notes.
  auto_test_dir = "some_dir",     # directory containing automated test results
  style_dir = "style_ref_dir"     # Directory that has style references for the generated docx files
)
```

 See [`?create_package_docs`](https://metrumresearchgroup.github.io/mrgvalidate/reference/create_package_docs.html) for other arguments to tweak how and where the documents are rendered.

## Usage: Metworx Platform (experimental)

```
create_metworx_docs(
  product_name = "Fake Product", 
  version = "vFake", 
  specs = spec_df,                # a tibble containing stories and requirements to validate
  release_notes_file = "NEWS.md", # file path to a formatted markdown doc of release notes.
  auto_test_dir = "some_dir",     # directory containing automated test results
  man_test_dir = "some_dir",      # directory containing automated test results
  style_dir = "style_ref_dir"     # Directory that has style references for the generated docx files
)
```

 See [`?create_metworx_docs`](https://metrumresearchgroup.github.io/mrgvalidate/reference/create_metworx_docs.html) for other arguments to tweak how and where the documents are rendered.


### Preprocessing and formatting input data

`mrgvalidate` requires a specific format for the input data. [The `mrgvalprep` package](https://github.com/metrumresearchgroup/mrgvalprep)  exists to transform a variety of common data sources into the format required by `mrgvalidate`. **Please see the [`mrgvalprep` Basic Usage vignette](https://metrumresearchgroup.github.io/mrgvalprep/articles/basic_usage.html) for details** on several different formats and use cases.

### Input checkers

There are several helper functions for checking the linkage between stories/requirements and tests. See [`?find_missing`](https://metrumresearchgroup.github.io/mrgvalidate/reference/find_missing.html) for details.


## Development

`mrgvalidate` uses [pkgr](https://github.com/metrumresearchgroup/pkgr) to manage 
development dependencies and [renv](https://rstudio.github.io/renv/) to provide 
isolation. To replicate this environment, 

1. clone the repo

2. install pkgr

3. open package in an R session and run `renv::init(bare = TRUE)` 
   - install `renv` > 0.8.3-4 into default `.libPaths()` if not already installed

3. run `pkgr install` in terminal within package directory

4. restart session

Then, launch R with the repo as the working directory (open the project in RStudio). renv will activate and find the project library.
