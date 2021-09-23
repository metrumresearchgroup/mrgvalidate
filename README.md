# mrgvalidate
The purpose of the `mrgvalidate` package is to generate three specific documents that are necessary for the software validation process at Metrum Research Group. Those documents are:

* requirements-specification.docx
* validation-testing.docx
* traceability-matrix.docx

## Installation

Install from source by [pulling the tarball](https://github.com/metrumresearchgroup/mrgvalidate/releases) or use:

```
devtools::install_github("metrumresearchgroup/mrgvalidate")
```

## Usage

If all input data conforms to what is described in [`?mrgvalidate::input_formats`](https://metrumresearchgroup.github.io/mrgvalidate/reference/input_formats.html) you should only have to run this single line:
```
create_validation_docs(
  product_name = "Fake Product", 
  version = "vFake", 
  specs = spec_df,           # a tibble containing stories and requirements to validate
  auto_test_dir = "some_dir" # directory containing automated test results
)
```

By default, this call will write `.md` and `.docx` files for all three documents into your working directory. See [`?create_validation_docs`](https://metrumresearchgroup.github.io/mrgvalidate/reference/create_validation_docs.html) for other arguments to tweak how and where the documents are rendered.

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
