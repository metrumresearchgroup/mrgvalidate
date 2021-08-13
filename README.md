# mrgvalidate
R package for generating validation documents for other R packages developed by Metrum.

## Installation

Install from source by [pulling the tarball](https://github.com/metrumresearchgroup/mrgvalidate/releases) or use:

```
devtools::install_github("metrumresearchgroup/mrgvalidate")
```

## One liner usage

If all input data conforms to what is described in `?mrgvalidate::input_formats` you should only have to run this single line:
```
create_validation_docs(
  product_name = "Fake Product", 
  version = "vFake", 
  specs = spec_df,           # a tibble containing stories and requirements to validate
  auto_test_dir = "some_dir" # directory containing automated test results
)
```

By default, this call will write `.md` and `.docx` files for all three documents into your working directory. See `?create_validation_docs` for other arguments to tweak how and where the documents are rendered.

### Preprocessing and formatting input data

`mrgvalidate requires a specific format for the input data. The [`mrgvalprep`](https://github.com/metrumresearchgroup/mrgvalprep) package exists to transform a variety of common data sources into the format required by `mrgvalidate`.

### Input checkers

There are several helper functions for checking the linkage between stories/requirements and tests. See `?find_missing` for details.


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
