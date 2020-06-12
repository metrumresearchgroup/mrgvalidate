# mrgvalidate
R package for generating validation documents for other R packages developed by Metrum.

See [the "Basic Usage" vignette](dead) for details on how to use for your package, as well as the documents that will be generated.

## Installation

Install from source by [pulling the tarball](https://github.com/metrumresearchgroup/mrgvalidate/releases) or use:

```
devtools::install_github("metrumresearchgroup/mrgvalidate")
```

## One liner usage

If everything is configured correctly (as described in the vignette mentioned above) you should only have to run this single line:
```
mrgvalidate::generate_docs(
  org = "metrumresearchgroup",
  repo = "yourpackage",
  milestone = "v0.1.0" # the name of the milestone in github
  version = "0.1.0"    # the tag that will be pulled for testing
)
```
