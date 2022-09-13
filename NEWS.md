# mrgvalidate 2.0.0

## New Features
 - Overhaul of entire package (is not backwards compatible).
 - Generated documents are now created from parameterized `rmarkdown` documents, instead of glueing text together.
 - There are a total of 7 validation documents (up from 3). The tracability matrix and requirements specification documents were carried over, but the other 5 are mostly new content.
 - `create_validation_docs` has been replaced by `create_package_docs()` and `create_metworx_docs()` for packages and the Metworx platform respectfully.
    - The arguments of these two functions are largely the same as `create_validation_docs`.
    - `create_package_docs()` has an added `language` argument, that will change the boilerplate text depending on the language the package was predominantly coded in.
 - Release notes are now required. This should contain two top-level headers for "Changes and New Features" and "Bug Fixes". For packages, this can typically be extracted from the relevant entry in the `NEWS.md` file.
 - More helpful warning and error messages.
 - `create_templates()` was introduced for easily creating templates for both packages and the Metworx platform.
 - More comprehensive test suite.

## Bug fixes

 - Missing links between requirements and stories were previously unreported. It will now error out if any are found.

# mrgvalidate 1.0.2

[`?mrgvalidate::input_formats`](https://metrumresearchgroup.github.io/mrgvalidate/reference/input_formats.html) now includes information about the expected directory layout for manual tests.

## Bug fixes

* Calling `create_validation_docs` with a relative path for `style_dir` did not work when `output_dir` was set to a value other than the current working directory.

# mrgvalidate 1.0.1

## Bug fixes

* `read_csv_test_results()` and `read_manual_test_results()` now provide a more helpful error message when called with a directory that doesn't have any test results.

* The wording of the traceability matrix's scope text has been improved.

# mrgvalidate 1.0.0

This release is a massive refactor, mainly because it moves all preprocessing functionality to a the new [`mrgvalprep`](https://github.com/metrumresearchgroup/mrgvalprep) package and streamlines `mrgvalidate` to be _only_ for rendering the final validation documents.

`mrgvalidate` now exports _only_ the following functions:

**For rendering validation documents:**

* `create_validation_docs()`

**For checking story/requirement to test linkage:**

* `find_missing()`
* `find_reqs_with_missing_tests()`
* `find_reqs_without_stories()`
* `find_tests_without_reqs()`
