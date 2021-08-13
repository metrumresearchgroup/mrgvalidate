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
