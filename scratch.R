
df <- create_validation_docs(
  read_requirements_gsheet(
    "1ScdlEACqC-ypJN1k8m9tjDmFIg1UfzMDyCKOzzOCUTg",
    title_col = "ReqID", story_col = "Story Description",
    risk = "Product Risk",
    test_ids_col = "TestID"),
  "inst/validation-results-sample/"
)
