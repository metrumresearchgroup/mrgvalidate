
df <- create_validation_docs(
  read_requirements_gsheet(
    "1ScdlEACqC-ypJN1k8m9tjDmFIg1UfzMDyCKOzzOCUTg",
    title_col = "ReqID", story_col = "Requirement",
    risk = "Category",  # bogus for now
    test_ids_col = "TestID"),
  "inst/validation-results-sample/"
)
