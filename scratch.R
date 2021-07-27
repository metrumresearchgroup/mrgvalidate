
reqs <- read_requirements_gsheet(
    "12vtANioHgI0BXVnq9nXbpnG1sXJaedVdmxEJsQyWHHU")

test_output_dir <- "inst/validation-results-sample"
df <- create_validation_docs(reqs, test_output_dir)
