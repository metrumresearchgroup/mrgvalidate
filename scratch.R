
reqs <- read_requirements_gsheet(
    "12vtANioHgI0BXVnq9nXbpnG1sXJaedVdmxEJsQyWHHU")

stories <- read_stories_gsheet(
  "1P5PZsFf2OPWhcWqzefBuIheSlzMAXB1kR2_9HUPXIXI")

test_output_dir <- "inst/validation-results-sample"
df <- create_validation_docs(reqs, test_output_dir)
