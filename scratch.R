
spec <- read_spec_gsheets(
  ss_stories = "1P5PZsFf2OPWhcWqzefBuIheSlzMAXB1kR2_9HUPXIXI",
  ss_req = "12vtANioHgI0BXVnq9nXbpnG1sXJaedVdmxEJsQyWHHU"
)

test_output_dir <- "inst/validation-results-sample"
df <- create_validation_docs(spec, test_output_dir)
