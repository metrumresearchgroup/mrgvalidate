###########################
# constants for test input
###########################

TEST_INPUTS_DIR <- system.file("test-inputs", package = "mrgvalidate")
CREATE_OUT_DF_NAMES <- c(
  "StoryId",
  "StoryName",
  "StoryDescription",
  "ProductRisk",
  "RequirementId",
  "RequirementDescription",
  "date",
  "test_type",
  "tests"
)

VAL_TITLE <- "Validation Testing"
VAL_BOILER <- '
## Scope

The purpose of this Validation Testing document is to define the conditions for
test execution and present the test results. All tests are specified and linked
to release candidate user stories as numbered issues in the Requirements
Specification document.
'

REQ_TITLE <- "# Requirements Specification"
REQ_BOILER <- '
## Scope

The purpose of this document is to define specific criteria for each testing
task.  Testing shall be conducted in accordance with the requirements within this
document. The Requirement Specifications ensure that each requirement is tested.
'

MAT_TITLE <- "# Traceability Matrix"
MAT_BOILER <- '
## Scope

This Traceability Matrix links test results to specific user stories for the
proposed software release. User stories, including requirements and test specifications,
are listed in the Requirements Specification.
'

