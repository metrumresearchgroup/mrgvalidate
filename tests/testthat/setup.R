###########################
# constants for test input
###########################

DOMAIN <- "github.com"
ORG <- "metrumresearchgroup"
REPO <- "mrgvalidatetestreference"
MILESTONE <- "v0.6.0"
TAG <- "0.6.0"

GHE_DOMAIN <- "ghe.metrumrg.com"
GHE_ORG <- "tech-solutions"
GHE_REPO <- "mrgvalidatetestreference"
GHE_MILESTONE <- "v0.6.0"
GHE_TAG <- "0.6.0"

STORY_RDS <- "stories_df.RDS"

OUTPUT_DIR <- "level2"

EXTRA_TESTS <- "inst/extra-tests"

#############################
# reference for test results
#############################

COMMIT_REF <- "15430d3a6d77adc2e955d4f1e22209e63e6d7f60"
GHE_COMMIT_REF <- "0817f579b2858cfce24975776f83302bff9162ba"

TEST_DF_ROWS <- 169
TEST_DF_COLS <- 6
TEST_DF_ROWS_EXTRA_TESTS <- 173

STORIES_DF_ROWS <- 5
STORIES_DF_COLS <- 5

VAL_TITLE <- "Validation Testing"
VAL_BOILER <- '
## Scope

The purpose of this Validation Testing document is to define the conditions for
test execution. All tests are specified and linked to release candidate user
stories as numbered issues in the Requirements Specification-Validation Plan
document.

----------------

## Test locations

Tests are in the following location

1. `tests/testthat`
'

REQ_TITLE <- "# Requirements Specification"
REQ_BOILER <- '
## Scope

The purpose of this document is to define specific criteria for each testing
task.  Testing shall be conducted in accordance to the requirements within this
document. The Requirement Specifications ensure that each requirement is tested.
'

MAT_TITLE <- "# Traceability Matrix"
MAT_BOILER <- '
## Scope

This traceability matrix links product risk, test names, and test results to
specific user stories for the proposed software release. User stories, including
requirements and test specifications are listed in the Requirements Specification
and Validation Plan.
'

