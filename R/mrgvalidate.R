#' Create validation documents from Stories, Requirements, and test outputs
#'
#' The `mrgvalidate` package ingests structured data including text describing
#' user stories and technical requirements, and testing outputs, and creates
#' validation documents by joining these together and inserting them into
#' pre-defined markdown templates that can then be automatically rendered into
#' `.docx` format. Specifications for input data can be found on the
#' [input_formats] help page. The companion `mrgvalprep` package contains
#' helpers for formatting input data to the specification in [input_formats].
#' @importFrom rlang abort warn inform %||%
#' @importFrom lifecycle badge
#' @name mrgvalidate
NULL
