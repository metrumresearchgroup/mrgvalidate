
#' Read test results from CSV files in a directory.
#' @param test_output_dir path to a directory containing test output files. Each
#'   CSV file should have an accompanying JSON file. See [input_formats].
#' @return List of two elements, a tibble with test result ("results") and
#'   metadata information for each test result set ("info").
#' @importFrom glue glue
#' @importFrom purrr map reduce
#' @importFrom readr read_csv cols
#' @importFrom stringr str_replace fixed
#' @importFrom tibble add_column
#' @importFrom jsonlite read_json
#' @keywords internal
read_csv_test_results <- function(test_output_dir) {
  csv_files <- list.files(test_output_dir, pattern = "\\.csv$", full.names = TRUE)
  json_files <- str_replace(csv_files, "\\.csv$", ".json")

  for (.j in json_files) {
    if (!fs::file_exists(.j)) {
      rlang::abort(
        glue("Expected JSON info file does not exist: {.j}"),
        "mrgvalidate_missing_result_info")
    }
  }

  results <- map(
    csv_files,
    ~{
      read_csv(.x,
               col_types = cols(TestName = "c", TestId = "c",
                                passed = "i", failed = "i")) %>%
        add_column(
          result_file = str_replace(basename(.x), fixed(".csv"), ""),
          .before = TRUE)
    }) %>%
    reduce(rbind)

  info <- map(json_files, jsonlite::read_json)
  names(info) <- str_replace(basename(csv_files), fixed(".csv"), "")
  return(list(results = results, info = info))
}

#' Read manual test results.
#' @param test_output_dir Directory that contains test subdirectories named by
#'   test ID.
#' @return Tibble with TestId and content columns. In the content, links to the
#'   test's assets subdirectory are switched from relative to absolute paths.
#' @importFrom purrr map_dfr
#' @importFrom readr read_file
#' @importFrom stringr str_replace str_replace_all fixed
#' @importFrom tidyr extract
#' @keywords internal
read_manual_test_results <- function(test_output_dir) {
  # Drop trailing slash to avoid ugly "//" in returned value (e.g.,
  # ".../foo//MAN-VSC-001").
  test_output_dir <- str_replace(test_output_dir, "/$", "")
  testdirs <- list.files(test_output_dir, pattern = "^MAN-[A-Z]+-[0-9]+",
                         full.names = TRUE)
  results <- map_dfr(testdirs, function(.dir) {
    .id <- basename(.dir)
    abs_asset_path <- file.path(.dir, paste0("assets_", .id))
    content <- read_file(file.path(.dir, "test.md")) %>%
      str_replace_all(fixed(paste0("(assets_", .id)),
                      paste0("(", abs_asset_path))
    list(TestId = .id, content = content)
  })

  results %>%
    extract(.data$content, "date", "\n\\* date: +(.*)",
            remove = FALSE) %>%
    extract(.data$content, "TestName", "## MAN-[A-Z]+-[0-9]+: +(.*)",
            remove = FALSE)
}
