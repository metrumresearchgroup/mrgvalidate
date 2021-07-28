
#' Read test results from CSV files in a directory.
#' @param test_output_dir path to a directory containing test output files. Each
#'   CSV file should have an accompanying JSON file. See [input-formats].
#' @return List of two elements, a tibble with test result ("results") and
#'   metadata information for each test result set ("info").
#' @importFrom glue glue
#' @importFrom purrr map reduce
#' @importFrom readr read_csv
#' @importFrom stringr str_replace fixed
#' @importFrom tibble add_column
#' @export
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
               col_types = cols(test_name = "c", test_tag = "c",
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
