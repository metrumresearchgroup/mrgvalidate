
############################
# reading and writing files
############################

#' Read in the csv will results from all the tests
#' @importFrom readr read_csv cols
#' @importFrom dplyr rename mutate_at
#' @importFrom rlang .data
#' @param test_path File path to file containing test results
read_test_df <- function(test_path = ALL_TESTS) {
  tst <- read_csv(test_path, col_types = cols())
  tst <- rename(tst,test_name = .data$tests, test_file = .data$file, number=.data$nb)
  tst <- mutate_at(tst,c("test_name","test_file"), .funs=list(trimws))
  return(tst)
}


get_output_format <- function(pdf_document, word_document) {
  if (all(c(pdf_document, word_document))) {
    out <- c("pdf_document", "word_document")
  } else if (isTRUE(pdf_document)) {
    out <- "pdf_document"
  } else if (isTRUE(word_document)) {
    out <- "word_document"
  }
  return(out)
}


#####################
# formatting helpers
#####################

#' Process a single issue
#' @importFrom purrr flatten_chr map_df
#' @importFrom tibble tibble
#' @param txt The raw text scraped from the body of the Github issue
proc_issue <- function(txt) {
  sp <- sp_sections(txt)
  ts <- strsplit(sp[2], "[\n\r]+") %>% flatten_chr %>% rm_blank
  story <- trimws(gsub("^ *[\r\n]+ *", "", sp[1]))

  file_start <- which(grepl("test-.*\\.R$", ts))
  test_start <- file_start+1
  test_end <- c((file_start - 1)[-1],length(ts))

  if (length(file_start) == 0) {
    no_tests_text <- paste0("\n\n_", NO_TESTS_STRING, " - ", paste(ts, collapse = " "), "_")
    return(tibble(test_file = NO_TESTS_STRING, test_name = NO_TESTS_STRING, story = paste0(story, no_tests_text)))
  }

  labs <- rm_h(ts[file_start])
  .lab <- basename(labs)
  dd <- map_df(seq_along(file_start), function(i) {
    se <- seq(test_start[i],test_end[i])
    tibble(test_file = .lab[i], test_name = rm_s(rm_h(ts[se])), story = story)
  })
  dd
}


#' format a story for inclusion in output documents
#' @importFrom dplyr select
#' @importFrom knitr kable
#' @importFrom rlang .data
#' @param x A single row from the [process_stories()] df
format_spec <- function(x) {
  header <- paste0("## ", x$StoryId[[1]], " ", x$StoryName[[1]], "\n")
  bod <- gsub("\r", "", x$StoryDescription[[1]])
  risk <- gsub("risk: ", "", x$ProductRisk[[1]])
  tst <- x %>%
    mutate(count = .data$passed + .data$failed) %>%
    select(`test ID` = .data$TestId, `test name` = .data$test_name,
           count = .data$count)
  tst_tab <- knitr::kable(tst, format="markdown")
  c(header, "**Product risk**: ", risk, "\n\n", "**Story**\n", bod, "\n\n", "**Tests**\n\n", tst_tab)
}

######################################
# helper functions for string cleanup
######################################

#' remove leading hyphen
#' @param x input to clean
rm_h <- function(x) trimws(gsub("^ *- *", "",x))

#' remove leading star/bullet
#' @param x input to clean
rm_s <- function(x) trimws(gsub("^ *\\* *", "",x))

#' drop blanks
#' @param x input to clean
rm_blank <- function(x) x[!grepl("^\\s*$",x)]

#' split body by Test and Summary
#' @param x input to clean
#' @importFrom purrr flatten_chr
sp_sections <- function(x) {
  x <- strsplit(x,"(\\# *Test\\w*|\\# *Summ\\w*)") %>% flatten_chr
  x <- rm_blank(x)
  if(length(x)!=2) {
    stop(paste("sp_sections() failed by splitting into wrong number of sections. Please see 'Basic Usage' vignette and check formatting in the following issue: ", x, collapse = " -------- "))
  }
  x
}

