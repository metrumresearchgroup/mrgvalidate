#' Get the SHA matching a git reference
#'
#' The `remotes` package provides nice functionality for handling a variety of
#' reference types, but many of the necessary functions are not exported.
#' Because we only need to consider two types, copying all that machinery is
#' probably more trouble than maintaining our own lightweight implementation.
#'
#' @param repo repository address in the format `username/repo`
#' @param ref name of either a tag or a branch
#' @param type type of reference for `ref`
#'
#' @return the SHA for `ref`
#' @keywords internal
get_sha <- function(repo, ref, type = c("tag", "branch")) {
  checkmate::assert_string(repo)
  checkmate::assert_string(ref)

  type <- match.arg(type)

  query <- switch(
    type,
    tag = "tags",
    branch = "git/refs/heads/:ref"
  )

  response <- gh::gh(
    glue::glue("/repos/{repo}/{query}"),
    ref = ref
  )

  accessor <- "object"
  if (type == "tag") {
    response <- get_sha_from_tags(response, ref)
    accessor <- "commit"
  }

  response[[accessor]][["sha"]]
}

#' Get the commit SHA matching a tag name
#'
#' There is no endpoint to get the commit SHA directly from a tag. Instead, we
#' can return the matching element from the set of tags.
#'
#' @param tags a list of tags, as returned by the `tags` API endpoint
#' @param name the tag name
#'
#' @return the SHA of the commit associated with tag `name`
#' @keywords internal
get_sha_from_tags <- function(tags, name) {
  checkmate::assert_string(name)

  tags %>%
    purrr::map(`[`, c("name", "commit")) %>%
    purrr::keep(function(x) x[[1]] == name) %>%
    purrr::flatten()
}
