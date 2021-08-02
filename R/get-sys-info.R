#' Capture system and session info
#'
#' Captures system and session info, returns invisibly, and optionally writes to
#' JSON file for later use.
#'
#' @details
#' Structure of returned (and written) list is as follows:
#'
#' * `"date"` -- Date and time when function is called.
#' * `"executor"` -- String passed through from `executor` param.
#' * `"info"` -- Contains system and session info in sub-elements:
#'   * `"sys"` (Optional) -- System info: sysname, version, release, machine.
#'   * `"env_vars"` (Optional) -- Environment variables passed by the user, with
#'   their current values at time of function call.
#'   * `"session"` (Optional) -- Character vector with output from
#'   `sessionInfo()` call. Only present if `session = TRUE` was passed.
#'
#' @return Invisibly returns named list with the elements described in
#'   "Details". This list is also written to JSON if a path is provided to
#'   `out_path`.
#'
#' @param out_path File path for resulting JSON file. Will also return
#'   the R list invisibly. If `NULL`, only returns list and does not write to
#'   file.
#' @param executor Person who is calling the function, and presumably running
#'   the test suite in question. If `NULL`, will pull `Sys.getenv("USER")`.
#' @param env_vars Character vector of enviroment variables to check and record,
#'   if present. Each will be a key in the `[["info"]][["env_vars"]]` element of
#'   the resulting json. Any that are _not_ set at call time will be present with
#'   a value of `""`.
#' @param sys_info Logical indicating whether to run `Sys.info()` and store
#'   the results under `[["info"]][["sys"]]`. Defaults to `FALSE`.
#' @param session Logical indicating whether to run `SessionInfo()` and store
#'   the results under `[["info"]][["session"]]`. Defaults to `FALSE`.
#'
#' @examples
#' get_sys_info(env_vars = c("USER", "METWORX_VERSION"))
#'
#' @importFrom jsonlite toJSON
#' @importFrom purrr map
#' @export
get_sys_info <- function(out_path = NULL, executor = NULL, env_vars = NULL, sys_info = FALSE, session = FALSE) {
  checkmate::assert_string(out_path, null.ok = TRUE)
  checkmate::assert_string(executor, null.ok = TRUE)
  checkmate::assert_character(env_vars, null.ok = TRUE)
  checkmate::assert_logical(sys_info)
  checkmate::assert_logical(session)

  res <- list(
    date = as.character(Sys.time()),
    executor = executor %||% Sys.getenv("USER"),
    info = list()
  )

  if (!is.null(env_vars)) {
    res[["info"]][["env_vars"]] <- list()
    for (.ev in env_vars) {
      res[["info"]][["env_vars"]][[.ev]] <- Sys.getenv(.ev)
    }
  }

  if (isTRUE(sys_info)) {
    res[["info"]][["sys"]] <- as.list(Sys.info())[c("sysname", "version", "release", "machine")]
  }

  if (isTRUE(session)) {
    res[["info"]][["session"]] <- capture.output(print(sessionInfo()))
  }

  if(!is.null(out_path)) {
    writeLines(toJSON(res, pretty = TRUE, auto_unbox = TRUE), out_path)
  }
  return(invisible(res))
}
