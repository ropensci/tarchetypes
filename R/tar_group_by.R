#' @title Group a data frame target by one or more variables.
#' @export
#' @family Grouped data frame targets
#' @description Create a target that outputs a grouped data frame
#'   with `dplyr::group_by()` and `targets::tar_group()`. Downstream
#'   dynamic branching targets will iterate over the groups of rows.
#' @return A target object to generate a grouped data frame
#'   to allows downstream dynamic targets to branch over the
#'   groups of rows.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target
#' @param ... Symbols, variables in the output data frame to group by.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   produce_data <- function() {
#'     expand.grid(var1 = c("a", "b"), var2 = c("c", "d"), rep = c(1, 2, 3))
#'   }
#'   list(
#'     tarchetypes::tar_group_by(data, produce_data(), var1, var2),
#'     tar_target(group, data, pattern = map(data))
#'   )
#' })
#' targets::tar_make()
#' # Read the first row group:
#' targets::tar_read(group, branches = 1)
#' # Read the second row group:
#' targets::tar_read(group, branches = 2)
#' })
#' }
tar_group_by <- function(
  name,
  command,
  ...,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = targets::tar_option_get("format"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue")
) {
  assert_package("dplyr")
  name <- deparse_language(substitute(name))
  assert_lgl(tidy_eval, "tidy_eval must be logical.")
  by <- all.vars(substitute(list(...)), functions = FALSE)
  assert_nonempty(by, "no columns to group by.")
  command <- tar_group_by_command(substitute(command), by, tidy_eval)
  targets::tar_target_raw(
    name = name,
    command = command,
    packages = packages,
    library = library,
    format = format,
    iteration = "group",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
}

tar_group_by_command <- function(command, by, tidy_eval) {
  envir <- targets::tar_option_get("envir")
  assert_envir(envir)
  command <- tar_tidy_eval(command, envir, tidy_eval)
  fun <- call_ns("tarchetypes", "tar_group_by_run")
  as.call(list(fun, data = command, by = by))
}

#' @title Generate a grouped data frame within tar_group_by()
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @param data A data frame to group.
#' @param by Nonempty character vector of names of variables to group by.
tar_group_by_run <- function(data, by) {
  assert_df(data, "tar_group_by() output must be a data frame.")
  assert_in(by, colnames(data), "tar_group_by() columns must be in data.")
  expr <- quote(dplyr::group_by(data, !!!by_syms))
  by_syms <- as_symbols(by)
  envir <- environment()
  expr <- tar_tidy_eval(expr, envir = envir, TRUE)
  grouped <- eval(expr, envir = envir)
  targets::tar_group(grouped)
}
