#' @title Group a data frame target with `tidyselect` semantics.
#' @export
#' @description Create a target that outputs a grouped data frame
#'   with `dplyr::group_by()` and `targets::tar_group()`.
#'   Unlike `tar_group_by()`, `tar_group_select()`
#'   expects you to select grouping variables using `tidyselect` semantics.
#'   Downstream dynamic branching targets will iterate over the groups of rows.
#' @return A target object to generate a grouped data frame
#'   to allows downstream dynamic targets to branch over the
#'   groups of rows.
#'   Target objects represent skippable steps of the analysis pipeline
#'   as described at <https://books.ropensci.org/targets/>.
#'   Please see the design specification at
#'   <https://books.ropensci.org/targets-design/>
#'   to learn about the structure and composition of target objects.
#' @inheritParams targets::tar_target
#' @param by Tidyselect semantics to specify variables to group over.
#'   Alternatively, you can supply a character vector.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   produce_data <- function() {
#'     expand.grid(var1 = c("a", "b"), var2 = c("c", "d"), rep = c(1, 2, 3))
#'   }
#'   list(
#'     tarchetypes::tar_group_select(data, produce_data(), starts_with("var")),
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
tar_group_select <- function(
  name,
  command,
  by = NULL,
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
  by <- as.expression(substitute(by))
  assert_nonempty(by[[1]], "`by` in tar_group_select() must be nonempty.")
  command <- tar_group_select_command(substitute(command), by, tidy_eval)
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

tar_group_select_command <- function(command, by, tidy_eval) {
  envir <- targets::tar_option_get("envir")
  assert_envir(envir)
  command <- tidy_eval(command, envir, tidy_eval)
  fun <- call_ns("tarchetypes", "tar_group_select_run")
  as.call(list(fun, data = command, by = by))
}

#' @title Generate a grouped data frame within tar_group_select()
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @param data A data frame to group.
#' @param by Nonempty character vector of names of variables to group by.
tar_group_select_run <- function(data, by) {
  assert_df(data, "tar_group_select() output must be a data frame.")
  by <- eval_tidyselect(by[[1]], colnames(data))
  tar_group_by_run(data = data, by = by)
}
