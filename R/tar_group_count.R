#' @title Group the rows of a data frame into a given number groups
#' @export
#' @family Grouped data frame targets
#' @description Create a target that outputs a grouped data frame
#'   for downstream dynamic branching. Set the maximum
#'   number of groups using `count`. The number of rows per group
#'   varies but is approximately uniform.
#' @return A target object to generate a grouped data frame
#'   to allows downstream dynamic targets to branch over the
#'   groups of rows.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target
#' @param count Positive integer, maximum number of row groups
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   produce_data <- function() {
#'     expand.grid(var1 = c("a", "b"), var2 = c("c", "d"), rep = c(1, 2, 3))
#'   }
#'   list(
#'     tarchetypes::tar_group_count(data, produce_data(), count = 2),
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
tar_group_count <- function(
  name,
  command,
  count,
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
  targets::tar_assert_package("dplyr")
  name <- targets::tar_deparse_language(substitute(name))
  targets::tar_assert_lgl(tidy_eval, "tidy_eval must be logical.")
  count <- as.integer(count)
  targets::tar_assert_nonempty(count, "count must be nonempty.")
  targets::tar_assert_scalar(count, "count must have length 1.")
  targets::tar_assert_dbl(count, "count must be numeric.")
  targets::tar_assert_ge(count, 1L, "count must be at least 1.")
  command <- substitute(command)
  command <- tar_group_count_command(command, count, tidy_eval)
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

tar_group_count_command <- function(command, count, tidy_eval) {
  envir <- targets::tar_option_get("envir")
  targets::tar_assert_envir(envir)
  command <- targets::tar_tidy_eval(command, envir, tidy_eval)
  fun <- call_ns("tarchetypes", "tar_group_count_run")
  as.call(list(fun, data = command, count))
}

#' @title Generate a grouped data frame within `tar_group_count()`.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @inheritParams tar_group_count_index
tar_group_count_run <- function(data, count) {
  data$tar_group <- tar_group_count_index(data = data, count = count)
  data
}

#' @title Generate the tar_group column for `tar_group_count()`.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @param data A data frame to group.
#' @param count Maximum number of groups.
tar_group_count_index <- function(data, count) {
  targets::tar_assert_df(data, "tar_group_count() output must be a data frame.")
  count <- min(count, nrow(data))
  if_any(
    count > 1L,
    as.integer(cut(seq_len(nrow(data)), breaks = count)),
    rep(1L, nrow(data))
  )
}
