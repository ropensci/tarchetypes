#' @title Group the rows of a data frame into groups
#'   of a given size.
#' @export
#' @description Create a target that outputs a grouped data frame
#'   for downstream dynamic branching. Row groups have
#'   the number of rows you supply to `size` (plus the remainder
#'   in a group of its own, if applicable.) The total number of groups
#'   varies.
#' @return A target object to generate a grouped data frame
#'   to allows downstream dynamic targets to branch over the
#'   groups of rows.
#'   Target objects represent skippable steps of the analysis pipeline
#'   as described at <https://books.ropensci.org/targets/>.
#'   Please see the design specification at
#'   <https://books.ropensci.org/targets-design/>
#'   to learn about the structure and composition of target objects.
#' @inheritParams targets::tar_target
#' @param size Positive integer, maximum number of rows in each group.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   produce_data <- function() {
#'     expand.grid(var1 = c("a", "b"), var2 = c("c", "d"), rep = c(1, 2, 3))
#'   }
#'   list(
#'     tarchetypes::tar_group_size(data, produce_data(), size = 7),
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
tar_group_size <- function(
  name,
  command,
  size,
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
  size <- as.integer(size)
  assert_nonempty(size, "size must be nonempty.")
  assert_scalar(size, "size must have length 1.")
  assert_dbl(size, "size must be numeric.")
  assert_ge(size, 1L, "size must be at least 1.")
  command <- substitute(command)
  command <- tar_group_size_command(command, size, tidy_eval)
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

tar_group_size_command <- function(command, size, tidy_eval) {
  envir <- targets::tar_option_get("envir")
  assert_envir(envir)
  command <- tidy_eval(command, envir, tidy_eval)
  fun <- call_ns("tarchetypes", "tar_group_size_run")
  as.call(list(fun, data = command, size))
}

#' @title Generate a grouped data frame within tar_group_size()
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @param data A data frame to group.
#' @param size Maximum number of rows in each group.
tar_group_size_run <- function(data, size) {
  assert_df(data, "tar_group_size() output must be a data frame.")
  max <- as.integer(nrow(data) / size) + 1L
  data$tar_group <- rep(seq_len(max), each = size)[seq_len(nrow(data))]
  data
}