#' @title Batched computation with dynamic branching.
#' @export
#' @family branching
#' @description Batching is important for optimizing the efficiency
#'   of heavily dynamically-branched workflows:
#'   <https://books.ropensci.org/targets/dynamic.html#batching>.
#'   [tar_map_reps()] uses dynamic branching to iterate
#'   over the batches and reps of existing upstream targets.
#' @return A new target object to perform batched computation.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target
#' @param ... Symbols to name one or more upstream batched targets.
#'   Each such target must be a list or data frame.
#'   and it must have its own batching structure.
#'   For upstream list targets, each element or dynamic branch is a batch,
#'   and each batch element is a rep. Batches and reps must all be lists,
#'   For upstream data frame targets, each batch is a dynamic branch
#'   or group (e.g. from `targets::tar_group()` or [tar_group_by()]),
#'   and rows within each branch are partitioned into reps.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   tar_rep(data1, data.frame(value = rnorm(1)), batches = 2, reps = 3),
#'   tar_rep(data2, data.frame(value = rnorm(2)), batches = 2, reps = 3),
#'   tar_map_reps(
#'     analysis,
#'     data.frame(
#'       mean_data1 = mean(data1$value),
#'       mean_data2 = mean(data2$value),
#'       n_data1 = nrow(data1),
#'       n_data2 = nrow(data2)
#'     ),
#'     data1,
#'     data2
#'   )
#' })
#' targets::tar_make()
#' targets::tar_read(analysis)
#' })
#' }
tar_map_reps <- function(
  name,
  command,
  ...,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = targets::tar_option_get("format"),
  iteration = targets::tar_option_get("iteration"),
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
  name <- deparse_language(substitute(name))
  envir <- targets::tar_option_get("envir")
  command <- tar_tidy_eval(substitute(command), envir, tidy_eval)
  targets <- as.character(match.call(expand.dots = FALSE)$...)
  tar_map_reps_raw(
    name = name,
    command = command,
    targets = targets,
    packages = packages,
    library = library,
    format = format,
    iteration = iteration,
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
