#' @title Batched replication over upstream batched targets.
#' @export
#' @family branching
#' @description [tar_map_reps()] performs batched replication similar
#'   to [tar_rep()], except it iterates over previously generated
#'   batches and reps created by upstream data frame or list
#'   targets through [tar_rep()].
#' @details If you supply multiple upstream [tar_rep()] targets,
#'   those targets must all have the same
#'   number of batches and reps per batch. Those upstream targets
#'   must return either data frames or lists.
#'   List targets should use
#'   `iteration = "list"` in [tar_rep()].
#' @return A new target object to perform batched replication.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target
#' @param ... Symbols, one or more names of upstream [tar_rep()] targets.
#'   If you supply more than one, those targets must all have the same
#'   number of batches and reps per batch. And they must all return either
#'   data frames or lists.
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
