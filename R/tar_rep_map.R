#' @title Batched computation downstream of [tar_rep()]
#' @export
#' @family branching
#' @description Batching is important for optimizing the efficiency
#'   of heavily dynamically-branched workflows:
#'   <https://books.ropensci.org/targets/dynamic.html#batching>.
#'   [tar_rep_map()] uses dynamic branching to iterate
#'   over the batches and reps of existing upstream targets.
#' @return A new target object to perform batched computation.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target
#' @param ... Symbols to name one or more upstream batched targets
#'   created by [tar_rep()].
#'   If you supply more than one such target, all those targets must have the
#'   same number of batches and reps per batch. And they must all return
#'   either data frames or lists. List targets must use `iteration = "list"`
#'   in [tar_rep()].
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   list(
#'     tarchetypes::tar_rep(
#'       data1,
#'       data.frame(value = rnorm(1)),
#'       batches = 2,
#'       reps = 3
#'     ),
#'     tarchetypes::tar_rep(
#'       data2,
#'       list(value = rnorm(1)),
#'       batches = 2, reps = 3,
#'       iteration = "list" # List iteration is important for batched lists.
#'     ),
#'     tarchetypes::tar_rep_map(
#'       aggregate,
#'       data.frame(value = data1$value + data2$value),
#'       data1,
#'       data2
#'     )
#'   )
#' })
#' targets::tar_make()
#' targets::tar_read(aggregate)
#' })
#' }
tar_rep_map <- function(
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
  tar_rep_map_raw(
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
