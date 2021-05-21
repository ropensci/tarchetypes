#' @title Batched computation downstream of [tar_rep()] (raw version)
#' @export
#' @family branching
#' @description Batching is important for optimizing the efficiency
#'   of heavily dynamically-branched workflows:
#'   <https://books.ropensci.org/targets/dynamic.html#batching>.
#'   `tar_rep_map_raw()`
#'   is just like [tar_rep_map()] except it accepts a character
#'   of length 1 for `name`, a language object for `command`,
#'   and a character vector of the names of the upstream batched targets.
#' @return A new target object to perform batched computation
#'   downstream of [tar_rep()].
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target
#' @param targets Character vector of names of upstream batched targets
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
#'     tarchetypes::tar_rep_map_raw(
#'       "aggregate",
#'       quote(data.frame(value = data1$value + data2$value)),
#'       targets = c("data1", "data2")
#'     )
#'   )
#' })
#' targets::tar_make()
#' targets::tar_read(aggregate)
#' })
#' }
tar_rep_map_raw <- function(
  name,
  command,
  targets,
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
  assert_chr(
    targets,
    "targets in tar_rep_map_raw() must be a character vector."
  )
  assert_nonempty(targets, "targets argument must be nonempty.")
  assert_nzchar(targets, "targets argument must not have 0-length elements.")
  command <- tar_raw_command(name, command)
  command <- tar_rep_map_command(
    command = command,
    targets = targets,
    iteration = iteration
  )
  pattern <- call_function("map", lapply(targets, as.symbol))
  targets::tar_target_raw(
    name = name,
    command = command,
    pattern = pattern,
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

tar_rep_map_command <- function(command, targets, iteration) {
  batches <- lapply(targets, as.symbol)
  names(batches) <- targets
  substitute(
    tarchetypes::tar_rep_map_run(
      command = command,
      batches = batches,
      iteration = iteration
    ),
    env = list(
      command = command,
      batches = call_list(batches),
      iteration = iteration
    )
  )
}

#' @title Run [tar_rep_map()] batches.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return The result of batched replication.
#' @param command R expression, the command to run on each rep.
#' @param batches Named list of batch data to map over.
#' @param iteration Iteration method: `"list"`, `"vector"`, or `"group"`.
tar_rep_map_run <- function(command, batches, iteration) {
  command <- substitute(command)
  assert_batches(batches)
  reps <- batch_count_reps(batches[[1]])
  out <- map(
    seq_len(reps),
    tar_rep_map_run_rep,
    command = command,
    batches = batches
  )
  tar_rep_bind(out, iteration)
}

tar_rep_map_run_rep <- function(index, command, batches) {
  slice <- slice_batches(batches, index)
  out <- eval(command, envir = slice, enclos = targets::tar_envir())
  out$tar_batch <- slice[[1]]$tar_batch[1]
  out$tar_rep <- slice[[1]]$tar_rep[1]
  out
}

slice_batches <- function(batches, index) {
  out <- lapply(batches, slice_batch, index = index)
  names(out) <- names(batches)
  out
}

slice_batch <- function(batch, index) {
  UseMethod("slice_batch")
}

#' @export
slice_batch.list <- function(batch, index) {
  batch[[index]]
}

#' @export
slice_batch.data.frame <- function(batch, index) {
  batch[batch$tar_rep == index,, drop = FALSE] # nolint
}
