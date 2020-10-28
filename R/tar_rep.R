#' @title Batched replication with dynamic branching.
#' @description Shorthand for a pattern that replicates a command
#'   using batches. Batches reduce the number of targets
#'   and thus reduce overhead.
#' @details `tar_rep()` and `tar_rep_raw()` each create two targets:
#'   an upstream local stem
#'   with an integer vector of batch ids, and a downstream pattern
#'   that maps over the batch ids. (Thus, each batch is a branch.)
#'   Each batch/branch replicates the command a certain number of times.
#'   The command must return
#'   a list or data frame because `tar_rep()` will try to append
#'   new elements/columns `tar_batch` and `tar_rep` to the output
#'   to denote the batch and rep-within-batch IDs, respectively.
#'
#'   Both batches and reps within each batch
#'   are aggregated according to the method you specify
#'   in the `iteration` argument. If `"list"`, reps and batches
#'   are aggregated with `list()`. If `"vector"`,
#'   then `vctrs::vec_c()`. If `"group"`, then `vctrs::vec_rbind()`.
#' @export
#' @inheritParams targets::tar_target
#' @return A list of two targets, one upstream and one downstream.
#'   The upstream target returns a numeric index of batch ids,
#'   and the downstream one dynamically maps over the batch ids
#'   to run the command multiple times. The command must return
#'   a list or data frame because `tar_rep()` will try to append
#'   new elements/columns `tar_batch` and `tar_rep` to the output
#'   to denote the batch and rep-within-batch IDs, respectively.
#'
#'   `tar_read(your_target)` (on the downstream target with the actual work)
#'   will return a list of lists, where the outer list has one element per
#'   batch and each inner list has one element per rep within batch.
#'   To un-batch this nested list, call
#'   `tar_read(your_target, recursive = FALSE)`.
#' @param command R code to run multiple times. Must return a list or
#'   data frame because `tar_rep()` will try to append new elements/columns
#'   `tar_batch` and `tar_rep` to the output to denote the batch
#'   and rep-within-batch IDs, respectively.
#' @param batches Number of batches. This is also the number of dynamic
#'   branches created during `tar_make()`.
#' @param reps Number of replications in each batch. The total number
#'   of replications is `batches * reps`.
#' @param tidy_eval Whether to invoke tidy evaluation
#'   (e.g. the `!!` operator from `rlang`) as soon as the target is defined
#'   (before `tar_make()`). Applies to the `command` argument.
#' @param iteration Character of length 1, name of the iteration mode
#'   of the target. Choices:
#'   * `"vector"`: branching happens with `vectors::vec_slice()` and
#'     aggregation happens with `vctrs::vec_c()`.
#'   * `"list"`, branching happens with `[[]]` and aggregation happens with
#'     `list()`. In the case of `tar_batch()`, `tar_read(your_target)`
#'     will return a list of lists, where the outer list has one element per
#'     batch and each inner list has one element per rep within batch.
#'     To un-batch this nested list, call
#'     `tar_read(your_target, recursive = FALSE)`.
#'   * `"group"`: `dplyr::group_by()`-like functionality to branch over
#'     subsets of a data frame. The target's return value must be a data
#'     frame with a special `tar_group` column of consecutive integers
#'     from 1 through the number of groups. Each integer designates a group,
#'     and a branch is created for each collection of rows in a group.
#'     See the [tar_group()] function to see how you can
#'     create the special `tar_group` column with `dplyr::group_by()`.
#' @examples
#' targets::tar_dir({
#' targets::tar_script({
#'   targets::tar_pipeline(
#'     tarchetypes::tar_rep(
#'       x,
#'       data.frame(x = sample.int(1e4, 2)),
#'       batches = 2,
#'       reps = 3
#'     )
#'   )
#' })
#' targets::tar_make(callr_function = NULL)
#' targets::tar_read(x)
#' })
tar_rep <- function(
  name,
  command,
  batches = 1,
  reps = 1,
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
  command <- tidy_eval(substitute(command), envir, tidy_eval)
  tar_rep_raw(
    name = name,
    command = command,
    batches = batches,
    reps = reps,
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

tar_rep_batch <- function(
  name_batch,
  batches,
  error,
  memory,
  garbage_collection,
  priority,
  cue
) {
  targets::tar_target_raw(
    name = name_batch,
    command = tar_rep_command_batch(batches),
    packages = character(0),
    format = "rds",
    iteration = "vector",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = "main",
    priority = priority,
    storage = "main",
    retrieval = "main",
    cue = cue
  )
}

tar_rep_target <- function(
  name,
  name_batch,
  command,
  batches,
  reps,
  packages,
  library,
  format,
  iteration,
  error,
  memory,
  garbage_collection,
  deployment,
  priority,
  resources,
  storage,
  retrieval,
  cue
) {
  targets::tar_target_raw(
    name = name,
    command = tar_rep_command_target(command, name_batch, reps, iteration),
    pattern = tar_rep_pattern(name_batch),
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

tar_rep_command_batch <- function(batches) {
  as.expression(substitute(seq_len(x), env = list(x = batches)))
}

tar_rep_command_target <- function(command, name_batch, reps, iteration) {
  out <- substitute(
    tarchetypes::tar_rep_run(
      command = command,
      batch = batch,
      reps = reps,
      iteration = iteration
    ),
    env = list(
      command = command,
      batch = rlang::sym(name_batch),
      reps = reps,
      iteration = iteration
    )
  )
  as.expression(out)
}

tar_rep_pattern <- function(name_batch) {
  substitute(map(x), env = list(x = rlang::sym(name_batch)))
}

#' @title Run a batch in a `tar_rep()` archetype.
#' @description Internal function needed for `tar_rep()`.
#'   Users should not invoke it directly.
#' @export
#' @keywords internal
#' @return Aggregated results of multiple executions of the command.
#' @param command Expression object, command to replicate.
#' @param batch Numeric, batch index.
#' @param reps Numeric, number of reps per batch.
#' @param iteration Character, iteration method.
tar_rep_run <- function(command, batch, reps, iteration) {
  expr <- substitute(command)
  envir <- parent.frame()
  switch(
    iteration,
    list = tar_rep_map(expr, envir, batch, reps),
    vector = do.call(vctrs::vec_c, tar_rep_map(expr, envir, batch, reps)),
    group = do.call(vctrs::vec_rbind, tar_rep_map(expr, envir, batch, reps)),
    throw_validate("unsupported iteration method")
  )
}

tar_rep_map <- function(expr, envir, batch, reps) {
  lapply(
    seq_len(reps),
    tar_rep_rep,
    expr = expr,
    envir = envir,
    batch = batch
  )
}

tar_rep_rep <- function(expr, envir, batch, rep) {
  out <- eval(expr, envir = envir)
  if (!is.list(out)) {
    throw_run("tar_rep() targets must return lists or data frames.")
  }
  out$tar_batch <- as.integer(batch)
  out$tar_rep <- as.integer(rep)
  out
}
