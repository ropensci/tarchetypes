#' @title Batched replication with dynamic branching.
#' @description Batching is important for optimizing the efficiency
#'   of heavily dynamically-branched workflows:
#'   <https://books.ropensci.org/targets/dynamic.html#batching>.
#'   [tar_rep()] replicates a command in strategically sized batches.
#' @export
#' @family branching
#' @details `tar_rep()` and `tar_rep_raw()` each create two targets:
#'   an upstream local stem
#'   with an integer vector of batch ids, and a downstream pattern
#'   that maps over the batch ids. (Thus, each batch is a branch.)
#'   Each batch/branch replicates the command a certain number of times.
#'   If the command returns a list or data frame, then
#'   the targets from `tar_rep()` will try to append new elements/columns
#'   `tar_batch`, `tar_rep`, and `tar_seed` to the output
#'   to denote the batch, rep-within-batch index, and rep-specific seed,
#'   respectively.
#'
#'   Both batches and reps within each batch
#'   are aggregated according to the method you specify
#'   in the `iteration` argument. If `"list"`, reps and batches
#'   are aggregated with `list()`. If `"vector"`,
#'   then `vctrs::vec_c()`. If `"group"`, then `vctrs::vec_rbind()`.
#' @inheritSection tar_map Target objects
#' @section Replicate-specific seeds:
#'   In ordinary pipelines, each target has its own unique deterministic
#'   pseudo-random number generator seed derived from its target name.
#'   In batched replicate, however, each batch is a target with multiple
#'   replicate within that batch. That is why [tar_rep()]
#'   and friends give each *replicate* its own unique seed.
#'   Each replicate-specific seed is created
#'   based on the dynamic parent target name,
#'   `tar_option_get("seed")` (for `targets` version 0.13.5.9000 and above),
#'   batch index, and rep-within-batch index.
#'   The seed is set just before the replicate runs.
#'   Replicate-specific seeds are invariant to batching structure.
#'   In other words,
#'   `tar_rep(name = x, command = rnorm(1), batches = 100, reps = 1, ...)`
#'   produces the same numerical output as
#'   `tar_rep(name = x, command = rnorm(1), batches = 10, reps = 10, ...)`
#'   (but with different batch names).
#'   Other target factories with this seed scheme are [tar_rep2()],
#'   [tar_map_rep()], [tar_map2_count()], [tar_map2_size()],
#'   and [tar_render_rep()].
#'   For the `tar_map2_*()` functions,
#'   it is possible to manually supply your own seeds
#'   through the `command1` argument and then invoke them in your
#'   custom code for `command2` (`set.seed()`, `withr::with_seed`,
#'   or `withr::local_seed()`). For [tar_render_rep()],
#'   custom seeds can be supplied to the `params` argument
#'   and then invoked in the individual R Markdown reports.
#'   Likewise with [tar_quarto_rep()] and the `execute_params` argument.
#' @return A list of two targets, one upstream and one downstream.
#'   The upstream target returns a numeric index of batch ids,
#'   and the downstream one dynamically maps over the batch ids
#'   to run the command multiple times.
#'   If the command returns a list or data frame, then
#'   the targets from `tar_rep()` will try to append new elements/columns
#'   `tar_batch` and `tar_rep` to the output
#'   to denote the batch and rep-within-batch IDs, respectively.
#'   See the "Target objects" section for background.
#'
#'   `tar_read(your_target)` (on the downstream target with the actual work)
#'   will return a list of lists, where the outer list has one element per
#'   batch and each inner list has one element per rep within batch.
#'   To un-batch this nested list, call
#'   `tar_read(your_target, recursive = FALSE)`.
#' @inheritParams targets::tar_target
#' @param command R code to run multiple times. Must return a list or
#'   data frame because `tar_rep()` will try to append new elements/columns
#'   `tar_batch` and `tar_rep` to the output to denote the batch
#'   and rep-within-batch IDs, respectively.
#' @param batches Number of batches. This is also the number of dynamic
#'   branches created during `tar_make()`.
#' @param reps Number of replications in each batch. The total number
#'   of replications is `batches * reps`.
#' @param rep_workers Positive integer of length 1, number of local R
#'   processes to use to run reps within batches in parallel. If 1,
#'   then reps are run sequentially within each batch. If greater than 1,
#'   then reps within batch are run in parallel using workers
#'   created with `future::plan(future.callr::callr, workers = rep_workers)`
#'   and invoked with `furrr::future_map()`.
#' @param tidy_eval Whether to invoke tidy evaluation
#'   (e.g. the `!!` operator from `rlang`) as soon as the target is defined
#'   (before `tar_make()`). Applies to the `command` argument.
#' @param iteration Character of length 1, name of the iteration mode
#'   of the target. Choices:
#'   * `"vector"`: branching happens with `vectors::vec_slice()` and
#'     aggregation happens with `vctrs::vec_c()`.
#'   * `"list"`, branching happens with `[[]]` and aggregation happens with
#'     `list()`. In the case of list iteration, `tar_read(your_target)`
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
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   list(
#'     tarchetypes::tar_rep(
#'       x,
#'       data.frame(x = sample.int(1e4, 2)),
#'       batches = 2,
#'       reps = 3
#'     )
#'   )
#' })
#' targets::tar_make()
#' targets::tar_read(x)
#' })
#' }
tar_rep <- function(
  name,
  command,
  batches = 1,
  reps = 1,
  rep_workers = 1,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = targets::tar_option_get("format"),
  repository = targets::tar_option_get("repository"),
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
  name <- targets::tar_deparse_language(substitute(name))
  envir <- targets::tar_option_get("envir")
  command <- targets::tar_tidy_eval(substitute(command), envir, tidy_eval)
  tar_rep_raw(
    name = name,
    command = command,
    batches = batches,
    reps = reps,
    rep_workers = rep_workers,
    packages = packages,
    library = library,
    format = format,
    repository = repository,
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
