#' @title Batched replication using dynamic branching.
#' @description Shorthand for a pattern that replicates a command
#'   using batches. Batches reduce the number of targets
#'   and thus reduce overhead.
#' @details `tar_batch()` and `tar_batch_raw()` each create two targets:
#'   an upstream local stem
#'   with an integer vector of batch ids, and a downstream pattern
#'   that maps over the batch ids. (Thus, each batch is a branch.)
#'   Each batch/branch replicates the command a certain number of times.
#'   The command must return
#'   a list or data frame because `tar_batch()` will try to append
#'   new elements/columns `tar_batch` and `tar_rep` to the output.
#'
#'   Both batches and reps within each batch
#'   are aggregated according to the method you specify
#'   in the `iteration` argument. If `"list"`, reps and batches
#'   are aggregated with `list()`. If `"vector"`,
#'   then `vctrs::vec_c()`. If `"group"`, then `vctrs::vec_rbind()`.
#'
#'   The output from running the downstream target has new elements
#'   `tar_batch`
#' @export
#' @inheritParams targets::tar_target
#' @return A list of two targets, one upstream and one downstream.
#'   The upstream target returns a numeric index of batch ids,
#'   and the downstream one dynamically maps over the batch ids
#'   to run the command multiple times. The command must return
#'   a list or data frame because `tar_batch()` will try to append
#'   new elements/columns `tar_batch` and `tar_rep` to the output.
#' @param command R code to run multiple times. Must return a list or
#'   data frame because `tar_batch()` will try to append new elements/columns
#'   `tar_batch` and `tar_rep` to the output.
#' @param batches Number of batches. This is also the number of dynamic
#'   branches created during `tar_make()`.
#' @param reps Number of replications in each batch. The total number
#'   of replications is `batches * reps`. 
#' @param tidy_eval Whether to invoke tidy evaluation
#'   (e.g. the `!!` operator from `rlang`) as soon as the target is defined
#'   (before `tar_make()`). Applies to the `command` argument.
#' @examples
#' \dontrun{
#' }
tar_batch <- function(
  name,
  command,
  batches = 1,
  reps = 1,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = targets::tar_option_get("format"),
  iteration = "list",
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  template = targets::tar_option_get("template"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue")
) {
  name <- deparse_language(substitute(name))
  envir <- targets::tar_option_get("envir")
  command <- tidy_eval(substitute(command), envir, tidy_eval)
  tar_batch_raw(
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
    deployment = deployment,
    priority = priority,
    template = template,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
}

#' @title Batched replication using dynamic branching
#'   (raw version).
#' @description Shorthand for a pattern that replicates a command
#'   using batches. Batches reduce the number of targets
#'   and thus reduce overhead.
#' @details `tar_batch()` and `tar_batch_raw` each create two targets:
#'   an upstream local stem
#'   with an integer vector of batch ids, and a downstream pattern
#'   that maps over the batch ids. (Thus, each batch is a branch.)
#'   Each batch/branch replicates the command a certain number of times.
#'
#'   Both batches and reps within each batch
#'   are aggregated according to the method you specify
#'   in the `iteration` argument. If `"list"`, reps and batches
#'   are aggregated with `list()`. If `"vector"`,
#'   then `vctrs::vec_c()`. If `"group"`, then `vctrs::vec_rbind()`.
#' @export
#' @inheritParams targets::tar_target_raw
#' @return A list of two targets, one upstream and one downstream.
#'   The upstream one does some work and returns some file paths,
#'   and the downstream target is a pattern that applies `format = "file"`.
#' @param command Expression object with code to run multiple times.
#'   Must return a list or data frame when evaluated.
#' @param batches Number of batches. This is also the number of dynamic
#'   branches created during `tar_make()`.
#' @param reps Number of replications in each batch. The total number
#'   of replications is `batches * reps`. 
#' @param tidy_eval Whether to invoke tidy evaluation
#'   (e.g. the `!!` operator from `rlang`) as soon as the target is defined
#'   (before `tar_make()`). Applies to the `command` argument.
#' @examples
#' \dontrun{
#' }
tar_batch_raw <- function(
  name,
  command,
  batches = 1,
  reps = 1,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = targets::tar_option_get("format"),
  iteration = "list",
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  template = targets::tar_option_get("template"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue")
) {
  name_batch <- paste0(name, "_batch")
  batch <- tar_batch_batch(
    name_batch = name_batch,
    batches = batches,
    error = error,
    memory = memory,
    priority = priority,
    cue = cue
  )
  target <- tar_batch_target(
    name = name,
    name_batch = name_batch,
    command,
    batches = batches,
    reps = reps,
    packages = packages,
    library = library,
    format = format,
    iteration = iteration,
    error = error,
    memory = memory,
    deployment = deployment,
    priority = priority,
    template = template,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
  list(batch, target)
}

tar_batch_batch <- function(
  name_batch,
  batches,
  error,
  memory,
  priority,
  cue
) {
  targets::tar_target_raw(
    name = name_batch,
    command = expression(seq_len(batches)),
    packages = character(0),
    format = "rds",
    iteration = "vector",
    error = error,
    memory = memory,
    deployment = "local",
    priority = priority,
    storage = "local",
    retrieval = "local",
    cue = cue
  )
}

tar_batch_target <- function(
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
  deployment,
  priority,
  template,
  resources,
  storage,
  retrieval,
  cue
) {
  targets::tar_target_raw(
    name = name,
    command = tar_batch_command(command, name_batch, reps, iteration),
    pattern = tar_batch_pattern(name_batch),
    packages = packages,
    library = library,
    format = format,
    iteration = iteration,
    error = error,
    memory = memory,
    deployment = deployment,
    priority = priority,
    template = template,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
}

tar_batch_command <- function(command, name_batch, reps, iteration) {
  out <- substitute(
    tarchetypes::tar_batch_run(
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

tar_batch_pattern <- function(name_batch) {
  substitute(map(x), env = list(x = rlang::sym(name_batch)))
}

#' @title Run a batch in a `tar_batch()` archetype.
#' @description Internal function needed for `tar_batch()`.
#'   Users should not invoke it directly.
#' @export
#' @keywords internal
#' @return Aggregated results of multiple executions of the command.
#' @param command Expression object, command to replicate.
#' @param batch Numeric, batch index.
#' @param reps Numeric, number of reps per batch.
#' @param iteration Character, iteration method.
tar_batch_run <- function(command, batch, reps, iteration) {
  expr <- substitute(command)
  envir <- parent.frame()
  switch(
    list = tar_batch_map(expr, envir, batch, reps),
    vector = do.call(vctrs::vec_c, tar_batch_map(expr, envir, batch, reps)),
    group = do.call(vctrs::vec_rbind, tar_batch_map(expr, envir, batch, reps)),
    throw_validate("unsupported iteration method")
  )
}

tar_batch_map <- function(expr, envir, batch, reps) {
  lapply(
    seq_len(reps),
    tar_batch_rep,
    expr = expr,
    envir = envir,
    batch = batch
  )
}

tar_batch_rep <- function(expr, envir, batch, rep) {
  out <- eval(expr, envir = envir)
  out$tar_batch <- as.integer(batch)
  out$tar_rep <- as.integer(rep)
  out
}
