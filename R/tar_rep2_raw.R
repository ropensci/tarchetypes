#' @title Dynamic batched computation downstream of [tar_rep()] (raw version).
#' @export
#' @family branching
#' @keywords internal
#' @description Batching is important for optimizing the efficiency
#'   of heavily dynamically-branched workflows:
#'   <https://books.ropensci.org/targets/dynamic.html#batching>.
#'   `tar_rep2_raw()`
#'   is just like [tar_rep2()] except it accepts a character
#'   of length 1 for `name`, a language object for `command`,
#'   and a character vector of the names of the upstream batched targets.
#' @return A new target object to perform batched computation
#'   downstream of [tar_rep()].
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritSection tar_rep Replicate-specific seeds
#' @inheritParams targets::tar_target
#' @inheritParams tar_rep
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
#'     tarchetypes::tar_rep2_raw(
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
tar_rep2_raw <- function(
  name,
  command,
  targets,
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
  targets::tar_assert_chr(targets)
  targets::tar_assert_nonempty(targets)
  targets::tar_assert_nzchar(targets)
  tar_assert_rep_workers(rep_workers)
  command <- tar_raw_command(name, command)
  command <- tar_rep2_command(
    command = command,
    targets = targets,
    iteration = iteration,
    rep_workers = rep_workers
  )
  pattern <- call_function("map", lapply(targets, as.symbol))
  targets::tar_target_raw(
    name = name,
    command = command,
    pattern = pattern,
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

tar_rep2_command <- function(command, targets, iteration, rep_workers) {
  batches <- lapply(targets, as.symbol)
  names(batches) <- targets
  substitute(
    tarchetypes::tar_rep2_run(
      command = command,
      batches = batches,
      iteration = iteration,
      rep_workers = rep_workers
    ),
    env = list(
      command = command,
      batches = call_list(batches),
      iteration = iteration,
      rep_workers = rep_workers
    )
  )
}

#' @title Run [tar_rep2()] batches.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return The result of batched replication.
#' @inheritParams tar_rep
#' @param command R expression, the command to run on each rep.
#' @param batches Named list of batch data to map over.
#' @param iteration Iteration method: `"list"`, `"vector"`, or `"group"`.
tar_rep2_run <- function(command, batches, iteration, rep_workers) {
  command <- substitute(command)
  assert_batches(batches)
  reps <- batch_count_reps(batches[[1]])
  pedigree <- targets::tar_definition()$pedigree
  name <- pedigree$parent
  batch <- pedigree$index
  seeds <- produce_batch_seeds(name = name, batch = batch, reps = reps)
  envir <- targets::tar_envir()
  slices <- split_batches(batches = batches, reps = reps)
  call <- quote(
    function(.x, .y, command, batch, seeds, envir) {
      tarchetypes::tar_rep2_run_rep(
        rep = .x,
        slice = .y,
        command = command,
        batch = batch,
        seeds = seeds,
        envir = envir
      )
    }
  )
  fun <- eval(call, envir = targets::tar_option_get("envir"))
  if (rep_workers > 1L) {
    plan_old <- future::plan()
    on.exit(future::plan(plan_old, .cleanup = FALSE))
    future::plan(future.callr::callr, workers = rep_workers, .cleanup = FALSE)
    out <- furrr::future_map2(
      .x = seq_len(reps),
      .y = slices,
      .f = fun,
      .options = furrr::furrr_options(
        seed = 1L,
        packages = targets::tar_definition()$command$packages,
        globals = names(targets::tar_option_get("envir"))
      ),
      command = as.expression(command),
      batch = batch,
      seeds = seeds,
      envir = envir
    )
  } else {
    out <- map2(
      x = seq_len(reps),
      y = slices,
      f = fun,
      command = as.expression(command),
      batch = batch,
      seeds = seeds,
      envir = envir
    )
  }
  tar_rep_bind(out, iteration)
}

#' @title Run a rep in a `tar_rep2()`-powered function.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return The result of running `expr`.
#' @param rep Rep number.
#' @param slice Slice of the upstream batch data of the given rep.
#' @param command R command to run.
#' @param batch Batch number.
#' @param seeds Random number generator seeds of the batch.
#' @param envir Environment of the target.
#' @examples
#' # See the examples of tar_rep2().
tar_rep2_run_rep <- function(rep, slice, command, batch, seeds, envir) {
  seed <- as.integer(if_any(anyNA(seeds), NA_integer_, seeds[rep]))
  out <- if_any(
    anyNA(seed),
    eval(command, envir = slice, enclos = envir),
    withr::with_seed(
      seed = seed,
      code = eval(command, envir = slice, enclos = envir)
    )
  )
  out$tar_batch <- as.integer(batch)
  out$tar_rep <- as.integer(rep)
  out$tar_seed <- as.integer(seed)
  out
}

split_batches <- function(batches, reps) {
  lapply(X = seq_len(reps), FUN = slice_batches, batches = batches)
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
