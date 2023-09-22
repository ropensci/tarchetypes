#' @title Batched replication with dynamic branching
#'   (raw version).
#' @export
#' @family branching
#' @description Batching is important for optimizing the efficiency
#'   of heavily dynamically-branched workflows:
#'   <https://books.ropensci.org/targets/dynamic.html#batching>.
#'   [tar_rep_raw()] is just like [tar_rep()] except the
#'   name is a character string and the command is a
#'   language object.
#' @details `tar_rep_raw()` creates two targets:
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
#' @inheritSection tar_rep Replicate-specific seeds
#' @return A list of two target objects, one upstream and one downstream.
#'   The upstream one does some work and returns some file paths,
#'   and the downstream target is a pattern that applies `format = "file"`.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target_raw
#' @inheritParams tar_rep
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
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   list(
#'     tarchetypes::tar_rep_raw(
#'       "x",
#'       expression(data.frame(x = sample.int(1e4, 2))),
#'       batches = 2,
#'       reps = 3
#'     )
#'   )
#' })
#' targets::tar_make(callr_function = NULL)
#' targets::tar_read(x)
#' })
#' }
tar_rep_raw <- function(
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
  tar_assert_rep_workers(rep_workers)
  rep_workers <- as.integer(rep_workers)
  command <- tar_raw_command(name, command)
  name_batch <- paste0(name, "_batch")
  batch <- tar_rep_batch(
    name_batch = name_batch,
    batches = batches,
    repository = repository,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    priority = priority,
    cue = cue
  )
  target <- tar_rep_target(
    name = name,
    name_batch = name_batch,
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
  list(batch, target)
}

tar_rep_batch <- function(
  name_batch,
  batches,
  repository,
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
    repository = repository,
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
  rep_workers,
  packages,
  library,
  format,
  repository,
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
  command <- tar_rep_command_target(
    command = command,
    name_batch = name_batch,
    reps = reps,
    rep_workers = rep_workers,
    iteration = iteration
  )
  targets::tar_target_raw(
    name = name,
    command = command,
    pattern = tar_rep_pattern(name_batch),
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

tar_rep_command_batch <- function(batches) {
  as.expression(substitute(seq_len(x), env = list(x = batches)))
}

tar_rep_command_target <- function(
  command,
  name_batch,
  reps,
  rep_workers,
  iteration
) {
  out <- substitute(
    tarchetypes::tar_rep_run(
      command = command,
      batch = batch,
      reps = reps,
      rep_workers = rep_workers,
      iteration = iteration
    ),
    env = list(
      command = command,
      batch = as.symbol(name_batch),
      reps = reps,
      iteration = iteration,
      rep_workers = rep_workers
    )
  )
  as.expression(out)
}

tar_rep_pattern <- function(name_batch) {
  substitute(map(x), env = list(x = as.symbol(name_batch)))
}

#' @title Run a batch in a `tar_rep()` archetype.
#' @description Internal function needed for `tar_rep()`.
#'   Users should not invoke it directly.
#' @export
#' @keywords internal
#' @return Aggregated results of multiple executions of the
#'   user-defined command supplied to [tar_rep()]. Depends on what
#'   the user specifies. Common use cases are simulated datasets.
#' @inheritParams tar_rep
#' @param command Expression object, command to replicate.
#' @param batch Numeric of length 1, batch index.
#' @param reps Numeric of length 1, number of reps per batch.
#' @param iteration Character, iteration method.
tar_rep_run <- function(command, batch, reps, iteration, rep_workers) {
  expr <- substitute(command)
  out <- tar_rep_run_map(
    expr = expr,
    batch = batch,
    reps = reps,
    rep_workers = rep_workers
  )
  tar_rep_bind(out, iteration)
}

tar_rep_bind <- function(out, iteration) {
  switch(
    iteration,
    list = out,
    vector = do.call(vctrs::vec_c, out),
    group = do.call(vctrs::vec_rbind, out),
    targets::tar_throw_validate("unsupported iteration method")
  )
}

tar_rep_run_map <- function(expr, batch, reps, rep_workers) {
  call <- quote(
    function(.x, expr, batch, seeds, envir) {
      tarchetypes::tar_rep_run_map_rep(
        rep = .x,
        expr = expr,
        batch = batch,
        seeds = seeds,
        envir = envir
      )
    }
  )
  fun <- eval(call, envir = targets::tar_option_get("envir"))
  name <- targets::tar_definition()$pedigree$parent
  seeds <- produce_batch_seeds(name = name, batch = batch, reps = reps)
  envir <- targets::tar_envir()
  if (rep_workers > 1L) {
    cluster <- make_psock_cluster(rep_workers)
    on.exit(parallel::stopCluster(cl = cluster))
    parallel::parLapply(
      cl = cluster,
      X = seq_len(reps),
      fun = fun,
      expr = expr,
      batch = batch,
      seeds = seeds,
      envir = envir
    )
  } else {
    map(
      x = seq_len(reps),
      f = fun,
      expr = expr,
      batch = batch,
      seeds = seeds,
      envir = envir
    )
  }
}

#' @title Run a rep in `tar_rep()`.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return The result of running `expr`.
#' @param rep Rep number.
#' @param expr R expression to run.
#' @param batch Batch number.
#' @param seeds Random number generator seeds of the batch.
#' @param envir Environment of the target.
#' @examples
#' # See the examples of tar_rep().
tar_rep_run_map_rep <- function(rep, expr, batch, seeds, envir) {
  seed <- as.integer(if_any(anyNA(seeds), NA_integer_, seeds[rep]))
  out <- if_any(
    anyNA(seed),
    eval(expr, envir = envir),
    withr::with_seed(
      seed = seed,
      code = eval(expr, envir = envir)
    )
  )
  if (is.list(out)) {
    out[["tar_batch"]] <- as.integer(batch)
    out[["tar_rep"]] <- as.integer(rep)
    out[["tar_seed"]] <- as.integer(seed)
  }
  out
}

produce_batch_seeds <- function(name, batch, reps) {
  seed <- if_any(
    "seed" %in% names(formals(targets::tar_option_set)),
    targets::tar_option_get("seed"),
    0L
  )
  if (anyNA(seed)) {
    return(NA_integer_)
  }
  strings <- paste(name, as.character(seq_len(reps) + reps * (batch - 1)))
  unname(map_int(x = strings, f = digest::digest2int, seed = seed))
}

tar_assert_rep_workers <- function(rep_workers) {
  targets::tar_assert_dbl(rep_workers)
  targets::tar_assert_scalar(rep_workers)
  targets::tar_assert_finite(rep_workers)
  targets::tar_assert_ge(rep_workers, 0)
  rep_workers <- as.integer(rep_workers)
}
