#' @rdname tar_map2
#' @export
#' @keywords internal
tar_map2_raw <- function(
  name,
  command1,
  command2,
  values = NULL,
  names = NULL,
  descriptions = quote(tidyselect::everything()),
  group = quote(rep(1L, nrow(as.data.frame(!!.x)))),
  combine = TRUE,
  columns1 = quote(tidyselect::everything()),
  columns2 = quote(tidyselect::everything()),
  suffix1 = "1",
  suffix2 = "2",
  rep_workers = 1,
  delimiter = "_",
  unlist = FALSE,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = targets::tar_option_get("format"),
  repository = targets::tar_option_get("repository"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
) {
  targets::tar_assert_scalar(name)
  targets::tar_assert_chr(name)
  targets::tar_assert_nzchar(name)
  targets::tar_assert_scalar(suffix1)
  targets::tar_assert_chr(suffix1)
  targets::tar_assert_nzchar(suffix1)
  targets::tar_assert_scalar(suffix2)
  targets::tar_assert_chr(suffix2)
  targets::tar_assert_nzchar(suffix2)
  targets::tar_assert_df(values %|||% data.frame())
  if (!is.null(values)) {
    targets::tar_assert_ge(nrow(values), 1L)
  }
  if (!is.null(names)) {
    targets::tar_assert_lang(names)
  }
  if (!is.null(descriptions)) {
    targets::tar_assert_lang(descriptions)
  }
  if (!is.null(columns1)) {
    targets::tar_assert_lang(columns1)
  }
  if (!is.null(columns2)) {
    targets::tar_assert_lang(columns2)
  }
  targets::tar_assert_scalar(combine)
  targets::tar_assert_lgl(combine)
  targets::tar_assert_lang(group)
  tar_assert_rep_workers(rep_workers)
  targets::tar_assert_chr(delimiter)
  targets::tar_assert_scalar(delimiter)
  rep_workers <- as.integer(rep_workers)
  envir <- targets::tar_option_get("envir")
  command1 <- tar_raw_command(name, command1)
  command2 <- tar_raw_command(name, command2)
  command1 <- targets::tar_tidy_eval(as.expression(command1), envir, tidy_eval)
  command2 <- targets::tar_tidy_eval(as.expression(command2), envir, tidy_eval)
  if (!is.null(values)) {
    columns1 <- targets::tar_tidyselect_eval(columns1, colnames(values))
    command1 <- tar_command_append_static_values(command1, columns1)
    command2 <- tar_command_append_static_values(command2, columns1)
  }
  name_upstream <- paste(name, suffix1, sep = delimiter)
  name_downstream <- paste(name, suffix2, sep = delimiter)
  sym_upstream <- as.symbol(name_upstream)
  sym_downstream <- as.symbol(name_downstream)
  target_upstream <- targets::tar_target_raw(
    name = name_upstream,
    command = tar_map2_command_upstream(command1, group),
    packages = packages,
    library = library,
    format = format,
    repository = repository,
    iteration = "group",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue,
    description = description
  )
  target_downstream <- targets::tar_target_raw(
    name = name_downstream,
    command = tar_map2_command_downstream(
      command = command2,
      sym_upstream = sym_upstream,
      columns2 = columns2,
      rep_workers = rep_workers
    ),
    pattern = rlang::call2("map", sym_upstream),
    packages = packages,
    library = library,
    format = format,
    repository = repository,
    iteration = "vector",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue,
    description = description
  )
  target_static <- if_any(
    is.null(values),
    list(target_upstream, target_downstream),
    do.call(
      tar_map,
      args = list(
        target_upstream,
        target_downstream,
        values = values,
        names = names,
        descriptions = descriptions,
        # Need to map over target_downstream branches and then unlist later.
        unlist = FALSE,
        delimiter = delimiter
      )
    )
  )
  target_combine <- NULL
  target_combine_dynamic <- NULL
  if (combine && !is.null(values)) {
    expr <- substitute(
      targets::tar_target_raw(
        name = paste0(name, "_combine"),
        command = as.symbol(name),
        packages = packages,
        library = library,
        format = format,
        repository = repository,
        iteration = "vector",
        error = error,
        memory = memory,
        garbage_collection = garbage_collection,
        deployment = deployment,
        priority = priority,
        resources = resources,
        storage = storage,
        retrieval = retrieval,
        cue = cue,
        description = description
      ),
      env = list(
        packages = packages,
        library = library,
        format = format,
        repository = repository,
        iteration = "vector",
        error = error,
        memory = memory,
        garbage_collection = garbage_collection,
        deployment = deployment,
        priority = priority,
        resources = resources,
        storage = storage,
        retrieval = retrieval,
        cue = cue,
        description = description
      )
    )
    target_combine_dynamic <- tar_eval_raw(
      expr = expr,
      values = list(
        name = map_chr(target_static[[2L]], ~.x$settings$name)
      )
    )
    names(target_combine_dynamic) <- map_chr(
      target_combine_dynamic,
      ~.x$settings$name
    )
    target_combine <- tar_combine_raw(
      name = name,
      target_combine_dynamic,
      command = tar_map_combine_command,
      use_names = TRUE,
      packages = character(0),
      format = format,
      repository = repository,
      iteration = "group",
      error = error,
      memory = memory,
      garbage_collection = garbage_collection,
      deployment = deployment,
      priority = priority,
      cue = cue,
      description = description
    )
  }
  out <- list(
    static_branches = target_static,
    combine_dynamic = target_combine_dynamic,
    combine = target_combine
  )
  if (unlist) {
    out <- unlist(out, recursive = TRUE)
    names(out) <- map_chr(out, ~.x$settings$name)
  }
  out
}

tar_map2_command_upstream <- function(command, group) {
  rlang::call2(
    "tar_map2_group",
    data = command,
    group = group,
    .ns = "tarchetypes"
  )
}

tar_map2_command_downstream <- function(
  command,
  sym_upstream,
  columns2,
  rep_workers
) {
  rlang::call2(
    "tar_map2_run",
    command = command,
    values = sym_upstream,
    columns = columns2,
    rep_workers = rep_workers,
    .ns = "tarchetypes"
  )
}

#' @title Append the `tar_group` variable to a `tar_map2()` target.
#' @export
#' @keywords internal
#' @details For internal use only. Users should not invoke
#'   this function directly.
#' @return A data frame with a `tar_group` column attached (if `group`
#'   is not `NULL`).
#' @param data Data frame to be returned from the target.
#' @param group Function on the data to return the `tar_group` column.
#'   If `group` is `NULL`, then no `tar_group` column is attached.
tar_map2_group <- function(data, group) {
  expr <- targets::tar_tidy_eval(
    expr = substitute(group),
    envir = list(.x = data),
    tidy_eval = TRUE
  )
  out <- eval(expr, envir = targets::tar_option_get("envir"))
  data[["tar_group"]] <- as.integer(as.factor(out))
  data
}

#' @title Run a dynamic batch of a `tar_map2()` target.
#' @export
#' @keywords internal
#' @details For internal use only. Users should not invoke
#'   this function directly.
#' @return A data frame with a `tar_group` column attached (if `group`
#'   is not `NULL`).
#' @inheritParams tar_rep
#' @param command Command to run.
#' @param values Data frame of named arguments produced by `command1`
#'   that `command2` dynamically maps over. Different from the `values`
#'   argument of `tar_map2()`.
#' @param columns tidyselect expression to select columns of `values`
#'   to append to the result.
tar_map2_run <- function(command, values, columns, rep_workers) {
  command <- substitute(command)
  columns <- substitute(columns)
  columns <- targets::tar_tidyselect_eval(columns, colnames(values))
  splits <- split(values, f = seq_len(nrow(values)))
  target <- targets::tar_definition()
  name <- target$pedigree$parent %|||% target$settings$name
  batch <- target$pedigree$index %|||% target$index
  reps <- length(splits)
  seeds <- produce_batch_seeds(name = name, batch = batch, reps = reps)
  envir <- targets::tar_envir()
  call <- quote(
    function(.x, .y, command, batch, seeds, columns, envir) {
      tarchetypes::tar_map2_run_rep(
        rep = .x,
        values = .y,
        command = command,
        batch = batch,
        seeds = seeds,
        columns = columns,
        envir = envir
      )
    }
  )
  fun <- eval(call, envir = envir)
  if (rep_workers > 1L) {
    cluster <- make_psock_cluster(rep_workers)
    on.exit(parallel::stopCluster(cl = cluster))
    out <- parallel::clusterMap(
      cl = cluster,
      fun = fun,
      .x = seq_along(splits),
      .y = splits,
      MoreArgs = list(
        command = as.expression(command),
        batch = batch,
        seeds = seeds,
        columns = columns,
        envir = envir
      ),
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    )
  } else {
    out <- map2(
      x = seq_along(splits),
      y = splits,
      f = fun,
      command = as.expression(command),
      batch = batch,
      seeds = seeds,
      columns = columns,
      envir = envir
    )
  }
  do.call(vctrs::vec_rbind, out)
}

#' @title Run a rep in a `tar_map2()`-powered function.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return The result of running `expr`.
#' @param rep Rep number.
#' @param values Data frame of mapped-over values.
#' @param command R command to run.
#' @param batch Batch number.
#' @param seeds Random number generator seeds of the batch.
#' @param columns Expression for appending static columns.
#' @param envir Environment of the target.
#' @examples
#' # See the examples of tar_map2_count().
tar_map2_run_rep <- function(
  rep,
  values,
  command,
  batch,
  seeds,
  columns,
  envir
) {
  names <- names(values)
  seed <- as.integer(if_any(anyNA(seeds), NA_integer_, seeds[rep]))
  lapply(
    X = seq_len(ncol(values)),
    FUN = function(index) {
      value <- if_any(
        is.list(values[[index]]) && length(values[[index]]) == 1L,
        values[[index]][[1]],
        values[[index]]
      )
      assign(
        x = names[index],
        value = value,
        envir = envir
      )
    }
  )
  if_any(anyNA(seed), NULL, targets::tar_seed_set(seed = seed))
  step_set(
    step = step_tar_rep,
    batch = batch,
    rep = rep,
    reps = length(seeds)
  )
  out <- eval(command, envir = envir)
  out <- tar_append_static_values(out, values[, columns])
  out[["tar_batch"]] <- as.integer(batch)
  out[["tar_rep"]] <- as.integer(rep)
  out[["tar_seed"]] <- as.integer(seed)
  out
}
