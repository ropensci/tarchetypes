#' @rdname tar_map_rep
#' @export
tar_map_rep_raw <- function(
  name,
  command,
  values = NULL,
  names = NULL,
  descriptions = quote(tidyselect::everything()),
  columns = quote(tidyselect::everything()),
  batches = 1,
  reps = 1,
  rep_workers = 1,
  combine = TRUE,
  delimiter = "_",
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
  if (!is.null(values)) {
    assert_values_list(values)
    values <- tibble::as_tibble(values)
    targets::tar_assert_ge(nrow(values), 1L)
  }
  if (!is.null(names)) {
    targets::tar_assert_lang(names)
  }
  if (!is.null(descriptions)) {
    targets::tar_assert_lang(descriptions)
  }
  if (!is.null(columns)) {
    targets::tar_assert_lang(columns)
  }
  targets::tar_assert_scalar(batches)
  targets::tar_assert_dbl(batches)
  targets::tar_assert_scalar(reps)
  targets::tar_assert_dbl(reps)
  targets::tar_assert_scalar(combine)
  targets::tar_assert_lgl(combine)
  tar_assert_rep_workers(rep_workers)
  targets::tar_assert_chr(delimiter)
  targets::tar_assert_scalar(delimiter)
  rep_workers <- as.integer(rep_workers)
  envir <- targets::tar_option_get("envir")
  command <- tar_raw_command(name, command)
  command <- targets::tar_tidy_eval(as.expression(command), envir, tidy_eval)
  if (!is.null(values)) {
    columns <- targets::tar_tidyselect_eval(columns, colnames(values))
    command <- tar_command_append_static_values(command, columns)
  }
  name_batch <- paste0(name, delimiter, "batch")
  target_batch <- targets::tar_target_raw(
    name = name_batch,
    command = substitute(seq_len(batches), env = list(batches = batches)),
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
    cue = cue,
    description = description
  )
  command_dynamic <- tar_rep_command_target(
    command = command,
    name_batch = name_batch,
    reps = reps,
    rep_workers = rep_workers,
    iteration = "vector"
  )
  target_dynamic <- targets::tar_target_raw(
    name = name,
    command = command_dynamic,
    pattern = tar_rep_pattern(name_batch),
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
    target_dynamic,
    do.call(
      tar_map,
      args = list(
        target_dynamic,
        values = values,
        names = names,
        descriptions = descriptions,
        delimiter = delimiter
      )
    )
  )
  target_combine <- if_any(
    is.null(values) || !combine,
    NULL,
    tar_combine_raw(
      name = name,
      target_static,
      command = tar_map_combine_command,
      use_names = TRUE,
      packages = character(0),
      format = format,
      repository = repository,
      iteration = "group",
      error = error,
      memory = memory,
      garbage_collection = garbage_collection,
      deployment = "main",
      priority = priority,
      cue = cue,
      description = description
    )
  )
  unlist(list(target_batch, target_static, target_combine), recursive = TRUE)
}

tar_map_combine_command <- expression({
  out <- dplyr::bind_rows(!!!.x, .id = "tar_group")
  out <- dplyr::mutate(out, tar_group = as.integer(as.factor(tar_group)))
  dplyr::select(out, -tidyselect::any_of("tar_group"), tar_group)
})

tar_command_append_static_values <- function(command, columns) {
  column_syms <- rlang::syms(columns)
  names(column_syms) <- columns
  values <- rlang::call2("list", !!!column_syms)
  rlang::call2(
    .fn = "tar_append_static_values",
    object = command,
    values = values,
    .ns = "tarchetypes"
  )
}

#' @title Append statically mapped values to target output.
#' @export
#' @keywords internal
#' @description For internal use only. Users should not invoke
#'   this function directly.
#' @param object Return value of a target. Must be a data frame.
#' @param values Tibble with the set of static values that the current target
#'   uses.
tar_append_static_values <- function(object, values) {
  if (!length(values)) {
    return(object)
  }
  targets::tar_assert_df(object)
  args <- list(.data = object)
  for (name in setdiff(names(values), names(object))) {
    args[[name]] <- if_any(
      length(values[[name]]) == 1L && is.atomic(values[[name]]),
      values[[name]],
      list(values[[name]])
    )
  }
  do.call(dplyr::mutate, args = args)
}
