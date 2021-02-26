#' @inheritParams targets::tar_target
tar_group_by <- function(
  name,
  command,
  ...,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = targets::tar_option_get("format"),
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
  assert_lgl(tidy_eval, "tidy_eval in tar_target() must be logical.")
  columns <- as.expression(substitute(list(...)))
  command <- tar_group_by_command(substitute(command), columns, envir, tidy_eval)
  targets::tar_target_raw(
    name = name,
    command = command,
    packages = packages,
    library = library,
    format = format,
    iteration = "group",
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

tar_group_by_command <- function(command, columns, envir, tidy_eval) {
  envir <- targets::tar_option_get("envir")
  assert_envir(envir)
  command <- tidy_eval(command, envir, tidy_eval)
  fun <- call_ns("tarchetypes", "tar_group_by_run")
  as.call(list(fun, command = command, columns = columns))
}
