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
  assert_package("dplyr")
  name <- deparse_language(substitute(name))
  assert_lgl(tidy_eval, "tidy_eval in tar_target() must be logical.")
  by <- all.vars(substitute(list(...)), functions = FALSE)
  assert_nonempty(by, "no columns to group by.")
  command <- tar_group_by_command(substitute(command), by, envir, tidy_eval)
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

tar_group_by_command <- function(command, by, envir, tidy_eval) {
  envir <- targets::tar_option_get("envir")
  assert_envir(envir)
  command <- tidy_eval(command, envir, tidy_eval)
  fun <- call_ns("tarchetypes", "tar_group_by_run")
  as.call(list(fun, data = command, by = by))
}

tar_group_by_run <- function(data, by) {
  assert_df(data, "tar_group_by() output must be a data frame.")
  assert_in(by, colnames(data), "tar_group_by() columns must be in data.")
  expr <- quote(dplyr::group_by(data, !!!by_syms))
  by_syms <- rlang::syms(by)
  envir <- environment()
  expr <- tidy_eval(expr, envir = envir, TRUE)
  grouped <- eval(expr, envir = envir)
  targets::tar_group(grouped)
}
