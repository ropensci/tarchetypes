#' @export
#' @rdname tar_formats
tar_rds <- function(
  name,
  command,
  pattern = NULL,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  iteration = targets::tar_option_get("iteration"),
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
  envir <- tar_option_get("envir")
  targets::tar_target_raw(
    name = deparse_language(substitute(name)),
    command = tidy_eval(substitute(command), envir, tidy_eval),
    pattern = pattern,
    packages = packages,
    library = library,
    format = "rds",
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
