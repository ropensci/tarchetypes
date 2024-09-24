#' @rdname tar_files
#' @export
tar_files_raw <- function(
  name,
  command,
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = c("file", "url", "aws_file", "file_fast"),
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
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
) {
  targets::tar_assert_chr(name, "name must be a character.")
  targets::tar_assert_scalar(name, "name must have length 1.")
  command <- tar_raw_command(name, command)
  name_files <- paste0(name, "_files")
  format <- match.arg(format)
  upstream <- targets::tar_target_raw(
    name = name_files,
    command = command,
    pattern = NULL,
    packages = packages,
    library = library,
    format = "rds",
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
    cue = targets::tar_cue(mode = "always"),
    description = description
  )
  name_files_sym <- as.symbol(name_files)
  downstream <- targets::tar_target_raw(
    name = name,
    command = as.expression(name_files_sym),
    pattern = as.expression(call_function("map", list(name_files_sym))),
    packages = character(0),
    library = library,
    format = format,
    repository = repository,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = "main",
    priority = priority,
    resources = resources,
    storage = "main",
    retrieval = "main",
    cue = cue,
    description = description
  )
  out <- list(upstream, downstream)
  names(out) <- c(name_files, name)
  out
}
