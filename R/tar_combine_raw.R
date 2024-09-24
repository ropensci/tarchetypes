#' @rdname tar_combine
#' @export
tar_combine_raw <- function(
  name,
  ...,
  command = expression(vctrs::vec_c(!!!.x)),
  use_names = TRUE,
  pattern = NULL,
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
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
) {
  tar_combine_impl(
    name = name,
    targets = unlist(list(...), recursive = TRUE),
    command = command,
    use_names = use_names,
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
    cue = cue,
    description = description
  )
}

tar_combine_impl <- function(
  name,
  targets,
  command,
  use_names,
  pattern,
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
  cue,
  description
) {
  targets::tar_assert_chr(name)
  targets::tar_assert_target_list(targets)
  names_chr <- map_chr(targets, ~.x$settings$name)
  names_sym <- as_symbols(names_chr)
  if (use_names) {
    names(names_sym) <- names_chr
  }
  command <- targets::tar_tidy_eval(
    command,
    envir = list(.x = names_sym),
    tidy_eval = TRUE
  )
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
    cue = cue,
    description = description
  )
}
