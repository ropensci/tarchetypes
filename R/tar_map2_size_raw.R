#' @rdname tar_map2_size
#' @export
tar_map2_size_raw <- function(
  name,
  command1,
  command2,
  values = NULL,
  names = NULL,
  descriptions = quote(tidyselect::everything()),
  size = Inf,
  combine = TRUE,
  suffix1 = "1",
  suffix2 = "2",
  columns1 = quote(tidyselect::everything()),
  columns2 = quote(tidyselect::everything()),
  rep_workers = 1,
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
  targets::tar_assert_scalar(size)
  targets::tar_assert_dbl(size)
  targets::tar_assert_positive(size)
  group <- substitute(
    tarchetypes::tar_group_size_index(!!.x, size),
    env = list(size = size)
  )
  tar_map2_raw(
    name = name,
    command1 = command1,
    command2 = command2,
    values = values,
    names = names,
    descriptions = descriptions,
    group = group,
    combine = combine,
    suffix1 = suffix1,
    suffix2 = suffix2,
    columns1 = columns1,
    columns2 = columns2,
    rep_workers = rep_workers,
    delimiter = delimiter,
    tidy_eval = tidy_eval,
    packages = packages,
    library = library,
    format = format,
    repository = repository,
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
