#' @title Dynamic-within-static branching for data frames
#'   (size batching).
#' @export
#' @family branching
#' @description Define targets for batched
#'   dynamic-within-static branching for data frames,
#'   where the user sets the (maximum) size of each batch.
#'
#'   [tar_map2_size()] expects unevaluated language for arguments
#'   `name`, `command1`, `command2`, `columns1`, and `columns2`.
#'   [tar_map2_size_raw()] expects a character string for `name`
#'   and an evaluated expression object  for each of
#'   `command1`, `command2`, `columns1`, and `columns2`.
#' @details Static branching creates one pair of targets
#'   for each row in `values`. In each pair,
#'   there is an upstream non-dynamic target that runs `command1`
#'   and a downstream dynamic target that runs `command2`.
#'   `command1` produces a data frame of arguments to
#'   `command2`, and `command2` dynamically maps over
#'   these arguments in batches.
#' @return A list of new target objects.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritSection tar_rep Replicate-specific seeds
#' @inheritParams tar_rep
#' @inheritParams tar_map2
#' @param size Positive integer of length 1,
#'   maximum number of rows in each batch for
#'   the downstream (`command2`) targets. Batches
#'   are formed from row groups of the `command1` target output.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   tarchetypes::tar_map2_size(
#'     x,
#'     command1 = tibble::tibble(
#'       arg1 = arg1,
#'       arg2 = seq_len(6)
#'      ),
#'     command2 = tibble::tibble(
#'       result = paste(arg1, arg2),
#'       random = sample.int(1e9, size = 1),
#'       length_input = length(arg1)
#'     ),
#'     values = tibble::tibble(arg1 = letters[seq_len(2)]),
#'     size = 2
#'    )
#' })
#' targets::tar_make()
#' targets::tar_read(x)
#' # With tar_map2_size_raw():
#' targets::tar_script({
#'   tarchetypes::tar_map2_size_raw(
#'     name = "x",
#'     command1 = quote(
#'       tibble::tibble(
#'         arg1 = arg1,
#'         arg2 = seq_len(6)
#'       )
#'     ),
#'     command2 = quote(
#'       tibble::tibble(
#'         result = paste(arg1, arg2),
#'         random = sample.int(1e9, size = 1),
#'         length_input = length(arg1)
#'       )
#'     ),
#'     values = tibble::tibble(arg1 = letters[seq_len(2)]),
#'     size = 2
#'    )
#' })
#' })
#' }
tar_map2_size <- function(
  name,
  command1,
  command2,
  values = NULL,
  names = NULL,
  descriptions = tidyselect::everything(),
  size = Inf,
  combine = TRUE,
  suffix1 = "1",
  suffix2 = "2",
  columns1 = tidyselect::everything(),
  columns2 = tidyselect::everything(),
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
  tar_map2_size_raw(
    name = deparse(substitute(name)),
    command1 = substitute(command1),
    command2 = substitute(command2),
    values = values,
    names = substitute(names),
    descriptions = substitute(descriptions),
    size = size,
    combine = combine,
    suffix1 = suffix1,
    suffix2 = suffix2,
    columns1 = substitute(columns1),
    columns2 = substitute(columns2),
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
