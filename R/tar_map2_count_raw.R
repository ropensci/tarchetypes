#' @title Dynamic-within-static branching for data frames
#'   (count batching; raw version).
#' @export
#' @family branching
#' @description Define targets for batched
#'   dynamic-within-static branching for data frames,
#'   where the user sets the (maximum) number of batches.
#'   Like `tar_map2_count()` except `name` is a character string
#'   and `command1`, `command2`, `names`, `columns1`, and `columns2`
#'   are all language objects.
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
#' @param batches Positive integer of length 1,
#'   maximum number of batches (dynamic branches within static branches)
#'   of the downstream (`command2`) targets. Batches
#'   are formed from row groups of the `command1` target output.
#' @inheritParams tar_map2_raw
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   tarchetypes::tar_map2_count_raw(
#'     "x",
#'     command1 = quote(
#'       tibble::tibble(
#'         arg1 = arg1,
#'         arg2 = seq_len(6)
#'        )
#'     ),
#'     command2 = quote(
#'       tibble::tibble(
#'         result = paste(arg1, arg2),
#'         random = sample.int(1e6, size = 1),
#'         length_input = length(arg1)
#'       )
#'     ),
#'     values = tibble::tibble(arg1 = letters[seq_len(2)]),
#'     batches = 3
#'    )
#' })
#' targets::tar_make()
#' targets::tar_read(x)
#' })
#' }
tar_map2_count_raw <- function(
  name,
  command1,
  command2,
  values = NULL,
  names = NULL,
  descriptions = quote(tidyselect::everything()),
  batches = 1L,
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
  targets::tar_assert_scalar(batches)
  targets::tar_assert_dbl(batches)
  targets::tar_assert_positive(batches)
  group <- substitute(
    tarchetypes::tar_group_count_index(!!.x, count),
    env = list(count = batches)
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
