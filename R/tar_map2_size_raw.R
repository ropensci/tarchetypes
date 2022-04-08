#' @title Dynamic-within-static branching for data frames
#'   (size batching; raw version).
#' @export
#' @family branching
#' @description Define targets for batched
#'   dynamic-within-static branching for data frames,
#'   where the user sets the (maximum) size of each batch.
#'   Like `tar_map2_size()` except `name` is a character string
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
#' @param size Positive integer of length 1,
#'   maximum number of rows in each batch for
#'   the downstream (`command2`) targets. Batches
#'   are formed from row groups of the `command1` target output.
#' @inheritParams tar_map2_raw
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   tarchetypes::tar_map2_size_raw(
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
#'     size = 2
#'    )
#' })
#' targets::tar_make()
#' targets::tar_read(x)
#' })
#' }
tar_map2_size_raw <- function(
  name,
  command1,
  command2,
  values = NULL,
  names = NULL,
  size = Inf,
  combine = TRUE,
  suffix1 = "1",
  suffix2 = "2",
  columns1 = quote(tidyselect::everything()),
  columns2 = quote(tidyselect::everything()),
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = targets::tar_option_get("format"),
  repository = targets::tar_option_get("repository"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue")
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
    group = group,
    combine = combine,
    suffix1 = suffix1,
    suffix2 = suffix2,
    columns1 = columns1,
    columns2 = columns2,
    tidy_eval = tidy_eval,
    packages = packages,
    library = library,
    format = format,
    repository = repository,
    error = error,
    memory = memory,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
}
