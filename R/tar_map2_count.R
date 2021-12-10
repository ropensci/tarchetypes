#' @title Dynamic-within-static branching for data frames
#'   (count batching).
#' @export
#' @family branching
#' @description Define targets for batched
#'   dynamic-within-static branching for data frames,
#'   where the user sets the (maximum) number of batches.
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
#' @param batches Positive integer of length 1,
#'   maximum number of batches (dynamic branches within static branches)
#'   of the downstream (`command2`) targets. Batches
#'   are formed from row groups of the `command1` targets.
#' @inheritSection tar_map Target objects
#' @inheritParams tar_map2
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   tarchetypes::tar_map2_count(
#'     x,
#'     command1 = tibble::tibble(
#'       arg1 = arg1,
#'       arg2 = sample.int(12)
#'      ),
#'     command2 = tibble::tibble(
#'       result = paste(arg1, arg2),
#'       length_input = length(arg1)
#'     ),
#'     values = tibble::tibble(arg1 = letters[seq_len(12)]),
#'     batches = 2
#'    )
#' })
#' targets::tar_make()
#' targets::tar_read(x)
#' })
#' }
tar_map2_count <- function(
  name,
  command1,
  command2,
  values = NULL,
  names = NULL,
  batches = 1L,
  combine = TRUE,
  suffix1 = "1",
  suffix2 = "2",
  columns1 = tidyselect::everything(),
  columns2 = tidyselect::everything(),
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = targets::tar_option_get("format"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue")
) {
  group <- substitute(
    tarchetypes::tar_group_count_index(!!.x, count),
    env = list(count = batches)
  )
  tar_map2_raw(
    name = deparse(substitute(name)),
    command1 = substitute(command1),
    command2 = substitute(command2),
    values = values,
    names = substitute(names),
    group = group,
    combine = combine,
    suffix1 = suffix1,
    suffix2 = suffix2,
    columns1 = substitute(columns1),
    columns2 = substitute(columns2),
    tidy_eval = tidy_eval,
    packages = packages,
    library = library,
    format = format,
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
