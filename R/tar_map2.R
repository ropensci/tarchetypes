#' @title Batched dynamic-within-static branching for data frames.
#' @keywords internal
#' @family branching
#' @description Define targets for batched
#'   dynamic-within-static branching for data frames.
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
#' @param command1 R code to create named arguments to `command2`.
#'   Must return a data frame with one row per call to `command2`.
#' @param command2 R code to map over the data frame of arguments
#'   produced by `command1`. Must return a data frame.
#' @param columns1 A tidyselect expression to select which columns of `values`
#'   to append to the output of all targets.
#' @param columns2 A tidyselect expression to select which columns of `command1`
#'   output to append to `command2` output.
#' @inheritSection tar_map Target objects
#' @inheritParams tar_map2_raw
#' @inheritParams tar_rep2
#' @inheritParams tar_map
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   tarchetypes::tar_map2(
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
#'     group = rep(LETTERS[seq_len(2)], each = nrow(.x) / 2)
#'    )
#' })
#' targets::tar_make()
#' targets::tar_read(x)
#' })
#' }
tar_map2 <- function(
  name,
  command1,
  command2,
  values = NULL,
  names = NULL,
  columns1 = tidyselect::everything(),
  columns2 = tidyselect::everything(),
  combine = TRUE,
  group = rep(1L, nrow(.x)),
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
  tar_map2_raw(
    name = deparse(substitute(name)),
    command1 = substitute(command1),
    command2 = substitute(command2),
    values = values,
    names = substitute(names),
    columns1 = substitute(columns1),
    columns2 = substitute(columns2),
    combine = combine,
    group = substitute(group),
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
