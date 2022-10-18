#' @title Batched dynamic-within-static branching for data frames.
#' @keywords internal
#' @family branching
#' @description Define targets for batched
#'   dynamic-within-static branching for data frames.
#'   Not a user-side function. Do not invoke directly.
#' @details Static branching creates one pair of targets
#'   for each row in `values`. In each pair,
#'   there is an upstream non-dynamic target that runs `command1`
#'   and a downstream dynamic target that runs `command2`.
#'   `command1` produces a data frame of arguments to
#'   `command2`, and `command2` dynamically maps over
#'   these arguments in batches.
#' @details Static branching creates one pair of targets
#'   for each row in `values`. In each pair,
#'   there is an upstream non-dynamic target that runs `command1`
#'   and a downstream dynamic target that runs `command2`.
#'   `command1` produces a data frame of arguments to
#'   `command2`, and `command2` dynamically maps over
#'   these arguments in batches.
#' @inheritSection tar_rep Replicate-specific seeds
#' @return A list of new target objects.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams tar_map2_raw
#' @param name Symbol, base name of the targets.
#' @param command1 R code to create named arguments to `command2`.
#'   Must return a data frame with one row per call to `command2`.
#' @param command2 R code to map over the data frame of arguments
#'   produced by `command1`. Must return a data frame.
#' @param columns1 A tidyselect expression to select which columns of `values`
#'   to append to the output of all targets.
#'   Columns already in the target output are not appended.
#' @param columns2 A tidyselect expression to select which columns of `command1`
#'   output to append to `command2` output.
#'   Columns already in the target output are not appended.
#'   `columns1` takes precedence over `columns2`.
tar_map2 <- function(
  name,
  command1,
  command2,
  values = NULL,
  names = NULL,
  group = rep(1L, nrow(as.data.frame(!!.x))),
  combine = TRUE,
  suffix1 = "1",
  suffix2 = "2",
  columns1 = tidyselect::everything(),
  columns2 = tidyselect::everything(),
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
  cue = targets::tar_option_get("cue")
) {
  tar_map2_raw(
    name = deparse(substitute(name)),
    command1 = substitute(command1),
    command2 = substitute(command2),
    values = values,
    names = substitute(names),
    group = substitute(group),
    combine = combine,
    suffix1 = suffix1,
    suffix2 = suffix2,
    columns1 = substitute(columns1),
    columns2 = substitute(columns2),
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
    cue = cue
  )
}
