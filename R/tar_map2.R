#' @title Batched dynamic-within-static branching for data frames.
#' @export
#' @keywords internal
#' @family branching
#' @description Define targets for batched
#'   dynamic-within-static branching for data frames.
#'   Not a user-side function. Do not invoke directly.
#'
#'   [tar_map2()] expects unevaluated language for arguments
#'   `name`, `command1`, `command2`, `columns1`, and `columns2`.
#'   [tar_map2_raw()] expects a character string for `name`
#'   and an evaluated expression object  for each of
#'   `command1`, `command2`, `columns1`, and `columns2`.
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
#' @inheritParams tar_map_rep
#' @inheritParams tar_rep2
#' @inheritParams tar_map
#' @inheritParams tar_rep
#' @param name Base name of the targets.
#'   In regular `tarchetypes` functions,
#'   the `name` argument is an unevaluated symbol.
#'   In the `"_raw"` versions
#'   of functions, `name` is a character string.
#' @param command1 R code to create named arguments to `command2`.
#'   Must return a data frame with one row per call to `command2` when run.
#'
#'   In regular `tarchetypes` functions,
#'   the `command1` argument is an unevaluated expression.
#'   In the `"_raw"` versions
#'   of functions, `command1` is an evaluated expression object.
#' @param command2 R code to map over the data frame of arguments
#'   produced by `command1`. Must return a data frame.
#'
#'   In regular `tarchetypes` functions,
#'   the `command2` argument is an unevaluated expression.
#'   In the `"_raw"` versions
#'   of functions, `command2` is an evaluated expression object.
#' @param columns1 A tidyselect expression to select which columns of `values`
#'   to append to the output of all targets.
#'   Columns already in the target output are not appended.
#'
#'   In regular `tarchetypes` functions,
#'   the `columns1` argument is an unevaluated expression.
#'   In the `"_raw"` versions
#'   of functions, `columns1` is an evaluated expression object.
#' @param columns2 A tidyselect expression to select which columns of
#'   `command1`
#'   output to append to `command2` output.
#'   Columns already in the target output are not appended.
#'   `columns1` takes precedence over `columns2`.
#'
#'   In regular `tarchetypes` functions,
#'   the `columns2` argument is an unevaluated expression.
#'   In the `"_raw"` versions
#'   of functions, `columns2` is an evaluated expression object.
#' @param suffix1 Character of length 1,
#'   suffix to apply to the `command1` targets to distinguish
#'   them from the `command2` targets.
#' @param suffix2 Character of length 1,
#'   suffix to apply to the `command2` targets to distinguish
#'   them from the `command1` targets.
tar_map2 <- function(
  name,
  command1,
  command2,
  values = NULL,
  names = NULL,
  descriptions = tidyselect::everything(),
  group = rep(1L, nrow(as.data.frame(!!.x))),
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
  tar_map2_raw(
    name = deparse(substitute(name)),
    command1 = substitute(command1),
    command2 = substitute(command2),
    values = values,
    names = substitute(names),
    descriptions = substitute(descriptions),
    group = substitute(group),
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
