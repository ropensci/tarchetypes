#' @title Target that responds to an arbitrary change.
#' @export
#' @family targets with custom invalidation rules
#' @description Create a target that responds to a change
#'   in an arbitrary value. If the value changes, the target reruns.
#' @details `tar_change()` creates a pair of targets, one upstream
#'   and one downstream. The upstream target always runs and returns
#'   an auxiliary value. This auxiliary value gets referenced in the
#'   downstream target, which causes the downstream target to rerun
#'   if the auxiliary value changes. The behavior is cancelled if
#'   `cue` is `tar_cue(depend = FALSE)` or `tar_cue(mode = "never")`.
#'
#'   Because the upstream target always runs,
#'   `tar_outdated()` and `tar_visnetwork()` will always
#'   show both targets as outdated. However, `tar_make()` will still
#'   skip the downstream one if the upstream target
#'   did not detect a change.
#' @return A list of two target objects, one upstream and one downstream.
#'   The upstream one triggers the change, and the downstream one
#'   responds to it.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target
#' @param change R code for the upstream change-inducing target.
#' @param tidy_eval Whether to invoke tidy evaluation
#'   (e.g. the `!!` operator from `rlang`) as soon as the target is defined
#'   (before `tar_make()`). Applies to arguments `command` and `change`.
#' @param cue An optional object from `tar_cue()`
#'   to customize the rules that decide whether the target is up to date.
#'   Only applies to the downstream target. The upstream target always runs.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   list(
#'     tarchetypes::tar_change(x, command = tempfile(), change = tempfile())
#'   )
#' })
#' targets::tar_make()
#' targets::tar_make()
#' })
#' }
tar_change <- function(
  name,
  command,
  change,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = targets::tar_option_get("format"),
  iteration = targets::tar_option_get("iteration"),
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
  name <- targets::tar_deparse_language(substitute(name))
  name_change <- paste0(name, "_change")
  envir <- tar_option_get("envir")
  command <- targets::tar_tidy_eval(substitute(command), envir, tidy_eval)
  change <- targets::tar_tidy_eval(substitute(change), envir, tidy_eval)
  tar_change_raw(
    name = name,
    name_change = name_change,
    command = command,
    change = change,
    packages = packages,
    library = library,
    format = format,
    iteration = iteration,
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

tar_change_raw <- function(
  name,
  name_change,
  command,
  change,
  packages,
  library,
  format,
  iteration,
  error,
  memory,
  garbage_collection,
  deployment,
  priority,
  resources,
  storage,
  retrieval,
  cue
) {
  upstream <- targets::tar_target_raw(
    name = name_change,
    command = change,
    pattern = NULL,
    packages = packages,
    library = library,
    format = "rds",
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = targets::tar_cue(mode = "always")
  )
  downstream <- targets::tar_target_raw(
    name = name,
    command = call_brace(list(as.symbol(name_change), command)),
    pattern = NULL,
    packages = packages,
    library = library,
    format = format,
    iteration = iteration,
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
  list(upstream, downstream)
}
