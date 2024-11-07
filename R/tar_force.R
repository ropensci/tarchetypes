#' @title Target with a custom condition to force execution.
#' @export
#' @family targets with custom invalidation rules
#' @description Create a target that always runs if a user-defined
#'   condition rule is met.
#' @details `tar_force()` creates a target that always runs
#'   when a custom condition is met. The implementation builds
#'   on top of [tar_change()]. Thus, a pair of targets is created:
#'   an upstream auxiliary target to indicate the custom condition
#'   and a downstream target that responds to it and does your work.
#'
#'   `tar_force()` does not actually use [tar_cue_force()], and the
#'   mechanism is totally different.
#'   Because the upstream target always runs,
#'   `tar_outdated()` and `tar_visnetwork()` will always
#'   show both targets as outdated. However, `tar_make()` will still
#'   skip the downstream one if the upstream custom condition is not met.
#' @return A list of 2 targets objects: one to indicate whether the custom
#'   condition is met, and another to respond to it and do your
#'   actual work.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target
#' @param force R code for the condition that forces a build.
#'   If it evaluates to `TRUE`, then your work will run during `tar_make()`.
#' @param tidy_eval Whether to invoke tidy evaluation
#'   (e.g. the `!!` operator from `rlang`) as soon as the target is defined
#'   (before `tar_make()`). Applies to arguments `command` and `force`.
#' @param cue An optional object from `tar_cue()`
#'   to customize the rules that decide whether the target is up to date.
#'   Only applies to the downstream target. The upstream target always runs.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   list(
#'     tarchetypes::tar_force(x, tempfile(), force = 1 > 0)
#'   )
#' })
#' targets::tar_make()
#' targets::tar_make()
#' })
#' }
tar_force <- function(
  name,
  command,
  force,
  tidy_eval = targets::tar_option_get("tidy_eval"),
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
  name <- targets::tar_deparse_language(substitute(name))
  name_change <- paste0(name, "_change")
  envir <- tar_option_get("envir")
  command <- targets::tar_tidy_eval(substitute(command), envir, tidy_eval)
  force <- targets::tar_tidy_eval(substitute(force), envir, tidy_eval)
  change <- as.call(list(call_ns("tarchetypes", "tar_force_change"), force))
  tar_change_raw(
    name = name,
    name_change = name_change,
    command = command,
    change = change,
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

#' @title Convert a condition into a change.
#' @description Supports [tar_force()]. This is really an internal function
#'   and not meant to be called by users directly.
#' @return A hash that changes when the downstream target is supposed to run.
#' @export
#' @keywords internal
#' @param condition Logical, whether to run the downstream target
#'   in [tar_force()].
tar_force_change <- function(condition) {
  name <- targets::tar_name()
  store <- targets::tar_runtime_object()$store
  path <- targets::tar_path_target(name = name, store = store)
  new <- basename(tempfile(pattern = ""))
  old <- if_any(file.exists(path), readRDS(path), new)
  if_any(condition, new, old)
}
