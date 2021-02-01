#' @title Target with a custom condition to force execution.
#' @description Create a target that always runs if a user-defined
#'   condition rule is met.
#' @details `tar_force()` creates a target that always runs
#'   when a custom condition is met. The implementation builds
#'   on top of [tar_change()]. Thus, a pair of targets is created:
#'   an upstream auxiliary target to indicate the custom condition
#'   and a downstream target that responds to it and does your work.
#' @export
#' @inheritParams targets::tar_target
#' @return A list of targets: one to indicate whether the custom
#'   condition is met, and another to respond to it and do your
#'   actual work.
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
  name <- deparse_language(substitute(name))
  name_change <- paste0(name, "_change")
  envir <- tar_option_get("envir")
  command <- tidy_eval(substitute(command), envir, tidy_eval)
  force <- tidy_eval(substitute(force), envir, tidy_eval)
  change <- as.call(list(call_ns("tarchetypes", "tar_force_change"), force))
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

#' @title Convert a condition into a change.
#' @description Supports [tar_force()]. This is really an internal function
#'   and not meant to be called by users directly.
#' @return A hash that changes when the downstream target is supposed to run.
#' @export
#' @keywords internal
#' @param condition Logical, whether to run the downstream target
#'   in [tar_force()].
tar_force_change <- function(condition) {
  path <- targets::tar_path()
  new <- basename(tempfile(pattern = ""))
  old <- trn(file.exists(path), readRDS(path), new)
  trn(condition, new, old)
}
