#' @title Target that responds to an arbitrary change.
#' @description Create a target that responds to a change
#'   in an arbitrary value. If the value changes, the target reruns.
#' @details `tar_change()` creates a pair of targets, one upstream
#'   and one downstream. The upstream target always runs and returns
#'   an auxiliary value. This auxiliary value gets referenced in the
#'   downstream target, which causes the downstream target to rerun
#'   if the auxiliary value changes. The behavior is cancelled if
#'   `cue` is `tar_cue(depend = FALSE)` or `tar_cue(mode = "never")`.
#' @export
#' @inheritParams targets::tar_target
#' @return A list of two targets, one upstream and one downstream.
#'   The upstream one triggers the change, and the downstream one
#'   responds to it. See the examples for details.
#' @param change R code for the upstream change-inducing target.
#' @param tidy_eval Whether to invoke tidy evaluation
#'   (e.g. the `!!` operator from `rlang`) as soon as the target is defined
#'   (before `tar_make()`). Applies to arguments `command` and `change`.
#' @examples
#' \dontrun{
#' # Without loss of generality,
#' # tar_change(your_target, command = fun(), change = aux())
#' # is equivalent to:
#' # list(
#' #   tar_target(your_target_change, aux(), cue = tar_cue(mode = "always")),
#' #   tar_target(your_target, {
#' #     x_change
#' #     fun()
#' #   })
#' # )
#' # Try it out. The following pipeline should always run
#' # both targets (x and x_change).
#' targets::tar_dir({
#' targets::tar_script({
#'   tar_pipeline(
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
  name <- deparse_language(substitute(name))
  name_change <- paste0(name, "_change")
  envir <- tar_option_get("envir")
  command <- tidy_eval(substitute(command), envir, tidy_eval)
  change <- tidy_eval(substitute(change), envir, tidy_eval)
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
  upstream <- tar_target_raw(
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
  downstream <- tar_target_raw(
    name = name,
    command = call_brace(list(rlang::sym(name_change), command)),
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
