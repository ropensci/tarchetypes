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
#' @inheritParams targets::tar_target_raw
#' @return A list of two targets, one upstream and one downstream.
#'   The upstream one triggers the change, and the downstream one
#'   responds to it. See the examples for details.
#' @param change R code for the upstream change-inducing target.
#' @examples
#' \dontrun{
#' # Without loss of generality,
#' tar_change(your_target, command = fun(), change = aux())
#' # is equivalent to:
#' list(
#'   tar_target(your_target_change, aux(), cue = tar_cue(mode = "always")),
#'   tar_target(your_target, {
#'     x_change
#'     fun()
#'   })
#' )
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
  template = targets::tar_option_get("template"),
  deployment = targets::tar_option_get("deployment"),
  priority = 0,
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
  upstream <- tar_target_raw(
    name = name_change,
    command = change,
    pattern = NULL,
    packages = packages,
    library = library,
    envir = envir,
    format = format,
    iteration = iteration,
    error = error,
    memory = memory,
    deployment = deployment,
    priority = priority,
    template = template,
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
    envir = envir,
    format = format,
    iteration = iteration,
    error = error,
    memory = memory,
    deployment = deployment,
    priority = priority,
    template = template,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
  list(upstream, downstream)
}
