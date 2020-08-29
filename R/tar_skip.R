#' @title Target with a custom cancellation condition.
#' @description Create a target that cancels itself if a user-defined
#'   decision rule is met.
#' @details `tar_skip()` creates a target that cancels itself
#'   whenever a custom condition is met. The mechanism of cancellation
#'   is `targets::tar_cancel(your_condition)`, which allows skipping to happen
#'   even if the target does not exist yet. This behavior differs from
#'   `tar_cue(mode = "never")`, which still runs if the target does not exist.
#' @export
#' @inheritParams targets::tar_target_raw
#' @return A target with `targets::tar_cancel(your_condition)` inserted
#'   into the command.
#' @param skip R code for the skipping condition. If it evaluates to `TRUE`
#'   during `tar_make()`, the target will cancel itself.
#' @param tidy_eval Whether to invoke tidy evaluation
#'   (e.g. the `!!` operator from `rlang`) as soon as the target is defined
#'   (before `tar_make()`). Applies to arguments `command` and `skip`.
#' @examples
#' \dontrun{
#' # Without loss of generality,
#' tar_skip(your_target, command = run_stuff(), skip = should_skip())
#' # is equivalent to:
#' tar_target(your_target, {
#'   tar_cancel(should_skip())
#'   run_stuff()
#' })
#' # Try it out.
#' targets::tar_dir({
#' targets::tar_script({
#'   tar_pipeline(
#'     tarchetypes::tar_skip(x, command = "value", skip = 1 > 0)
#'   )
#' })
#' targets::tar_make()
#' })
#' }
tar_skip <- function(
  name,
  command,
  skip,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = targets::tar_option_get("format"),
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  template = targets::tar_option_get("template"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue")
) {
  name <- deparse_language(substitute(name))
  envir <- tar_option_get("envir")
  command <- tidy_eval(substitute(command), envir, tidy_eval)
  skip <- tidy_eval(substitute(skip), envir, tidy_eval)
  skip <- as.call(list(call_ns("targets", "tar_cancel"), skip))
  command <- call_brace(list(skip, command))
  tar_target_raw(
    name = name,
    command = command,
    pattern = NULL,
    packages = packages,
    library = library,
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
}
