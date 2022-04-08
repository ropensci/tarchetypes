#' @title Target with a custom cancellation condition.
#' @export
#' @family targets with custom invalidation rules
#' @description Create a target that cancels itself if a user-defined
#'   decision rule is met.
#' @details `tar_skip()` creates a target that cancels itself
#'   whenever a custom condition is met. The mechanism of cancellation
#'   is `targets::tar_cancel(your_condition)`, which allows skipping to happen
#'   even if the target does not exist yet. This behavior differs from
#'   `tar_cue(mode = "never")`, which still runs if the target does not exist.
#' @inheritParams targets::tar_target
#' @return A target object with `targets::tar_cancel(your_condition)` inserted
#'   into the command.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @param skip R code for the skipping condition. If it evaluates to `TRUE`
#'   during `tar_make()`, the target will cancel itself.
#' @param tidy_eval Whether to invoke tidy evaluation
#'   (e.g. the `!!` operator from `rlang`) as soon as the target is defined
#'   (before `tar_make()`). Applies to arguments `command` and `skip`.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   list(
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
  cue = targets::tar_option_get("cue")
) {
  name <- targets::tar_deparse_language(substitute(name))
  envir <- tar_option_get("envir")
  command <- targets::tar_tidy_eval(substitute(command), envir, tidy_eval)
  skip <- targets::tar_tidy_eval(substitute(skip), envir, tidy_eval)
  skip <- as.call(list(call_ns("targets", "tar_cancel"), skip))
  command <- call_brace(list(skip, command))
  targets::tar_target_raw(
    name = name,
    command = command,
    pattern = NULL,
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
    cue = cue
  )
}
