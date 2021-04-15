#' @title Cue to skip a target if a condition is true
#' @export
#' @family cues
#' @description `tar_cue_skip()` creates a cue object to
#'   skip a target if an arbitrary condition evaluates to `TRUE`.
#'   The target still builds if it was never built before.
#'   Supply the returned cue object to the `cue` argument of
#'   `targets::tar_target()` or similar.
#' @return A cue object. See the "Cue objects" section for background.
#' @inheritSection tar_cue_force Cue objects
#' @inheritParams targets::tar_cue
#' @param condition Logical vector evaluated locally when the target is
#'   defined. If any element of `condition` is `TRUE`, the pipeline
#'   will skip the target unless the target has never been built before.
#'   If all elements of `condition` are `FALSE`, then
#'   the target may or may not rerun, depending
#'   on the other invalidation rules. `condition` is evaluated
#'   when this cue factory is called, so the condition cannot
#'   depend on upstream targets, and it should be quick to calculate.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   library(tarchetypes)
#'   list(
#'     targets::tar_target(
#'       data,
#'       data.frame(x = seq_len(26)),
#'       cue = tarchetypes::tar_cue_skip(1 > 0)
#'     )
#'   )
#' })
#' targets::tar_make()
#' targets::tar_script({
#'   library(tarchetypes)
#'   list(
#'     targets::tar_target(
#'       data,
#'       data.frame(x = seq_len(25)), # Change the command.
#'       cue = tarchetypes::tar_cue_skip(1 > 0)
#'     )
#'   )
#' })
#' targets::tar_make()
#' targets::tar_make()
#' })
#' }
tar_cue_skip <- function(
  condition,
  command = TRUE,
  depend = TRUE,
  format = TRUE,
  iteration = TRUE,
  file = TRUE
) {
  mode <- if_any(as.logical(condition), "never", "thorough")
  targets::tar_cue(
    mode = mode,
    command = command,
    depend = depend,
    format = format,
    iteration = iteration,
    file = file
  )
}
