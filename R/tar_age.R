#' @title Create a target that runs when the last run gets old
#' @export
#' @family cues
#' @description `tar_age()` creates a target that reruns
#'   itself when it gets old enough.
#'   In other words, the target reruns periodically at regular
#'   intervals of time.
#' @details `tar_age()` uses the cue from [tar_cue_age()], which
#'   uses the time stamps from `tar_meta()$time`.
#'   If no time stamp is recorded, the cue defaults to the ordinary
#'   invalidation rules (i.e. `mode = "thorough"` in `targets::tar_cue()`).
#' @section Dynamic branches at regular time intervals:
#'   Time stamps are not recorded for whole dynamic targets,
#'   so `tar_age()` is not a good fit for dynamic branching.
#'   To invalidate dynamic branches at regular intervals,
#'   it is recommended to use `targets::tar_older()` in combine
#'   with `targets::tar_invalidate()` right before calling `tar_make()`.
#'   For example,
#'   `tar_invalidate(all_of(tar_older(Sys.time - as.difftime(1, units = "weeks"))))` # nolint
#'   invalidates all targets more than a week old. Then, the next `tar_make()`
#'   will rerun those targets.
#' @return A target object. See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams tar_cue_age_raw
#' @inheritParams targets::tar_target
#' @param command R code to run the target and return a value.
#' @param cue A `targets::tar_cue()` object. (See the "Cue objects"
#'   section for background.) This cue object should contain any
#'   optional secondary invalidation rules, anything except
#'   the `mode` argument. `mode` will be automatically determined
#'   by the `age` argument of `tar_age()`.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   library(tarchetypes)
#'   list(
#'     tarchetypes::tar_age(
#'       data,
#'       data.frame(x = seq_len(26)),
#'       age = as.difftime(0.5, units = "secs")
#'     )
#'   )
#' })
#' targets::tar_make()
#' Sys.sleep(0.6)
#' targets::tar_make()
#' })
#' }
tar_age <- function(
  name,
  command,
  age,
  pattern = NULL,
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
  envir <- tar_option_get("envir")
  command <- as.expression(substitute(command))
  pattern <- as.expression(substitute(pattern))
  command <- targets::tar_tidy_eval(command, envir, tidy_eval)
  pattern <- targets::tar_tidy_eval(pattern, envir, tidy_eval)
  cue <- tar_cue_age_raw(
    name = name,
    age = age,
    command = cue$command,
    depend = cue$depend,
    format = cue$format,
    iteration = cue$iteration,
    file = cue$file
  )
  targets::tar_target_raw(
    name = name,
    command = command,
    pattern = pattern,
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
