#' @title Cue to run a target when the last output reaches a certain age
#' @export
#' @family cues
#' @description `tar_cue_age()` creates a cue object to
#'   rerun a target if the most recent output data becomes old enough.
#'   The age of the target is determined by `targets::tar_timestamp()`,
#'   and the way the time stamp is calculated is explained
#'   in the Details section of the help file of that function.
#'
#'   `tar_cue_age()` expects an unevaluated symbol for the `name`
#'   argument, whereas `tar_cue_age_raw()` expects a character string
#'   for `name`.
#' @details `tar_cue_age()` uses the time stamps from `tar_meta()$time`.
#'   If no time stamp is recorded, the cue defaults to the ordinary
#'   invalidation rules (i.e. `mode = "thorough"` in `targets::tar_cue()`).
#' @inheritSection tar_age Dynamic branches at regular time intervals
#' @return A cue object. See the "Cue objects" section for background.
#' @inheritSection tar_cue_force Cue objects
#' @inheritSection tar_cue_force Cue objects
#' @inheritParams targets::tar_cue
#' @param name Name of the target.
#'   `tar_cue_age()` expects an unevaluated symbol for the `name`
#'   argument, whereas `tar_cue_age_raw()` expects a character string
#'   for `name`.
#' @param age A `difftime` object of length 1, such as
#'   `as.difftime(3, units = "days")`. If the target's output data
#'   files are older than `age` (according to the most recent
#'   time stamp over all the target's output files)
#'   then the target will rerun.
#'   On the other hand, if at least one data file is
#'   younger than `Sys.time() - age`, then the ordinary
#'   invalidation rules apply, and the target may or not rerun.
#'   If you want to force the target to run every 3 days,
#'   for example, set `age = as.difftime(3, units = "days")`.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   library(tarchetypes)
#'   list(
#'     targets::tar_target(
#'       data,
#'       data.frame(x = seq_len(26)),
#'       cue = tarchetypes::tar_cue_age(
#'         name = data,
#'         age = as.difftime(0.5, units = "secs")
#'       )
#'     )
#'   )
#' })
#' targets::tar_make()
#' Sys.sleep(0.6)
#' targets::tar_make()
#' })
#' }
tar_cue_age <- function(
  name,
  age,
  command = TRUE,
  depend = TRUE,
  format = TRUE,
  repository = TRUE,
  iteration = TRUE,
  file = TRUE
) {
  name <- targets::tar_deparse_language(substitute(name))
  tar_cue_age_raw(
    name = name,
    age = age,
    command = command,
    depend = depend,
    format = format,
    repository = repository,
    iteration = iteration,
    file = file
  )
}
