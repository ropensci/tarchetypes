#' @title Cue to run a target when the last run reaches a certain age
#' @export
#' @family cues
#' @description `tar_cue_age()` creates a cue object to
#'   rerun a target if the last run becomes old enough.
#'   In other words, the target reruns periodically at regular
#'   intervals of time.
#' @details `tar_cue_age()` uses the time stamps from `tar_meta()$time`.
#'   If no time stamp is recorded, the cue defaults to the ordinary
#'   invalidation rules (i.e. `mode = "thorough"` in `targets::tar_cue()`).
#'   That means `tar_cue_age()` cannot help with URL targets,
#'   e.g. `format = "url"`. (But if you are using `format = "url"`
#'   and your URLs have either ETags or "last-modified" time stamps,
#'   then you are better off without `tar_cue_age()` anyway.)
#' @return A cue object. See the "Cue objects" section for background.
#' @inheritSection tar_cue_force Cue objects
#' @inheritParams tar_cue_age_raw
#' @param name Symbol, name of the target.
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
  iteration = TRUE,
  file = TRUE
) {
  name <- deparse_language(substitute(name))
  tar_cue_age_raw(
    name = name,
    age = age,
    command = command,
    depend = depend,
    format = format,
    iteration = iteration,
    file = file
  )
}
