#' @title Cue to run a target when the last run reaches a certain age
#'   (raw version)
#' @export
#' @family cues
#' @description `tar_cue_age_raw()` acts like `tar_cue_age()`
#'   except the `name` argument is a character string,
#'   not a symbol. `tar_cue_age_raw()` creates a cue object to
#'   rerun a target if the most recent output data becomes old enough.
#'   In other words, if the target produces any data
#'   (as opposed to tracking input files or URLs)
#'   then the target will rerun periodically at regular
#'   intervals of time.
#' @details `tar_cue_age_raw()` uses the time stamps from `tar_meta()$time`.
#'   If no time stamp is recorded, the cue defaults to the ordinary
#'   invalidation rules (i.e. `mode = "thorough"` in `targets::tar_cue()`).
#'   That means `tar_cue_age_raw()` cannot help with input file targets
#'   or URL targets (but if you are using `format = "url"`
#'   and your URLs have either ETags or "last-modified" time stamps,
#'   then you are better off without `tar_cue_age_raw()` anyway.)
#' @return A cue object. See the "Cue objects" section for background.
#' @inheritSection tar_cue_force Cue objects
#' @inheritParams targets::tar_cue
#' @param name Character of length 1, name of the target.
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
#'       cue = tarchetypes::tar_cue_age_raw(
#'         name = "data",
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
tar_cue_age_raw <- function(
  name,
  age,
  command = TRUE,
  depend = TRUE,
  format = TRUE,
  iteration = TRUE,
  file = TRUE
) {
  assert_chr(name, "name must be a character.")
  assert_scalar(name, "name must have length 1.")
  assert_nzchar(name, "name must be nonempty.")
  assert_inherits(
    age,
    "difftime",
    "age must be a difftime object, e.g. as.difftime(3, units = \"days\")."
  )
  meta <- if_any(
    targets::tar_exist_meta(),
    targets::tar_meta(),
    data.frame(
      name = character(0),
      time = character(0),
      stringsAsFactors = FALSE
    )
  )
  names <- c(name, unlist(meta$children[meta$name == name]))
  times <- as.POSIXct(meta$time[meta$name %in% names])
  time <- max(c(times, -Inf), na.rm = TRUE)
  span <- difftime(time1 = Sys.time(), time2 = time, units = units(age))
  mode <- if_any((span > age) %||NA% FALSE, "always", "thorough")
  targets::tar_cue(
    mode = mode,
    command = command,
    depend = depend,
    format = format,
    iteration = iteration,
    file = file
  )
}
