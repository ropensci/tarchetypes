#' @rdname tar_cue_age
#' @export
tar_cue_age_raw <- function(
  name,
  age,
  command = TRUE,
  depend = TRUE,
  format = TRUE,
  repository = TRUE,
  iteration = TRUE,
  file = TRUE
) {
  targets::tar_assert_chr(name, "name must be a character.")
  targets::tar_assert_scalar(name, "name must have length 1.")
  targets::tar_assert_nzchar(name, "name must be nonempty.")
  targets::tar_assert_inherits(
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
    repository = repository,
    iteration = iteration,
    file = file
  )
}
