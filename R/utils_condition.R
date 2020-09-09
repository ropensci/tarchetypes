throw_run <- function(...) {
  stop(condition_run(...))
}

throw_validate <- function(...) {
  stop(condition_validate(...))
}

warn_validate <- function(...) {
  warning(warning_validate(...))
}

condition_run <- function(...) {
  structure(
    list(message = paste0(..., collapse = ""), call = NULL),
    class = c("condition_run", "condition_targets", "error", "condition")
  )
}

condition_validate <- function(...) {
  structure(
    list(message = paste0(..., collapse = ""), call = NULL),
    class = c(
      "condition_validate",
      "condition_tarchetypes",
      "error",
      "condition"
    )
  )
}

warning_validate <- function(...) {
  structure(
    list(message = paste0(..., collapse = ""), call = NULL),
    class = c(
      "condition_validate",
      "condition_targets",
      "warning",
      "condition"
    )
  )
}
