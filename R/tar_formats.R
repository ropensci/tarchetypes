#' @title Target formats
#' @name tar_formats
#' @description Target archetypes for specialized storage formats.
#' @details These functions are shorthand for targets with specialized
#'   storage formats. For example, `tar_qs(name, fun())` is equivalent to
#'   `tar_target(name, fun(), format = "qs")`.
#'   For details on specialized storage formats, open the help file of the
#'   `targets::tar_target()` function and read about the `format` argument.
#' @return A `tar_target()` object.
#' @inheritParams targets::tar_target
#' @examples
#' \dontrun{
#' targets::tar_script(
#'   tar_pipeline(
#'     tar_rds(x, 1) # equivalent to tar_target(x, 1, format = "rds")
#'   )
#' )
#' targets::tar_make()
#' }
NULL
