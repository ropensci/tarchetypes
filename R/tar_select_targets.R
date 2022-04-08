#' @title Select target objects from a target list
#' @export
#' @family target selection
#' @description Select target objects from a target list.
#' @return A list of target objects. See the "Target objects" section
#'   of this help file.
#' @inheritSection tar_map Target objects
#' @param targets A list of target objects as described in the
#'   "Target objects" section. It does not matter how nested
#'   the list is as long as the only leaf nodes are targets.
#' @param ... One or more comma-separated `tidyselect` expressions,
#'   e.g. `starts_with("prefix")`. Just like `...` in `dplyr::select()`.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets <- list(
#'   list(
#'     targets::tar_target(x, 1),
#'     targets::tar_target(y1, 2)
#'   ),
#'   targets::tar_target(y2, 3),
#'   targets::tar_target(z, 4)
#' )
#' tar_select_targets(targets, starts_with("y"), contains("z"))
#' })
#' }
tar_select_targets <- function(targets, ...) {
  targets <- unlist(list(targets), recursive = TRUE)
  names(targets) <- map_chr(targets, ~.x$settings$name)
  out <- tidyselect::eval_select(
    rlang::expr(c(...)),
    data = targets,
    strict = FALSE
  )
  targets[sort(names(out))]
}
