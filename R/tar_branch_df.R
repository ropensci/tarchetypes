#' @title Batched dynamic-within-static branching for data frames
#' @export
#' @family branching
#' @description Create data frame targets with static branching,
#'   then map over the row groups (batches) of those data frames
#'   with dynamic branching.
#' @return A list of new target objects. If `unlist` is `FALSE`,
#'   the list is nested and sub-lists are named and grouped by the original
#'   input targets. If `unlist = TRUE`, the return value is a flat list of
#'   targets named by the new target names.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams tar_map
#' @inheritParams tar_rep
tar_branch_df <- function(
  name,
  command_static,
  command_dynamic,
  columns_static = tidyselect::everything(),
  columns_dynamic = tidyselect::everything(),
  values = NULL,
  names = tidyselect::everything(),
  batches = 2,
  unlist = FALSE,
  combine = TRUE
) {
  
}