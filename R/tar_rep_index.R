#' @title Get overall rep index.
#' @export
#' @family Dynamic batched replication indexing
#' @description Get the integer index of the current replication
#'   in certain target factories.
#' @details
#'   [tar_rep_index()] cannot run in your interactive R session
#'   or even the setup portion of `_targets.R`.
#'   It must be part of the R command of a target actively
#'   running in a pipeline.
#'
#'   In addition, [tar_rep_index()] is only compatible with
#'   [tar_rep()], [tar_rep2()], [tar_map_rep()], [tar_map2_count()],
#'   and [tar_map2_size()].
#'   In the latter 3 cases, [tar_rep_index()] cannot be part of
#'   the `values` or `command1` arguments.
#'
#'   In [tar_map_rep()], each row of the `values` argument
#'   (each "scenario") gets its own independent set of index values from 1 to
#'   `batches * reps`.
#' @return Positive integer from 1 to `batches * reps`,
#'   index of the current replication in an ongoing pipeline.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   tar_map_rep(
#'     x,
#'     data.frame(index = tar_rep_index()),
#'     batches = 2L,
#'     reps = 3L,
#'     values = list(value = c("a", "b"))
#'   )
#' })
#' targets::tar_make()
#' x <- targets::tar_read(x)
#' all(x$index == x$tar_rep + (3L * (x$tar_batch - 1L)))
#' #> TRUE
#' })
#' }
tar_rep_index <- function() {
  index <- .subset2(step_tar_rep, "index")
  if (is.null(index)) {
    message <-  paste(
      "tar_rep_index() can only run inside the R command of a target",
      "actively running in a pipeline, and it is only compatible with",
      "these specific target factories:",
      "tar_rep(), tar_rep2(), tar_map_rep(),",
      "tar_map2_count(), and tar_map2_size()."
    )
    targets::tar_throw_run(message)
  }
  index
}
