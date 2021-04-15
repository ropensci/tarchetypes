#' @title Easy dynamic branching over known existing
#'   input files or urls.
#' @export
#' @family Dynamic branching over files
#' @description Shorthand for a pattern that correctly
#'   branches over known existing files or urls.
#' @details `tar_files_input()` is like `tar_files()`
#'   but more convenient when the files in question already
#'   exist and are known in advance. Whereas `tar_files()`
#'   always appears outdated (e.g. with `tar_outdated()`)
#'   because it always needs to check which files it needs to
#'   branch over, `tar_files_input()` will appear up to date
#'   if the files have not changed since last `tar_make()`.
#'   In addition, `tar_files_input()` automatically groups
#'   input files into batches to reduce overhead and
#'   increase the efficiency of parallel processing.
#'
#'   `tar_files_input()` creates a pair of targets, one upstream
#'   and one downstream. The upstream target does some work
#'   and returns some file paths, and the downstream
#'   target is a pattern that applies `format = "file"`,
#'   `format = "url"`, or `format = "aws_file"`.
#'   This is the correct way to dynamically
#'   iterate over file/url targets. It makes sure any downstream patterns
#'   only rerun some of their branches if the files/urls change.
#'   For more information, visit
#'   <https://github.com/ropensci/targets/issues/136> and
#'   <https://github.com/ropensci/drake/issues/1302>.
#' @return A list of two targets, one upstream and one downstream.
#'   The upstream one does some work and returns some file paths,
#'   and the downstream target is a pattern that applies `format = "file"`
#'   or `format = "url"`.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams tar_files_input_raw
#' @inheritParams targets::tar_target
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   # Do not use temp files in real projects
#'   # or else your targets will always rerun.
#'   paths <- unlist(replicate(4, tempfile()))
#'   file.create(paths)
#'   list(
#'     tarchetypes::tar_files_input(
#'       x,
#'       paths,
#'       batches = 2
#'     )
#'   )
#' })
#' targets::tar_make()
#' targets::tar_read(x)
#' targets::tar_read(x, branches = 1)
#' })
#' }
tar_files_input <- function(
  name,
  files,
  batches = length(files),
  format = c("file", "url", "aws_file"),
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  cue = targets::tar_option_get("cue")
) {
  name <- deparse_language(substitute(name))
  format <- match.arg(format)
  tar_files_input_raw(
    name = name,
    files = files,
    batches = batches,
    format = format,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    priority = priority,
    resources = resources,
    cue = cue
  )
}
