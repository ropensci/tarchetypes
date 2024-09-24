#' @title Dynamic branching over input files or URLs
#' @export
#' @family Dynamic branching over files
#' @description Dynamic branching over input files or URLs.
#'
#'   [tar_files_input()] expects a unevaluated symbol for the `name` argument,
#'   whereas
#'   [tar_files_input_raw()] expects a character string for `name`.
#'   See the examples
#'   for a demo.
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
#'   `format = "file_fast"`, or `format = "url"`.
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
#' @inheritParams targets::tar_target
#' @param name Name of the target.
#'   [tar_files_input()] expects a unevaluated symbol for the `name` argument,
#'   whereas
#'   [tar_files_input_raw()] expects a character string for `name`.
#'   See the examples
#'   for a demo.
#' @param files Nonempty character vector of known existing input files
#'   to track for changes.
#' @param batches Positive integer of length 1, number of batches
#'   to partition the files. The default is one file per batch
#'   (maximum number of batches) which is simplest to handle but
#'   could cause a lot of overhead and consume a lot of computing resources.
#'   Consider reducing the number of batches below the number of files
#'   for heavy workloads.
#' @param iteration Character, iteration method. Must be a method
#'   supported by the `iteration` argument of `targets::tar_target()`.
#'   The iteration method for the upstream target is always `"list"`
#'   in order to support batching.
#' @param format Character, either `"file"`, `"file_fast"`, or `"url"`.
#'   See the `format` argument of `targets::tar_target()` for details.
#' @param cue An optional object from `tar_cue()`
#'   to customize the rules that decide whether the target is up to date.
#'   Only applies to the downstream target. The upstream target always runs.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   library(tarchetypes)
#'   # Do not use temp files in real projects
#'   # or else your targets will always rerun.
#'   paths <- unlist(replicate(4, tempfile()))
#'   file.create(paths)
#'   list(
#'     tar_files_input(
#'       name = x,
#'       files = paths,
#'       batches = 2
#'     ),
#'     tar_files_input_raw(
#'       name = "y",
#'       files = paths,
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
  format = c("file", "file_fast", "url", "aws_file"),
  repository = targets::tar_option_get("repository"),
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
) {
  name <- targets::tar_deparse_language(substitute(name))
  format <- match.arg(format)
  tar_files_input_raw(
    name = name,
    files = files,
    batches = batches,
    format = format,
    repository = repository,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    priority = priority,
    resources = resources,
    cue = cue,
    description = description
  )
}
