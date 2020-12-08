#' @title Easy dynamic branching over known existing
#'   input files or urls.
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
#'   <https://github.com/wlandau/targets/issues/136> and
#'   <https://github.com/ropensci/drake/issues/1302>.
#' @export
#' @return A list of two targets, one upstream and one downstream.
#'   The upstream one does some work and returns some file paths,
#'   and the downstream target is a pattern that applies `format = "file"`
#'   or `format = "url"`.
#' @inheritParams tar_files_input_raw
#' @inheritParams targets::tar_target
#' @examples
#' if (identical(Sys.getenv("TARCHETYPES_LONG_EXAMPLES"), "true")) {
#' # Without loss of generality,
#' # tar_files_input(
#' #   your_target,
#' #   files = c("a.txt", "b.txt", "c.txt", "d.txt")),
#' #   batches = 2
#' # )
#' # is equivalent to:
#' # list(
#' #   tar_target(
#' #     your_target_files,
#' #     list(c("a.txt", "b.txt"), c("c.txt", "d.txt")),
#' #     iteration = "list",
#' #     deployment = "main",
#' #     storage = "main",
#' #     retrieval = "main"
#' #   ),
#' #   tar_target(
#' #     your_target,
#' #     your_target_files,
#' #     pattern = map(your_target_files),
#' #     format = "file",
#' #     deployment = "main",
#' #     storage = "main",
#' #     retrieval = "main"
#' #   )
#' # )
#' # Try it out.
#' targets::tar_dir({
#' file.create(c("a.txt", "b.txt", "c.txt", "d.txt"))
#' targets::tar_script({
#'   tar_pipeline(
#'     tarchetypes::tar_files_input(
#'       x,
#'       c("a.txt", "b.txt", "c.txt", "d.txt"),
#'       batches = 2
#'     )
#'   )
#' })
#' targets::tar_make()
#' # Should be up to date now.
#' targets::tar_make()
#' # If we change one file, `tar_make()` will only rerun one batch.
#' writeLines("a", "a.txt")
#' targets::tar_make()
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
