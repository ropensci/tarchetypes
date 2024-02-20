#' @title Dynamic branching over output or input files.
#' @export
#' @family Dynamic branching over files
#' @description Dynamic branching over output or input files.
#' @details `tar_files()` creates a pair of targets, one upstream
#'   and one downstream. The upstream target does some work
#'   and returns some file paths, and the downstream
#'   target is a pattern that applies `format = "file"`,
#'   `format = "file_fast"`, or `format = "url"`.
#'   (URLs are input-only, they must already exist beforehand.)
#'   This is the correct way to dynamically
#'   iterate over file/url targets. It makes sure any downstream patterns
#'   only rerun some of their branches if the files/urls change.
#'   For more information, visit
#'   <https://github.com/ropensci/targets/issues/136> and
#'   <https://github.com/ropensci/drake/issues/1302>.
#' @return A list of two targets, one upstream and one downstream.
#'   The upstream one does some work and returns some file paths,
#'   and the downstream target is a pattern that applies `format = "file"`,
#'   `format = "file_fast"`, or `format = "url"`.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams tar_files_raw
#' @inheritParams targets::tar_target
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   # Do not use temp files in real projects
#'   # or else your targets will always rerun.
#'   paths <- unlist(replicate(2, tempfile()))
#'   file.create(paths)
#'   list(
#'     tarchetypes::tar_files(x, paths)
#'   )
#' })
#' targets::tar_make()
#' targets::tar_read(x)
#' })
#' }
tar_files <- function(
  name,
  command,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = c("file", "file_fast", "url", "aws_file"),
  repository = targets::tar_option_get("repository"),
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
) {
  name <- targets::tar_deparse_language(substitute(name))
  envir <- tar_option_get("envir")
  command <- targets::tar_tidy_eval(substitute(command), envir, tidy_eval)
  format <- match.arg(format)
  tar_files_raw(
    name = name,
    command = command,
    packages = packages,
    library = library,
    format = format,
    repository = repository,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue,
    description = description
  )
}
