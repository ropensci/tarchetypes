#' @title Easy dynamic branching over files or urls (raw version).
#' @export
#' @family Dynamic branching over files
#' @description Shorthand for a pattern that correctly
#'   branches over files or urls.
#' @details `tar_files_raw()` is similar to [tar_files()]
#'   except the `name` argument must be a character string
#'   and `command` must be a language object.
#'
#'   `tar_files_raw()` creates a pair of targets, one upstream
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
#' @inheritParams targets::tar_target
#' @param format Character of length 1.
#'   Must be `"file"`, `"url"`, or `"aws_file"`. See the `format`
#'   argument of `targets::tar_target()` for details.
#' @param cue An optional object from `tar_cue()`
#'   to customize the rules that decide whether the target is up to date.
#'   Only applies to the downstream target. The upstream target always runs.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   # Do not use temp files in real projects
#'   # or else your targets will always rerun.
#'   paths <- unlist(replicate(2, tempfile()))
#'   file.create(paths)
#'   command <- as.call(list(`c`, paths))
#'   list(
#'     tarchetypes::tar_files_raw("x", command)
#'   )
#' })
#' targets::tar_make()
#' targets::tar_read(x)
#' })
#' }
tar_files_raw <- function(
  name,
  command,
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = c("file", "url", "aws_file"),
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue")
) {
  assert_chr(name, "name must be a character.")
  assert_scalar(name, "name must have length 1.")
  command <- tar_raw_command(name, command)
  name_files <- paste0(name, "_files")
  format <- match.arg(format)
  upstream <- targets::tar_target_raw(
    name = name_files,
    command = command,
    pattern = NULL,
    packages = packages,
    library = library,
    format = "rds",
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = targets::tar_cue(mode = "always")
  )
  name_files_sym <- as.symbol(name_files)
  downstream <- targets::tar_target_raw(
    name = name,
    command = as.expression(name_files_sym),
    pattern = as.expression(call_function("map", list(name_files_sym))),
    packages = character(0),
    library = library,
    format = format,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = "main",
    priority = priority,
    resources = resources,
    storage = "main",
    retrieval = "main",
    cue = cue
  )
  out <- list(upstream, downstream)
  names(out) <- c(name_files, name)
  out
}
