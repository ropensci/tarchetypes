#' @title Easy dynamic branching over files or urls.
#' @export
#' @description Shorthand for a pattern that correctly
#'   branches over files or urls.
#' @details `tar_files()` creates a pair of targets, one upstream
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
#'   Target objects represent skippable steps of the analysis pipeline
#'   as described at <https://books.ropensci.org/targets/>.
#'   Please see the design specification at
#'   <https://books.ropensci.org/targets-design/>
#'   to learn about the structure and composition of target objects.
#' @inheritParams tar_files_raw
#' @inheritParams targets::tar_target
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({
#' file.create(c("a.txt", "b.txt"))
#' targets::tar_script({
#'   list(
#'     tarchetypes::tar_files(x, c("a.txt", "b.txt"))
#'   )
#' })
#' targets::tar_make()
#' # Should be up to date now.
#' targets::tar_make()
#' # If we change one file, `tar_make()` will only rerun one branch.
#' writeLines("a", "a.txt")
#' targets::tar_make()
#' })
#' }
tar_files <- function(
  name,
  command,
  tidy_eval = targets::tar_option_get("tidy_eval"),
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
  name <- deparse_language(substitute(name))
  envir <- tar_option_get("envir")
  command <- tidy_eval(substitute(command), envir, tidy_eval)
  format <- match.arg(format)
  tar_files_raw(
    name = name,
    command = command,
    packages = packages,
    library = library,
    format = format,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
}

#' @title Easy dynamic branching over files or urls (raw version).
#' @export
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
#' @inheritParams targets::tar_target
#' @param format Character of length 1.
#'   Must be `"file"`, `"url"`, or `"aws_file"`. See the `format`
#'   argument of `targets::tar_target()` for details.
#' @param cue An optional object from `tar_cue()`
#'   to customize the rules that decide whether the target is up to date.
#'   Only applies to the downstream target. The upstream target always runs.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' # Without loss of generality,
#' # tar_files_raw(
#' #   "your_target",
#' #   quote(fn_that_returns_paths(c("a.txt", "b.txt")))
#' # )
#' # is equivalent to:
#' # list(
#' #   tar_target(
#' #     your_target_files,
#' #     fn_that_returns_paths(c("a.txt", "b.txt")),
#' #     cue = tar_cue(mode = "always")
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
#' file.create(c("a.txt", "b.txt"))
#' targets::tar_script({
#'   list(
#'     tarchetypes::tar_files_raw("x", quote(c("a.txt", "b.txt")))
#'   )
#' })
#' targets::tar_make()
#' # Should be up to date now.
#' targets::tar_make()
#' # If we change one file, `tar_make()` will only rerun one branch.
#' writeLines("a", "a.txt")
#' targets::tar_make()
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
  assert_lang(command, "command must be a language object.")
  name_files <- paste0(name, "_files")
  format <- match.arg(format)
  upstream <- tar_target_raw(
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
  name_files_sym <- rlang::sym(name_files)
  downstream <- tar_target_raw(
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
