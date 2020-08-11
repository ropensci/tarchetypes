#' @title Easy branching over dynamic files.
#' @description Shorthand for a pattern that correctly
#'   branches over files.
#' @details `tar_files()` creates a pair of targets, one upstream
#'   and one downstream. The upstream target does some work
#'   and returns some file paths, and the downstream
#'   target is a pattern that applies `format = "file"`.
#'   This is the correct way to dynamically
#'   iterate over file targets. It makes sure any downstream patterns
#'   only rerun some of their branches if the files change.
#'   For more information, visit
#'   <https://github.com/wlandau/targets/issues/136> and
#'   <https://github.com/ropensci/drake/issues/1302>.
#' @export
#' @inheritParams targets::tar_target_raw
#' @return A list of two targets, one upstream and one downstream.
#'   The upstream one does some work and returns some file paths,
#'   and the downstream target is a pattern that applies `format = "file"`.
#' @param tidy_eval Whether to invoke tidy evaluation
#'   (e.g. the `!!` operator from `rlang`) as soon as the target is defined
#'   (before `tar_make()`). Applies to the `command` argument.
#' @examples
#' \dontrun{
#' # Without loss of generality,
#' tar_files(your_target, c("a.txt", "b.txt"))
#' # is equivalent to:
#' list(
#'   tar_target(your_target_files, c("a.txt", "b.txt")),
#'   tar_target(
#'     your_target,
#'     your_target_files,
#'     pattern = map(your_target_files),
#'     format = "file",
#'     deployment = "local",
#'     storage = "local",
#'     retrieval = "local"
#'   )
#' )
#' # Try it out.
#' file.create(c("a.txt", "b.txt"))
#' targets::tar_dir({
#' targets::tar_script({
#'   tar_pipeline(
#'     tarchetypes::tar_files(x, c("a.txt", "b.txt"))
#'   )
#' })
#' targets::tar_make()
#' # Should be up to date now.
#' targets::tar_make()
#' # If we change one file, `tar_make()` will only rerun one branch.
#' writeLines("a", "a.txt")
#' tar_make()
#' })
#' }
tar_files <- function(
  name,
  command,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = targets::tar_option_get("format"),
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  template = targets::tar_option_get("template"),
  deployment = targets::tar_option_get("deployment"),
  priority = 0,
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue")
) {
  name <- deparse_language(substitute(name))
  name_files <- paste0(name, "_files")
  envir <- tar_option_get("envir")
  command <- tidy_eval(substitute(command), envir, tidy_eval)
  files <- tidy_eval(substitute(files), envir, tidy_eval)
  tar_files_raw(
    name = name,
    name_files = name_files,
    command = command,
    files = files,
    packages = packages,
    library = library,
    format = format,
    iteration = iteration,
    error = error,
    memory = memory,
    deployment = deployment,
    priority = priority,
    template = template,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
}

tar_files_raw <- function(
  name,
  name_files,
  command,
  files,
  packages,
  library,
  format,
  iteration,
  error,
  memory,
  deployment,
  priority,
  template,
  resources,
  storage,
  retrieval,
  cue
) {
  upstream <- tar_target_raw(
    name = name_files,
    command = command,
    pattern = NULL,
    packages = packages,
    library = library,
    format = "rds",
    iteration = "vector",
    error = error,
    memory = memory,
    deployment = deployment,
    priority = priority,
    template = template,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
  name_files_sym <- rlang::sym(name_files)
  downstream <- tar_target_raw(
    name = name,
    command = as.expression(name_files_sym),
    pattern = as.expression(call_function("map", list(name_files_sym))),
    packages = character(0),
    library = library,
    format = "file",
    iteration = iteration,
    error = error,
    memory = memory,
    deployment = "local",
    priority = priority,
    template = template,
    resources = resources,
    storage = "local",
    retrieval = "local",
    cue = cue
  )
  list(upstream, downstream)
}
