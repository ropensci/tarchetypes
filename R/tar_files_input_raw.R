#' @title Easy dynamic branching over known existing
#'   files or urls (raw version).
#' @export
#' @family Dynamic branching over files
#' @description Shorthand for a pattern that correctly
#'   branches over files or urls.
#' @details `tar_files_input_raw()` is similar to [tar_files_input()]
#'   except the `name` argument must be a character string.
#'
#'   `tar_files_input_raw()` creates a pair of targets, one upstream
#'   and one downstream. The upstream target does some work
#'   and returns some file paths, and the downstream
#'   target is a pattern that applies `format = "file"`
#'   or `format = "url"`.
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
#' @param format Character, either `"file"` or `"url"`. See the `format`
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
#'   paths <- unlist(replicate(4, tempfile()))
#'   file.create(paths)
#'   list(
#'     tarchetypes::tar_files_input_raw(
#'       "x",
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
tar_files_input_raw <- function(
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
  targets::tar_assert_chr(name, "name must be a character.")
  targets::tar_assert_scalar(name, "name must have length 1.")
  targets::tar_assert_chr(files, "files must be a character vector.")
  targets::tar_assert_nonempty(files, "files must have length > 0.")
  targets::tar_assert_dbl(batches, "batches must be numeric.")
  targets::tar_assert_scalar(batches, "batches must have length 1.")
  format <- match.arg(format)
  name_files <- paste0(name, "_files")
  files <- tar_files_input_batch_files(files, batches)
  upstream <- targets::tar_target_raw(
    name = name_files,
    command = parse(text = targets::tar_deparse_safe(files, collapse = " ")),
    pattern = NULL,
    packages = character(0),
    format = "rds",
    iteration = "list",
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
  name_files_sym <- as.symbol(name_files)
  downstream <- targets::tar_target_raw(
    name = name,
    command = as.expression(name_files_sym),
    pattern = as.expression(call_function("map", list(name_files_sym))),
    packages = character(0),
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

tar_files_input_batch_files <- function(files, batches) {
  batches <- min(batches, length(files))
  index <- if_any(
    batches > 1L,
    as.integer(cut(seq_along(files), breaks = batches)),
    rep(1L, length(files))
  )
  unname(split(files, f = index))
}
