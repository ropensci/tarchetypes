#' @title Target with a knitr document (raw version).
#' @export
#' @family Literate programming targets
#' @description Shorthand to include a knitr document in a
#'   `targets` pipeline (raw version)
#' @details `tar_knit_raw()` is just like `tar_knit()`
#'   except that it uses standard evaluation. The `name` argument
#'   is a character vector, and the `knit_arguments` argument
#'   is a language object.
#' @return A `tar_target()` object with `format = "file"`.
#'   When this target runs, it returns a character vector
#'   of file paths. The first file paths are the output files
#'   (returned by `knitr::knit()`) and the knitr
#'   source file is last. But unlike `knitr::knit()`,
#'   all returned paths are *relative* paths to ensure portability
#'   (so that the project can be moved from one file system to another
#'   without invalidating the target).
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams tar_knit
#' @param name Character of length 1, name of the target.
#' @param knit_arguments Optional language object with a list
#'   of named arguments to `knitr::knit()`.
#'   Cannot be an expression object.
#'   (Use `quote()`, not `expression()`.)
#'   The reason for quoting is that these arguments may depend on
#'   upstream targets whose values are not available at
#'   the time the target is defined, and because `tar_knit_raw()`
#'   is the "raw" version of a function, we want to avoid
#'   all non-standard evaluation.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   # Ordinarily, you should create the report outside
#'   # tar_script() and avoid temporary files.
#'   lines <- c(
#'     "---",
#'     "title: report",
#'     "output_format: html_document",
#'     "---",
#'     "",
#'     "```{r}",
#'     "targets::tar_read(data)",
#'     "```"
#'   )
#'   path <- tempfile()
#'   writeLines(lines, path)
#'   list(
#'     targets::tar_target(data, data.frame(x = seq_len(26), y = letters)),
#'     tarchetypes::tar_knit_raw("report", path)
#'   )
#' })
#' targets::tar_make()
#' })
#' }
tar_knit_raw <- function(
  name,
  path,
  output_file = NULL,
  working_directory = NULL,
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  deployment = "main",
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description"),
  quiet = TRUE,
  knit_arguments = quote(list())
) {
  targets::tar_assert_package("knitr")
  targets::tar_assert_file(path)
  targets::tar_assert_not_dirs(path)
  targets::tar_assert_chr(output_file %|||% "x")
  targets::tar_assert_scalar(output_file %|||% "x")
  if (!is.null(working_directory)) {
    targets::tar_assert_file(working_directory)
  }
  targets::tar_assert_lang(knit_arguments)
  targets::tar_assert_not_expr(knit_arguments)
  targets::tar_target_raw(
    name = name,
    command = tar_knit_command(
      path,
      output_file,
      working_directory,
      knit_arguments,
      quiet
    ),
    packages = packages,
    library = library,
    format = "file",
    repository = "local",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    retrieval = retrieval,
    cue = cue,
    description = description
  )
}
