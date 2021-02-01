#' @title Target with a knitr document (raw version).
#' @export
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
#'
#'   Target objects represent skippable steps of the analysis pipeline
#'   as described at <https://books.ropensci.org/targets/>.
#'   Please see the design specification at
#'   <https://books.ropensci.org/targets-design/>
#'   to learn about the structure and composition of target objects.
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
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  error = targets::tar_option_get("error"),
  deployment = "main",
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  quiet = TRUE,
  knit_arguments = quote(list())
) {
  assert_package("knitr", "tar_knit_raw() requires knitr.")
  assert_scalar(path, "tar_knit_raw() only takes one file at a time.")
  assert_chr(path, "path argument of tar_knit_raw() must be a character.")
  assert_path(path, paste("path", path, "for tar_knit_raw() does not exist"))
  assert_lang(knit_arguments, "knit_arguments must be a language object.")
  msg <- "knit_arguments must not be an expression object."
  assert_not_expr(knit_arguments, msg)
  tar_target_raw(
    name = name,
    command = tar_knit_command(path, knit_arguments, quiet),
    packages = packages,
    library = library,
    format = "file",
    error = error,
    deployment = deployment,
    priority = priority,
    resources = resources,
    retrieval = retrieval,
    cue = cue
  )
}
