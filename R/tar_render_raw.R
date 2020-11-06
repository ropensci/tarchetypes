#' @title Target with an R Markdown document (raw version).
#' @export
#' @description Shorthand to include an R Markdown document in a
#'   `targets` pipeline (raw version)
#' @details `tar_render_raw()` is just like `tar_render()`
#'   except that it uses standard evaluation. The `name` argument
#'   is a character vector, and the `render_arguments` argument
#'   is a language object.
#' @return A `tar_target()` object with `format = "file"`.
#'   When this target runs, it returns a character vector
#'   of file paths. The first file paths are the output files
#'   (returned by `rmarkdown::render()`) and the R Markdown
#'   source file is last. But unlike `rmarkdown::render()`,
#'   all returned paths are *relative* paths to ensure portability
#'   (so that the project can be moved from one file system to another
#'   without invalidating the target).
#' @inheritParams tar_render
#' @param name Character of length 1, name of the target.
#' @param render_arguments Optional language object with a list
#'   of named arguments to `rmarkdown::render()`.
#'   Cannot be an expression object.
#'   (Use `quote()`, not `expression()`.)
#'   The reason for quoting is that these arguments may depend on
#'   upstream targets whose values are not available at
#'   the time the target is defined, and because `tar_render_raw()`
#'   is the "raw" version of a function, we want to avoid
#'   all non-standard evaluation.
#' @examples
#' \dontrun{
#' targets::tar_dir({
#' # Unparameterized R Markdown:
#' lines <- c(
#'   "---",
#'   "title: report",
#'   "output_format: html_document",
#'   "---",
#'   "",
#'   "```{r}",
#'   "targets::tar_read(data)",
#'   "```"
#' )
#' writeLines(lines, "report.Rmd")
#' targets::tar_script({
#'   library(tarchetypes)
#'   tar_pipeline(
#'     tar_target(data, data.frame(x = seq_len(26), y = letters)),
#'     tar_render_raw("report", "report.Rmd")
#'   )
#' })
#' targets::tar_make()
#' # browseURL("report.html") # View the report.
#' # Parameterized R Markdown:
#' lines <- c(
#'   "---",
#'   "title: report",
#'   "output_format: html_document",
#'   "params:",
#'   "  your_param: \"default value\"",
#'   "---",
#'   "",
#'   "```{r}",
#'   "print(params$your_param)",
#'   "```"
#' )
#' writeLines(lines, "report.Rmd")
#' targets::tar_script({
#'   library(tarchetypes)
#'   tar_pipeline(
#'     tar_target(data, data.frame(x = seq_len(26), y = letters)),
#'     tar_render_raw(
#'       "report",
#'       "report.Rmd",
#'       render_arguments = list(params = list(your_param = data))
#'     )
#'   )
#' })
#' # targets::tar_visnetwork() # The report should be connected to the data.
#' targets::tar_make()
#' # browseURL("report.html") # View the report.
#' })
#' }
tar_render_raw <- function(
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
  render_arguments = quote(list())
) {
  assert_package("rmarkdown", "tar_render_raw() requires rmarkdown.")
  assert_scalar(path, "tar_render_raw() only takes one file at a time.")
  assert_chr(path, "path argument of tar_render_raw() must be a character.")
  assert_path(path, paste("path", path, "for tar_render_raw() does not exist"))
  assert_lang(render_arguments, "render_arguments must be a language object.")
  msg <- "render_arguments must not be an expression object."
  assert_not_expr(render_arguments, msg)
  tar_target_raw(
    name = name,
    command = tar_render_command(path, render_arguments, quiet),
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
