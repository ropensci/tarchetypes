#' @title Target with an R Markdown document (raw version).
#' @export
#' @family Literate programming targets
#' @description Shorthand to include an R Markdown document in a
#'   `targets` pipeline (raw version)
#' @details `tar_render_raw()` is just like `tar_render()`
#'   except that it uses standard evaluation. The `name` argument
#'   is a character vector, and the `render_arguments` argument
#'   is a language object.
#' @return A target object with `format = "file"`.
#'   When this target runs, it returns a character vector
#'   of file paths: the rendered document, the source file,
#'   and then the `*_files/` directory if it exists.
#'   Unlike `rmarkdown::render()`,
#'   all returned paths are *relative* paths to ensure portability
#'   (so that the project can be moved from one file system to another
#'   without invalidating the target).
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritSection tar_render Literate programming limitations
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
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' # Unparameterized R Markdown report:
#' lines <- c(
#'   "---",
#'   "title: 'report.Rmd source file'",
#'   "output_format: html_document",
#'   "---",
#'   "Assume these lines are in report.Rmd.",
#'   "```{r}",
#'   "targets::tar_read(data)",
#'   "```"
#' )
#' # Include the report in the pipeline as follows:
#' targets::tar_script({
#'   library(tarchetypes)
#'   list(
#'     tar_target(data, data.frame(x = seq_len(26), y = letters)),
#'     tar_render_raw("report", "report.Rmd")
#'   )
#' }, ask = FALSE)
#' # Then, run the targets pipeline as usual.
#'
#' # Parameterized R Markdown:
#' lines <- c(
#'   "---",
#'   "title: 'report.Rmd source file with parameters.'",
#'   "output_format: html_document",
#'   "params:",
#'   "  your_param: \"default value\"",
#'   "---",
#'   "Assume these lines are in report.Rmd.",
#'   "```{r}",
#'   "print(params$your_param)",
#'   "```"
#' )
#' # Include this parameterized report in the pipeline as follows.
#' targets::tar_script({
#'   library(tarchetypes)
#'   list(
#'     tar_target(data, data.frame(x = seq_len(26), y = letters)),
#'     tar_render_raw(
#'       "report",
#'       "report.Rmd",
#'       render_arguments = quote(list(params = list(your_param = data)))
#'     )
#'   )
#' }, ask = FALSE)
#' # Then, run the targets pipeline as usual.
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
  targets::tar_assert_package("rmarkdown")
  targets::tar_assert_scalar(path)
  targets::tar_assert_chr(path)
  targets::tar_assert_path(path)
  targets::tar_assert_not_dirs(path)
  targets::tar_assert_lang(render_arguments)
  targets::tar_assert_not_expr(render_arguments)
  targets::tar_target_raw(
    name = name,
    command = tar_render_command(path, render_arguments, quiet),
    packages = packages,
    library = library,
    format = "file",
    repository = "local",
    error = error,
    deployment = deployment,
    priority = priority,
    resources = resources,
    retrieval = retrieval,
    cue = cue
  )
}


tar_render_command <- function(path, args, quiet) {
  args$input <- path
  args$knit_root_dir <- quote(getwd())
  args$quiet <- quiet
  deps <- call_list(as_symbols(knitr_deps(path)))
  fun <- call_ns("tarchetypes", "tar_render_run")
  exprs <- list(fun, path = path, args = args, deps = deps)
  as.expression(as.call(exprs))
}

#' @title Render an R Markdown report inside a `tar_render()` target.
#' @description Internal function needed for `tar_render()`.
#'   Users should not invoke it directly.
#' @export
#' @keywords internal
#' @return Character vector with the path to the R Markdown source file
#'   and the relative path to the output. These paths depend on the input
#'   source file path and have no defaults.
#' @param path Path to the R Markdown source file.
#' @param args A named list of arguments to `rmarkdown::render()`.
#' @param deps An unnamed list of target dependencies of the R Markdown
#'   report, automatically created by `tar_render()`.
tar_render_run <- function(path, args, deps) {
  rm(deps)
  gc()
  targets::tar_assert_package("rmarkdown")
  withr::local_options(list(crayon.enabled = NULL))
  envir <- parent.frame()
  args$envir <- args$envir %|||% targets::tar_envir(default = envir)
  force(args$envir)
  output <- do.call(rmarkdown::render, args)
  tar_render_paths(output, path)
}

tar_render_paths <- function(output, source) {
  output <- fs::path_rel(output)
  source <- fs::path_rel(source)
  files <- paste0(fs::path_ext_remove(output[1]), "_files")
  files <- if_any(all(file.exists(files)), files, character(0))
  c(sort(output), sort(source), files)
}
