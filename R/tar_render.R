#' @title Target with an R Markdown document.
#' @export
#' @family Literate programming targets
#' @description Shorthand to include an R Markdown document in a
#'   `targets` pipeline.
#' @details `tar_render()` is an alternative to `tar_target()` for
#'   R Markdown reports that depend on other targets. The R Markdown source
#'   should mention dependency targets with `tar_load()` and `tar_read()`
#'   in the active code chunks (which also allows you to render the report
#'   outside the pipeline if the `_targets/` data store already exists).
#'   (Do not use `tar_load_raw()` or `tar_read_raw()` for this.)
#'   Then, `tar_render()` defines a special kind of target. It
#'     1. Finds all the `tar_load()`/`tar_read()` dependencies in the report
#'       and inserts them into the target's command.
#'       This enforces the proper dependency relationships.
#'       (Do not use `tar_load_raw()` or `tar_read_raw()` for this.)
#'     2. Sets `format = "file"` (see `tar_target()`) so `targets`
#'       watches the files at the returned paths and reruns the report
#'       if those files change.
#'     3. Configures the target's command to return both the output
#'       report files and the input source file. All these file paths
#'       are relative paths so the project stays portable.
#'     4. Forces the report to run in the user's current working directory
#'       instead of the working directory of the report.
#'     5. Sets convenient default options such as `deployment = "main"`
#'       in the target and `quiet = TRUE` in `rmarkdown::render()`.
#' @section Literate programming limitations:
#'   Literate programming files are messy and variable,
#'   so functions like [tar_render()] have limitations:
#'     * Child documents are not tracked for changes.
#'     * Upstream target dependencies are not detected if [tar_read()]
#'       and/or [tar_load()] are called from a user-defined function.
#'       In addition, single target names must be mentioned and they must
#'       be symbols. `tar_load("x")` and `tar_load(contains("x"))` may not
#'       detect target `x`.
#'     * Special/optional input/output files may not be detected in all cases.
#'     * [tar_render()] and friends are for local files only. They do not
#'       integrate with the cloud storage capabilities of `targets`.
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
#' @inheritParams targets::tar_target
#' @inheritParams rmarkdown::render
#' @param path Character string, file path to the R Markdown source file.
#'   Must have length 1.
#' @param output_file Character string, file path to the rendered output file.
#' @param working_directory Optional character string,
#'   path to the working directory
#'   to temporarily set when running the report.
#'   The default is `NULL`, which runs the report from the
#'   current working directory at the time the pipeline is run.
#'   This default is recommended in the vast majority of cases.
#'   To use anything other than `NULL`, you must manually set the value
#'   of the `store` argument relative to the working directory in all calls
#'   to `tar_read()` and `tar_load()` in the report. Otherwise,
#'   these functions will not know where to find the data.
#' @param ... Named arguments to `rmarkdown::render()`.
#'   These arguments are evaluated when the target actually runs in
#'   `tar_make()`, not when the target is defined. That means, for
#'   example, you can use upstream targets as parameters of
#'   parameterized R Markdown reports.
#'   `tar_render(your_target, "your_report.Rmd", params = list(your_param = your_target))` # nolint
#'   will run `rmarkdown::render("your_report.Rmd", params = list(your_param = your_target))`. # nolint
#'   For parameterized reports, it is recommended to supply a distinct
#'   `output_file` argument to each `tar_render()` call
#'   and set useful defaults for parameters in the R Markdown source.
#'  See the examples section for a demonstration.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({  # tar_dir() runs code from a temporary directory.
#' # Unparameterized R Markdown:
#' lines <- c(
#'   "---",
#'   "title: report.Rmd source file",
#'   "output_format: html_document",
#'   "---",
#'   "Assume these lines are in report.Rmd.",
#'   "```{r}",
#'   "targets::tar_read(data)",
#'   "```"
#' )
#' # Include the report in a pipeline as follows.
#' targets::tar_script({
#'   library(tarchetypes)
#'   list(
#'     tar_target(data, data.frame(x = seq_len(26), y = letters)),
#'     tar_render(report, "report.Rmd")
#'   )
#' }, ask = FALSE)
#' # Then, run the targets pipeline as usual.
#'
#' # Parameterized R Markdown:
#' lines <- c(
#'   "---",
#'   "title: 'report.Rmd source file with parameters'",
#'   "output_format: html_document",
#'   "params:",
#'   "  your_param: \"default value\"",
#'   "---",
#'   "Assume these lines are in report.Rmd.",
#'   "```{r}",
#'   "print(params$your_param)",
#'   "```"
#' )
#' # Include the report in the pipeline as follows.
#' targets::tar_script({
#'   library(tarchetypes)
#'   list(
#'     tar_target(data, data.frame(x = seq_len(26), y = letters)),
#'     tar_render(report, "report.Rmd", params = list(your_param = data))
#'   )
#' }, ask = FALSE)
#' })
#' # Then, run the targets pipeline as usual.
#' }
tar_render <- function(
  name,
  path,
  output_file = NULL,
  working_directory = NULL,
  tidy_eval = targets::tar_option_get("tidy_eval"),
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
  ...
) {
  targets::tar_assert_package("rmarkdown")
  targets::tar_assert_file(path)
  targets::tar_assert_chr(output_file %|||% "x")
  targets::tar_assert_scalar(output_file %|||% "x")
  targets::tar_assert_nzchar(output_file %|||% "x")
  if (!is.null(working_directory)) {
    targets::tar_assert_file(working_directory)
  }
  envir <- tar_option_get("envir")
  args <- targets::tar_tidy_eval(
    substitute(list(...)),
    envir = envir,
    tidy_eval = tidy_eval
  )
  targets::tar_target_raw(
    name = targets::tar_deparse_language(substitute(name)),
    command = tar_render_command(
      path,
      output_file,
      working_directory,
      args,
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
