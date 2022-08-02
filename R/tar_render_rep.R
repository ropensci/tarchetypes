#' @title Parameterized R Markdown with dynamic branching.
#' @export
#' @family Literate programming targets
#' @description Targets to render a parameterized R Markdown report
#'   with multiple sets of parameters.
#' @details `tar_render_rep()` is an alternative to `tar_target()` for
#'   parameterized R Markdown reports that depend on other targets.
#'   Parameters must be given as a data frame with one row per
#'   rendered report and one column per parameter. An optional
#'   `output_file` column may be included to set the output file path
#'   of each rendered report.
#'   The R Markdown source should mention other dependency targets
#'   `tar_load()` and `tar_read()` in the active code chunks
#'   (which also allows you to render the report
#'   outside the pipeline if the `_targets/` data store already exists
#'   and appropriate defaults are specified for the parameters).
#'   (Do not use `tar_load_raw()` or `tar_read_raw()` for this.)
#'   Then, `tar_render()` defines a special kind of target. It
#'     1. Finds all the `tar_load()`/`tar_read()` dependencies in the report
#'       and inserts them into the target's command.
#'       This enforces the proper dependency relationships.
#'       (Do not use `tar_load_raw()` or `tar_read_raw()` for this.)
#'     2. Sets `format = "file"` (see `tar_target()`) so `targets`
#'       watches the files at the returned paths and reruns the report
#'       if those files change.
#'     3. Configures the target's command to return the output
#'       report files: the rendered document, the source file,
#'       and then the `*_files/` directory if it exists. All these file paths
#'       are relative paths so the project stays portable.
#'     4. Forces the report to run in the user's current working directory
#'       instead of the working directory of the report.
#'     5. Sets convenient default options such as `deployment = "main"`
#'       in the target and `quiet = TRUE` in `rmarkdown::render()`.
#' @return A list of target objects to render the R Markdown
#'   reports. Changes to the parameters, source file, dependencies, etc.
#'   will cause the appropriate targets to rerun during `tar_make()`.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target
#' @inheritParams rmarkdown::render
#' @inheritParams tar_render_rep_raw
#' @param path Character string, file path to the R Markdown source file.
#'   Must have length 1.
#' @param params Code to generate a data frame or `tibble`
#'   with one row per rendered report
#'   and one column per R Markdown parameter. You may also include an
#'   `output_file` column to specify the path of each rendered report.
#'   This `params` argument is converted into the command for a target
#'   that supplies the R Markdown parameters.
#' @param ... Other named arguments to `rmarkdown::render()`.
#'   Unlike [tar_render()], these arguments are evaluated when the target
#'   is defined, not when it is run. (The only reason to delay evaluation
#'   in [tar_render()] was to handle R Markdown parameters, and
#'   `tar_render_rep()` handles them differently.)
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' # Parameterized R Markdown:
#' lines <- c(
#'   "---",
#'   "title: 'report.Rmd file'",
#'   "output_format: html_document",
#'   "params:",
#'   "  par: \"default value\"",
#'   "---",
#'   "Assume these lines are in a file called report.Rmd.",
#'   "```{r}",
#'   "print(params$par)",
#'   "```"
#' )
#' # The following pipeline will run the report for each row of params.
#' targets::tar_script({
#'   library(tarchetypes)
#'   list(
#'     tar_render_rep(
#'       report,
#'       "report.Rmd",
#'       params = tibble::tibble(par = c(1, 2))
#'     )
#'   )
#' }, ask = FALSE)
#' # Then, run the targets pipeline as usual.
#' })
#' }
tar_render_rep <- function(
  name,
  path,
  params = data.frame(),
  batches = NULL,
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = targets::tar_option_get("format"),
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  quiet = TRUE,
  ...
) {
  tar_render_rep_raw(
    name = targets::tar_deparse_language(substitute(name)),
    path = path,
    params = substitute(params),
    batches = batches,
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
    retrieval = retrieval,
    cue = cue,
    args = list(...)
  )
}
