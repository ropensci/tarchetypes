#' @title Target with a Quarto project.
#' @export
#' @family Literate programming targets
#' @description Shorthand to include a Quarto project in a
#'   `targets` pipeline.
#' @details `tar_quarto()` is an alternative to `tar_target()` for
#'   Quarto projects and standalone Quarto source documents
#'   that depend on upstream targets. The Quarto
#'   R source documents (`*.qmd` and `*.Rmd` files)
#'   should mention dependency targets with `tar_load()` and `tar_read()`
#'   in the active R code chunks (which also allows you to render the project
#'   outside the pipeline if the `_targets/` data store already exists).
#'   (Do not use `tar_load_raw()` or `tar_read_raw()` for this.)
#'   Then, `tar_quarto()` defines a special kind of target. It
#'     1. Finds all the `tar_load()`/`tar_read()` dependencies in the
#'       R source reports and inserts them into the target's command.
#'       This enforces the proper dependency relationships.
#'       (Do not use `tar_load_raw()` or `tar_read_raw()` for this.)
#'     2. Sets `format = "file"` (see `tar_target()`) so `targets`
#'       watches the files at the returned paths and reruns the report
#'       if those files change.
#'     3. Configures the target's command to return both the output
#'       rendered files and the input dependency files (such as
#'       Quarto source documents). All these file paths
#'       are relative paths so the project stays portable.
#'     4. Forces the report to run in the user's current working directory
#'       instead of the working directory of the report.
#'     5. Sets convenient default options such as `deployment = "main"`
#'       in the target and `quiet = TRUE` in `quarto::quarto_render()`.
#' @return A target object with `format = "file"`.
#'   When this target runs, it returns a character vector
#'   of file paths: the rendered documents, the Quarto source files,
#'   and other input and output files.
#'   The output files are determined by the YAML front-matter of
#'   standalone Quarto documents and `_quarto.yml` in Quarto projects,
#'   and you can see these files with [tar_quarto_files()]
#'   (powered by `quarto::quarto_inspect()`).
#'   All returned paths are *relative* paths to ensure portability
#'   (so that the project can be moved from one file system to another
#'   without invalidating the target).
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target
#' @inheritParams tar_quarto_raw
#' @param execute_params Code, cannot be `NULL`.
#'   `execute_params` evaluates to a named list of parameters
#'   for parameterized Quarto documents. These parameters override the custom
#'   custom elements of the `params` list in the YAML front-matter of the
#'   Quarto source files. The list is quoted
#'   (not evaluated until the target runs)
#'   so that upstream targets can serve as parameter values.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({  # tar_dir() runs code from a temporary directory.
#' # Unparameterized Quarto document:
#' lines <- c(
#'   "---",
#'   "title: report.qmd source file",
#'   "output_format: html",
#'   "---",
#'   "Assume these lines are in report.qmd.",
#'   "```{r}",
#'   "targets::tar_read(data)",
#'   "```"
#' )
# In tar_dir(), not part of the user's file space:
#' writeLines(lines, "report.qmd")
#' # Include the report in a pipeline as follows.
#' targets::tar_script({
#'   library(tarchetypes)
#'   list(
#'     tar_target(data, data.frame(x = seq_len(26), y = letters)),
#'     tar_quarto(report, path = "report.qmd")
#'   )
#' }, ask = FALSE)
#' # Then, run the pipeline as usual.
#'
#' # Parameterized Quarto:
#' lines <- c(
#'   "---",
#'   "title: 'report.qmd source file with parameters'",
#'   "output_format: html_document",
#'   "params:",
#'   "  your_param: \"default value\"",
#'   "---",
#'   "Assume these lines are in report.qmd.",
#'   "```{r}",
#'   "print(params$your_param)",
#'   "```"
#' )
# In tar_dir(), not part of the user's file space:
#' writeLines(lines, "report.qmd")
#' # Include the report in the pipeline as follows.
#' unlink("_targets.R") # In tar_dir(), not the user's file space.
#' targets::tar_script({
#'   library(tarchetypes)
#'   list(
#'     tar_target(data, data.frame(x = seq_len(26), y = letters)),
#'     tar_quarto(
#'       report,
#'       path = "report.qmd",
#'       execute_params = list(your_param = data)
#'     )
#'   )
#' }, ask = FALSE)
#' })
#' # Then, run the pipeline as usual.
#' }
tar_quarto <- function(
  name,
  path = ".",
  extra_files = character(0),
  execute = TRUE,
  execute_params = list(),
  cache = NULL,
  cache_refresh = FALSE,
  debug = FALSE,
  quiet = TRUE,
  pandoc_args = NULL,
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
  cue = targets::tar_option_get("cue")
) {
  name <- targets::tar_deparse_language(substitute(name))
  execute_params <- targets::tar_tidy_eval(
    substitute(execute_params),
    envir = tar_option_get("envir"),
    tidy_eval = tidy_eval
  )
  tar_quarto_raw(
    name = name,
    path = path,
    extra_files = extra_files,
    execute = execute,
    execute_params = execute_params,
    cache = cache,
    cache_refresh = cache_refresh,
    debug = debug,
    quiet = quiet,
    pandoc_args = pandoc_args,
    packages = packages,
    library = library,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    retrieval = retrieval,
    cue = cue
  )
}
