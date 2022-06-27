#' @title Target with a Quarto project.
#' @export
#' @family Literate programming targets
#' @description Shorthand to include a Quarto project in a
#'   `targets` pipeline.
#' @details `tar_quarto()` is an alternative to `tar_target()` for
#'   Quarto projects that depend on upstream targets. The Quarto project
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
#'   All returned paths are *relative* paths to ensure portability
#'   (so that the project can be moved from one file system to another
#'   without invalidating the target).
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target
#' @inheritParams quarto::quarto_render
#' @param input character vector,
#'   the value passed to the input argument of
#'   `quarto::quarto_render()`.
#'   Must be the input file or project directory to be rendered with
#'   `quarto::quarto_render()`.
#' @param files Character vector of file and directory paths
#'   with input and output that `targets`
#'   should track for changes. These files should include rendered reports,
#'   `_quarto.yml`, bibliographies, stylesheets, and/or `_book`.
#'   Not all the files need to exist before rendering,
#'   but they should exist afterwards. Files in the `sources` argument are
#'   automatically tracked for changes and do not need to be included in the
#'   `files` argument.
#' @param sources Character vector of literate programming source documents
#'   (`*.qmd` and `*.Rmd` files)
#'   that use output from upstream targets. These source documents reference
#'   dependent targets with `tar_load()` and `tar_read()` in active
#'   R code chunks. The default value is a character vector of all the
#'   `*.qmd` and `*.Rmd` files that can be found in the input argument.
#' @param output_format Character of length 1, Quarto document output format.
#'   See the help file of `quarto::quarto_render()` for more details.
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
#' # Include the report in a pipeline as follows.
#' targets::tar_script({
#'   library(tarchetypes)
#'   list(
#'     tar_target(data, data.frame(x = seq_len(26), y = letters)),
#'     tar_quarto(report, "report.qmd")
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
#' # Include the report in the pipeline as follows.
#' targets::tar_script({
#'   library(tarchetypes)
#'   list(
#'     tar_target(data, data.frame(x = seq_len(26), y = letters)),
#'     tar_quarto(
#'       report,
#'       "report.qmd",
#'       execute_params = list(your_param = data)
#'     )
#'   )
#' }, ask = FALSE)
#' })
#' # Then, run the pipeline as usual.
#' }
tar_quarto <- function(
  name,
  files,
  input = NULL,
  sources = tarchetypes::tar_quarto_sources(input),
  output_format = NULL,
  output_file = NULL,
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
  deployment = "main",
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue")
) {
  name <- targets::tar_deparse_language(substitute(name))
  envir <- tar_option_get("envir")
  execute_params <- targets::tar_tidy_eval(
    substitute(execute_params),
    envir = envir,
    tidy_eval = tidy_eval
  )
  tar_quarto_raw(
    name = name,
    files = files,
    input = input,
    sources = sources,
    output_format = output_format,
    output_file = output_file,
    execute = execute,
    execute_params = execute_params,
    cache = cache,
    cache_refresh = cache_refresh,
    debug = debug,
    quiet = quiet,
    pandoc_args = pandoc_args,
    tidy_eval = tidy_eval,
    packages = packages,
    library = library,
    error = error,
    deployment = deployment,
    priority = priority,
    resources = resources,
    retrieval = retrieval,
    cue = cue
  )
}
