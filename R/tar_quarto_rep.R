#' @title Parameterized Quarto with dynamic branching.
#' @export
#' @family Literate programming targets
#' @description Targets to render a parameterized Quarto document
#'   with multiple sets of parameters.
#'
#'   [tar_quarto_rep()] expects an unevaluated symbol for the `name`
#'   argument and an unevaluated expression for the `exectue_params` argument.
#'   [tar_quarto_rep_raw()] expects a character string for the `name`
#'   argument and an evaluated expression object
#'   for the `exectue_params` argument.
#' @details `tar_quarto_rep()` is an alternative to `tar_target()` for
#'   a parameterized Quarto document that depends on other targets.
#'   Parameters must be given as a data frame with one row per
#'   rendered report and one column per parameter. An optional
#'   `output_file` column may be included to set the output file path
#'   of each rendered report. (See the `execute_params` argument for details.)
#'
#'   The Quarto source should mention other dependency targets
#'   `tar_load()` and `tar_read()` in the active code chunks
#'   (which also allows you to render the report
#'   outside the pipeline if the `_targets/` data store already exists
#'   and appropriate defaults are specified for the parameters).
#'   (Do not use `tar_load_raw()` or `tar_read_raw()` for this.)
#'   Then, `tar_quarto()` defines a special kind of target. It
#'     1. Finds all the `tar_load()`/`tar_read()` dependencies in the report
#'       and inserts them into the target's command.
#'       This enforces the proper dependency relationships.
#'       (Do not use `tar_load_raw()` or `tar_read_raw()` for this.)
#'     2. Sets `format = "file"` (see `tar_target()`) so `targets`
#'       watches the files at the returned paths and reruns the report
#'       if those files change.
#'     3. Configures the target's command to return the output
#'       report files: the rendered document, the source file,
#'       and file paths mentioned in `files`. All these file paths
#'       are relative paths so the project stays portable.
#'     4. Forces the report to run in the user's current working directory
#'       instead of the working directory of the report.
#'     5. Sets convenient default options such as `deployment = "main"`
#'       in the target and `quiet = TRUE` in `quarto::quarto_render()`.
#' @return A list of target objects to render the Quarto
#'   reports. Changes to the parameters, source file, dependencies, etc.
#'   will cause the appropriate targets to rerun during `tar_make()`.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritSection tar_rep Replicate-specific seeds
#' @inheritSection tar_render Literate programming limitations
#' @inheritSection tar_quarto Quarto troubleshooting
#' @inheritParams tar_quarto
#' @inheritParams tar_rep
#' @inheritParams targets::tar_target
#' @inheritParams quarto::quarto_render
#' @param tidy_eval Logical of length 1, whether to use tidy evaluation
#'   to resolve `execute_params`. Similar to the `tidy_eval`
#'   argument of `targets::tar_target()`.
#' @param name Name of the target.
#'   [tar_quarto_rep()] expects an unevaluated symbol for the `name`
#'   argument, and
#'   [tar_quarto_rep_raw()] expects a character string for `name`.
#' @param execute_params Code to generate
#'   a data frame or `tibble` with one row per rendered report
#'   and one column per Quarto parameter.
#'   [tar_quarto_rep()] expects an unevaluated expression for the
#'   `exectue_params` argument, whereas
#'   [tar_quarto_rep_raw()] expects an evaluated expression object.
#'
#'   You may also include an
#'   `output_file` column in the parameters
#'   to specify the path of each rendered report.
#'   If included, the `output_file` column must be a character vector
#'   with one and only one output file for each row of parameters.
#'   If an `output_file` column is not included,
#'   then the output files are automatically determined using the parameters,
#'   and the default file format is determined by the YAML front-matter
#'   of the Quarto source document. Only the first file format is used,
#'   the others are not generated.
#'   Quarto parameters must not be named `tar_group` or `output_file`.
#'   This `execute_params` argument is converted into the command for a target
#'   that supplies the Quarto parameters.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' # Parameterized Quarto:
#' lines <- c(
#'   "---",
#'   "title: 'report.qmd file'",
#'   "output_format: html_document",
#'   "params:",
#'   "  par: \"default value\"",
#'   "---",
#'   "Assume these lines are in a file called report.qmd.",
#'   "```{r}",
#'   "print(params$par)",
#'   "```"
#' )
#' writeLines(lines, "report.qmd") # In tar_dir(), not the user's file space.
#' # The following pipeline will run the report for each row of params.
#' targets::tar_script({
#'   library(tarchetypes)
#'   list(
#'     tar_quarto_rep(
#'       name = report,
#'       path = "report.qmd",
#'       execute_params = tibble::tibble(par = c(1, 2))
#'     ),
#'     tar_quarto_rep_raw(
#'       name = "report",
#'       path = "report.qmd",
#'       execute_params = quote(tibble::tibble(par = c(1, 2)))
#'     )
#'   )
#' }, ask = FALSE)
#' # Then, run the targets pipeline as usual.
#' })
#' }
tar_quarto_rep <- function(
  name,
  path,
  working_directory = NULL,
  execute_params = data.frame(),
  batches = NULL,
  extra_files = character(0),
  execute = TRUE,
  cache = NULL,
  cache_refresh = FALSE,
  debug = FALSE,
  quiet = TRUE,
  quarto_args = NULL,
  pandoc_args = NULL,
  rep_workers = 1,
  tidy_eval = targets::tar_option_get("tidy_eval"),
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
  description = targets::tar_option_get("description")
) {
  execute_params <- targets::tar_tidy_eval(
    substitute(execute_params),
    envir = targets::tar_option_get("envir"),
    tidy_eval = tidy_eval
  )
  tar_quarto_rep_raw(
    name = targets::tar_deparse_language(substitute(name)),
    path = path,
    working_directory = working_directory,
    execute_params = execute_params,
    batches = batches,
    extra_files = extra_files,
    execute = execute,
    cache = cache,
    cache_refresh = cache_refresh,
    debug = debug,
    quiet = quiet,
    quarto_args = quarto_args,
    pandoc_args = pandoc_args,
    rep_workers = rep_workers,
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
    description = description
  )
}
