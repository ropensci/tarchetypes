#' @title Target with a Quarto project (raw version).
#' @export
#' @family Literate programming targets
#' @description Shorthand to include a Quarto project in a
#'   `targets` pipeline.
#' @details `tar_quarto_raw()` is just like [tar_quarto()]
#'   except that it uses standard evaluation for the
#'   `name` and `execute_params` arguments (instead of quoting them).
#' @return A target object with `format = "file"`.
#'   When this target runs, it returns a sorted character vector
#'   of all the important file paths: the rendered documents,
#'   the Quarto source files, and other input and output files.
#'   All returned paths are *relative* paths to ensure portability
#'   (so that the project can be moved from one file system to another
#'   without invalidating the target).
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target_raw
#' @inheritParams tar_quarto
#' @param execute_params A non-expression language object
#'   (use `quote()`, not `expression()`) that
#'   evaluates to a named list of parameters
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
#' # In tar_dir(), not part of the user's file space:
#' writeLines(lines, "report.qmd")
#' # Include the report in a pipeline as follows.
#' targets::tar_script({
#'   library(tarchetypes)
#'   list(
#'     tar_target(data, data.frame(x = seq_len(26), y = letters)),
#'     tar_quarto_raw("report", input = "report.qmd", files = "report.html")
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
#' # In tar_dir(), not part of the user's file space:
#' writeLines(lines, "report.qmd")
#' # Include the report in the pipeline as follows.
#' targets::tar_script({
#'   library(tarchetypes)
#'   list(
#'     tar_target(data, data.frame(x = seq_len(26), y = letters)),
#'     tar_quarto_raw(
#'       "report",
#'       input = "report.qmd",
#'       files = "report.html",
#'       execute_params = quote(list(your_param = data))
#'     )
#'   )
#' }, ask = FALSE)
#' # Then, run the pipeline as usual.
#' })
#' }
tar_quarto_raw <- function(
  name,
  input,
  files,
  sources = tarchetypes::tar_quarto_sources(input),
  output_format = NULL,
  output_file = NULL,
  execute = TRUE,
  execute_params = NULL,
  execute_dir = NULL,
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
  assert_quarto()
  targets::tar_assert_scalar(name)
  targets::tar_assert_chr(name)
  targets::tar_assert_nzchar(name)
  targets::tar_assert_chr(files)
  targets::tar_assert_nzchar(files)
  targets::tar_assert_scalar(input %|||% ".")
  targets::tar_assert_chr(input %|||% ".")
  targets::tar_assert_nzchar(input %|||% ".")
  targets::tar_assert_path(input)
  targets::tar_assert_chr(sources)
  targets::tar_assert_nzchar(sources)
  targets::tar_assert_path(sources)
  targets::tar_assert_scalar(output_format %|||% ".")
  targets::tar_assert_chr(output_format %|||% ".")
  targets::tar_assert_nzchar(output_format %|||% ".")
  targets::tar_assert_chr(output_file %|||% ".")
  targets::tar_assert_nzchar(output_file %|||% ".")
  targets::tar_assert_scalar(execute)
  targets::tar_assert_lgl(execute)
  targets::tar_assert_lang(execute_params)
  targets::tar_assert_not_expr(execute_params)
  targets::tar_assert_scalar(cache %|||% TRUE)
  targets::tar_assert_lgl(cache %|||% TRUE)
  targets::tar_assert_scalar(cache_refresh)
  targets::tar_assert_lgl(cache_refresh)
  targets::tar_assert_scalar(debug)
  targets::tar_assert_lgl(debug)
  targets::tar_assert_scalar(quiet)
  targets::tar_assert_lgl(quiet)
  targets::tar_assert_chr(pandoc_args %|||% ".")
  command <- tar_quarto_command(
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
    pandoc_args = pandoc_args
  )
  targets::tar_target_raw(
    name = name,
    command = command,
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

#' @title Detect R source documents in a Quarto project.
#' @description Not a user-side function. Do not invoke directly.
#' @export
#' @keywords internal
#' @return A character vector of relative file paths to the Quarto source
#'   documents of the project.
#' @param input Character vector, `input` argument of `quarto::quarto_render()`
#'   containing the name of the Quarto source document or the root
#'   directory of the Quarto project.
#' @examples
#' tar_quarto_sources(".")
tar_quarto_sources <- function(input = NULL) {
  input <- input %|||% "."
  targets::tar_assert_scalar(input)
  targets::tar_assert_chr(input)
  targets::tar_assert_nzchar(input)
  if (dir.exists(input)) {
    input <- list.files(
      path = input,
      pattern = "\\.[Qq]md$|\\.[Rr]md",
      full.names = TRUE,
      recursive = TRUE
    )
  }
  as.character(fs::path_rel(path = input))
}

tar_quarto_command <- function(
  files,
  input,
  sources,
  output_format,
  output_file,
  execute,
  execute_params,
  cache,
  cache_refresh,
  debug,
  quiet,
  pandoc_args
) {
  args <- substitute(
    list(
      input = input,
      output_format = output_format,
      output_file = output_file,
      execute = execute,
      execute_params = execute_params,
      execute_dir = quote(getwd()),
      execute_daemon = 0,
      execute_daemon_restart = FALSE,
      execute_debug = FALSE,
      cache = cache,
      cache_refresh = cache_refresh,
      debug = debug,
      quiet = quiet,
      pandoc_args = pandoc_args,
      as_job = FALSE
    ),
    env = list(
      input = input,
      output_format = output_format,
      output_file = output_file,
      execute = execute,
      execute_params = execute_params,
      cache = cache,
      cache_refresh = cache_refresh,
      debug = debug,
      quiet = quiet,
      pandoc_args = pandoc_args
    )
  )
  deps <- sort(unique(unlist(map(sources, ~knitr_deps(.x)))))
  deps <- call_list(as_symbols(deps))
  fun <- call_ns("tarchetypes", "tar_quarto_run")
  expr <- list(fun, args = args, deps = deps, files = sort(c(files, sources)))
  as.expression(as.call(expr))
}

#' @title Render a Quarto project inside a `tar_quarto()` target.
#' @description Internal function needed for `tar_quarto()`.
#'   Users should not invoke it directly.
#' @export
#' @keywords internal
#' @return Sorted character vector with the paths to all the important
#'   files that `targets` should track for changes.
#' @param args A named list of arguments to `quarto::quarto_render()`.
#' @param deps An unnamed list of target dependencies of the Quarto
#'   source files.
#' @param files Character vector with the paths to all the important
#'   files that `targets` should track for changes.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({  # tar_dir() runs code from a temporary directory.
#' # Unparameterized Quarto document:
#' lines <- c(
#'   "---",
#'   "title: Quarto source file",
#'   "output_format: html",
#'   "---",
#'   "Assume these lines are in the Quarto source file.",
#'   "```{r}",
#'   "1 + 1",
#'   "```"
#' )
#' tmp <- tempfile(fileext = ".qmd")
#' writeLines(lines, tmp)
#' args <- list(input = tmp, quiet = TRUE)
#' files <- fs::path_ext_set(tmp, "html")
#' tar_quarto_run(args = args, deps = list(), files = files)
#' file.exists(files)
#' })
#' }
tar_quarto_run <- function(args, deps, files) {
  rm(deps)
  gc()
  assert_quarto()
  args <- args[!map_lgl(args, is.null)]
  do.call(what = quarto::quarto_render, args = args)
  sort(as.character(fs::path_rel(unlist(files))))
}
