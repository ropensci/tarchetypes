#' @rdname tar_quarto
#' @export
tar_quarto_raw <- function(
  name,
  path = ".",
  output_file = NULL,
  working_directory = NULL,
  extra_files = character(0),
  execute = TRUE,
  execute_params = NULL,
  cache = NULL,
  cache_refresh = FALSE,
  debug = FALSE,
  quiet = TRUE,
  quarto_args = NULL,
  pandoc_args = NULL,
  profile = NULL,
  packages = NULL,
  library = NULL,
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  deployment = "main",
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
) {
  assert_quarto()
  targets::tar_assert_scalar(name)
  targets::tar_assert_chr(name)
  targets::tar_assert_nzchar(name)
  targets::tar_assert_chr(extra_files)
  targets::tar_assert_nzchar(extra_files)
  targets::tar_assert_file(path)
  if (!is.null(output_file)) {
    targets::tar_assert_chr(output_file)
    targets::tar_assert_nzchar(output_file)
  }
  if (!is.null(working_directory)) {
    targets::tar_assert_file(working_directory)
  }
  targets::tar_assert_scalar(execute)
  targets::tar_assert_lgl(execute)
  targets::tar_assert_lang(execute_params %|||% quote(x))
  targets::tar_assert_not_expr(execute_params)
  targets::tar_assert_scalar(cache %|||% TRUE)
  targets::tar_assert_lgl(cache %|||% TRUE)
  targets::tar_assert_scalar(cache_refresh)
  targets::tar_assert_lgl(cache_refresh)
  targets::tar_assert_scalar(debug)
  targets::tar_assert_lgl(debug)
  targets::tar_assert_scalar(quiet)
  targets::tar_assert_lgl(quiet)
  targets::tar_assert_chr(quarto_args %|||% ".")
  targets::tar_assert_chr(pandoc_args %|||% ".")
  targets::tar_assert_scalar(profile %|||% ".")
  targets::tar_assert_chr(profile %|||% ".")
  targets::tar_assert_nzchar(profile %|||% ".")
  info <- tar_quarto_files(path = path, profile = profile, quiet = quiet)
  sources <- info$sources
  output <- info$output
  if (!is.null(output_file)) {
    output <- file.path(dirname(output), output_file)
  }
  input <- sort(unique(c(info$input, extra_files)))
  if (!is.null(packages) || !is.null(library)) {
    targets::tar_warn_deprecate(
      "Arguments packages and library of tar_quarto() were ",
      "deprecated on 2023-09-05 (version 0.7.8.9000). Please ",
      "load R packages inside the Quarto report itself."
    )
  }
  command <- tar_quarto_command(
    path = path,
    output_file = output_file,
    working_directory = working_directory,
    sources = sources,
    output = output,
    input = input,
    execute = execute,
    execute_params = execute_params,
    cache = cache,
    cache_refresh = cache_refresh,
    debug = debug,
    quiet = quiet,
    quarto_args = quarto_args,
    pandoc_args = pandoc_args,
    profile = profile
  )
  targets::tar_target_raw(
    name = name,
    command = command,
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

tar_quarto_command <- function(
  path,
  output_file,
  working_directory,
  sources,
  output,
  input,
  execute,
  execute_params,
  cache,
  cache_refresh,
  debug,
  quiet,
  quarto_args,
  pandoc_args,
  profile
) {
  args <- substitute(
    list(
      input = path,
      output_file = output_file,
      execute = execute,
      execute_params = execute_params,
      execute_dir = execute_dir,
      execute_daemon = 0,
      execute_daemon_restart = FALSE,
      execute_debug = FALSE,
      cache = cache,
      cache_refresh = cache_refresh,
      debug = debug,
      quiet = quiet,
      quarto_args = quarto_args,
      pandoc_args = pandoc_args,
      as_job = FALSE
    ),
    env = list(
      path = path,
      output_file = output_file,
      execute = execute,
      execute_dir = working_directory %|||% quote(getwd()),
      execute_params = execute_params,
      cache = cache,
      cache_refresh = cache_refresh,
      debug = debug,
      quiet = quiet,
      quarto_args = quarto_args,
      pandoc_args = pandoc_args
    )
  )
  deps <- sort(unique(unlist(map(sources, ~knitr_deps(.x)))))
  deps <- call_list(as_symbols(deps))
  fun <- call_ns("tarchetypes", "tar_quarto_run")
  expr <- list(
    fun,
    args = args,
    deps = deps,
    sources = sources,
    output = output,
    input = input,
    profile = profile
  )
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
#' @param sources Character vector of Quarto source files.
#' @param output Character vector of Quarto output files and directories.
#' @param input Character vector of non-source Quarto input files
#'   and directories.
#' @param profile Quarto profile.
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
tar_quarto_run <- function(args, deps, sources, output, input, profile) {
  rm(deps)
  gc()
  assert_quarto()
  if (!is.null(profile)) {
    withr::local_envvar(.new = c(QUARTO_PROFILE = profile))
  }
  args <- args[!map_lgl(args, is.null)]
  do.call(what = quarto::quarto_render, args = args)
  support <- sprintf("%s_files", fs::path_ext_remove(basename(args$input)))
  output <- if_any(dir.exists(support), unique(c(output, support)), output)
  out <- unique(c(sort(output), sort(sources), sort(input)))
  as.character(fs::path_rel(out))
}
