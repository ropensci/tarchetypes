#' @title Parameterized Quarto with dynamic branching (raw version).
#' @export
#' @family Literate programming targets
#' @description Targets to render a parameterized Quarto document
#'   with multiple sets of parameters (raw version). Same as
#'   `tar_quarto_rep()` except `name` is a character string,
#'   `params` is an expression object,
#'   and extra arguments to `quarto::quarto_render()` are passed through
#'   the `args` argument instead of `...`.
#' @details `tar_quarto_rep_raw()` is an alternative to `tar_target_raw()` for
#'   parameterized Quarto reports that depend on other targets.
#'   Parameters must be given as a data frame with one row per
#'   rendered report and one column per parameter. An optional
#'   `output_file` column may be included to set the output file path
#'   of each rendered report.
#'   If an `output_file` column is not included,
#'   then the output files are automatically determined using the parameters,
#'   and the default file extension is `"html"`.
#'   (`"html"` may not be correct for your use case, so if you need a different
#'   file extension, then you will need to supply an `output_file` column.)
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
#'       and then the `*_files/` directory if it exists.
#'       All these file paths
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
#' @inheritParams quarto::quarto_render
#' @inheritParams targets::tar_target
#' @inheritParams tar_quarto_rep_run
#' @param input Character string, file path to the Quarto source file.
#'   Must have length 1.
#' @param execute_params Expression object with code to generate
#'   a data frame or `tibble` with one row per rendered report
#'   and one column per Quarto parameter. You may also include an
#'   `output_file` column to specify the path of each rendered report.
#'   If an `output_file` column is not included,
#'   then the output files are automatically determined using the parameters,
#'   and the default file extension is `"html"`.
#'   (`"html"` may not be correct for your use case, so if you need a different
#'   file extension, then you will need to supply an `output_file` column.)
#'   Quarto parameters must not be named `tar_group` or `output_file`.
#'   This `execute_params` argument is converted into the command for a target
#'   that supplies the Quarto parameters.
#' @param batches Number of batches to group the Quarto files.
#'   For a large number of reports, increase the number of batches
#'   to decrease target-level overhead. Defaults to the number of
#'   reports to render (1 report per batch).
#' @param format Character of length 1, `format` argument to `tar_target()`
#'   to store the data frame of Quarto parameters.
#' @param iteration Character of length 1, `iteration` argument
#'   to `tar_target()` for the Quarto documents. Does not apply
#'   to the target with Quarto parameters (whose iteration
#'   is always `"group"`).
#' @param args Named list of other arguments to `quarto::quarto_render()`.
#'   Must not include `params` or `output_file`. Evaluated when the target
#'   is defined.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' # Parameterized Quarto:
#' lines <- c(
#'   "---",
#'   "title: 'report.qmd source file'",
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
#'     tar_quarto_rep_raw(
#'       "report",
#'       input = "report.qmd",
#'       execute_params = quote(tibble::tibble(par = c(1, 2)))
#'     )
#'   )
#' }, ask = FALSE)
#' # Then, run the targets pipeline as usual.
#' })
#' }
tar_quarto_rep_raw <- function(
    name,
    input,
    execute_params = expression(NULL),
    batches = NULL,
    files = character(0),
    execute = TRUE,
    cache = NULL,
    cache_refresh = FALSE,
    debug = FALSE,
    quiet = TRUE,
    pandoc_args = NULL,
    packages = targets::tar_option_get("packages"),
    library = targets::tar_option_get("library"),
    format = targets::tar_option_get("format"),
    iteration = targets::tar_option_get("iteration"),
    error = targets::tar_option_get("error"),
    deployment = targets::tar_option_get("deployment"),
    priority = targets::tar_option_get("priority"),
    resources = targets::tar_option_get("resources"),
    retrieval = targets::tar_option_get("retrieval"),
    cue = targets::tar_option_get("cue")
) {
  assert_quarto()
  targets::tar_assert_scalar(name)
  targets::tar_assert_chr(name)
  targets::tar_assert_nzchar(name)
  targets::tar_assert_scalar(input)
  targets::tar_assert_chr(input)
  targets::tar_assert_nzchar(input)
  targets::tar_assert_path(input)
  targets::tar_assert_not_dirs(input)
  targets::tar_assert_lang(execute_params)
  targets::tar_assert_dbl(batches %|||% 0L, "batches must be numeric.")
  targets::tar_assert_scalar(batches %|||% 0L, "batches must have length 1.")
  targets::tar_assert_chr(files)
  targets::tar_assert_nzchar(files)
  targets::tar_assert_scalar(execute)
  targets::tar_assert_lgl(execute)
  targets::tar_assert_scalar(cache %|||% TRUE)
  targets::tar_assert_lgl(cache %|||% TRUE)
  targets::tar_assert_scalar(cache_refresh)
  targets::tar_assert_lgl(cache_refresh)
  targets::tar_assert_scalar(debug)
  targets::tar_assert_lgl(debug)
  targets::tar_assert_scalar(quiet)
  targets::tar_assert_lgl(quiet)
  targets::tar_assert_chr(pandoc_args %|||% ".")
  name_params <- paste0(name, "_params")
  sym_params <- as.symbol(name_params)
  target_params <- targets::tar_target_raw(
    name = name_params,
    command = tar_quarto_rep_params_command(
      execute_params = execute_params,
      batches = batches
    ),
    packages = packages,
    library = library,
    format = format,
    iteration = "group",
    error = error,
    deployment = deployment,
    priority = priority,
    resources = resources,
    retrieval = retrieval,
    cue = cue
  )
  target <- targets::tar_target_raw(
    name = name,
    command = tar_quarto_rep_command(
      name = name,
      input = input,
      files = files,
      execute = execute,
      cache = cache,
      cache_refresh = cache_refresh,
      debug = debug,
      quiet = quiet,
      pandoc_args = pandoc_args
    ),
    pattern = substitute(map(x), env = list(x = sym_params)),
    packages = packages,
    library = library,
    format = "file",
    repository = "local",
    iteration = iteration,
    error = error,
    deployment = deployment,
    priority = priority,
    resources = resources,
    retrieval = retrieval,
    cue = cue
  )
  out <- list(target_params, target)
  names(out) <- c(name_params, name)
  out
}

tar_quarto_rep_params_command <- function(execute_params, batches) {
  fun <- call_ns("tarchetypes", "tar_quarto_rep_run_params")
  exprs <- list(fun, execute_params = execute_params, batches = batches)
  as.expression(as.call(exprs))
}

#' @title Prepare Quarto parameters for `tar_quarto_rep()`.
#' @description Internal function needed for `tar_quarto_rep()`.
#'   Users should not invoke it directly.
#' @export
#' @keywords internal
#' @return A batched data frame of Quarto parameters.
#' @param execute_params Data frame of Quarto parameters.
#' @param batches Number of batches to split up the renderings.
#' @examples
#' execute_params <- tibble::tibble(param1 = letters[seq_len(4)])
#' tar_quarto_rep_run_params(execute_params, 1)
#' tar_quarto_rep_run_params(execute_params, 2)
#' tar_quarto_rep_run_params(execute_params, 3)
#' tar_quarto_rep_run_params(execute_params, 4)
tar_quarto_rep_run_params <- function(execute_params, batches) {
  batches <- batches %|||% nrow(execute_params)
  execute_params$tar_group <- if_any(
    batches > 1L,
    as.integer(cut(seq_len(nrow(execute_params)), breaks = batches)),
    rep(1L, nrow(execute_params))
  )
  execute_params
}

tar_quarto_rep_command <- function(
  name,
  input,
  files,
  execute,
  cache,
  cache_refresh,
  debug,
  quiet,
  pandoc_args
) {
  args <- substitute(
    list(
      input = input,
      execute = execute,
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
      execute = execute,
      cache = cache,
      cache_refresh = cache_refresh,
      debug = debug,
      quiet = quiet,
      pandoc_args = pandoc_args
    )
  )
  args <- args[!is.null(args)]
  execute_params <- as.symbol(paste0(name, "_params"))
  deps <- call_list(as_symbols(knitr_deps(input)))
  fun <- call_ns("tarchetypes", "tar_quarto_rep_run")
  exprs <- list(
    fun = fun,
    args = args,
    execute_params = execute_params,
    files = files,
    deps = deps
  )
  as.expression(as.call(exprs))
}

#' @title Render a batch of parameterized Quarto reports
#'   inside a `tar_quarto_rep()` target.
#' @description Internal function needed for `tar_quarto()`.
#'   Users should not invoke it directly.
#' @export
#' @keywords internal
#' @return Character vector with the path to the Quarto
#'   source file and the rendered output file. Both paths
#'   depend on the input source path, and they have no defaults.
#' @param args A named list of arguments to `quarto::quarto_render()`.
#' @param execute_params A data frame of Quarto parameters to branch over.
#' @param files Character vector of extra files that `targets`
#'   should track for changes. If the content of one of these files changes,
#'   then the report will rerun over all the parameters on the next
#'   `tar_make()`. These files are *extra* files, and they
#'   do not include the Quarto
#'   source document or rendered output document,
#'   which are already tracked for changes. Examples include
#'   bibliographies, style sheets, and supporting image files.
#' @param deps An unnamed list of target dependencies of the Quarto
#'   report, automatically created by `tar_quarto_rep()`.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' # Parameterized Quarto:
#' lines <- c(
#'   "---",
#'   "title: 'report.qmd source file'",
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
#' args <- list(
#'   input = "report.qmd",
#'   execute = TRUE,
#'   execute_dir = quote(getwd()),
#'   execute_daemon = 0,
#'   execute_daemon_restart = FALSE,
#'   execute_debug = FALSE,
#'   cache = FALSE,
#'   cache_refresh = FALSE,
#'   debug = FALSE,
#'   quiet = TRUE,
#'   as_job = FALSE
#' )
#' execute_params <- tibble::tibble(
#'   par = c("non-default value 1", "non-default value 2"),
#'   output_file = c("report1.html", "report2.html")
#' )
#' tar_quarto_rep_run(
#'   args = args,
#'   execute_params = execute_params,
#'   files = character(0),
#'   deps = NULL
#' )
#' })
#' }
tar_quarto_rep_run <- function(args, execute_params, files, deps) {
  assert_quarto()
  rm(deps)
  gc()
  execute_params <- split(execute_params, f = seq_len(nrow(execute_params)))
  out <- unname(unlist(map(execute_params, ~tar_quarto_rep_rep(args, .x))))
  support <- sprintf("%s_files", fs::path_ext_remove(basename(args$input)))
  out <- if_any(dir.exists(support), c(out, support), out)
  sort(unique(c(out, files)))
}

tar_quarto_rep_rep <- function(args, execute_params) {
  withr::local_options(list(crayon.enabled = NULL))
  default_path <- tar_quarto_rep_default_path(args$input, execute_params)
  args$output_file <- execute_params[["output_file"]] %|||% default_path
  args$execute_params <- execute_params
  args$execute_params[["output_file"]] <- NULL
  args$execute_params[["tar_group"]] <- NULL
  do.call(quarto::quarto_render, args)
  as.character(fs::path_rel(unlist(c(args$input, args$output_file))))
}

tar_quarto_rep_default_path <- function(input, execute_params) {
  out <- fs::path_ext_remove(input)
  hash <- digest::digest(execute_params, algo = "xxhash32")
  sprintf("%s_%s.html", out, hash)
}
