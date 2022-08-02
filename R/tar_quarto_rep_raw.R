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
#' @param path Character string, file path to the Quarto source file.
#'   Must have length 1.
#' @param execute_params Expression object with code to generate
#'   a data frame or `tibble` with one row per rendered report
#'   and one column per Quarto parameter. You may also include an
#'   `output_file` column to specify the path of each rendered report.
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
#'       path = "report.qmd",
#'       execute_params = quote(tibble::tibble(par = c(1, 2)))
#'     )
#'   )
#' }, ask = FALSE)
#' # Then, run the targets pipeline as usual.
#' })
#' }
tar_quarto_rep_raw <- function(
  name,
  path,
  execute_params = expression(NULL),
  batches = NULL,
  extra_files = character(0),
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
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
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
  targets::tar_assert_scalar(path)
  targets::tar_assert_chr(path)
  targets::tar_assert_nzchar(path)
  targets::tar_assert_path(path)
  targets::tar_assert_not_dirs(path)
  targets::tar_assert_lang(execute_params)
  targets::tar_assert_dbl(batches %|||% 0L, "batches must be numeric.")
  targets::tar_assert_scalar(batches %|||% 0L, "batches must have length 1.")
  targets::tar_assert_chr(extra_files)
  targets::tar_assert_nzchar(extra_files)
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
  default_output_file <- utils::head(tar_quarto_files(path)$output, n = 1L)
  default_output_file <- default_output_file %||%
    fs::path_ext_set(path, "html")
  target_params <- targets::tar_target_raw(
    name = name_params,
    command = tar_quarto_rep_params_command(
      execute_params = execute_params,
      batches = batches,
      default_output_file = default_output_file
    ),
    packages = packages,
    library = library,
    format = format,
    iteration = "group",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
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
      path = path,
      extra_files = extra_files,
      execute = execute,
      cache = cache,
      cache_refresh = cache_refresh,
      debug = debug,
      quiet = quiet,
      pandoc_args = pandoc_args,
      default_output_file = default_output_file
    ),
    pattern = substitute(map(x), env = list(x = sym_params)),
    packages = packages,
    library = library,
    format = "file",
    repository = "local",
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
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

tar_quarto_rep_params_command <- function(
  execute_params,
  batches,
  default_output_file
) {
  fun <- call_ns("tarchetypes", "tar_quarto_rep_run_params")
  exprs <- list(
    fun,
    execute_params = execute_params,
    batches = batches,
    default_output_file = default_output_file
  )
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
#' @param default_output_file Default output file path deduced
#'   from the YAML front-matter of the Quarto source document.
#' @examples
#' execute_params <- tibble::tibble(param1 = letters[seq_len(4)])
#' tar_quarto_rep_run_params(execute_params, 1, "report.html")
#' tar_quarto_rep_run_params(execute_params, 2, "report.html")
#' tar_quarto_rep_run_params(execute_params, 3, "report.html")
#' tar_quarto_rep_run_params(execute_params, 4, "report.html")
tar_quarto_rep_run_params <- function(
  execute_params,
  batches,
  default_output_file
) {
  targets::tar_assert_df(execute_params)
  illegal <- "tar_group"
  intersect <- intersect(illegal, colnames(execute_params))
  targets::tar_assert_le(
    length(intersect),
    0L,
    paste(
      "illegal columns in execute_params:",
      paste(intersect, collapse = ", ")
    )
  )
  if ("output_file" %in% colnames(execute_params)) {
    targets::tar_assert_unique(
      execute_params$output_file,
      msg = paste(
        "If an output_file column is given in the execute_params argument of",
        "tar_quarto_rep(), then all the output files must be unique."
      )
    )
  } else {
    targets::tar_assert_unique(
      hash_rows(execute_params),
      msg = paste(
        "Rows of execute_params in tar_quarto_rep() must be unique",
        "if an output_file column is absent."
      )
    )
    execute_params$output_file <- tar_quarto_rep_default_output_file(
      execute_params,
      default_output_file
    )
  }
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
  path,
  extra_files,
  execute,
  cache,
  cache_refresh,
  debug,
  quiet,
  pandoc_args,
  default_output_file
) {
  args <- substitute(
    list(
      input = path,
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
      path = path,
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
  deps <- call_list(as_symbols(knitr_deps(path)))
  fun <- call_ns("tarchetypes", "tar_quarto_rep_run")
  exprs <- list(
    fun = fun,
    args = args,
    execute_params = execute_params,
    extra_files = extra_files,
    deps = deps,
    default_output_file = default_output_file
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
#' @param extra_files Character vector of extra files that `targets`
#'   should track for changes. If the content of one of these files changes,
#'   then the report will rerun over all the parameters on the next
#'   `tar_make()`. These files are *extra* files, and they
#'   do not include the Quarto
#'   source document or rendered output document,
#'   which are already tracked for changes. Examples include
#'   bibliographies, style sheets, and supporting image files.
#' @param deps An unnamed list of target dependencies of the Quarto
#'   report, automatically created by `tar_quarto_rep()`.
#' @param default_output_file Output file path determined by the
#'   YAML front-matter of the Quarto source document.
#'   Automatic output file names are based on this file.
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
#'   extra_files = character(0),
#'   deps = NULL,
#'   default_output_file = "report_default.html"
#' )
#' })
#' }
tar_quarto_rep_run <- function(
  args,
  execute_params,
  extra_files,
  deps,
  default_output_file
) {
  assert_quarto()
  rm(deps)
  gc()
  execute_params <- split(execute_params, f = seq_len(nrow(execute_params)))
  out <- map(execute_params, ~tar_quarto_rep_rep(args, .x, default_output_file))
  out <- unname(unlist(out))
  support <- sprintf("%s_files", fs::path_ext_remove(basename(args$input)))
  extra_files <- if_any(
    dir.exists(support),
    c(extra_files, support),
    extra_files
  )
  extra_files <- sort(unique(extra_files))
  unique(c(out, args$input, extra_files))
}

tar_quarto_rep_rep <- function(args, execute_params, default_output_file) {
  withr::local_options(list(crayon.enabled = NULL))
  args$output_file <- basename(execute_params[["output_file"]])
  args$execute_params <- execute_params
  args$execute_params[["output_file"]] <- NULL
  args$execute_params[["tar_group"]] <- NULL
  do.call(quarto::quarto_render, args)
  sort(as.character(fs::path_rel(unlist(args$output_file))))
}

tar_quarto_rep_default_output_file <- function(params, default_output_file) {
  base <- fs::path_ext_remove(default_output_file)
  hash <- hash_rows(params)
  ext <- fs::path_ext(default_output_file)
  out <- file.path(
    dirname(default_output_file),
    sprintf("%s_%s.%s", base, hash, ext)
  )
  gsub("^\\.\\/", "", out)
}
