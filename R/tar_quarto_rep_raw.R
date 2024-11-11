#' @rdname tar_quarto_rep
#' @export
tar_quarto_rep_raw <- function(
  name,
  path,
  working_directory = NULL,
  execute_params = expression(NULL),
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
  assert_quarto()
  targets::tar_assert_scalar(name)
  targets::tar_assert_chr(name)
  targets::tar_assert_nzchar(name)
  targets::tar_assert_not_dirs(path)
  targets::tar_assert_file(path)
  if (!is.null(working_directory)) {
    targets::tar_assert_file(working_directory)
  }
  targets::tar_assert_lang(execute_params %|||% quote(x))
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
  targets::tar_assert_chr(quarto_args %|||% ".")
  targets::tar_assert_chr(pandoc_args %|||% ".")
  tar_assert_rep_workers(rep_workers)
  rep_workers <- as.integer(rep_workers)
  name_params <- paste0(name, "_params")
  sym_params <- as.symbol(name_params)
  default_output_file <- utils::head(
    tar_quarto_files(path, quiet = quiet)$output,
    n = 1L
  )
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
    cue = cue,
    description = description
  )
  target <- targets::tar_target_raw(
    name = name,
    command = tar_quarto_rep_command(
      name = name,
      path = path,
      working_directory = working_directory,
      extra_files = extra_files,
      execute = execute,
      cache = cache,
      cache_refresh = cache_refresh,
      debug = debug,
      quiet = quiet,
      quarto_args = quarto_args,
      pandoc_args = pandoc_args,
      default_output_file = default_output_file,
      rep_workers = rep_workers
    ),
    pattern = substitute(map(x), env = list(x = sym_params)),
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
    cue = cue,
    description = description
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
  working_directory,
  extra_files,
  execute,
  cache,
  cache_refresh,
  debug,
  quiet,
  quarto_args,
  pandoc_args,
  default_output_file,
  rep_workers
) {
  args <- substitute(
    list(
      input = path,
      execute = execute,
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
      execute = execute,
      execute_dir = working_directory %|||% quote(getwd()),
      cache = cache,
      cache_refresh = cache_refresh,
      debug = debug,
      quiet = quiet,
      quarto_args = quarto_args,
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
    default_output_file = default_output_file,
    rep_workers = rep_workers
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
#' @inheritParams tar_rep
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
  default_output_file,
  rep_workers
) {
  assert_quarto()
  rm(deps)
  gc()
  execute_params <- split(execute_params, f = seq_len(nrow(execute_params)))
  call <- quote(
    function(.x, .y, args, default_output_file, seeds) {
      tarchetypes::tar_quarto_rep_rep(
        rep = .x,
        execute_params = .y,
        args = args,
        default_output_file = default_output_file,
        seeds = seeds
      )
    }
  )
  fun <- eval(call, envir = targets::tar_option_get("envir"))
  target <- targets::tar_definition()
  name <- target$pedigree$parent %|||% target$settings$name
  batch <- target$pedigree$index %|||% target$index
  reps <- length(execute_params)
  seeds <- produce_batch_seeds(name = name, batch = batch, reps = reps)
  if (rep_workers > 1L) {
    cluster <- make_psock_cluster(rep_workers)
    on.exit(parallel::stopCluster(cl = cluster))
    out <- parallel::clusterMap(
      cl = cluster,
      fun = fun,
      .x = seq_along(execute_params),
      .y = execute_params,
      MoreArgs = list(
        args = args,
        default_output_file = default_output_file,
        seeds = seeds
      ),
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    )
  } else {
    out <- map2(
      x = seq_along(execute_params),
      y = execute_params,
      f = fun,
      args = args,
      default_output_file = default_output_file,
      seeds = seeds
    )
  }
  out <- unname(unlist(out))
  support <- sprintf("%s_files", fs::path_ext_remove(args$input))
  extra_files <- if_any(
    dir.exists(support),
    c(extra_files, support),
    extra_files
  )
  extra_files <- sort(unique(extra_files))
  unique(c(out, args$input, extra_files))
}

#' @title Run a rep in a `tar_quarto_rep()`.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return Output file paths.
#' @param rep Rep number.
#' @param execute_params Quarto parameters.
#' @param args Arguments to `quarto::quarto_render()`.
#' @param default_output_file Default Quarto output file.
#' @param seeds Random number generator seeds of the batch.
#' @examples
#' # See the examples of tar_quarto_rep().
tar_quarto_rep_rep <- function(
  rep,
  execute_params,
  args,
  default_output_file,
  seeds
) {
  withr::local_options(list(crayon.enabled = NULL))
  destination_file <- execute_params[["output_file"]]
  fs::dir_create(dirname(destination_file))
  extension <- paste0(".", fs::path_ext(destination_file))
  temporary_file <- basename(tempfile(fileext = extension))
  args$output_file <- temporary_file
  args$execute_params <- execute_params
  args$execute_params[["output_file"]] <- NULL
  args$execute_params[["tar_group"]] <- NULL
  seed <- as.integer(if_any(anyNA(seeds), NA_integer_, seeds[rep]))
  if_any(anyNA(seed), NULL, targets::tar_seed_set(seed = seed))
  result <- do.call(quarto::quarto_render, args)
  file.rename(temporary_file, destination_file)
  sort(as.character(fs::path_rel(unlist(destination_file))))
}

tar_quarto_rep_default_output_file <- function(params, default_output_file) {
  base <- basename(fs::path_ext_remove(default_output_file))
  hash <- hash_rows(params)
  ext <- fs::path_ext(default_output_file)
  out <- file.path(
    dirname(default_output_file),
    sprintf("%s_%s.%s", base, hash, ext)
  )
  gsub("^\\.\\/", "", out)
}
