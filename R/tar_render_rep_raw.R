#' @title Parameterized R Markdown with dynamic branching (raw version).
#' @export
#' @family Literate programming targets
#' @description Targets to render a parameterized R Markdown report
#'   with multiple sets of parameters (raw version). Same as
#'   `tar_render_rep()` except `name` is a character string,
#'   `params` is an expression object,
#'   and extra arguments to `rmarkdown::render()` are passed through
#'   the `args` argument instead of `...`.
#' @details `tar_render_rep_raw()` is an alternative to `tar_target_raw()` for
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
#'       and then the `*_files/` directory if it exists.
#'       All these file paths
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
#' @inheritSection tar_rep Replicate-specific seeds
#' @inheritParams targets::tar_target
#' @inheritParams rmarkdown::render
#' @inheritParams tar_rep
#' @param path Character string, file path to the R Markdown source file.
#'   Must have length 1.
#' @param params Expression object with code to generate
#'   a data frame or `tibble` with one row per rendered report
#'   and one column per R Markdown parameter. You may also include an
#'   `output_file` column to specify the path of each rendered report.
#'   R Markdown parameters must not be named `tar_group` or `output_file`.
#'   This `params` argument is converted into the command for a target
#'   that supplies the R Markdown parameters.
#' @param batches Number of batches to group the R Markdown files.
#'   For a large number of reports, increase the number of batches
#'   to decrease target-level overhead. Defaults to the number of
#'   reports to render (1 report per batch).
#' @param format Character of length 1, `format` argument to `tar_target()`
#'   to store the data frame of R Markdown parameters.
#' @param iteration Character of length 1, `iteration` argument
#'   to `tar_target()` for the R Markdown documents. Does not apply
#'   to the target with R Markdown parameters (whose iteration
#'   is always `"group"`).
#' @param args Named list of other arguments to `rmarkdown::render()`.
#'   Must not include `params` or `output_file`. Evaluated when the target
#'   is defined.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' # Parameterized R Markdown:
#' lines <- c(
#'   "---",
#'   "title: 'report.Rmd source file'",
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
#'     tar_render_rep_raw(
#'       "report",
#'       "report.Rmd",
#'       params = quote(tibble::tibble(par = c(1, 2)))
#'     )
#'   )
#' }, ask = FALSE)
#' # Then, run the targets pipeline as usual.
#' })
#' }
tar_render_rep_raw <- function(
  name,
  path,
  params = expression(NULL),
  batches = NULL,
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
  quiet = TRUE,
  args = list()
) {
  targets::tar_assert_package("rmarkdown")
  targets::tar_assert_scalar(path)
  targets::tar_assert_chr(path)
  targets::tar_assert_path(path)
  targets::tar_assert_not_dirs(path)
  targets::tar_assert_lang(params %|||% quote(x))
  targets::tar_assert_dbl(batches %|||% 0L, "batches must be numeric.")
  targets::tar_assert_scalar(batches %|||% 0L, "batches must have length 1.")
  targets::tar_assert_list(args, "args must be a named list.")
  targets::tar_assert_nonempty(
    names(args %||% list(x = 1)),
    "args must be a named list."
  )
  tar_assert_rep_workers(rep_workers)
  rep_workers <- as.integer(rep_workers)
  name_params <- paste0(name, "_params")
  sym_params <- as.symbol(name_params)
  target_params <- targets::tar_target_raw(
    name = name_params,
    command = tar_render_rep_params_command(params, batches),
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
    command = tar_render_rep_command(name, path, quiet, args, rep_workers),
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

tar_render_rep_params_command <- function(params, batches) {
  fun <- call_ns("tarchetypes", "tar_render_rep_run_params")
  exprs <- list(fun, params = params, batches = batches)
  as.expression(as.call(exprs))
}

#' @title Prepare R Markdown parameters for `tar_render_rep()`.
#' @description Internal function needed for `tar_render_rep()`.
#'   Users should not invoke it directly.
#' @export
#' @keywords internal
#' @return A batched data frame of R Markdown parameters.
#' @param params Data frame of R Markdown parameters.
#' @param batches Number of batches to split up the renderings.
#' @examples
#' params <- tibble::tibble(param1 = letters[seq_len(4)])
#' tar_render_rep_run_params(params, 1)
#' tar_render_rep_run_params(params, 2)
#' tar_render_rep_run_params(params, 3)
#' tar_render_rep_run_params(params, 4)
tar_render_rep_run_params <- function(params, batches) {
  targets::tar_assert_df(params)
  illegal <- "tar_group"
  intersect <- intersect(illegal, colnames(params))
  targets::tar_assert_le(
    length(intersect),
    0L,
    paste(
      "illegal columns in params:",
      paste(intersect, collapse = ", ")
    )
  )
  if ("output_file" %in% colnames(params)) {
    targets::tar_assert_unique(
      params$output_file,
      msg = paste(
        "If an output_file column is given in the params argument of",
        "tar_render_rep(), then all the output files must be unique."
      )
    )
  } else {
    targets::tar_assert_unique(
      hash_rows(params),
      msg = "Rows of params in tar_render_rep() must be unique."
    )
  }
  batches <- batches %|||% nrow(params)
  params$tar_group <- if_any(
    batches > 1L,
    as.integer(cut(seq_len(nrow(params)), breaks = batches)),
    rep(1L, nrow(params))
  )
  params
}

tar_render_rep_command <- function(name, path, quiet, args, rep_workers) {
  args$input <- path
  args$knit_root_dir <- quote(getwd())
  args$quiet <- quiet
  params <- as.symbol(paste0(name, "_params"))
  deps <- call_list(as_symbols(knitr_deps(path)))
  fun <- call_ns("tarchetypes", "tar_render_rep_run")
  exprs <- list(
    fun,
    path = path,
    params = params,
    args = args,
    deps = deps,
    rep_workers = rep_workers
  )
  as.expression(as.call(exprs))
}

#' @title Render a batch of parameterized R Markdown reports
#'   inside a `tar_render_rep()` target.
#' @description Internal function needed for `tar_render()`.
#'   Users should not invoke it directly.
#' @export
#' @keywords internal
#' @return Character vector with the path to the R Markdown
#'   source file and the rendered output file. Both paths
#'   depend on the input source path, and they have no defaults.
#' @inheritParams tar_rep
#' @param path Path to the R Markdown source file.
#' @param args A named list of arguments to `rmarkdown::render()`.
#' @param deps An unnamed list of target dependencies of the R Markdown
#'   report, automatically created by `tar_render_rep()`.
tar_render_rep_run <- function(path, params, args, deps, rep_workers) {
  targets::tar_assert_package("rmarkdown")
  rm(deps)
  gc()
  envir <- parent.frame()
  args$envir <- args$envir %|||% targets::tar_envir(default = envir)
  force(args$envir)
  params <- split(params, f = seq_len(nrow(params)))
  call <- quote(
    function(.x, .y, args, path, seeds) {
      tarchetypes::tar_render_rep_rep(
        rep = .x,
        params = .y,
        args = args,
        path = path,
        seeds = seeds
      )
    }
  )
  fun <- eval(call, envir = targets::tar_option_get("envir"))
  pedigree <- targets::tar_definition()$pedigree
  name <- pedigree$parent
  batch <- pedigree$index
  reps <- length(params)
  seeds <- produce_batch_seeds(name = name, batch = batch, reps = reps)
  if (rep_workers > 1L) {
    plan_old <- future::plan()
    on.exit(future::plan(plan_old, .cleanup = FALSE))
    future::plan(future.callr::callr, workers = rep_workers, .cleanup = FALSE)
    out <- furrr::future_map2(
      .x = seq_along(params),
      .y = params,
      .f = fun,
      .options = furrr::furrr_options(
        seed = 1L,
        packages = targets::tar_definition()$command$packages,
        globals = names(targets::tar_option_get("envir"))
      ),
      args = args,
      path = path,
      seeds = seeds
    )
  } else {
    out <- map2(
      x = seq_along(params),
      y = params,
      f = fun,
      args = args,
      path = path,
      seeds = seeds
    )
  }
  unname(unlist(out))
}

#' @title Run a rep in a `tar_render_rep()`.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return Output file paths.
#' @param rep Rep number.
#' @param params R Markdown parameters.
#' @param args Arguments to `rmarkdown::render()`.
#' @param path R Markdown output file.
#' @param seeds Random number generator seeds of the batch.
#' @examples
#' # See the examples of tar_quarto_rep().
tar_render_rep_rep <- function(rep, params, args, path, seeds) {
  withr::local_options(list(crayon.enabled = NULL))
  default_path <- tar_render_rep_default_path(path, params)
  args$output_file <- params[["output_file"]] %|||% default_path
  fs::dir_create(dirname(args$output_file))
  args$params <- params
  args$params[["output_file"]] <- NULL
  args$params[["tar_group"]] <- NULL
  args$intermediates_dir <- fs::dir_create(tempfile())
  seed <- as.integer(if_any(anyNA(seeds), NA_integer_, seeds[rep]))
  output <- if_any(
    anyNA(seed),
    do.call(rmarkdown::render, args),
    withr::with_seed(
      seed = seed,
      code = do.call(rmarkdown::render, args)
    )
  )
  tar_render_paths(output, path)
}

tar_render_rep_default_path <- function(path, params) {
  out <- fs::path_ext_remove(path)
  hash <- digest::digest(params, algo = "xxhash32")
  sprintf("%s_%s", out, hash)
}
