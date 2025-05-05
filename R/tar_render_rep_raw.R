#' @rdname tar_render_rep
#' @export
tar_render_rep_raw <- function(
  name,
  path,
  working_directory = NULL,
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
  description = targets::tar_option_get("description"),
  quiet = TRUE,
  args = list()
) {
  targets::tar_assert_package("rmarkdown")
  targets::tar_assert_file(path)
  targets::tar_assert_not_dirs(path)
  if (!is.null(working_directory)) {
    targets::tar_assert_file(working_directory)
  }
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
    cue = cue,
    description = description
  )
  target <- targets::tar_target_raw(
    name = name,
    command = tar_render_rep_command(
      name,
      path,
      working_directory,
      quiet,
      args,
      rep_workers
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
    cue = cue,
    description = description
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

tar_render_rep_command <- function(
  name,
  path,
  working_directory,
  quiet,
  args,
  rep_workers
) {
  args$input <- path
  args$knit_root_dir <- working_directory %|||% quote(getwd())
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
  target <- targets::tar_definition()
  name <- target$pedigree$parent %|||% target$settings$name
  batch <- target$pedigree$index %|||% target$index
  reps <- length(params)
  seeds <- produce_batch_seeds(name = name, batch = batch, reps = reps)
  if (rep_workers > 1L) {
    cluster <- make_psock_cluster(rep_workers)
    on.exit(parallel::stopCluster(cl = cluster))
    out <- parallel::clusterMap(
      cl = cluster,
      fun = fun,
      .x = seq_along(params),
      .y = params,
      MoreArgs = list(
        args = args,
        path = path,
        seeds = seeds
      ),
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
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
  withr::local_options(list(cli.num_colors = 1L, cli.dynamic = FALSE))
  default_path <- tar_render_rep_default_path(path, params)
  args$output_file <- params[["output_file"]] %|||% default_path
  fs::dir_create(dirname(args$output_file))
  args$params <- params
  args$params[["output_file"]] <- NULL
  args$params[["tar_group"]] <- NULL
  args$intermediates_dir <- fs::dir_create(tempfile())
  seed <- as.integer(if_any(anyNA(seeds), NA_integer_, seeds[rep]))
  if_any(anyNA(seed), NULL, targets::tar_seed_set(seed = seed))
  output <- do.call(rmarkdown::render, args)
  tar_render_paths(output, path)
}

tar_render_rep_default_path <- function(path, params) {
  sprintf("%s_%s", fs::path_ext_remove(path), hash_object(params))
}
