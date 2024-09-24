#' @rdname tar_render
#' @export
tar_render_raw <- function(
  name,
  path,
  output_file = NULL,
  working_directory = NULL,
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  error = targets::tar_option_get("error"),
  deployment = "main",
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description"),
  quiet = TRUE,
  render_arguments = quote(list())
) {
  targets::tar_assert_package("rmarkdown")
  targets::tar_assert_file(path)
  targets::tar_assert_chr(output_file %|||% "x")
  targets::tar_assert_scalar(output_file %|||% "x")
  targets::tar_assert_nzchar(output_file %|||% "x")
  if (!is.null(working_directory)) {
    targets::tar_assert_file(working_directory)
  }
  targets::tar_assert_lang(render_arguments)
  targets::tar_assert_not_expr(render_arguments)
  targets::tar_target_raw(
    name = name,
    command = tar_render_command(
      path,
      output_file,
      working_directory,
      render_arguments,
      quiet
    ),
    packages = packages,
    library = library,
    format = "file",
    repository = "local",
    error = error,
    deployment = deployment,
    priority = priority,
    resources = resources,
    retrieval = retrieval,
    cue = cue,
    description = description
  )
}


tar_render_command <- function(path, output, working_directory, args, quiet) {
  args$input <- path
  args$output_file <- output
  args$knit_root_dir <- working_directory %|||% quote(getwd())
  args$quiet <- quiet
  deps <- call_list(as_symbols(knitr_deps(path)))
  fun <- call_ns("tarchetypes", "tar_render_run")
  exprs <- list(fun, path = path, args = args, deps = deps)
  as.expression(as.call(exprs))
}

#' @title Render an R Markdown report inside a `tar_render()` target.
#' @description Internal function needed for `tar_render()`.
#'   Users should not invoke it directly.
#' @export
#' @keywords internal
#' @return Character vector with the path to the R Markdown source file
#'   and the relative path to the output. These paths depend on the input
#'   source file path and have no defaults.
#' @param path Path to the R Markdown source file.
#' @param args A named list of arguments to `rmarkdown::render()`.
#' @param deps An unnamed list of target dependencies of the R Markdown
#'   report, automatically created by `tar_render()`.
tar_render_run <- function(path, args, deps) {
  rm(deps)
  gc()
  targets::tar_assert_package("rmarkdown")
  withr::local_options(list(crayon.enabled = NULL))
  envir <- parent.frame()
  args$envir <- args$envir %|||% targets::tar_envir(default = envir)
  force(args$envir)
  output <- do.call(rmarkdown::render, args)
  tar_render_paths(output, path)
}

tar_render_paths <- function(output, source) {
  output <- fs::path_rel(output)
  source <- fs::path_rel(source)
  files <- paste0(fs::path_ext_remove(output[1]), "_files")
  files <- if_any(all(file.exists(files)), files, character(0))
  c(sort(output), sort(source), files)
}
