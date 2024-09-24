#' @title Target with a `knitr` document.
#' @export
#' @family Literate programming targets
#' @description Shorthand to include `knitr` document in a
#'   `targets` pipeline.
#'
#'   [tar_knit()] expects an unevaluated symbol for the `name` argument,
#'   and it supports named `...` arguments for `knitr::knit()` arguments.
#'   [tar_knit_raw()] expects a character string for `name` and
#'   supports an evaluated expression object
#'   `knit_arguments` for `knitr::knit()` arguments.
#' @details `tar_knit()` is an alternative to `tar_target()` for
#'   `knitr` reports that depend on other targets. The `knitr` source
#'   should mention dependency targets with `tar_load()` and `tar_read()`
#'   in the active code chunks (which also allows you to knit the report
#'   outside the pipeline if the `_targets/` data store already exists).
#'   (Do not use `tar_load_raw()` or `tar_read_raw()` for this.)
#'   Then, `tar_knit()` defines a special kind of target. It
#'     1. Finds all the `tar_load()`/`tar_read()` dependencies in the report
#'       and inserts them into the target's command.
#'       This enforces the proper dependency relationships.
#'       (Do not use `tar_load_raw()` or `tar_read_raw()` for this.)
#'     2. Sets `format = "file"` (see `tar_target()`) so `targets`
#'       watches the files at the returned paths and reruns the report
#'       if those files change.
#'     3. Configures the target's command to return both the output
#'       report files and the input source file. All these file paths
#'       are relative paths so the project stays portable.
#'     4. Forces the report to run in the user's current working directory
#'       instead of the working directory of the report.
#'     5. Sets convenient default options such as `deployment = "main"`
#'       in the target and `quiet = TRUE` in `knitr::knit()`.
#' @return A `tar_target()` object with `format = "file"`.
#'   When this target runs, it returns a character vector
#'   of file paths. The first file paths are the output files
#'   (returned by `knitr::knit()`) and the `knitr`
#'   source file is last. But unlike `knitr::knit()`,
#'   all returned paths are *relative* paths to ensure portability
#'   (so that the project can be moved from one file system to another
#'   without invalidating the target).
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @inheritParams targets::tar_target
#' @inheritParams knitr::knit
#' @inheritParams tar_render
#' @param name Name of the target.
#'   [tar_knit()] expects an unevaluated symbol for the `name` argument,
#'   whereas [tar_knit_raw()] expects a character string for `name`.
#' @param path Character string, file path to the `knitr` source file.
#'   Must have length 1.
#' @param ... Named arguments to `knitr::knit()`.
#'   These arguments are unevaluated when supplied to [tar_knit()].
#'   They are only evaluated when the target actually runs in
#'   `tar_make()`, not when the target is defined.
#' @param knit_arguments Optional language object with a list
#'   of named arguments to `knitr::knit()`.
#'   Cannot be an expression object.
#'   (Use `quote()`, not `expression()`.)
#'   The reason for quoting is that these arguments may depend on
#'   upstream targets whose values are not available at
#'   the time the target is defined, and because `tar_knit_raw()`
#'   is the "raw" version of a function, we want to avoid
#'   all non-standard evaluation.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   library(tarchetypes)
#'   # Ordinarily, you should create the report outside
#'   # tar_script() and avoid temporary files.
#'   lines <- c(
#'     "---",
#'     "title: report",
#'     "output_format: html_document",
#'     "---",
#'     "",
#'     "```{r}",
#'     "targets::tar_read(data)",
#'     "```"
#'   )
#'   path <- tempfile()
#'   writeLines(lines, path)
#'   list(
#'     tar_target(data, data.frame(x = seq_len(26), y = letters)),
#'     tar_knit(name = report, path = path),
#'     tar_knit_raw(name = "report2", path = path)
#'   )
#' })
#' targets::tar_make()
#' })
#' }
tar_knit <- function(
  name,
  path,
  output_file = NULL,
  working_directory = NULL,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  deployment = "main",
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description"),
  quiet = TRUE,
  ...
) {
  targets::tar_assert_package("knitr")
  targets::tar_assert_file(path)
  targets::tar_assert_chr(output_file %|||% "x")
  targets::tar_assert_scalar(output_file %|||% "x")
  if (!is.null(working_directory)) {
    targets::tar_assert_file(working_directory)
  }
  envir <- tar_option_get("envir")
  args <- targets::tar_tidy_eval(
    substitute(list(...)),
    envir = envir,
    tidy_eval = tidy_eval
  )
  targets::tar_target_raw(
    name = targets::tar_deparse_language(substitute(name)),
    command = tar_knit_command(
      path,
      output_file,
      working_directory,
      args,
      quiet
    ),
    packages = packages,
    library = library,
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

tar_knit_command <- function(
  path,
  output_file,
  working_directory,
  args,
  quiet
) {
  args$input <- path
  args$output <- output_file
  args$quiet <- quiet
  deps <- call_list(as_symbols(knitr_deps(path)))
  fun <- call_ns("tarchetypes", "tar_knit_run")
  exprs <- list(
    fun,
    path = path,
    working_directory = working_directory,
    args = args,
    deps = deps
  )
  as.expression(as.call(exprs))
}

#' @title Run a `knitr` report inside a `tar_knit()` target.
#' @description Internal function needed for `tar_knit()`.
#'   Users should not invoke it directly.
#' @export
#' @keywords internal
#' @return Character with the path to the `knitr` source file
#'   and the relative path to the output `knitr` report.
#'   The output path depends on the input path argument,
#'   which has no default.
#' @inheritParams tar_knit
#' @param args A named list of arguments to `knitr::knit()`.
#' @param deps An unnamed list of target dependencies of the `knitr`
#'   report, automatically created by `tar_knit()`.
tar_knit_run <- function(path, working_directory, args, deps) {
  targets::tar_assert_package("knitr")
  withr::local_options(list(crayon.enabled = NULL))
  opt <- knitr::opts_knit$get("root.dir")
  knitr::opts_knit$set(root.dir = working_directory %|||% getwd())
  on.exit(knitr::opts_knit$set(root.dir = opt))
  envir <- parent.frame()
  args$envir <- args$envir %|||% targets::tar_envir(default = envir)
  force(args$envir)
  fs::path_rel(c(do.call(knitr::knit, args), path))
}
