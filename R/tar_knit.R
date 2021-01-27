#' @title Target with a `knitr` document.
#' @export
#' @description Shorthand to include `knitr` document in a
#'   `targets` pipeline.
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
#'
#'   Target objects represent skippable steps of the analysis pipeline
#'   as described at <https://books.ropensci.org/targets/>.
#'   Please see the design specification at
#'   <https://books.ropensci.org/targets-design/>
#'   to learn about the structure and composition of target objects.
#' @inheritParams targets::tar_target
#' @inheritParams knitr::knit
#' @param path Character string, file path to the `knitr` source file.
#'   Must have length 1.
#' @param ... Named arguments to `knitr::knit()`.
#'   These arguments are evaluated when the target actually runs in
#'   `tar_make()`, not when the target is defined.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({
#' lines <- c(
#'   "---",
#'   "title: report",
#'   "output_format: html_document",
#'   "---",
#'   "",
#'   "```{r}",
#'   "targets::tar_read(data)",
#'   "```"
#' )
#' writeLines(lines, "report.Rmd")
#' targets::tar_script({
#'   library(tarchetypes)
#'   list(
#'     tar_target(data, data.frame(x = seq_len(26), y = letters)),
#'     tar_knit(report, "report.Rmd")
#'   )
#' })
#' targets::tar_make()
#' })
#' }
tar_knit <- function(
  name,
  path,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  error = targets::tar_option_get("error"),
  deployment = "main",
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  quiet = TRUE,
  ...
) {
  assert_package("knitr", "tar_knit() requires the knitr package.")
  assert_scalar(path, "tar_knit() only takes one file at a time.")
  assert_chr(path, "path argument of tar_knit() must be a character.")
  assert_path(path, paste("the path", path, "for tar_knit() does not exist"))
  envir <- tar_option_get("envir")
  args <- tidy_eval(substitute(list(...)), envir = envir, tidy_eval = tidy_eval)
  tar_target_raw(
    name = deparse_language(substitute(name)),
    command = tar_knit_command(path, args, quiet),
    packages = packages,
    library = library,
    format = "file",
    error = error,
    deployment = deployment,
    priority = priority,
    resources = resources,
    retrieval = retrieval,
    cue = cue
  )
}

tar_knit_command <- function(path, args, quiet) {
  args$input <- path
  args$quiet <- quiet
  deps <- call_list(rlang::syms(knitr_deps(path)))
  fun <- call_ns("tarchetypes", "tar_knit_run")
  exprs <- list(fun, path = path, args = args, deps = deps)
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
#' @param path Path to the `knitr` source file.
#' @param args A named list of arguments to `knitr::knit()`.
#' @param deps An unnamed list of target dependencies of the `knitr`
#'   report, automatically created by `tar_knit()`.
tar_knit_run <- function(path, args, deps) {
  assert_package("knitr")
  opt <- knitr::opts_knit$get("root.dir")
  knitr::opts_knit$set(root.dir = getwd())
  on.exit(knitr::opts_knit$set(root.dir = opt))
  envir <- parent.frame()
  args$envir <- args$envir %||% targets::tar_envir(default = envir)
  force(args$envir)
  fs::path_rel(c(do.call(knitr::knit, args), path))
}
