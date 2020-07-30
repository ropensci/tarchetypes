#' @title Alternative to `tar_target()` for an R Markdown document.
#' @export
#' @description Shorthand to include an R Markdown report in a
#'   `targets` pipeline.
#' @details `tar_render()` is an alternative to `tar_target()` for
#'   R Markdown reports that depend on other targets. The R Markdown source
#'   should mention dependency targets with `tar_load()` and `tar_read()`
#'   in the active code chunks (which also allows you to render the report
#'   outside the pipeline if the `_targets/` data store already exists).
#'   Then, `tar_render()` defines a special kind of target. It
#'     1. Finds all the `tar_load()`/`tar_read()` dependencies in the report
#'       and inserts them into the target's command.
#'       This enforces the proper dependency relationships.
#'     2. Sets `format = "file"` (see `tar_target()`) so `targets`
#'       watches the files at the returned paths and reruns the report
#'       if those files change.
#'     3. Configures the target's command to return both the output
#'       report files and the input source file. All these file paths
#'       are relative paths so the project stays portable.
#'     4. Forces the report to run in the user's current working directory
#'       instead of the working directory of the report.
#'     5. Sets convenient default options such as `deployment = "local"`
#'       in the target and `quiet = TRUE` in `rmarkdown::render()`.
#' @return A `tar_target()` object with `format = "file"`.
#'   When this target runs, it returns a character vector
#'   of file paths. The first file paths are the output files
#'   (returned by `rmarkdown::render()`) and the R Markdown
#'   source file is last. But unlike `rmarkdown::render()`,
#'   all returned paths are *relative* paths to ensure portability
#'   (so that the project can be moved from one file system to another
#'   without invalidating the target).
#' @inheritParams targets::tar_target_raw
#' @inheritParams rmarkdown::render
#' @param path Character string, file path to the R Markdown source file.
#'   Must have length 1.
#' @param ... Named arguments to `rmarkdown::render()`
#' @examples
#' \dontrun{
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
#'   tar_pipeline(
#'     tar_target(data, data.frame(x = seq_len(26), y = letters)),
#'     tar_render(report, "report.Rmd")
#'   )
#' })
#' targets::tar_make()
#' })
#' }
tar_render <- function(
  name,
  path,
  packages = targets::tar_option("packages", (.packages())),
  library = targets::tar_option("library"),
  error = targets::tar_option("error", "stop"),
  deployment = targets::tar_option("deployment", "local"),
  priority = 0,
  template = targets::tar_option("template", NULL),
  resources = targets::tar_option("resources", list()),
  retrieval = targets::tar_option("retrieval", "local"),
  cue = targets::tar_option("cue", NULL),
  quiet = TRUE,
  ...
) {
  assert_package("rmarkdown", "tar_render() requires the rmarkdown package.")
  assert_scalar(path, "tar_render() only takes one file at a time.")
  assert_chr(path, "path argument of tar_render() must be a character.")
  assert_path(path, paste("the path", path, "for tar_render() does not exist"))
  tar_target_raw(
    name = deparse_language(substitute(name)),
    command = tar_render_command(path, quiet, list(...)),
    packages = packages,
    library = library,
    envir = targets::tar_option("envir", globalenv()),
    format = "file",
    error = error,
    deployment = deployment,
    priority = priority,
    template = template,
    resources = resources,
    retrieval = retrieval,
    cue = cue
  )
}

tar_render_command <- function(path, quiet, args) {
  deps <- rlang::syms(knitr_deps(path))
  args$input <- path
  args$knit_root_dir <- quote(getwd())
  args$quiet <- quiet
  expr_deps <- call_list(deps)
  expr_render <- call_path_rel(call_c(list(call_render(args), path)))
  expr <- call_brace(list(expr_deps, expr_render))
  as.expression(expr)
}

call_render <- function(args) {
  expr_render_ns <- as.call(c(sym_ns, rlang::syms(c("rmarkdown", "render"))))
  expr_render <- as.call(c(rlang::sym("render"), args))
  expr_render[[1]] <- expr_render_ns
  expr_render <- match.call(rmarkdown::render, expr_render)
}
