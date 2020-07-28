#' @title Target that renders an R Markdown report
#'   which depends on upstream targets.
#' @export
#' @description Dependency targets are explicitly declared with
#'   `tar_load()` or `tar_read()` in active R code chunks in the
#'   report itself (stems and patterns only, no branch names).
#' @section Working directory:
#'   The current working directory (i.e. `getwd()`) must contain the
#'   `_targets/` data store not only when `tar_knitr()` is evaluated,
#'   but also when the actual report is run. That is why `tar_render()`
#'   always sets `knitr_root_dir` to the working directory from which
#'   `rmarkdown::render()` is called.
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
