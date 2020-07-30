#' @title Alternative to `tar_target()` for a `knitr` document.
#' @export
#' @description Shorthand to include `knitr` document in a
#'   `targets` pipeline.
#' @details `tar_knit()` is an alternative to `tar_target()` for
#'   `knitr` reports that depend on other targets. The R Markdown source
#'   should mention dependency targets with `tar_load()` and `tar_read()`
#'   in the active code chunks (which also allows you to knit the report
#'   outside the pipeline if the `_targets/` data store already exists).
#'   Then, `tar_knit()` defines a special kind of target. It
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
#'       in the target and `quiet = TRUE` in `knitr::knit()`.
#' @return A `tar_target()` object with `format = "file"`.
#'   When this target runs, it returns a character vector
#'   of file paths. The first file paths are the output files
#'   (returned by `knitr::knit()`) and the R Markdown
#'   source file is last. But unlike `knitr::knit()`,
#'   all returned paths are *relative* paths to ensure portability
#'   (so that the project can be moved from one file system to another
#'   without invalidating the target).
#' @inheritParams targets::tar_target_raw
#' @inheritParams knitr::knit
#' @param path Character string, file path to the R Markdown source file.
#'   Must have length 1.
#' @param ... Named arguments to `knitr::knit()`
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
#'     tar_knit(report, "report.Rmd")
#'   )
#' })
#' targets::tar_make()
#' })
#' }
tar_knit <- function(
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
  assert_package("knitr", "tar_knit() requires the knitr package.")
  assert_scalar(path, "tar_knit() only takes one file at a time.")
  assert_chr(path, "path argument of tar_knit() must be a character.")
  assert_path(path, paste("the path", path, "for tar_knit() does not exist"))
  tar_target_raw(
    name = deparse_language(substitute(name)),
    command = tar_knit_command(path, quiet, list(...)),
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

tar_knit_command <- function(path, quiet, args) {
  deps <- rlang::syms(knitr_deps(path))
  args$input <- path
  args$quiet <- quiet
  expr_deps <- call_list(deps)
  expr_knit <- call_path_rel(call_c(list(call_knit(args), path)))
  expr_opt <- quote(opt <- knitr::opts_knit$get("root.dir"))
  expr_set <- quote(knitr::opts_knit$set(root.dir = getwd()))
  expr_exit <- quote(on.exit(knitr::opts_knit$set(root.dir = opt)))
  expr <- call_brace(list(expr_deps, expr_opt, expr_set, expr_exit, expr_knit))
  as.expression(expr)
}

call_knit <- function(args) {
  expr_knit_ns <- as.call(c(sym_ns, rlang::syms(c("knitr", "knit"))))
  expr_knit <- as.call(c(rlang::sym("knit"), args))
  expr_knit[[1]] <- expr_knit_ns
  expr_knit <- match.call(knitr::knit, expr_knit)
}
