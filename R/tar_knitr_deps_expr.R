#' @title Expression with literate programming dependencies.
#' @export
#' @family Literate programming utilities
#' @description Construct an expression whose global variable dependencies
#'   are the target dependencies of one or more literate programming reports
#'   (R Markdown or `knitr`). This helps third-party developers create their
#'   own third-party target factories for literate programming targets
#'   (similar to [tar_knit()] and [tar_render()]).
#' @return Expression object to name the dependency targets
#'   of the `knitr` report, which will be detected in the
#'   static code analysis of `targets`.
#' @param path Character vector, path to one or more R Markdown or
#'   `knitr` reports.
#' @examples
#' lines <- c(
#'   "---",
#'   "title: report",
#'   "output_format: html_document",
#'   "---",
#'   "",
#'   "```{r}",
#'   "targets::tar_load(data1)",
#'   "targets::tar_read(data2)",
#'   "```"
#' )
#' report <- tempfile()
#' writeLines(lines, report)
#' tar_knitr_deps_expr(report)
tar_knitr_deps_expr <- function(path) {
  call_list(as_symbols(tar_knitr_deps(path)))
}
