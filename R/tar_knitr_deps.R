#' @title List literate programming dependencies.
#' @export
#' @description List the target dependencies of one or more
#'   literate programming reports (R Markdown or `knitr`).
#' @return Character vector of the names of targets
#'   that are dependencies of the `knitr` report.
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
#' tar_knitr_deps(report)
tar_knitr_deps <- function(path) {
  assert_path(path)
  sort(unique(unlist(map(path, knitr_deps))))
}
