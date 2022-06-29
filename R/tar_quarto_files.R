#' @title Quarto file detection
#' @export
#' @family Literate programming utilities
#' @description Detect the important files in a Quarto project.
#' @return A named list of important file paths in a Quarto project:
#'   source files to scrape for target dependencies, as well as
#'   other input and output files to track for changes.
#' @param path Character of length 1, either the file path
#'   to a Quarto source document or the directory path
#'   to a Quarto project. Defaults to the Quarto project in the
#'   current working directory.
#' @examples
#' lines <- c(
#'   "---",
#'   "title: source file",
#'   "---",
#'   "Assume these lines are in report.qmd.",
#'   "```{r}",
#'   "1 + 1",
#'   "```"
#' )
#' path <- tempfile(fileext = ".qmd")
#' writeLines(lines, path)
#' tar_quarto_files(path)
tar_quarto_files <- function(path = ".") {
  assert_quarto()
  targets::tar_assert_scalar(path)
  targets::tar_assert_chr(path)
  targets::tar_assert_nzchar(path)
  targets::tar_assert_path(path)
  out <- if_any(
    fs::is_dir(path),
    tar_quarto_files_project(path),
    tar_quarto_files_document(path)
  )
  for (field in names(out)) {
    out[[field]] <- as.character(sort(fs::path_rel(unique(out[[field]]))))
  }
  out
}

tar_quarto_files_document <- function(path) {
  info <- quarto::quarto_inspect(input = path)
  out <- list(sources = path)
  for (format in info$formats) {
    out$output <- c(
      out$output,
      file.path(dirname(path), format$pandoc$`output-file`)
    )
  }
  out
}

tar_quarto_files_project <- function(path) {
  info <- quarto::quarto_inspect(input = path)
  targets::tar_assert_nonempty(
    info$config$project$`output-dir`,
    paste(
      "Quarto project must have an output-dir field",
      "for compatibility with {tarchetypes}. Visit",
      "quarto.org to learn how to set output-dir in _quarto.yml."
    )
  )
  input <- info$files
  input$input <- NULL
  input <- unlist(input)
  input <- input[file.exists(input)]
  list(
    sources = info$files$input[file.exists(info$files$input)],
    output = file.path(path, info$config$project$`output-dir`),
    input = input
  )
}
