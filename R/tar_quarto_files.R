#' @title Quarto file detection
#' @export
#' @family Literate programming utilities
#' @description Detect the important files in a Quarto project.
#' @details This function is just a thin wrapper that interprets the output
#'   of `quarto::quarto_inspect()` and returns what `tarchetypes` needs to
#'   know about the current Quarto project or document.
#' @return A named list of important file paths in a Quarto project or document:
#'   * `sources`: source files with `tar_load()`/`tar_read()`
#'     target dependencies in R code chunks.
#'   * `output`: output files that will be generated during
#'     `quarto::quarto_render()`.
#'   * `input`: pre-existing files required to render the project or document,
#'     such as `_quarto.yml`.
#' @param path Character of length 1, either the file path
#'   to a Quarto source document or the directory path
#'   to a Quarto project. Defaults to the Quarto project in the
#'   current working directory.
#' @param profile Character of length 1, Quarto profile. If `NULL`,
#'   the default profile will be used. Requires Quarto version 1.2 or higher.
#'   See <https://quarto.org/docs/projects/profiles.html> for details.
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
#' # If Quarto is installed, run:
#' # tar_quarto_files(path)
tar_quarto_files <- function(path = ".", profile = NULL) {
  assert_quarto()
  targets::tar_assert_scalar(path)
  targets::tar_assert_chr(path)
  targets::tar_assert_nzchar(path)
  targets::tar_assert_path(path)
  targets::tar_assert_scalar(profile %|||% ".")
  targets::tar_assert_chr(profile %|||% ".")
  targets::tar_assert_nzchar(profile %|||% ".")
  if (!is.null(profile)) {
    withr::local_envvar(.new = c(QUARTO_PROFILE = profile))
  }
  out <- if_any(
    fs::is_dir(path),
    tar_quarto_files_project(path),
    tar_quarto_files_document(path)
  )
  for (field in c("sources", "output", "input")) {
    out[[field]] <- sort(fs::path_rel(unique(as.character(out[[field]]))))
    out[[field]] <- as.character(out[[field]])
  }
  out
}

tar_quarto_files_document <- function(path) {
  info <- quarto::quarto_inspect(input = path)
  out <- list(sources = path)

  # Collect data about output files.
  for (format in info$formats) {
    out$output <- c(
      out$output,
      file.path(dirname(path), format$pandoc$`output-file`)
    )
  }

  # Collect data about input files.
  for (myfile in names(info$fileInformation)) {
    out$input <- c(
      out$input,
      # `myfile` are relative paths starting from `path`.
      myfile,
      # `includeMap` contains relative paths starting from `myfile`.
      file.path(
        dirname(path),
        info$fileInformation[[myfile]]$includeMap$target
      )
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

  # Detect source files.
  sources <- info$files$input[file.exists(info$files$input)]

  # Detect input files.
  input <- info$files
  input <- unlist(input)
  input <- input[file.exists(input)]
  for (myfile in names(info$fileInformation)) {
    input <- c(
      input,
      # `myfile` is an absolute path.
      myfile,
      # `includeMap` files are relative starting from `myfile`.
      file.path(
        dirname(myfile),
        info$fileInformation[[myfile]]$includeMap$target
      )
    )
  }

  list(
    sources = sources,
    output = file.path(path, info$config$project$`output-dir`),
    input = input
  )
}
