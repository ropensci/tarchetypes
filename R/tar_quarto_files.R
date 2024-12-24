#' @title Quarto file detection
#' @export
#' @family Literate programming utilities
#' @description Detect the important files in a Quarto project.
#' @details This function is just a thin wrapper that interprets the output
#'   of `quarto::quarto_inspect()` and returns what `tarchetypes` needs to
#'   know about the current Quarto project or document.
#' @return A named list of important file paths in a Quarto project or document:
#'   * `sources`: source files which may reference upstream target
#'     dependencies in code chunks using `tar_load()`/`tar_read()`.
#'   * `output`: output files that will be generated during
#'     `quarto::quarto_render()`.
#'   * `input`: pre-existing files required to render the project or document,
#'     such as `_quarto.yml` and quarto extensions.
#' @inheritParams quarto::quarto_render
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
tar_quarto_files <- function(path = ".", profile = NULL, quiet = TRUE) {
  assert_quarto()
  targets::tar_assert_scalar(path)
  targets::tar_assert_chr(path)
  targets::tar_assert_nzchar(path)
  targets::tar_assert_path(path)
  targets::tar_assert_scalar(profile %|||% ".")
  targets::tar_assert_chr(profile %|||% ".")
  targets::tar_assert_nzchar(profile %|||% ".")
  out <- if_any(
    fs::is_dir(path),
    tar_quarto_files_project(path, profile, quiet),
    tar_quarto_files_document(path, profile, quiet)
  )
  for (field in c("sources", "output", "input")) {
    out[[field]] <- as.character(fs::path_rel(out[[field]]))
    out[[field]] <- unique(sort(out[[field]]))
  }
  out
}

tar_quarto_files_document <- function(path, profile, quiet) {
  info <- quarto::quarto_inspect(
    input = path,
    profile = profile,
    quiet = quiet
  )
  out <- list()
  # Collect data about source files.
  out$sources <- tar_quarto_files_get_source_files(info$fileInformation)
  # Collect data about output files.
  output_dir <- info$project$config$project$`output-dir`
  for (format in info$formats) {
    out$output <- c(
      out$output,
      if_any(
        is.null(output_dir),
        file.path(dirname(path), format$pandoc$`output-file`),
        file.path(output_dir, dirname(path), format$pandoc$`output-file`)
      )
    )
  }
  # Collect data about input files. As this is not a project, there doesn't
  # exist the `info$files` key. However, we can include resources if present.
  out$input <- as.character(info$resources)
  out$input <- out$input[file.exists(out$input)]
  out
}

tar_quarto_files_project <- function(path, profile, quiet) {
  info <- quarto::quarto_inspect(
    input = path,
    profile = profile,
    quiet = quiet
  )
  targets::tar_assert_nonempty(
    info$config$project$`output-dir`,
    paste(
      "Quarto project must have an output-dir field",
      "for compatibility with {tarchetypes}. Visit",
      "quarto.org to learn how to set output-dir in _quarto.yml."
    )
  )
  out <- list(output = file.path(path, info$config$project$`output-dir`))
  # Collect data about source files.
  out$sources <- tar_quarto_files_get_source_files(info$fileInformation)
  # Detect input files like the config file (`_quarto.yml`) and resources like
  # quarto extensions. Make sure in the end that these files exist.
  out$input <- unlist(c(info$files$config, info$files$resources))
  out$input <- out$input[file.exists(out$input)]
  out
}

#' @title Get Source Files From Quarto Inspect
#' @keywords internal
#' @description Collects all files from the
#'   `fileInformation` field that are used in the current report.
#' @details `fileInformation` contains a list of files. Each file entry contains
#'   two data frames. The first, `includeMap`, contains a `source` column (files
#'   that include other files, e.g. the main report file) and a `target` column
#'   (files that get included by the `source` files). The `codeCells` data frame
#'   contains all code cells from the files represented in `includeMap`.
#' @return A character vector of Quarto source files.
#' @param file_information The `fileInformation` element of the list
#'   returned by `quarto::quarto_inspect()`.
tar_quarto_files_get_source_files <- function(file_information) {
  out <- character(0)
  for (myfile in names(file_information)) {
    # Collect relevant source files. The files in `includeMap$target` are always
    # relative to the main entry point of the report. Thus, we need to add the
    # corresponding paths to the entries.
    # We don't need to include the `source` column as all files are also present
    # in `target` or are `myfile`.
    out <- c(
      out,
      myfile,
      file.path(
        dirname(myfile),
        file_information[[myfile]]$includeMap$target
      )
    )
  }
  # Check that these files actually exist.
  out <- out[file.exists(out)]
  # We don't need to call `unique` here on `out` as this happens on the main
  # function.
  out
}
