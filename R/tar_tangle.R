#' @title Convert Quarto or R Markdown to a pipeline
#' @export
#' @family Domain-specific languages for pipeline construction
#' @description Convert a literate programming source file
#'   into a `targets` pipeline.
#' @details The word "tangle" comes from the early days of literate
#'   programming (see Knuth 1984).
#'   To "tangle" means to convert a literate programming source document
#'   into pure code: code that a human may find cryptic
#'   but a machine can run.
#'   For example, `knitr::knit(tangle = TRUE)` (and [knitr::purl()])
#'   accepts an `.Rmd` file and returns an R script with all the
#'   R code chunks pasted together.
#'
#'   `tar_tangle()` is similar, but for a `targets` pipeline.
#'   It accepts a Quarto or R Markdown source file as input,
#'   and it returns a list of target definition objects.
#'   Each target definition object comes from evaluating
#'   [targets::tar_target()] on the each of the assignment statements
#'   in each R code chunk in the file.
#'
#'   For example, consider the following code chunk:
#'
#'       ```{r, deployment = "main"}
#'       #| pattern: map(data)
#'       #| format: qs
#'       #| cue: tar_cue(mode = "always")
#'       data <- get_data(data)
#'       ```
#'
#'   `tar_tangle()` converts this chunk into:
#'
#'       tar_target(
#'         name = target_name,
#'         command = command_to_run(data),
#'         pattern = map(data),
#'         format = "qs",
#'         deployment = "main",
#'         cue = tar_cue(mode = "always")
#'       )
#'
#'   To put it all together, suppose our `_targets.R` script
#'   for the pipeline looks like this:
#'
#'       library(targets)
#'       tar_source()
#'       list(
#'         tar_tangle("example.qmd"),
#'         tar_target(model, fit_model(data))
#'       )
#'
#'   The pipeline above is equivalent to:
#'
#'       library(targets)
#'       tar_source()
#'       list(
#'         tar_target(
#'           name = target_name,
#'           command = command_to_run(data),
#'           pattern = map(data),
#'           format = "qs",
#'           deployment = "main",
#'           cue = tar_cue(mode = "always")
#'         ),
#'         tar_target(model, fit_model(data))
#'       )
#'
#'   See the "Examples" section in this help file for a
#'   runnable demonstration with multiple code chunks.
#'
#'   Each code chunk can have more than one top-level assignment statement
#'   (with `<-`, `=`, or `->`), and each assignment statement gets
#'   converted into its own target. Non-assignment statements such as
#'   `library(dplyr)` are ignored.
#'   It is always good practice to check your pipeline with
#'   [targets::tar_manifest()] and [targets::tar_visnetwork()]
#'   before running it with [targets::tar_make()].
#' @return A list of new target definition objects.
#'   See the "Target definition objects" section for background.
#' @param path File path to the literate programming source file.
#'   The file can be a Quarto or R Markdown document.
#' @references
#'   * Knuth, Donald E. (1984). "Literate Programming".
#'     The Computer Journal. 27 (2). British Computer Society: 97-11.
#'     <doi:10.1093/comjnl/27.2.97>.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({  # tar_dir() runs code from a temporary directory.
#' write.csv(airquality, "data.csv")
#' lines <- c(
#'   "---",
#'   "title: \"Example pipeline\"",
#'   "---",
#'   "",
#'   "```{r}",
#'   "#| format: file",
#'   "file <- \"data.csv\"",
#'   "```",
#'   "",
#'   "```{r}",
#'   "#| memory: persistent",
#'   "#| packages: [dplyr, readr]",
#'   "data <- read_csv(file, col_types = cols()) |>",
#'   "  filter(!is.na(Ozone))",
#'   "```",
#'   "",
#'   "```{r}",
#'   "#| format: qs",
#'   "#| cue: tar_cue(mode = \"never\")",
#'   "model <- lm(Ozone ~ Temp, data) |>",
#'   "  coefficients()",
#'   "```",
#'   "",
#'   "```{r}",
#'   "#| deployment: main",
#'   "#| packages: ggplot2",
#'   "plot <- ggplot(data) +",
#'   "  geom_point(aes(x = Temp, y = Ozone)) +",
#'   "  geom_abline(intercept = model[1], slope = model[2]) +",
#'   "  theme_gray(24)",
#'   "```"
#' )
#' writeLines(lines, "pipeline.qmd")
#' targets::tar_script(tarchetypes::tar_tangle("pipeline.qmd"))
#' targets::tar_make()
#' targets::tar_read(plot)
#' })
#' }
tar_tangle <- function(path) {
  rlang::check_installed("parsermd")
  unlist(lapply(parsermd::parse_qmd(path)@nodes, tar_tangle_node))
}

tar_tangle_node <- function(node) {
  is_r_chunk <- inherits(node, "rmd_chunk") &&
    identical(tolower(as.character(node@engine)), "r")
  if (!is_r_chunk) {
    return()
  }
  commands <- tar_tangle_commands(node@code)
  if (!length(commands)) {
    return()
  }
  options <- tar_tangle_options(node@options)
  lapply(
    names(commands),
    tar_tangle_target,
    commands = commands,
    options = options
  )
}

tar_tangle_target <- function(name, commands, options) {
  arguments <- options
  arguments$name <- as.symbol(name)
  arguments$command <- commands[[name]]
  do.call(what = targets::tar_target, args = arguments)
}

tar_tangle_commands <- function(code) {
  code <- as.list(parse(text = code))
  code <- code[vapply(code, tar_tangle_valid, FUN.VALUE = logical(1L))]
  commands <- lapply(code, function(x) x[[3L]])
  names(commands) <- lapply(code, function(x) x[[2L]])
  commands
}

tar_tangle_valid <- function(statement) {
  operator <- as.character(statement[[1L]])
  is.character(operator) &&
    length(operator) == 1L &&
    !anyNA(operator) &&
    operator %in% c("<-", "=")
}

tar_tangle_options <- function(options) {
  # Options set in the curly braces of a chunk
  # may leading and trailing quotes.
  options <- lapply(options, function(x) {
    gsub("^[\"']+|[\"']+$", "", x)
  })
  for (option in c("pattern", "cue", "resources")) {
    if (!is.null(options[[option]])) {
      options[[option]] <- parse(text = options[[option]])[[1L]]
    }
  }
  for (option in c("format", "repository")) {
    # Need to detect function calls because of tar_format()
    # and tar_repository_cas().
    if (any(grepl("(", options[[option]], fixed = TRUE))) {
      options[[option]] <- parse(text = options[[option]])[[1L]]
    }
  }
  options
}
