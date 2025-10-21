#' @title Convert qmd/Rmd to a pipeline
#' @export
#' @family Domain-specific languages for pipeline construction
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
  commands <- lapply(code, tar_tangle_command)
  names(commands) <- lapply(code, tar_tangle_name)
  commands
}

tar_tangle_valid <- function(statement) {
  operator <- as.character(statement[[1L]])
  is.character(operator) &&
    length(operator) == 1L &&
    !anyNA(operator) &&
    operator %in% c("<-", "=", "->")
}

tar_tangle_name <- function(statement) {
  operator <- as.character(statement[[1L]])
  if (operator %in% c("<-", "=")) {
    statement[[2L]]
  } else {
    statement[[3L]]
  }
}

tar_tangle_command <- function(statement) {
  operator <- as.character(statement[[1L]])
  if (operator %in% c("<-", "=")) {
    statement[[3L]]
  } else {
    statement[[2L]]
  }
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
