knitr_deps <- function(path) {
  expr <- knitr_expr(path)
  knitr_expr_warn_raw(expr)
  walk_ast(expr, walk_call_knitr)
}

knitr_expr <- function(path) {
  tryCatch(
    parse(text = knitr_lines(path)),
    error = function(e) {
      targets::tar_throw_validate(
        "Could not parse knitr report ",
        path,
        " to detect dependencies: ",
        conditionMessage(e)
      )
    }
  )
}

knitr_expr_warn_raw <- function(expr) {
  vars <- all.vars(expr, functions = TRUE)
  if (any(c("tar_load_raw", "tar_read_raw") %in% vars)) {
    targets::tar_warn_validate(
      "targets loaded with tar_load_raw() or tar_read_raw() ",
      "will not be detected as dependencies in literate programming reports. ",
      "To properly register target dependencies of reports, use tar_load() ",
      "or tar_read() instead."
    )
  }
}

knitr_lines <- function(path) {
  handle <- basename(tempfile())
  connection <- textConnection(handle, open = "w", local = TRUE)
  on.exit(close(connection))
  withr::with_options(
    new = list(knitr.purl.inline = TRUE),
    code = knitr::knit(path, output = connection, tangle = TRUE, quiet = TRUE)
  )
  textConnectionValue(connection)
}

#' @title Code analysis for knitr reports.
#' @export
#' @description Walk an abstract syntax tree and capture knitr dependencies.
#' @details For internal use only. Not a user-side function.
#'   Powers  automatic detection of `tar_load()`/`tar_read()`
#'   dependencies in [tar_render()].
#'   Packages `codetools` and `CodeDepends` have different (more sophisticated
#'   and elaborate) implementations of the concepts documented at
#'   <https://adv-r.hadley.nz/expressions.html#ast-funs>.
#' @keywords internal
#' @return A character vector of target names found during static code analysis.
#' @param expr A language object or function to scan.
#' @param counter An internal counter object that keeps track of detected
#'   target names so far.
#' @examples
#' # How tar_render() really works:
#' expr <- quote({
#'   if (a > 1) {
#'     tar_load(target_name)
#'   }
#'   process_stuff(target_name)
#' })
#' walk_ast(expr, walk_call_knitr)
walk_call_knitr <- function(expr, counter) {
  name <- targets::tar_deparse_safe(expr[[1]], backtick = FALSE)
  if (any(name %in% paste0(c("", "targets::", "targets:::"), "tar_load"))) {
    walk_load(expr, counter)
  }
  if (any(name %in% paste0(c("", "targets::", "targets:::"), "tar_read"))) {
    walk_read(expr, counter)
  }
}

walk_load <- function(expr, counter) {
  expr <- match.call(targets::tar_load, as.call(expr))
  if (is.null(expr$names)) {
    targets::tar_warn_validate(
      "Found empty tar_load() call in a knitr / R Markdown ",
      "code chunk. Dependencies cannot be detected statically, ",
      "so they will be ignored."
    )
  }
  walk_target_name(expr$names, counter)
}

walk_read <- function(expr, counter) {
  expr <- match.call(targets::tar_read, as.call(expr))
  if (is.null(expr$name)) {
    targets::tar_warn_validate(
      "Found empty tar_read() call in a knitr / R Markdown ",
      "code chunk. Dependencies cannot be detected statically, ",
      "so they will be ignored."
    )
  }
  walk_target_name(expr$name, counter)
}

walk_target_name <- function(expr, counter) {
  if (!length(expr)) {
    return()
  } else if (is.name(expr)) {
    counter_set_names(counter, as.character(expr))
  } else if (is.character(expr)) {
    counter_set_names(counter, expr)
  } else if (is.pairlist(expr) || is.recursive(expr) || is.call(expr)) {
    walk_tidyselect(expr, counter)
  }
}

walk_tidyselect <- function(expr, counter) {
  if (is.call(expr)) {
    name <- targets::tar_deparse_safe(expr[[1]], backtick = FALSE)
    if (name %in% tidyselect_names()) {
      targets::tar_warn_validate(
        "found ", name, "() from tidyselect in a call to tar_load() or ",
        "tar_read() in a knitr / R Markdown code chunk. These dependencies ",
        "cannot be detected statically, so they will be ignored."
      )
      return()
    }
    expr <- expr[-1]
  }
  lapply(expr, walk_target_name, counter = counter)
}

tidyselect_names <- function() {
  tidyselect <- c(
    "all_of",
    "any_of",
    "contains",
    "ends_with",
    "everything",
    "last_col",
    "matches",
    "num_range",
    "one_of",
    "starts_with"
  )
  out <- c(tidyselect, paste0("tidyselect::", tidyselect))
  c(out, paste0("tidyselect:::", tidyselect))
}
