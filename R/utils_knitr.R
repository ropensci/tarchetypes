knitr_deps <- function(path) {
  expr <- knitr_expr(path)
  knitr_expr_warn_raw(expr)
  counter <- counter_init()
  walk_expr(expr, counter)
  sort(counter_get_names(counter))
}

knitr_expr <- function(path) {
  tryCatch(
    parse(text = knitr_lines(path)),
    error = function(e) {
      throw_validate(
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
    warn_validate(
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

walk_expr <- function(expr, counter) {
  if (!length(expr)) {
    return()
  } else if (is.call(expr)) {
    walk_call(expr, counter)
  } else if (typeof(expr) == "closure") {
    walk_expr(formals(expr), counter = counter)
    walk_expr(body(expr), counter = counter)
  } else if (is.pairlist(expr) || is.recursive(expr)) {
    lapply(expr, walk_expr, counter = counter)
  }
}

walk_call <- function(expr, counter) {
  name <- deparse_safe(expr[[1]], backtick = FALSE)
  if (name %in% paste0(c("", "targets::", "targets:::"), "tar_load")) {
    walk_load(expr, counter)
  }
  if (name %in% paste0(c("", "targets::", "targets:::"), "tar_read")) {
    walk_read(expr, counter)
  }
  lapply(expr, walk_expr, counter = counter)
}

walk_load <- function(expr, counter) {
  expr <- match.call(targets::tar_load, as.call(expr))
  if (is.null(expr$names)) {
    warn_validate(
      "Found empty tar_load() call in a knitr / R Markdown ",
      "code chunk. Dependencies cannot be detected statically, ",
      "so they will be ignored."
    )
  }
  walk_expr_names(expr$names, counter)
}

walk_read <- function(expr, counter) {
  expr <- match.call(targets::tar_read, as.call(expr))
  if (is.null(expr$name)) {
    warn_validate(
      "Found empty tar_read() call in a knitr / R Markdown ",
      "code chunk. Dependencies cannot be detected statically, ",
      "so they will be ignored."
    )
  }
  walk_expr_names(expr$name, counter)
}

walk_expr_names <- function(expr, counter) {
  if (!length(expr)) {
    return()
  } else if (is.name(expr)) {
    counter_set_names(counter, as.character(expr))
  } else if (is.character(expr)) {
    counter_set_names(counter, expr)
  } else if (is.pairlist(expr) || is.recursive(expr) || is.call(expr)) {
    walk_expr_names_recursive(expr, counter)
  }
}

walk_expr_names_recursive <- function(expr, counter) {
  if (is.call(expr)) {
    name <- deparse_safe(expr[[1]], backtick = FALSE)
    if (name %in% tidyselect_names()) {
      warn_validate(
        "found ", name, "() from tidyselect in a call to tar_load() or ",
        "tar_read() in a knitr / R Markdown code chunk. These dependencies ",
        "cannot be detected statically, so they will be ignored."
      )
      return()
    }
    expr <- expr[-1]
  }
  lapply(expr, walk_expr_names, counter = counter)
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
