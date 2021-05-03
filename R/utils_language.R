as_symbols <- function(x) {
  lapply(x, as.symbol)
}

call_brace <- function(args) {
  call_function("{", args)
}

call_list <- function(args) {
  call_function("list", args)
}

call_ns <- function(pkg, fun) {
  call_function("::", as_symbols(c(pkg, fun)))
}

call_function <- function(name, args) {
  as.call(c(as.symbol(name), args))
}

call_substitute <- function(expr, env) {
  call_function("substitute", args = list(expr = expr, env = env))
}

deparse_language <- function(x) {
  if_any(!is.character(x) && !is.null(x), deparse_safe(x), x)
}

deparse_safe <- function(x, collapse = "\n", backtick = TRUE) {
  out <- deparse_direct(
    x,
    control = deparse_control_custom,
    backtick = backtick
  )
  if (length(out) > 1L) {
    out <- paste(out, collapse = collapse)
  }
  out
}

deparse_control_custom <- .deparseOpts(c("keepNA", "keepInteger"))

deparse_direct <- function(...) {
  produce_deparse_direct()(...)
}

produce_deparse_direct <- function() {
  .deparseOpts <- identity
  environment(deparse) <- environment()
  deparse
}

substitute_expr <- function(expr, env) {
  as.expression(lapply(expr, substitute_lang, env = env))
}

substitute_lang <- function(lang, env) {
  eval(call_substitute(lang, env), envir = baseenv())
}

tar_tidy_eval <- function(expr, envir, tidy_eval) {
  if (tidy_eval) {
    expr <- as.call(c(quote(rlang::expr), expr))
    expr <- rlang::quo_squash(eval(expr, envir = envir))
  }
  expr
}
