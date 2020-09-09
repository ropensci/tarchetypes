call_brace <- function(args) {
  call_function("{", args)
}

call_list <- function(args) {
  call_function("list", args)
}

call_ns <- function(pkg, fun) {
  call_function("::", rlang::syms(c(pkg, fun)))
}

call_function <- function(name, args) {
  as.call(c(rlang::sym(name), args))
}

call_substitute <- function(expr, env) {
  call_function("substitute", args = list(expr = expr, env = env))
}

deparse_language <- function(x) {
  trn(!is.character(x) && !is.null(x), safe_deparse(x), x)
}

safe_deparse <- function(x, collapse = "\n", backtick = TRUE) {
  out <- direct_deparse(
    x,
    control = deparse_control_custom,
    backtick = backtick
  )
  trn(length(out) > 1L, paste(out, collapse = collapse), out)
}

deparse_control_custom <- .deparseOpts(c("keepNA", "keepInteger"))

direct_deparse <- function(...) {
  produce_direct_deparse()(...)
}

produce_direct_deparse <- function() {
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

tidy_eval <- function(expr, envir, tidy_eval) {
  if (tidy_eval) {
    expr <- as.call(c(quote(rlang::expr), expr))
    expr <- rlang::quo_squash(eval(expr, envir = envir))
  }
  expr
}
