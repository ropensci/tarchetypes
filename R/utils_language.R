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

substitute_expr <- function(expr, env) {
  as.expression(lapply(expr, substitute_lang, env = env))
}

substitute_lang <- function(lang, env) {
  eval(call_substitute(lang, env), envir = baseenv())
}

tar_raw_command <- function(name, command) {
  targets::tar_assert_nonmissing(
    command,
    paste("target", name, "has no command.")
  )
  targets::tar_assert_lang(command, "command must be a language object.")
  targets::tar_assert_scalar(
    as.expression(command),
    paste("the command of target", name, "must have length 1.")
  )
  command <- as.expression(command)[[1]]
}
