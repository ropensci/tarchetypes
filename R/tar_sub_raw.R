#' @rdname tar_sub
#' @export
tar_sub_raw <- function(expr, values) {
  targets::tar_assert_lang(expr)
  assert_values_list(values)
  lapply(transpose(values), tar_sub_expr, expr = expr)
}

tar_sub_expr <- function(expr, values) {
  as.expression(tar_sub_lang(expr, values))
}

tar_sub_lang <- function(expr, values) {
  out <- substitute(
    substitute(expr = expr, env = env),
    env = list(expr = expr, env = values)
  )
  eval(out)
}
