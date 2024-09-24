#' @rdname tar_eval
#' @export
tar_eval_raw <- function(expr, values, envir = parent.frame()) {
  targets::tar_assert_lang(expr)
  assert_values_list(values)
  force(envir)
  exprs <- tar_sub_raw(expr = expr, values = values)
  lapply(X = exprs, FUN = eval, envir = envir)
}
