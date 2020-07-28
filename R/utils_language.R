call_brace <- function(args) {
  as.call(c(rlang::sym("{"), args))
}

call_c <- function(args) {
  as.call(c(rlang::sym("c"), args))
}

call_list <- function(args) {
  as.call(c(rlang::sym("list"), args))
}

call_path_rel <- function(args) {
  expr_path_rel_ns <- as.call(c(sym_ns, rlang::syms(c("fs", "path_rel"))))
  as.call(c(expr_path_rel_ns, args))
}

call_render <- function(args) {
  expr_render_ns <- as.call(c(sym_ns, rlang::syms(c("rmarkdown", "render"))))
  expr_render <- as.call(c(rlang::sym("render"), args))
  expr_render[[1]] <- expr_render_ns
  expr_render <- match.call(rmarkdown::render, expr_render)
}

sym_ns <- rlang::sym("::")

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
