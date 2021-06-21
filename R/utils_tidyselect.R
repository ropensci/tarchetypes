eval_tidyselect <- function(names_quosure, choices) {
  if (is.null(rlang::quo_squash(names_quosure)) || !length(choices)) {
    return(NULL)
  }
  names(choices) <- choices
  out <- tidyselect::eval_select(names_quosure, data = choices, strict = FALSE)
  out <- names(out)
  targets::tar_assert_chr(
    out %|||% character(0),
    paste(
      "the names arg of tar_make() and friends supports tidyselect syntax",
      "but must resolve to a character vector in the end."
    )
  )
  out
}
