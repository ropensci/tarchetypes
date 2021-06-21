assert_hook_expr <- function(target) {
  name <- target$settings$name
  targets::tar_assert_expr(
    target$command$expr,
    paste("command of target", name, "is not an expression.")
  )
  targets::tar_assert_scalar(
    target$command$expr,
    paste0(
      "hooks are only supported if the command of the ",
      "target is an expression of length 1. Target ",
      name,
      " has a command of length ",
      length(target$command$expr),
      "."
    )
  )
}

assert_hook_placeholder <- function(x, msg = NULL) {
  if (!(".x" %in% all.names(x))) {
    default_msg <- paste(
      "inner and outer hooks must contain the symbol .x",
      "so tarchetypes knows where to substitute the original",
      "commands/variables."
    )
    targets::tar_throw_validate(msg %|||% default_msg)
  }
}

assert_values_list <- function(values) {
  targets::tar_assert_list(values)
  targets::tar_assert_nonempty(names(values))
  targets::tar_assert_unique(names(values))
  targets::tar_assert_chr(names(values))
  targets::tar_assert_nzchar(names(values))
  targets::tar_assert_names(names(values))
  targets::tar_assert_nonempty(values)
  targets::tar_assert_equal_lengths(values)
}
