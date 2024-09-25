#' @title A `drake`-plan-like pipeline DSL
#' @export
#' @family Pipeline factories
#' @description Simplify target specification in pipelines.
#' @details Allows targets with just targets and commands
#'   to be written in the pipeline as `target = command` instead of
#'   `tar_target(target, command)`. Also supports ordinary
#'   target objects if they are unnamed.
#'   `tar_plan(x = 1, y = 2, tar_target(z, 3), tar_render(r, "r.Rmd"))`
#'   is equivalent to
#'   `list(tar_target(x, 1), tar_target(y, 2), tar_target(z, 3), tar_render(r, "r.Rmd"))`. # nolint
#' @return A list of `tar_target()` objects.
#'   See the "Target objects" section for background.
#' @inheritSection tar_map Target objects
#' @param ... Named and unnamed targets. All named targets must follow
#'   the `drake`-plan-like `target = command` syntax, and all unnamed
#'   arguments must be explicit calls to create target objects,
#'   e.g. `tar_target()`, target factories like [tar_render()], or similar.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#'   library(tarchetypes)
#'   tar_plan(
#'     tarchetypes::tar_fst_tbl(data, data.frame(x = seq_len(26))),
#'     means = colMeans(data) # No need for tar_target() for simple cases.
#'   )
#' })
#' targets::tar_make()
#' })
#' }
tar_plan <- function(...) {
  commands <- tar_plan_parse(match.call(expand.dots = FALSE)$...)
  lapply(commands, eval, envir = targets::tar_option_get("envir"))
}

tar_plan_parse <- function(commands) {
  commands <- commands[!map_lgl(commands, rlang::is_missing)]
  names <- names(commands) %|||% rep("", length(commands))
  is_named <- !is.na(names) & nzchar(names)
  commands[is_named] <- tar_plan_parse_named(commands[is_named])
  commands
}

tar_plan_parse_named <- function(commands) {
  lapply(names(commands), tar_plan_parse_command, commands = commands)
}

tar_plan_parse_command <- function(name, commands) {
  env <- list(name = as.symbol(name), command = commands[[name]])
  substitute(targets::tar_target(name, command), env = env)
}
