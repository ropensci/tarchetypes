#' targets: Archetypes for Targets
#' @docType package
#' @description A pipeline toolkit for R, the `targets` package brings together
#'   function-oriented programming and Make-like declarative pipelines for
#'   Statistics and data science. The `tarchetypes` package provides
#'   convenient helper functions to create specialized targets, making
#'   pipelines in targets easier and cleaner to write and understand.
#' @name tarchetypes-package
#' @importFrom digest digest
#' @importFrom fs path_ext_remove path_rel
#' @importFrom rlang as_function sym syms
#' @importFrom targets tar_cue tar_dir tar_envir tar_load tar_option_get
#'   tar_path tar_read tar_script tar_target tar_target_raw tar_test
#' @importFrom tidyselect all_of any_of contains ends_with everything
#'   last_col matches num_range one_of starts_with
#' @importFrom utils globalVariables
#' @importFrom vctrs vec_c vec_rbind
#' @importFrom withr with_options
NULL

utils::globalVariables(".x")
