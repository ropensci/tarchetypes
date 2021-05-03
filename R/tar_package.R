#' targets: Archetypes for Targets
#' @docType package
#' @description A pipeline toolkit for R, the `targets` package brings together
#'   function-oriented programming and Make-like declarative pipelines for
#'   Statistics and data science. The `tarchetypes` package provides
#'   convenient helper functions to create specialized targets, making
#'   pipelines in targets easier and cleaner to write and understand.
#' @name tarchetypes-package
#' @importFrom digest digest
#' @importFrom fs dir_create path_ext_remove path_rel
#' @importFrom rlang as_function check_installed enquo expr is_missing
#'   quo_squash
#' @importFrom targets tar_cue tar_dir tar_envir tar_exist_meta
#'   tar_group tar_load tar_meta tar_option_get tar_path tar_read tar_script
#'   tar_target tar_target_raw tar_test tar_timestamp_raw
#' @importFrom tidyselect all_of any_of contains ends_with everything
#'   last_col matches num_range one_of starts_with
#' @importFrom utils download.file globalVariables
#' @importFrom vctrs vec_c vec_rbind
#' @importFrom withr local_options with_options
NULL

utils::globalVariables(".x")
