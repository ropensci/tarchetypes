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
#' @importFrom rlang as_function check_installed enquo expr
#'   inform is_missing quo_squash
#' @importFrom targets tar_assert_chr tar_assert_dbl tar_assert_df
#'   tar_assert_envir tar_assert_equal_lengths tar_assert_expr tar_assert_flag
#'   tar_assert_function tar_assert_ge tar_assert_identical
#'   tar_assert_in tar_assert_not_dirs tar_assert_not_dir
#'   tar_assert_not_in tar_assert_inherits tar_assert_int
#'   tar_assert_internet tar_assert_lang tar_assert_le
#'   tar_assert_list tar_assert_lgl tar_assert_name
#'   tar_assert_names tar_assert_nonempty tar_assert_not_expr
#'   tar_assert_nzchar tar_assert_package tar_assert_path
#'   tar_assert_match tar_assert_nonmissing tar_assert_positive
#'   tar_assert_scalar tar_assert_target tar_assert_target_list
#'   tar_assert_true tar_assert_unique tar_assert_unique_targets
#'   tar_cue tar_deparse_language tar_deparse_safe
#'   tar_dir tar_envir tar_exist_meta
#'   tar_group tar_load tar_meta tar_option_get tar_path tar_read tar_script
#'   tar_target tar_target_raw tar_test tar_tidy_eval
#'   tar_throw_validate tar_warn_validate
#' @importFrom tibble as_tibble
#' @importFrom tidyselect all_of any_of contains ends_with everything
#'   last_col matches num_range one_of starts_with
#' @importFrom utils download.file globalVariables
#' @importFrom vctrs vec_c vec_rbind
#' @importFrom withr local_options with_options
NULL

utils::globalVariables(".x")
