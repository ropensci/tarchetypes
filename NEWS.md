# tarchetypes 0.13.0

* Support `pattern` in `tar_skip()` (#212, @CorradoLanera).
* Allow `tar_quarto_rep()` to write reports to subdirectories with the help of a project-level `_quarto.yml` (#211, @lgaborini).
* `tar_map2*()` functions now aggregate dynamic branches in parallel over static branches (#213).
* `tar_map2*()` functions gain an `unlist` argument.
* Call `parallel::clusterExport()` in `make_psock_cluster()` to make sure globals carry over to parallel socket clusters.

# tarchetypes 0.12.0

* Fix `tar_combine()` help file examples (#206, @weberse2).
* Account for project-level `output_dir` in non-project `tar_quarto()` calls (#207, @brndngrhm).
* Use name when passing the `quiet` argument to `quarto_inspect()` (#208, @yusuke-sasaki-jprep).
* Explicitly pass the `profile` argument to `quarto_inspect()` and `quarto_render()`. Requires R package `quarto >= 1.4`, which is already in the `DESCIRPTION`.
* `tar_map_rep()` now aggregates dynamic branches in parallel over static branches (#204).
* Add `tar_rep_index()` (#203).

# tarchetypes 0.11.0

* Add an `output_file` argument to `tar_quarto()` and `tar_quarto_raw()` for single documents (#198, @mutlusun).
* Detect child quarto documents (#199, @mutlusun).
* Improve reporting of static branch names from `tar_map()` and `tar_map_rep()` (#201, @kkmann).
* Ensure compatibility with `targets` after https://github.com/ropensci/targets/issues/1368.
* Improve detection of Quarto files in `tar_quarto_inspect()` (#200, @multusun).

# tarchetypes 0.10.0

* Add a `delimiter` argument to `tar_map()` etc. for customizing separators in target names (#177, @psychelzh).
* Add "raw" hook functions (#185, @multimeric).
* Add `tar_assign()` (#186, https://github.com/ropensci/targets/issues/1309, @hadley).
* Merge help files of "_raw" functions (#191, @hadley).
* Supersede `tar_format_feather()` in favor of `tar_arrow_feather()` (#190).
* Supersede the `tar_aws_*()` target factories. They are obsolete because of the `repository` argument in `tar_target()`.

# tarchetypes 0.9.0

## Invalidating changes

* To align with https://github.com/ropensci/targets/issues/1244 and https://github.com/ropensci/targets/pull/1262, switch the hashing functions from `digest::digest()` to `secretbase::siphash13()`.

# tarchetypes 0.8.0

* Expose the new `description` argument of `tar_target()` in `targets` 1.5.1.9001.
* `tar_map()` and other static branching target factories now append values to the target descriptions. Use the `descriptions` argument of those functions to customize.
* Ensure consistent `repository` settings in `tar_change()` and `tar_map_rep()`.
* `tar_knit()`, `tar_render()`, `tar_quarto()`, and their "rep" and "raw" versions all gain a `working_directory` argument to change the working directory the report knits from. Users who set `working_directory` need to supply the `store` argument of `tar_load()` and `tar_read()` relative to the working directory so the report knows where to find the data (#169).
* `tar_knit()`, `tar_render()`, `tar_quarto()`, and their "raw" versions all gain an `output_file` argument to more conveniently set the file path to the rendered output file.
* `tar_quarto()` and its "rep" and "raw" versions all gain a new `quarto_args` argument for advanced Quarto arguments (#166, @petrbouchal).

# tarchetypes 0.7.12

* Adjust tests because group iteration is now explicitly prohibited for dynamic targets.

# tarchetypes 0.7.11

* Use `tar_seed_create()` and `tar_seed_set()` from `targets`.
* Document limitations of literate programming target factories like `tar_render()` (#158).
* Make `tar_rep2()` compatible with `tar_map()` (#165).

# tarchetypes 0.7.10

* Prepare to use `tar_seed_create()` and `tar_seed_set()` (https://github.com/ropensci/targets/issues/1139). Future versions of `tarchetypes` should use these package functions, but this version cannot because of the compatibility constraints of the release cycle.
* Migrate tests to `targets` >= 1.3.2.9004 progress statuses ("completed" instead of "built", "dispatched" instead of "started").

# tarchetypes 0.7.9

* Deprecate the `packages` and `library` arguments of `tar_quarto()` and `tar_quarto_raw()` (#155, @svraka).
* Switch to from `furrr` to `parallel` for `rep_workers` in `tar_rep()` etc. (#251, @solmos).

# tarchetypes 0.7.8

* Relax overly strict assertion on R Markdown / Quarto parameter lists (@rmgpanw, #152).
* Adjust a test to comply with upcoming `targets` 1.3.0.

# tarchetypes 0.7.7

* Allow `format = "file_fast"` in target factories.

# tarchetypes 0.7.6

* Support Quarto profiles through the `QUARTO_PROFILE` environment variable (#139, @andrewheiss).
* Take the basename of the source file for #129 so the output files land correctly when the source file is in a subdirectory (#129, `targets` issue 1047, @joelnitta).
* Use `targets::tar_runtime_object()$store` instead of `targets::tar_runtime_object()$get_store()` to ensure forward compatibility with `targets`.
* Use interactive test for `tar_download()` to avoid unpredictable network issues outside our control.

# tarchetypes 0.7.5

* Implement a new `set_deps` argument in the hook functions to force modified targets to keep the dependencies they had before applying the hook (#131, @edalfon).
* Forward all settings to `tar_copy_target()` (#131, @edalfon).
* Initialize the directory of output files in `tar_quarto_rep()` and `tar_render_rep()` (#129, @benzipperer).
* Work around https://github.com/quarto-dev/quarto-cli/pull/2456 by writing temporary local files in `tar_quarto_rep()` (#129, @benzipperer).

# tarchetypes 0.7.4

* Implement `rep_workers` to control inner parallelism in batched replication functions (#117).
* Ensure the function passed to `furrr` functions has environment `tar_option_get("envir")`.
* Allow subdirectories of rendered reports with `tar_quarto_rep()` (#129, @mglev1n).

# tarchetypes 0.7.3

* Support nested futures for parallelism among reps within batches (#117, @kkmann).
* Add Quarto troubleshooting section to help files.

# tarchetypes 0.7.2

* Migrate away from deprecated `targets::tar_path()`.
* Implement and return resilient seeds in batched replication (#111, #113).

# tarchetypes 0.7.1

* Document <https://github.com/ropensci/tarchetypes/discussions/105> (@MarekGierlinski).
* Adapt tests to changes in `tar_manfiest()` default output.

# tarchetypes 0.7.0

* Add new functions `tar_quarto()` and `tar_quarto_rep()` (and "raw" versions) for Quarto documents and projects in pipelines (#89).
* Add new function `tar_quarto_files()` to inspect Quarto projects and documents for important files: source files to inspect for target dependencies, output documents, and Quarto-specific inputs like `_quarto.yml`. Uses `quarto::quarto_inspect()` and powers the automatic file detection in `tar_quarto()` etc. (#89).
* Add runtime guardrails to the `params` argument of `tar_render_rep()` (must be a data frame with unique rows (or unique elements of `output_file`)).
* Temporarily change `root.dir` when scanning for dependencies so `knitr` child documents work (#93, @mutlusun).
* Use `format = "rds"` for `target_batch` in `tar_map_rep_raw()` so the global format option does not mess up the pipeline.
* Handle non-atomic length-one list columns in `tar_append_static_values()`.
* Allow `tar_render()` to work with just one row of parameters (#96, #97, @ugoebel73).
* Remove dependencies and collect garbage before running reports.
* Make sure all the target factories have `memory` and `garbage_collection` arguments.

# tarchetypes 0.6.0

* Implement `tar_file_read()` (#84, @petrbouchal).
* Suppress warnings for deprecated AWS formats.
* Select the correct targets in `tar_select_targets()` (#92, @arcruz0).
* Support the `repository` argument for `targets` >= 0.11.0.

# tarchetypes 0.4.1

* Select list elements from `command1` using `[[` and not `[` in `tar_map2()` functions.

# tarchetypes 0.4.0

* Implement `tar_map_rep()` and `tar_map_rep_raw()` for dynamic batched replication within static branching for data frames (#78).
* Implement `tar_map2_count()`, `tar_map2_count_raw()`, `tar_map2_size()`, and `tar_map2_size_raw()` for batched dynamic-within-static branching for data frames (#78).
* Deprecate `tar_rep_map()` in favor of `tar_rep2()` to avoid name confusion. Likewise with `tar_rep_map_raw()` to `tar_rep2_raw()` (#78).

# tarchetypes 0.3.2

* Allow empty / `NULL` target list in `tar_map()` (@kkami1115).
* Do not claim to support `"aws_file"` format in `tar_files()` or related target factories.

# tarchetypes 0.3.1

* Relax assertion on language objects.
* Explain `targets` timestamps correctly in the help files of `tar_age()` and `tar_cue_age()`.

# tarchetypes 0.3.0

## Invalidating changes

* When `names = NULL` in `tar_map()`, use hashes instead of numeric indexes for generated target names (#67). That way, target names are no longer sensitive to the order of `values`, and so targets will incorrectly invalidate less often. *Unfortunately, this is an invalidating change: some targets will automatically rerun after you install this version of `tarchetypes`.* I apologize for the inconvenience this causes. However, we do need this patch in order to solve #67, and targets will incorrectly invalidate less frequently in the future.

## Enhancements

* Migrate to utilities for error handling and metaprogramming exported from `targets` (#59).

# tarchetypes 0.2.1

## Bug fixes

* Make the `*_raw()` target factories process `command` the same way whether it is an expression or ordinary language object.
* Ensure compatibility with `targets` 0.5.0.9000, which logs skipped targets.

## New features

* Add `tar_rep_map()` and `tar_rep_map_raw()` to perform batched computation downstream of `tar_rep()` (#50).
* Add `tar_select_names()` and `tar_select_targets()` to make certain metaprogramming tasks easier.
* In `tar_map()`, attempt to convert the elements of `values` into lists of language objects.

# tarchetypes 0.2.0

* Allow trailing commas in `tar_plan()` (#40, @kendonB).
* Implement `tar_age()` based on `tar_cue_age()` (#39, @petrbouchal).
* Implement new cue factories `tar_cue_age()`, `tar_cue_age_raw()`, `tar_cue_force()`, and `tar_cue_skip()` (#39).
* Implement `tar_download()` (#38, @noamross, @petrbouchal)
* Set intermediate temporary directory to remove race condition in `tar_render_rep()` (#36, @gorgitko). 
* Prefix internal condition classes with "tar_".
* Add new format helpers such as `tar_aws_rds()` and `tar_parquet()`.
* Support hooks `tar_hook_before()`, `tar_hook_inner()`, and `tar_hook_outer()` (#44).
* Deep-copy the cue in `tar_map()`.

# tarchetypes 0.1.1

* Unset `crayon.enabled` for literate programming.
* Switch meaning of `%||%` and `%|||%` to conform to historical precedent.

# tarchetypes 0.1.0

* Add new functions for easier grouping of data frames for dynamic branching: `tar_group_by()`, `tar_group_select()`, `tar_group_size()`, `tar_group_count()` (#32, @liutiming).
* In `tar_render()` and related functions, track the `*_files/` output directory if it exists (#30).
* Implement an external `walk_ast()` function to make it easier for other developers to extend the static code analysis of `tarchetypes` (@MilesMcBain).

# tarchetypes 0.0.4

* Skip literate programming tests if pandoc is missing or has an insufficient version.
* Use explicit temp files in examples even when running inside `targets::tar_dir()`. (`targets::tar_dir()` and `targets::tar_test()` already run code in a temporary directory.)
* Add comments in the examples to emphasize that `targets::tar_dir()` runs code in a temporary directory, which means all ostensibly files created in the enclosed expression will actually be written to temporary storage and not the user's file space.

# tarchetypes 0.0.2

* Make sure every function with a help file in `man/` has Rd-tags `\value` and `\arguments`.
* For every function with a help file in `man/`, describe the return value in the `\value` Rd tag. For each function that returns a target object or list of target objects, the `\value` tag now links to <https://books.ropensci.org/targets/>, the user manual where the purpose of target objects is explained, and <https://books.ropensci.org/targets-design/>, the design specification which documents the structure and composition of target objects.
* Ensure that examples, vignettes, and test do not write to the home file space of the user.
* Ensure that no function defined in the `tarchetypes` package writes by default to the home file space of the user. The paths of all output files are controlled by non-`tarchetypes` functions that invoke `tarchetypes`.

# tarchetypes 0.0.1

* `tar_plan()` now returns a list of target objects rather than a pipeline object. Related: <https://github.com/ropensci/targets/issues/253>.

# tarchetypes 0.0.0.9000

* First version.
* Implement `tar_knitr_deps()` and `tar_knitr_deps_expr()` to accommodate custom multi-file literate programming projects like R Markdown sites and `bookdown` projects (#23, @tjmahr).
