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
