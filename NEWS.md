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
