# tarchetypes 0.0.2

* Make sure every function with a help file in `man/` has Rd-tags `\value` and `\arguments`.
* For every function with a help file in `man/`, describe the return value in the `\value` Rd tag. For each function that returns a target object or list of target objects, the `\value` tag now links to <https://books.ropensci.org/targets/>, the user manual where the purpose of target objects is explained, and <https://books.ropensci.org/targets-design/>, the design specification which documents the structure and composition of target objects.
* Ensure that examples, vignettes, and test do not write to the home file space of the user.

# tarchetypes 0.0.1

* `tar_plan()` now returns a list of target objects rather than a pipeline object. Related: <https://github.com/ropensci/targets/issues/253>.

# tarchetypes 0.0.0.9000

* First version.
* Implement `tar_knitr_deps()` and `tar_knitr_deps_expr()` to accommodate custom multi-file literate programming projects like R Markdown sites and `bookdown` projects (#23, @tjmahr).
