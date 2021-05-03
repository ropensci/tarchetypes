targets::tar_test("walk_ast() with walk_call_knitr()", {
  expr <- quote({
    if (a > 1) {
      tar_load(target_name)
    }
    process_stuff(target_name)
  })
  expect_equal(walk_ast(expr, walk_call_knitr), "target_name")
})

targets::tar_test("walk_ast() with custom walker", {
  walk_custom <- function(expr, counter) {
    name <- deparse(expr[[1]])
    if (identical(name, "detect_this")) {
      counter_set_names(counter, as.character(expr[[2]]))
    }
  }
  expr <- quote({
    for (i in seq_len(10)) {
      for (j in seq_len(20)) {
        if (i > 1) {
          detect_this("prize")
        } else {
          ignore_this("penalty")
        }
      }
    }
  })
  expect_equal(walk_ast(expr, walk_custom), "prize")
})
