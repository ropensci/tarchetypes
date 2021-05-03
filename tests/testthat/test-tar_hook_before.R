targets::tar_test("tar_hook_before() inserts code", {
  targets::tar_script({
    targets <- list(
      list(
        targets::tar_target(x1, task1()),
        targets::tar_target(x2, task2(x1))
      ),
      targets::tar_target(x3, task3(x2)),
      targets::tar_target(y1, task4(x3))
    )
    tarchetypes::tar_hook_before(
      targets = targets,
      hook = print("Running hook."),
      names = NULL
    )
  })
  out <- targets::tar_manifest(callr_function = NULL)
})
