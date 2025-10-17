targets::tar_test("tar_translate() manifest", {
  targets::tar_script(
    tar_translate(
      {
        data <- data.frame(a = 1L, b = 2L)
        sum <- {
          output <- data$a + data$b
          output
        }
        product = sum * 2L
        3 * sum -> product2
      },
      format = "file"
    )
  )
  manifest <- targets::tar_manifest(
    fields = tidyselect::any_of(c("name", "command", "format")),
    callr_function = NULL
  )
  expect_equal(
    sort(manifest$name),
    sort(c("data", "sum", "product", "product2"))
  )
  expect_equal(unique(manifest$format), "file")
})

targets::tar_test("tar_translate() graph", {
  targets::tar_script(
    tar_translate({
      data <- data.frame(a = 1L, b = 2L)
      sum <- {
        output <- data$a + data$b
        output
      }
      product = sum * 2L
      3 * sum -> product2
    })
  )
  network <- targets::tar_network(
    targets_only = TRUE,
    callr_function = NULL
  )
  expect_equal(
    sort(network$vertices$name),
    sort(c("data", "sum", "product", "product2"))
  )
  expect_equal(nrow(network$edges), 3L)
  expect_equal(
    network$edges$to[network$edges$from == "data"],
    "sum"
  )
  expect_equal(
    sort(network$edges$to[network$edges$from == "sum"]),
    sort(c("product", "product2"))
  )
})

targets::tar_test("tar_translate() results", {
  skip_on_cran()
  targets::tar_script(
    tar_translate({
      data <- data.frame(a = 1L, b = 2L)
      sum <- {
        output <- data$a + data$b
        output
      }
      product = sum * 2L
      3 * sum -> product2
    })
  )
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(data), data.frame(a = 1L, b = 2L))
  expect_equal(targets::tar_read(sum), 3L)
  expect_equal(targets::tar_read(product), 6L)
  expect_equal(targets::tar_read(product2), 9L)
})
