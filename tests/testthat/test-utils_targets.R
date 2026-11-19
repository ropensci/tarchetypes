targets::tar_test("local_tar_ask() sets TAR_ASK if already false", {
  withr::local_envvar(c(TAR_ASK = "false"))
  f <- function() {
    local_tar_ask()
    Sys.getenv("TAR_ASK")
  }
  expect_equal(f(), "false")
  expect_equal(Sys.getenv("TAR_ASK"), "false")
})

targets::tar_test("local_tar_ask() leaves TAR_ASK unset outside pipelines", {
  withr::local_envvar(c(TAR_ASK = NA))
  f <- function() {
    local_tar_ask()
    Sys.getenv("TAR_ASK", unset = "unset")
  }
  expect_equal(f(), "unset")
  expect_equal(Sys.getenv("TAR_ASK", unset = "unset"), "unset")
})

targets::tar_test("local_tar_ask() leaves TAR_ASK = true outside pipelines", {
  withr::local_envvar(c(TAR_ASK = "true"))
  f <- function() {
    local_tar_ask()
    Sys.getenv("TAR_ASK")
  }
  expect_equal(f(), "true")
  expect_equal(Sys.getenv("TAR_ASK"), "true")
})

targets::tar_test("local_tar_ask() sets TAR_ASK = false in a pipeline", {
  skip_on_cran()
  withr::local_envvar(c(TAR_ASK = "true"))
  targets::tar_script(
    list(
      targets::tar_target(
        x,
        {
          f <- function() {
            tarchetypes:::local_tar_ask()
            Sys.getenv("TAR_ASK")
          }
          c(inside = f(), after = Sys.getenv("TAR_ASK"))
        }
      )
    )
  )
  targets::tar_make(callr_function = NULL, reporter = "silent")
  out <- targets::tar_read(x)
  expect_equal(unname(out["inside"]), "false")
  expect_equal(unname(out["after"]), "true")
  expect_equal(Sys.getenv("TAR_ASK"), "true")
})

targets::tar_test("tar_render() report sees TAR_ASK = false in a pipeline", {
  skip_on_cran()
  skip_rmarkdown()
  withr::local_envvar(c(TAR_ASK = "true"))
  lines <- c(
    "---",
    "title: report",
    "output_format: html_document",
    "---",
    "",
    "```{r}",
    "writeLines(Sys.getenv(\"TAR_ASK\"), \"tar_ask.txt\")",
    "```"
  )
  writeLines(lines, "report.Rmd")
  targets::tar_script({
    library(tarchetypes)
    list(tar_render(report, "report.Rmd"))
  })
  targets::tar_make(callr_function = NULL, reporter = "silent")
  expect_equal(readLines("tar_ask.txt"), "false")
  expect_equal(Sys.getenv("TAR_ASK"), "true")
})

targets::tar_test("tar_quarto() report sees TAR_ASK = false in a pipeline", {
  skip_on_cran()
  skip_quarto()
  withr::local_envvar(c(TAR_ASK = "true"))
  lines <- c(
    "---",
    "title: report",
    "format: html",
    "---",
    "",
    "```{r}",
    "writeLines(Sys.getenv(\"TAR_ASK\"), \"tar_ask.txt\")",
    "```"
  )
  writeLines(lines, "report.qmd")
  targets::tar_script({
    library(tarchetypes)
    list(tar_quarto(report, "report.qmd"))
  })
  targets::tar_make(callr_function = NULL, reporter = "silent")
  expect_equal(readLines("tar_ask.txt"), "false")
  expect_equal(Sys.getenv("TAR_ASK"), "true")
})
