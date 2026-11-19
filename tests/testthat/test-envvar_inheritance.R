# Generic checks that reports rendered outside of pipelines
# inherit environment variables from the calling R session.
# Uses a throwaway variable rather than TAR_ASK.
envvar_inheritance_name <- "TARCHETYPES_TEST_ENVVAR_INHERIT_7Q4Z"
envvar_inheritance_value <- "sentinel_x9k2"

envvar_inheritance_lines <- function(format) {
  c(
    "---",
    "title: report",
    format,
    "---",
    "",
    "```{r}",
    sprintf(
      "writeLines(Sys.getenv(\"%s\", unset = \"unset\"), \"envvar.txt\")",
      envvar_inheritance_name
    ),
    "```"
  )
}

targets::tar_test("rmarkdown::render() inherits env vars from the caller", {
  skip_on_cran()
  skip_rmarkdown()
  Sys.unsetenv(envvar_inheritance_name)
  expect_equal(Sys.getenv(envvar_inheritance_name, unset = "unset"), "unset")
  withr::local_envvar(
    stats::setNames(envvar_inheritance_value, envvar_inheritance_name)
  )
  writeLines(
    envvar_inheritance_lines("output_format: html_document"),
    "report.Rmd"
  )
  rmarkdown::render("report.Rmd", quiet = TRUE)
  expect_equal(readLines("envvar.txt"), envvar_inheritance_value)
})

targets::tar_test("quarto::quarto_render() inherits env vars from the caller", {
  skip_on_cran()
  skip_quarto()
  Sys.unsetenv(envvar_inheritance_name)
  expect_equal(Sys.getenv(envvar_inheritance_name, unset = "unset"), "unset")
  withr::local_envvar(
    stats::setNames(envvar_inheritance_value, envvar_inheritance_name)
  )
  writeLines(envvar_inheritance_lines("format: html"), "report.qmd")
  quarto::quarto_render("report.qmd", quiet = TRUE)
  expect_equal(readLines("envvar.txt"), envvar_inheritance_value)
})
