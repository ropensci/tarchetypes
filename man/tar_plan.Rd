% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tar_plan.R
\name{tar_plan}
\alias{tar_plan}
\title{A \code{drake}-plan-like pipeline DSL}
\usage{
tar_plan(...)
}
\arguments{
\item{...}{Named and unnamed targets. All named targets must follow
the \code{drake}-plan-like \code{target = command} syntax, and all unnamed
arguments must be explicit calls to create target objects,
e.g. \code{tar_target()}, target factories like \code{\link[=tar_render]{tar_render()}}, or similar.}
}
\value{
A list of \code{tar_target()} objects.
See the "Target objects" section for background.
}
\description{
Simplify target specification in pipelines.
}
\details{
Allows targets with just targets and commands
to be written in the pipeline as \code{target = command} instead of
\code{tar_target(target, command)}. Also supports ordinary
target objects if they are unnamed.
\code{tar_plan(x = 1, y = 2, tar_target(z, 3), tar_render(r, "r.Rmd"))}
is equivalent to
\code{list(tar_target(x, 1), tar_target(y, 2), tar_target(z, 3), tar_render(r, "r.Rmd"))}. # nolint
}
\section{Target objects}{

Most \code{tarchetypes} functions are target factories,
which means they return target objects
or lists of target objects.
Target objects represent skippable steps of the analysis pipeline
as described at \url{https://books.ropensci.org/targets/}.
Please read the walkthrough at
\url{https://books.ropensci.org/targets/walkthrough.html}
to understand the role of target objects in analysis pipelines.

For developers,
\url{https://wlandau.github.io/targetopia/contributing.html#target-factories}
explains target factories (functions like this one which generate targets)
and the design specification at
\url{https://books.ropensci.org/targets-design/}
details the structure and composition of target objects.
}

\examples{
if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
targets::tar_dir({ # tar_dir() runs code from a temporary directory.
targets::tar_script({
  library(tarchetypes)
  tar_plan(
    tarchetypes::tar_fst_tbl(data, data.frame(x = seq_len(26))),
    means = colMeans(data) # No need for tar_target() for simple cases.
  )
})
targets::tar_make()
})
}
}
\concept{Pipeline factories}
