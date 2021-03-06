% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tar_select_names.R
\name{tar_select_names}
\alias{tar_select_names}
\title{Select target names from a target list}
\usage{
tar_select_names(targets, ...)
}
\arguments{
\item{targets}{A list of target objects as described in the
"Target objects" section. It does not matter how nested
the list is as long as the only leaf nodes are targets.}

\item{...}{One or more comma-separated \code{tidyselect} expressions,
e.g. \code{starts_with("prefix")}. Just like \code{...} in \code{dplyr::select()}.}
}
\value{
A character vector of target names.
}
\description{
Select the names of targets from a target list.
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
targets <- list(
  list(
    targets::tar_target(x, 1),
    targets::tar_target(y1, 2)
  ),
  targets::tar_target(y2, 3),
  targets::tar_target(z, 4)
)
tar_select_names(targets, starts_with("y"), contains("z"))
})
}
}
\seealso{
Other target selection: 
\code{\link{tar_select_targets}()}
}
\concept{target selection}
