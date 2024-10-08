% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tar_eval.R, R/tar_eval_raw.R
\name{tar_eval}
\alias{tar_eval}
\alias{tar_eval_raw}
\title{Evaluate multiple expressions created with symbol substitution.}
\usage{
tar_eval(expr, values, envir = parent.frame())

tar_eval_raw(expr, values, envir = parent.frame())
}
\arguments{
\item{expr}{Starting expression. Values are iteratively substituted
in place of symbols in \code{expr} to create each new expression,
and then each new expression is evaluated.

\code{\link[=tar_eval]{tar_eval()}} expects an unevaluated expression for
the \code{expr} object, whereas \code{\link[=tar_eval_raw]{tar_eval_raw()}} expects an
evaluated expression object.}

\item{values}{List of values to substitute into \code{expr} to create
the expressions. All elements of \code{values} must have the same length.}

\item{envir}{Environment in which to evaluate the new expressions.}
}
\value{
A list of return values from the generated expression objects.
Often, these values are target objects.
See the "Target objects" section for background
on target objects specifically.
}
\description{
Loop over a grid of values, create an expression object
from each one, and then evaluate that expression.
Helps with general metaprogramming.

\code{\link[=tar_eval]{tar_eval()}} expects an unevaluated expression for
the \code{expr} object, whereas \code{\link[=tar_eval_raw]{tar_eval_raw()}} expects an
evaluated expression object.
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
# tar_map() is incompatible with tar_render() because the latter
# operates on preexisting tar_target() objects. By contrast,
# tar_eval() and tar_sub() iterate over the literal code
# farther upstream.
values <- list(
  name = lapply(c("name1", "name2"), as.symbol),
  file = list("file1.Rmd", "file2.Rmd")
)
tar_sub(list(name, file), values = values)
tar_sub(tar_render(name, file), values = values)
path <- tempfile()
file.create(path)
str(tar_eval(tar_render(name, path), values = values))
str(tar_eval_raw(quote(tar_render(name, path)), values = values))
# So in your _targets.R file, you can define a pipeline like as below.
# Just make sure to set a unique name for each target
# (which tar_map() does automatically).
values <- list(
  name = lapply(c("name1", "name2"), as.symbol),
  file = c(path, path)
)
list(
  tar_eval(tar_render(name, file), values = values)
)
}
\seealso{
Other Metaprogramming utilities: 
\code{\link{tar_sub}()}
}
\concept{Metaprogramming utilities}
