% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tar_files_input.R
\name{tar_files_input}
\alias{tar_files_input}
\title{Easy dynamic branching over known existing
input files or urls.}
\usage{
tar_files_input(
  name,
  files,
  batches = length(files),
  format = c("file", "url", "aws_file"),
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  cue = targets::tar_option_get("cue")
)
}
\arguments{
\item{name}{Symbol, name of the target. Subsequent targets
can refer to this name symbolically to induce a dependency relationship:
e.g. \code{tar_target(downstream_target, f(upstream_target))} is a
target named \code{downstream_target} which depends on a target
\code{upstream_target} and a function \code{f()}. In addition, a target's
name determines its random number generator seed. In this way,
each target runs with a reproducible seed so someone else
running the same pipeline should get the same results,
and no two targets in the same pipeline share the same seed.
(Even dynamic branches have different names and thus different seeds.)
You can recover the seed of a completed target
with \code{tar_meta(your_target, seed)} and run \code{set.seed()} on the result
to locally recreate the target's initial RNG state.}

\item{files}{Nonempty character vector of known existing input files
to track for changes.}

\item{batches}{Positive integer of length 1, number of batches
to partition the files. The default is one file per batch
(maximum number of batches) which is simplest to handle but
could cause a lot of overhead and consume a lot of computing resources.
Consider reducing the number of batches below the number of files
for heavy workloads.}

\item{format}{Character, either \code{"file"} or \code{"url"}. See the \code{format}
argument of \code{targets::tar_target()} for details.}

\item{iteration}{Character, iteration method. Must be a method
supported by the \code{iteration} argument of \code{targets::tar_target()}.
The iteration method for the upstream target is always \code{"list"}
in order to support batching.}

\item{error}{Character of length 1, what to do if the target
runs into an error. If \code{"stop"}, the whole pipeline stops
and throws an error. If \code{"continue"}, the error is recorded,
but the pipeline keeps going. \code{error = "workspace"} is just like
\code{error = "stop"} except \code{targets} saves a special workspace file
to support interactive debugging outside the pipeline.
(Visit \url{https://books.ropensci.org/targets/debugging.html}
to learn how to debug targets using saved workspaces.)}

\item{memory}{Character of length 1, memory strategy.
If \code{"persistent"}, the target stays in memory
until the end of the pipeline (unless \code{storage} is \code{"worker"},
in which case \code{targets} unloads the value from memory
right after storing it in order to avoid sending
copious data over a network).
If \code{"transient"}, the target gets unloaded
after every new target completes.
Either way, the target gets automatically loaded into memory
whenever another target needs the value.
For cloud-based dynamic files such as \code{format = "aws_file"},
this memory policy applies to
temporary local copies of the file in \verb{_targets/scratch/"}:
\code{"persistent"} means they remain until the end of the pipeline,
and \code{"transient"} means they get deleted from the file system
as soon as possible. The former conserves bandwidth,
and the latter conserves local storage.}

\item{garbage_collection}{Logical, whether to run \code{base::gc()}
just before the target runs.}

\item{priority}{Numeric of length 1 between 0 and 1. Controls which
targets get deployed first when multiple competing targets are ready
simultaneously. Targets with priorities closer to 1 get built earlier
(and polled earlier in \code{\link[targets:tar_make_future]{tar_make_future()}}).}

\item{resources}{Object returned by \code{tar_resources()}
with optional settings for high-performance computing
functionality, alternative data storage formats,
and other optional capabilities of \code{targets}.
See \code{tar_resources()} for details.}

\item{cue}{An optional object from \code{tar_cue()}
to customize the rules that decide whether the target is up to date.
Only applies to the downstream target. The upstream target always runs.}
}
\value{
A list of two targets, one upstream and one downstream.
The upstream one does some work and returns some file paths,
and the downstream target is a pattern that applies \code{format = "file"}
or \code{format = "url"}.
See the "Target objects" section for background.
}
\description{
Shorthand for a pattern that correctly
branches over known existing files or urls.
}
\details{
\code{tar_files_input()} is like \code{tar_files()}
but more convenient when the files in question already
exist and are known in advance. Whereas \code{tar_files()}
always appears outdated (e.g. with \code{tar_outdated()})
because it always needs to check which files it needs to
branch over, \code{tar_files_input()} will appear up to date
if the files have not changed since last \code{tar_make()}.
In addition, \code{tar_files_input()} automatically groups
input files into batches to reduce overhead and
increase the efficiency of parallel processing.

\code{tar_files_input()} creates a pair of targets, one upstream
and one downstream. The upstream target does some work
and returns some file paths, and the downstream
target is a pattern that applies \code{format = "file"},
\code{format = "url"}, or \code{format = "aws_file"}.
This is the correct way to dynamically
iterate over file/url targets. It makes sure any downstream patterns
only rerun some of their branches if the files/urls change.
For more information, visit
\url{https://github.com/ropensci/targets/issues/136} and
\url{https://github.com/ropensci/drake/issues/1302}.
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
  # Do not use temp files in real projects
  # or else your targets will always rerun.
  paths <- unlist(replicate(4, tempfile()))
  file.create(paths)
  list(
    tarchetypes::tar_files_input(
      x,
      paths,
      batches = 2
    )
  )
})
targets::tar_make()
targets::tar_read(x)
targets::tar_read(x, branches = 1)
})
}
}
\seealso{
Other Dynamic branching over files: 
\code{\link{tar_files_input_raw}()},
\code{\link{tar_files_raw}()},
\code{\link{tar_files}()}
}
\concept{Dynamic branching over files}
