% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tar_files_input.R, R/tar_files_input_raw.R
\name{tar_files_input}
\alias{tar_files_input}
\alias{tar_files_input_raw}
\title{Dynamic branching over input files or URLs}
\usage{
tar_files_input(
  name,
  files,
  batches = length(files),
  format = c("file", "file_fast", "url", "aws_file"),
  repository = targets::tar_option_get("repository"),
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
)

tar_files_input_raw(
  name,
  files,
  batches = length(files),
  format = c("file", "file_fast", "url", "aws_file"),
  repository = targets::tar_option_get("repository"),
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
)
}
\arguments{
\item{name}{Name of the target.
\code{\link[=tar_files_input]{tar_files_input()}} expects a unevaluated symbol for the \code{name} argument,
whereas
\code{\link[=tar_files_input_raw]{tar_files_input_raw()}} expects a character string for \code{name}.
See the examples
for a demo.}

\item{files}{Nonempty character vector of known existing input files
to track for changes.}

\item{batches}{Positive integer of length 1, number of batches
to partition the files. The default is one file per batch
(maximum number of batches) which is simplest to handle but
could cause a lot of overhead and consume a lot of computing resources.
Consider reducing the number of batches below the number of files
for heavy workloads.}

\item{format}{Character, either \code{"file"}, \code{"file_fast"}, or \code{"url"}.
See the \code{format} argument of \code{targets::tar_target()} for details.}

\item{repository}{Character of length 1, remote repository for target
storage. Choices:
\itemize{
\item \code{"local"}: file system of the local machine.
\item \code{"aws"}: Amazon Web Services (AWS) S3 bucket. Can be configured
with a non-AWS S3 bucket using the \code{endpoint} argument of
\code{\link[targets:tar_resources_aws]{tar_resources_aws()}}, but versioning capabilities may be lost
in doing so.
See the cloud storage section of
\url{https://books.ropensci.org/targets/data.html}
for details for instructions.
\item \code{"gcp"}: Google Cloud Platform storage bucket.
See the cloud storage section of
\url{https://books.ropensci.org/targets/data.html}
for details for instructions.
\item A character string from \code{\link[targets:tar_repository_cas]{tar_repository_cas()}} for content-addressable
storage.
}

Note: if \code{repository} is not \code{"local"} and \code{format} is \code{"file"}
then the target should create a single output file.
That output file is uploaded to the cloud and tracked for changes
where it exists in the cloud. As of \code{targets} version 1.11.0 and higher,
the local file is no longer deleted after the target runs.}

\item{iteration}{Character, iteration method. Must be a method
supported by the \code{iteration} argument of \code{targets::tar_target()}.
The iteration method for the upstream target is always \code{"list"}
in order to support batching.}

\item{error}{Character of length 1, what to do if the target
stops and throws an error. Options:
\itemize{
\item \code{"stop"}: the whole pipeline stops and throws an error.
\item \code{"continue"}: the whole pipeline keeps going.
\item \code{"null"}: The errored target continues and returns \code{NULL}.
The data hash is deliberately wrong so the target is not
up to date for the next run of the pipeline. In addition,
as of \code{targets} version 1.8.0.9011, a value of \code{NULL} is given
to upstream dependencies with \code{error = "null"} if loading fails.
\item \code{"abridge"}: any currently running targets keep running,
but no new targets launch after that.
\item \code{"trim"}: all currently running targets stay running. A queued
target is allowed to start if:
\enumerate{
\item It is not downstream of the error, and
\item It is not a sibling branch from the same \code{\link[targets:tar_target]{tar_target()}} call
(if the error happened in a dynamic branch).
}

The idea is to avoid starting any new work that the immediate error
impacts. \code{error = "trim"} is just like \code{error = "abridge"},
but it allows potentially healthy regions of the dependency graph
to begin running.
(Visit \url{https://books.ropensci.org/targets/debugging.html}
to learn how to debug targets using saved workspaces.)
}}

\item{memory}{Character of length 1, memory strategy. Possible values:
\itemize{
\item \code{"auto"} (default): equivalent to \code{memory = "transient"} in almost
all cases. But to avoid superfluous reads from disk,
\code{memory = "auto"} is equivalent to \code{memory = "persistent"} for
for non-dynamically-branched targets that other targets
dynamically branch over. For example: if your pipeline has
\code{tar_target(name = y, command = x, pattern = map(x))},
then \code{tar_target(name = x, command = f(), memory = "auto")}
will use persistent memory for \code{x}
in order to avoid rereading all of \code{x}
for every branch of \code{y}.
\item \code{"transient"}: the target gets unloaded
after every new target completes.
Either way, the target gets automatically loaded into memory
whenever another target needs the value.
\item \code{"persistent"}: the target stays in memory
until the end of the pipeline (unless \code{storage} is \code{"worker"},
in which case \code{targets} unloads the value from memory
right after storing it in order to avoid sending
copious data over a network).
}

For cloud-based file targets
(e.g. \code{format = "file"} with \code{repository = "aws"}),
the \code{memory} option applies to the
temporary local copy of the file:
\code{"persistent"} means it remains until the end of the pipeline
and is then deleted,
and \code{"transient"} means it gets deleted as soon as possible.
The former conserves bandwidth,
and the latter conserves local storage.}

\item{garbage_collection}{Logical: \code{TRUE} to run \code{base::gc()}
just before the target runs, in whatever R process it is about to run
(which could be a parallel worker).
\code{FALSE} to omit garbage collection.
Numeric values get converted to \code{FALSE}.
The \code{garbage_collection} option in \code{\link[targets:tar_option_set]{tar_option_set()}}
is independent of the
argument of the same name in \code{\link[targets:tar_target]{tar_target()}}.}

\item{priority}{Deprecated on 2025-04-08 (\code{targets} version 1.10.1.9013).
\code{targets} has moved to a more efficient scheduling algorithm
(\url{https://github.com/ropensci/targets/issues/1458})
which cannot support priorities.
The \code{priority} argument of \code{\link[targets:tar_target]{tar_target()}} no longer has a reliable
effect on execution order.}

\item{resources}{Object returned by \code{tar_resources()}
with optional settings for high-performance computing
functionality, alternative data storage formats,
and other optional capabilities of \code{targets}.
See \code{tar_resources()} for details.}

\item{cue}{An optional object from \code{tar_cue()}
to customize the rules that decide whether the target is up to date.
Only applies to the downstream target. The upstream target always runs.}

\item{description}{Character of length 1, a custom free-form human-readable
text description of the target. Descriptions appear as target labels
in functions like \code{\link[targets:tar_manifest]{tar_manifest()}} and \code{\link[targets:tar_visnetwork]{tar_visnetwork()}},
and they let you select subsets of targets for the \code{names} argument of
functions like \code{\link[targets:tar_make]{tar_make()}}. For example,
\code{tar_manifest(names = tar_described_as(starts_with("survival model")))}
lists all the targets whose descriptions start with the character
string \code{"survival model"}.}
}
\value{
A list of two targets, one upstream and one downstream.
The upstream one does some work and returns some file paths,
and the downstream target is a pattern that applies \code{format = "file"}
or \code{format = "url"}.
See the "Target objects" section for background.
}
\description{
Dynamic branching over input files or URLs.

\code{\link[=tar_files_input]{tar_files_input()}} expects a unevaluated symbol for the \code{name} argument,
whereas
\code{\link[=tar_files_input_raw]{tar_files_input_raw()}} expects a character string for \code{name}.
See the examples
for a demo.
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
\code{format = "file_fast"}, or \code{format = "url"}.
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
  library(tarchetypes)
  # Do not use temp files in real projects
  # or else your targets will always rerun.
  paths <- unlist(replicate(4, tempfile()))
  file.create(paths)
  list(
    tar_files_input(
      name = x,
      files = paths,
      batches = 2
    ),
    tar_files_input_raw(
      name = "y",
      files = paths,
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
\code{\link{tar_files}()}
}
\concept{Dynamic branching over files}
