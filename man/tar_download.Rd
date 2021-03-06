% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tar_download.R
\name{tar_download}
\alias{tar_download}
\title{Target that downloads URLs.}
\usage{
tar_download(
  name,
  urls,
  paths,
  method = NULL,
  quiet = TRUE,
  mode = "w",
  cacheOK = TRUE,
  extra = NULL,
  headers = NULL,
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
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

\item{urls}{Character vector of URLs to track and download.
Must be known and declared before the pipeline runs.}

\item{paths}{Character vector of local file paths to
download each of the URLs.
Must be known and declared before the pipeline runs.}

\item{method}{Method to be used for downloading files.  Current
    download methods are \code{"internal"}, \code{"wininet"} (Windows
    only) \code{"libcurl"}, \code{"wget"} and \code{"curl"}, and there
    is a value \code{"auto"}: see \sQuote{Details} and \sQuote{Note}.

    The method can also be set through the option
    \code{"download.file.method"}: see \code{\link{options}()}.
  }

\item{quiet}{If \code{TRUE}, suppress status messages (if any), and
    the progress bar.}

\item{mode}{character.  The mode with which to write the file.  Useful
    values are \code{"w"}, \code{"wb"} (binary), \code{"a"} (append) and
    \code{"ab"}.  Not used for methods \code{"wget"} and \code{"curl"}.
    See also \sQuote{Details}, notably about using \code{"wb"} for Windows.
  }

\item{cacheOK}{logical.  Is a server-side cached value acceptable?}

\item{extra}{character vector of additional command-line arguments for
    the \code{"wget"} and \code{"curl"} methods.}

\item{headers}{named character vector of HTTP headers to use in HTTP
    requests.  It is ignored for non-HTTP URLs.  The \code{User-Agent}
    header, coming from the \code{HTTPUserAgent} option (see
    \code{\link{options}}) is used as the first header, automatically.}

\item{iteration}{Character of length 1, name of the iteration mode
of the target. Choices:
\itemize{
\item \code{"vector"}: branching happens with \code{vctrs::vec_slice()} and
aggregation happens with \code{vctrs::vec_c()}.
\item \code{"list"}, branching happens with \verb{[[]]} and aggregation happens with
\code{list()}.
\item \code{"group"}: \code{dplyr::group_by()}-like functionality to branch over
subsets of a data frame. The target's return value must be a data
frame with a special \code{tar_group} column of consecutive integers
from 1 through the number of groups. Each integer designates a group,
and a branch is created for each collection of rows in a group.
See the \code{\link[targets:tar_group]{tar_group()}} function to see how you can
create the special \code{tar_group} column with \code{dplyr::group_by()}.
}}

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

\item{deployment}{Character of length 1, only relevant to
\code{\link[targets:tar_make_clustermq]{tar_make_clustermq()}} and \code{\link[targets:tar_make_future]{tar_make_future()}}. If \code{"worker"},
the target builds on a parallel worker. If \code{"main"},
the target builds on the host machine / process managing the pipeline.}

\item{priority}{Numeric of length 1 between 0 and 1. Controls which
targets get deployed first when multiple competing targets are ready
simultaneously. Targets with priorities closer to 1 get built earlier
(and polled earlier in \code{\link[targets:tar_make_future]{tar_make_future()}}).}

\item{resources}{Object returned by \code{tar_resources()}
with optional settings for high-performance computing
functionality, alternative data storage formats,
and other optional capabilities of \code{targets}.
See \code{tar_resources()} for details.}

\item{storage}{Character of length 1, only relevant to
\code{\link[targets:tar_make_clustermq]{tar_make_clustermq()}} and \code{\link[targets:tar_make_future]{tar_make_future()}}.
If \code{"main"}, the target's return value is sent back to the
host machine and saved locally. If \code{"worker"}, the worker
saves the value.}

\item{retrieval}{Character of length 1, only relevant to
\code{\link[targets:tar_make_clustermq]{tar_make_clustermq()}} and \code{\link[targets:tar_make_future]{tar_make_future()}}.
If \code{"main"}, the target's dependencies are loaded on the host machine
and sent to the worker before the target builds.
If \code{"worker"}, the worker loads the targets dependencies.}

\item{cue}{An optional object from \code{tar_cue()} to customize the
rules that decide whether the target is up to date.}
}
\value{
A list of two target objects, one upstream and one downstream.
The upstream one watches a URL for changes, and the downstream one
downloads it.
See the "Target objects" section for background.
}
\description{
Create a target that downloads file from one or more URLs
and automatically reruns when the remote data changes
(according to the ETags or last-modified time stamps).
}
\details{
\code{tar_download()} creates a pair of targets, one upstream
and one downstream. The upstream target uses \code{format = "url"}
(see \code{targets::tar_target()}) to track files at one or more URLs,
and automatically invalidate the target if the ETags
or last-modified time stamps change. The downstream target
depends on the upstream one, downloads the files,
and tracks them using \code{format = "file"}.
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
  list(
    tarchetypes::tar_download(
      x,
      urls = c("https://httpbin.org/etag/test", "https://r-project.org"),
      paths = c("downloaded_file_1", "downloaded_file_2")
    )
  )
})
targets::tar_make()
targets::tar_read(x)
})
}
}
\seealso{
Other targets with custom invalidation rules: 
\code{\link{tar_change}()},
\code{\link{tar_force}()},
\code{\link{tar_skip}()}
}
\concept{targets with custom invalidation rules}
