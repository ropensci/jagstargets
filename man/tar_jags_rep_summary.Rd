% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tar_jags_rep_summary.R
\name{tar_jags_rep_summary}
\alias{tar_jags_rep_summary}
\title{Tidy posterior summaries from multiple MCMCs per model.}
\usage{
tar_jags_rep_summary(
  name,
  jags_files,
  parameters.to.save,
  data = list(),
  data_copy = character(0),
  data_omit = character(0),
  variables = NULL,
  summaries = NULL,
  summary_args = NULL,
  batches = 1L,
  reps = 1L,
  combine = TRUE,
  n.cluster = 1,
  n.chains = 3,
  n.iter = 2000,
  n.burnin = as.integer(n.iter/2),
  n.thin = 1,
  jags.module = c("glm", "dic"),
  inits = NULL,
  RNGname = c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
    "Mersenne-Twister"),
  jags.seed = 123,
  quiet = TRUE,
  progress.bar = "text",
  refresh = 0,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  error = targets::tar_option_get("error"),
  memory = "transient",
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
\item{name}{Symbol, base name for the collection of targets.
Serves as a prefix for target names.}

\item{jags_files}{Character vector of JAGS model files. If you
supply multiple files, each model will run on the one shared dataset
generated by the code in \code{data}. If you supply an unnamed vector,
\code{tools::file_path_sans_ext(basename(jags_files))} will be used
as target name suffixes. If \code{jags_files} is a named vector,
the suffixed will come from \code{names(jags_files)}.}

\item{parameters.to.save}{character vector of the names of the
    parameters to save which should be monitored.}

\item{data}{Code to generate the \code{data} for the JAGS model.}

\item{data_copy}{Character vector of names of scalars in \code{data}.
These values will be inserted as columns in the output data frame
for each rep. Useful for simulation studies where you want to
check the results against some "true value" in the data. See the
\code{mcmc_rep} vignette for an example.}

\item{data_omit}{Elements of the data to omit from the JAGS model.
Useful if you want to pass elements to \code{data_copy} without
supplying them to the model. (Superfluous data causes warnings
in JAGS.)}

\item{variables}{Character vector of model parameter names.
The output posterior summaries are restricted to these variables.}

\item{summaries}{List of summary functions passed to \code{...} in
\code{posterior::summarize_draws()} through \verb{$summary()}
on the \code{CmdStanFit} object.}

\item{summary_args}{List of summary function arguments passed to
\code{.args} in \code{posterior::summarize_draws()} through \verb{$summary()}
on the \code{CmdStanFit} object.}

\item{batches}{Number of batches. Each batch runs a model \code{reps} times.}

\item{reps}{Number of replications per batch. Ideally, each rep
should produce its own random dataset using the code
supplied to \code{data}.}

\item{combine}{Logical, whether to create a target to
combine all the model results
into a single data frame downstream. Convenient, but
duplicates data.}

\item{n.cluster}{number of clusters to use to run parallel chains.
    Default equals n.chains.}

\item{n.chains}{number of Markov chains (default: 3)}

\item{n.iter}{number of total iterations per chain (including burn in;
    default: 2000)}

\item{n.burnin}{length of burn in, i.e. number of iterations to
    discard at the beginning. Default is \code{n.iter/2}, that is,
    discarding the first half of the simulations. If n.burnin is 0,
    \code{jags()} will run 100 iterations for adaption.}

\item{n.thin}{thinning rate. Must be a positive integer.  Set
    \code{n.thin} > 1 to save memory and computation time if
    \code{n.iter} is large.  Default is \code{max(1, floor(n.chains *
    (n.iter-n.burnin) / 1000))} which will only thin if there are at
    least 2000 simulations.}

\item{jags.module}{the vector of jags modules to be loaded.  Default are \dQuote{glm} and \dQuote{dic}. Input NULL if you don't want to load any jags module.}

\item{inits}{a list with \code{n.chains} elements; each element of the
    list is itself a list of starting values for the \code{BUGS} model,
    \emph{or} a function creating (possibly random) initial values. If inits is
    \code{NULL}, \code{JAGS} will generate initial values for parameters.}

\item{RNGname}{the name for random number generator used in JAGS. There are four RNGS
  supplied by the base moduale in JAGS: \code{Wichmann-Hill}, \code{Marsaglia-Multicarry},
  \code{Super-Duper}, \code{Mersenne-Twister}}

\item{jags.seed}{random seed for \code{JAGS}, default is 123.  This function is used for jags.parallell() and does not work for jags().  Use set.seed() instead if you want to produce identical result with jags() }

\item{quiet}{Logical, whether to suppress the output stream. Does not
suppress messages, warnings, or errors.}

\item{progress.bar}{type of progress bar. Possible values are \dQuote{text},
      \dQuote{gui}, and \dQuote{none}. Type \dQuote{text} is displayed
      on the R console. Type \dQuote{gui} is a graphical progress bar
      in a new window. The progress bar is suppressed if \code{progress.bar} is
      \dQuote{none}}

\item{refresh}{refresh frequency for progress bar, default is \code{n.iter/50}}

\item{tidy_eval}{Logical, whether to enable tidy evaluation
when interpreting \code{command} and \code{pattern}. If \code{TRUE}, you can use the
"bang-bang" operator \verb{!!} to programmatically insert
the values of global objects.}

\item{packages}{Character vector of packages to load right before
the target builds. Use \code{tar_option_set()} to set packages
globally for all subsequent targets you define.}

\item{library}{Character vector of library paths to try
when loading \code{packages}.}

\item{error}{Character of length 1, what to do if the target
runs into an error. If \code{"stop"}, the whole pipeline stops
and throws an error. If \code{"continue"}, the error is recorded,
but the pipeline keeps going.}

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
simultaneously. Targets with priorities closer to 1 get built earlier.
Only applies to \code{\link[targets:tar_make_future]{tar_make_future()}} and \code{\link[targets:tar_make_clustermq]{tar_make_clustermq()}}
(not \code{\link[targets:tar_make]{tar_make()}}). \code{\link[targets:tar_make_future]{tar_make_future()}} with no extra settings is
a drop-in replacement for \code{\link[targets:tar_make]{tar_make()}} in this case.}

\item{resources}{A named list of computing resources. Uses:
\itemize{
\item Template file wildcards for \code{future::future()} in \code{\link[targets:tar_make_future]{tar_make_future()}}.
\item Template file wildcards \code{clustermq::workers()} in \code{\link[targets:tar_make_clustermq]{tar_make_clustermq()}}.
\item Custom target-level \code{future::plan()}, e.g.
\code{resources = list(plan = future.callr::callr)}.
\item Custom \code{curl} handle if \code{format = "url"},
e.g. \code{resources = list(handle = curl::new_handle())}.
\item Custom preset for \code{qs::qsave()} if \code{format = "qs"}, e.g.
\code{resources = list(handle = "archive")}.
\item Custom compression level for \code{fst::write_fst()} if
\code{format} is \code{"fst"}, \code{"fst_dt"}, or \code{"fst_tbl"}, e.g.
\code{resources = list(compress = 100)}.
\item AWS bucket and prefix for the \code{"aws_"} formats, e.g.
\code{resources = list(bucket = "your-bucket", prefix = "folder/name")}.
\code{bucket} is required for AWS formats. See the cloud computing chapter
of the manual for details.
}}

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
\code{tar_jags_rep_dic(name = x, jags_files = "y.jags")}
returns a list of \code{targets::tar_target()} objects:
\itemize{
\item \code{x_file_y}: reproducibly track the jags model file.
\item \code{x_lines_y}: contents of the jags model file.
\item \code{x_data}: dynamic branching target with simulated datasets.
\item \code{x_y}: dynamic branching target with tidy data frames of posterior
summaries.
\item \code{x}: combine all the model-specific summary targets into
a single data frame with columns to distinguish among the models.
Suppressed if \code{combine} is \code{FALSE}.
}
}
\description{
Run multiple MCMCs on simulated datasets
and return posterior summaries and the effective number of parameters
for each run.
}
\details{
The MCMC targets use \code{R2jags::jags()} if \code{n.cluster} is \code{1} and
\code{R2jags::jags.parallel()} otherwise. Most arguments to \code{tar_jags()}
are forwarded to these functions.
}
\examples{
# First, write your JAGS model file. Example:
# tar_jags_example_file() # Writes jagstargets_example.jags
# Then in _targets.R, write the pipeline:
targets::tar_pipeline(
  tar_jags_rep_summary(
    your_model,
    jags_files = "jagstargets_example.jags",
    data = tar_jags_example_data(),
    parameters.to.save = "beta",
    batches = 40,
    reps = 25
  )
)
}