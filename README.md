
# jagstargets <img src='man/figures/logo.png' align="right" height="139"/>

[![R
Targetopia](https://img.shields.io/badge/R_Targetopia-member-blue?style=flat&labelColor=gray)](https://wlandau.github.io/targetopia/)
<!--
[![cran](http://www.r-pkg.org/badges/version/jagstargets)](https://cran.r-project.org/package=jagstargets)
-->
[![status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![check](https://github.com/wlandau/jagstargets/workflows/check/badge.svg)](https://github.com/wlandau/jagstargets/actions?query=workflow%3Acheck)
[![codecov](https://codecov.io/gh/wlandau/jagstargets/branch/main/graph/badge.svg?token=3T5DlLwUVl)](https://codecov.io/gh/wlandau/jagstargets)
[![lint](https://github.com/wlandau/jagstargets/workflows/lint/badge.svg)](https://github.com/wlandau/jagstargets/actions?query=workflow%3Alint)

Bayesian data analysis usually incurs long runtimes and cumbersome
custom code, and the process of prototyping and deploying custom
[JAGS](https://mcmc-jags.sourceforge.io) models can become a daunting
software engineering challenge. To ease this burden, the `jagstargets` R
package creates [JAGS](https://mcmc-jags.sourceforge.io) pipelines that
are concise, efficient, scalable, and tailored to the needs of Bayesian
statisticians. Leveraging
[`targets`](https://docs.ropensci.org/targets/), `jagstargets` pipelines
automatically parallelize the computation and skip expensive steps when
the results are already up to date. Minimal custom user-side code is
required, and there is no need to manually configure branching, so
`jagstargets` is easier to use than
[`targets`](https://docs.ropensci.org/targets/) and
[`R2jags`](https://CRAN.R-project.org/package=R2jags) directly.

## Prerequisites

1.  The [prerequisites of the `targets` R
    package](https://docs.ropensci.org/targets/#prerequisites).
2.  Basic familiarity with
    [`targets`](https://docs.ropensci.org/targets/): watch minutes 7
    through 40 of [this video](https://youtu.be/Gqn7Xn4d5NI?t=439), then
    read [this
    chapter](https://books.ropensci.org/targets/walkthrough.html) of the
    [user manual](https://books.ropensci.org/targets/).
3.  Familiarity with Bayesian Statistics and
    [JAGS](https://mcmc-jags.sourceforge.io/). Prior knowledge of
    [`rjags`](https://cran.r-project.org/package=rjags) or
    [`R2jags`](https://cran.r-project.org/package=R2jags) helps.

## How to get started

Read the `jagstargets` tutorial vignettes
[here](https://wlandau.github.io/jagstargets/articles/mcmc.html) and
[here](https://wlandau.github.io/jagstargets/articles/mcmc_rep.html),
then use <https://wlandau.github.io/jagstargets/> as a reference while
constructing your own workflows.

## Installation

Install the GitHub development version to access the latest features and
patches.

``` r
remotes::install_github("wlandau/jagstargets")
```

And if you have not done so already, install JAGS from
<https://mcmc-jags.sourceforge.io/>.

## Usage

First, write a [`_targets.R`
file](https://books.ropensci.org/targets/walkthrough.html) that loads
your packages, defines a function to generate JAGS data, and lists a
pipeline of targets. The target list can call target factories like
[`tar_jags()`](https://wlandau.github.io/jagstargets/reference/tar_jags.html)
as well as ordinary targets with
[`tar_target()`](https://docs.ropensci.org/targets/reference/tar_target.html).
The following minimal example is simple enough to contain entirely
within the `_targets.R` file, but for larger projects, you may wish to
store functions in separate files as in the
[`targets-keras`](https://github.com/wlandau/targets-keras) example.

``` r
# _targets.R
library(targets)
library(jagstargets)

generate_data <- function() {
  true_beta <- stats::rnorm(n = 1, mean = 0, sd = 1)
  x <- seq(from = -1, to = 1, length.out = n)
  y <- stats::rnorm(n, x * true_beta, 1)
  out <- list(n = n, x = x, y = y, true_beta = true_beta)
}

list(
  tar_jags(
    example,
    jags_files = "x.jags", # You provide this file.
    parameters.to.save = "beta",
    data = generate_data()
  )
)
```

Run
[`tar_visnetwork()`](https://docs.ropensci.org/targets/reference/tar_visnetwork.html)
to check `_targets.R` for correctness, then call
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
to run the pipeline. Access the results using
[`tar_read()`](https://docs.ropensci.org/targets/reference/tar_read.html),
e.g. `tar_read(tar_read(example_summary_x)`. Visit [this
vignette](https://wlandau.github.io/jagstargets/articles/mcmc.html) to
read more about this example.

## How the package works

`jagstargets` supports specialized [target
factories](https://ropensci.org/blog/2021/02/03/targets/#target-factories)
that create ensembles of [target
objects](https://docs.ropensci.org/targets/reference/tar_target.html)
for [`R2jags`](https://CRAN.R-project.org/package=R2jags) workflows.
These [target
factories](https://ropensci.org/blog/2021/02/03/targets/#target-factories)
abstract away the details of
[`targets`](https://docs.ropensci.org/targets/) and
[`R2jags`](https://CRAN.R-project.org/package=R2jags) and make both
packages easier to use. For details, please read the [vignette on
non-branching MCMC
pipelines](https://wlandau.github.io/jagstargets/articles/mcmc.html).

## Help

If you have trouble using `jagstargets`, you can ask for help in the
[GitHub discussions
forum](https://github.com/wlandau/jagstargets/discussions/categories/help).
Because the purpose of `jagstargets` is to combine
[`targets`](https://docs.ropensci.org/targets/) and
[`R2jags`](https://CRAN.R-project.org/package=R2jags), your issue may
have something to do with one of the latter two packages, a [dependency
of
`targets`](https://github.com/ropensci/targets/blob/4e3ef2a6c986f558a25e544416f480fc01236b6b/DESCRIPTION#L49-L88),
or [`R2jags`](https://CRAN.R-project.org/package=R2jags) itself. When
you troubleshoot, peel back as many layers as possible to isolate the
problem. For example, if the issue comes from
[`R2jags`](https://CRAN.R-project.org/package=R2jags), create a
[reproducible example](https://reprex.tidyverse.org) that directly
invokes [`R2jags`](https://CRAN.R-project.org/package=R2jags) without
invoking `jagstargets`. The GitHub discussion and issue forums of those
packages are great resources.

## Participation

Development is a community effort, and we welcome discussion and
contribution. By participating in this project, you agree to abide by
the [code of
conduct](https://github.com/wlandau/jagstargets/blob/main/CODE_OF_CONDUCT.md)
and the [contributing
guide](https://github.com/wlandau/jagstargets/blob/main/CONTRIBUTING.md).

## Citation

``` r
citation("jagstargets")
#> 
#> To cite package 'jagstargets' in publications use:
#> 
#>   William Michael Landau (NA). jagstargets: Targets for JAGS Workflows.
#>   https://wlandau.github.io/jagstargets/,
#>   https://github.com/wlandau/jagstargets.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {jagstargets: Targets for JAGS Workflows},
#>     author = {William Michael Landau},
#>     note = {https://wlandau.github.io/jagstargets/, https://github.com/wlandau/jagstargets},
#>   }
```
