
# jagstargets <img src='man/figures/logo.png' align="right" height="139"/>

[![R
Targetopia](https://img.shields.io/badge/R_Targetopia-member-blue?style=flat&labelColor=gray)](https://wlandau.github.io/targetopia/)
[![cran](http://www.r-pkg.org/badges/version/jagstargets)](https://cran.r-project.org/package=jagstargets)
[![status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![check](https://github.com/wlandau/jagstargets/workflows/check/badge.svg)](https://github.com/wlandau/jagstargets/actions?query=workflow%3Acheck)
[![codecov](https://codecov.io/gh/wlandau/jagstargets/branch/main/graph/badge.svg?token=3T5DlLwUVl)](https://codecov.io/gh/wlandau/jagstargets)
[![lint](https://github.com/wlandau/jagstargets/workflows/lint/badge.svg)](https://github.com/wlandau/jagstargets/actions?query=workflow%3Alint)

The `jagstargets` R package is an extension to
[`targets`](https://github.com/wlandau/targets) and
[`R2jags`](https://CRAN.R-project.org/package=R2jags) for Bayesian data
analysis. `jagstargets` makes it super easy to set up useful scalable
JAGS pipelines that automatically parallelize the computation and skip
expensive steps when the results are already up to date. Minimal custom
code is required, and there is no need to manually configure branching,
so usage is much easier than
[`targets`](https://github.com/wlandau/targets) alone.

## Prerequisites

1.  The [prerequisites of the `targets` R
    package](https://wlandau.github.io/targets/#prerequisites).
2.  Basic familiarity with
    [`targets`](https://wlandau.github.io/targets/): watch minutes 6
    through 40 of [this video](https://youtu.be/Gqn7Xn4d5NI), then read
    [this
    chapter](https://wlandau.github.io/targets-manual/walkthrough.html)
    of the [user manual](https://wlandau.github.io/targets-manual/).
3.  Familiarity with Bayesian Statistics and
    [JAGS](http://mcmc-jags.sourceforge.net/). Prior knowledge of
    [`rjags`](https://cran.r-project.org/package=rjags) or
    [`R2jags`](https://cran.r-project.org/package=R2jags) helps.

## How to get started

Read the `jagstargets` tutorial vignettes
[here](https://wlandau.github.io/jagstargets/articles/mcmc.html) and
[here](https://wlandau.github.io/jagstargets/articles/mcmc_rep.html),
then use <https://wlandau.github.io/jagstargets/> as a reference while
constructing your own worklows.

## Installation

Install the GitHub development version to access the latest features and
patches.

``` r
remotes::install_github("wlandau/jagstargets")
```

And if you have not done so already, install JAGS from
<http://mcmc-jags.sourceforge.net/>.

## Usage

First, write a [`_targets.R`
file](https://wlandau.github.io/targets-manual/walkthrough.html) that
loads your packages, defines a function to generate JAGS data, and lists
a pipeline of targets. The target list can call target factories like
[`tar_jags()`](https://wlandau.github.io/jagstargets/reference/tar_jags.html)
as well as ordinary targets with
[`tar_target()`](https://wlandau.github.io/targets/reference/tar_target.html).

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
    data = generate_data(),
    log = R.utils::nullfile()
  )
)
```

Run
[`tar_visnetwork()`](https://wlandau.github.io/targets/reference/tar_visnetwork.html)
to check `_targets.R` for correctness, then call
[`tar_make()`](https://wlandau.github.io/targets/reference/tar_make.html)
to run the pipeline. Access the results using
[`tar_read()`](https://wlandau.github.io/targets/reference/tar_read.html),
e.g. `tar_read(tar_read(example_summary_x)`. Visit [this
vignette](https://wlandau.github.io/jagstargets/articles/mcmc.html) to
read more about this example.

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
#> Warning in citation("jagstargets"): no date field in DESCRIPTION file of package
#> 'jagstargets'
#> Warning in citation("jagstargets"): could not determine year for 'jagstargets'
#> from package DESCRIPTION file
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
