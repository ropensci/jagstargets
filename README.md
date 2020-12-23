
# jagstargets <img src='man/figures/logo.png' align="right" height="139"/>

[![R
Targetopia](https://img.shields.io/badge/R_Targetopia-member-000062?style=flat&labelColor=gray)](https://wlandau.github.io/targetopia.html)
[![cran](http://www.r-pkg.org/badges/version/jagstargets)](https://cran.r-project.org/package=jagstargets)
[![status](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![check](https://github.com/wlandau/jagstargets/workflows/check/badge.svg)](https://github.com/wlandau/jagstargets/actions?query=workflow%3Acheck)
[![codecov](https://codecov.io/gh/wlandau/jagstargets/branch/main/graph/badge.svg?token=3T5DlLwUVl)](https://codecov.io/gh/wlandau/targets)
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

## Installation

Install the GitHub development version to access the latest features and
patches.

``` r
remotes::install_github("wlandau/jagstargets")
```

And if you have not done so already, install JAGS from
<http://mcmc-jags.sourceforge.net/>.

## Documentation

The `jagstargets` website at <https://wlandau.github.io/jagstargets/>
has function documentation and vignettes. Prior familiarity with
[`targets`](https://github.com/wlandau/targets) and
[`R2jags`](https://CRAN.R-project.org/package=R2jags) is highly
recommended. For [`targets`](https://github.com/wlandau/targets), you
can learn more at <https://wlandau.github.io/targets>. The JAGS user
manual is linked from <http://mcmc-jags.sourceforge.net/>.

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
