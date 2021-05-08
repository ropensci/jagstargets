---
title: 'The jagstargets R package: a reproducible workflow framework for Bayesian data analysis with JAGS'
tags:
- R
- reproducibility
- high-performance computing
- pipeline
- workflow
- Make
- Bayesian
- JAGS
date: "2020"
output: pdf_document
authors:
- name: William Michael Landau
  orcid: 0000-0003-1878-3253
  email: will.landau@gmail.com
  affiliation: 1
bibliography: paper.bib
affiliations:
- name: Eli Lilly and Company
  index: 1
---

# Statement of need

Bayesian statisticians regularly experiment with models to refine, compare, and understand them [@bayesworkflow]. Each fitted model informs subsequent model-building choices, and even for experienced practitioners, the investigation often leads to final models that differ from the ones originally proposed. Fitting a model usually means applying Markov chain Monte Carlo or a similar method to approximate the full joint posterior distribution of the parameters [@bda3]. Flexible probabilistic programming languages such as JAGS have made model specification quick and straightforward [@jags], but computation time is still a bottleneck. A workflow can take several minutes or hours to run, and in subsequent iterations, researchers struggle to keep the results up to date with frequent changes to the models, code, and data.

# Summary

The [`jagstargets`](https://docs.ropensci.org/jagstargets/) R package [@jagstargets] is a pipeline toolkit for Bayesian data analysis with JAGS. It automatically detects the dependency relationships among the results, models, and datasets, it watches these computational steps for changes, and it deploys the work with optional distributed computing while skipping the tasks that are already synchronized with their upstream dependencies. This automated selective skipping reduces the runtime of successive experiments, and when the entire workflow is up to date, the user has tangible evidence of reproducibility.

The [`jagstargets`](https://docs.ropensci.org/jagstargets/) package is a bridge between packages [`R2jags`](https://github.com/suyusung/R2jags) [@R2jags] and [`targets`](https://docs.ropensci.org/targets/) [@targets]. [`targets`](https://docs.ropensci.org/targets/) is a general-purpose pipeline toolkit for reproducible research and high-performance computing, and it is not specific to Bayesian data analysis. [`jagstargets`](https://docs.ropensci.org/jagstargets/) leverages the existing capabilities of [`targets`](https://docs.ropensci.org/targets/) to more easily and concisely express computational pipelines for Bayesian statisticians, from single analyses or large-scale simulation studies. [`jagstargets`](https://docs.ropensci.org/targets/) is similar to the [`stantargets`](https://docs.ropensci.org/stantargets/) R package [@stantargets], the latter of which streamlines pipeline construction of Bayesian data analysis pipelines with Stan [@stan].

# References
