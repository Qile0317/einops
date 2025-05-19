# einops

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/Qile0317/einops/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Qile0317/einops/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/Qile0317/einops/graph/badge.svg)](https://app.codecov.io/gh/Qile0317/einops)
<!-- badges: end -->

This is a work in progress implementation of the einops library for R. The einops library is a powerful tool for manipulating tensors and arrays in a flexible and readable way. It provides a set of functions for reshaping, reducing, and repeating tensors, making it easier to work with complex data structures.

## Installation

You can install the development version of einops like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(einops)
## basic example code
```

## Planned Backends & Implementation

> The python einops implementation uses wrappers for each datastructure over some central mixins, but the current plan for the R version is to simply use S3 dispatch with some helpers. Functions like `reduce` will by default override existing conventions so aliases may be needed

- R torch tensors
- R 2d matrices (and matrix like objects like dfs)
    - SparseMatrices
- R multidim arrays
- R keras/TF datastructures?

Issue: If using Rcpp backend and we are modifying by reference, need to be careful about the case where a user does NOT reassign the result of a function into the same binding, need to ensure the original object is not modified. An easy solution is to just make a copy and work on that - but this is suboptimal.
