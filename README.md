# einops

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/Qile0317/einops/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Qile0317/einops/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/Qile0317/einops/graph/badge.svg)](https://app.codecov.io/gh/Qile0317/einops)
<!-- badges: end -->

This is a work in progress implementation of the einops library for R. The einops library is a powerful tool for manipulating tensors and arrays in a flexible and readable way. It provides a set of functions for reshaping, reducing, and repeating tensors, in addition to several syntactic conveniences for use in torch.

## Installation

```R
devtools::install_github("Qile0317/einops")
```

## Usage

``` r
library(einops)
## basic example code
```

## Roadmap (Subject to change)

- [x] Lexer
- [ ] Parser & Ast datastructure
- [ ] Semantic checker for each context
- [ ] Intermediate Representation Generator (planner)
- [ ] Executor (this handles all different backends, starting with `base::array`)
- [ ] `einops:::parse_shape.array()`
- [ ] `einops:::rearrange.array()`
- [ ] `einops:::reduce.array()`
- [ ] `einops:::repeat.array()`
- [ ] `einops:::parse_shape.torch_tensor()`
- [ ] `einops:::rearrange.torch_tensor()`
- [ ] `einops:::torchRearrange()`
- [ ] `einops:::reduce.torch_tensor()`
- [ ] `einops:::torchReduce()`
- [ ] packing and unpacking expression interpretation
- [ ] `einops:::pack.array()`
- [ ] `einops:::pack.torch_tensor()`
- [ ] EinMix for torch
- [ ] `einops::einsum.array()`
- [ ] `einops::einsum.torch_tensor()`
- [ ] Expand Roadmp by incorporating more backends 
