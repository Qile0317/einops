# einops

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/Qile0317/einops/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Qile0317/einops/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/Qile0317/einops/graph/badge.svg)](https://app.codecov.io/gh/Qile0317/einops)
[![MIT license](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/Qile0317/einops/blob/main/LICENSE.md)
<!-- badges: end -->

> This is a work in progress R implementation of [einops](https://einops.rocks/).

Flexible and powerful tensor operations for readable and reliable code. <br />
Supports base R arrays (this includes matrices, a subtype of base arrays), and more types and frameworks such as torch in the future.

## Tweets (From the original python implementation)

> In case you need convincing arguments for setting aside time to learn about einsum and einops...
[Tim Rocktäschel](https://twitter.com/_rockt/status/1230818967205425152)

> Writing better code with PyTorch and einops 👌
[Andrej Karpathy](https://twitter.com/karpathy/status/1290826075916779520)

> Slowly but surely, einops is seeping in to every nook and cranny of my code. If you find yourself shuffling around bazillion dimensional tensors, this might change your life
[Nasim Rahaman](https://twitter.com/nasim_rahaman/status/1216022614755463169)

[More testimonials](https://einops.rocks/pages/testimonials/)

## Installation

```R
devtools::install_github("Qile0317/einops")
```

## Usage

`einops` has a minimalistic yet powerful API.

Three core operations provided ([einops tutorial](https://github.com/arogozhnikov/einops/blob/main/docs/)
shows those cover stacking, reshape, transposition, squeeze/unsqueeze, repeat, tile, concatenate, view and numerous reductions)

``` r
library(einops)
# rearrange elements according to the pattern
output_tensor <- rearrange(input_tensor, 't b c -> b c t')
# combine rearrangement and reduction
output_tensor <- reduce(input_tensor, 'b c (h h2) (w w2) -> b h w c', 'mean', h2 = 2, w2 = 2)
# copy along a new axis (note: repeat is surrounded by ticks. einops.repeat() works too)
output_tensor <- `repeat`(input_tensor, 'h w -> h w c', c = 3)
```

A ***MAJOR CAVEAT*** is that R's `base::arrays` are column-major, while Python's multidimensional arrays are row-major. This is reflected in their respective indexing operations. To fully replicate a python expression using a column-major array, each of the three core functions have a `.row_major` optional argument which replicates the python row-major behaviour during einops operations, while ensuring the output is still column-major.

<!-- TODO pack and unpack -->
<!-- TODO ### EinMix -->
<!-- TODO ### Layers -->

## Vignettes

The vignette is the most convenient way to see `einops` in action

- read it using [`vignette("basics", package = "einops")`](https://qile0317.github.io/einops/articles/basics.html)

Kapil Sachdeva recorded a small [intro to einops](https://www.youtube.com/watch?v=xGy75Pjsqzo) for the python version.

## Naming

`einops` stands for Einstein-Inspired Notation for operations 
(though "Einstein operations" is more attractive and easier to remember).

Notation was loosely inspired by Einstein summation (in particular by `numpy.einsum` operation).

<!-- TODO ## Why use `einops` notation?! -->

<!-- ## Supported frameworks

Einops works with ...

- `base::array`

Additionally, einops can be used with any framework that supports R's array access S3 generics -->

## Implementations in other languages

- [Python (Original)](https://einops.rocks/)
- [Julia](https://murrellgroup.github.io/Einops.jl/stable/)
- [Rust](https://docs.rs/einops/latest/einops/)
- [C++](https://github.com/dorpxam/einops-cpp)
