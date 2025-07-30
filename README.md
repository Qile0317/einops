# einops

<!-- badges: start -->
[![R-CMD-check](https://github.com/Qile0317/einops/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Qile0317/einops/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/Qile0317/einops/graph/badge.svg)](https://app.codecov.io/gh/Qile0317/einops)
[![Documentation](https://img.shields.io/badge/docs-stable-blue.svg)](https://qile0317.github.io/einops/)
[![MIT license](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/Qile0317/einops/blob/main/LICENSE.md)
<!-- badges: end -->

> This is a work in progress R implementation of [einops](https://einops.rocks/).

Flexible and powerful tensor operations for readable and reliable code. <br />
Supports base R arrays (this includes matrices, a subtype of base arrays), and more types and frameworks such as torch in the future.

## Installation

`einops` is available on CRAN:

```R
install.packages("einops")
```

For the latest semi-stable development version try:

```R
devtools::install_github("Qile0317/einops@dev")
```

## Usage

`einops` has a minimalistic yet powerful API.

Three core operations provided ([einops tutorial](https://qile0317.github.io/einops/articles/basics.html)
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

## Vignettes

The vignette is the most convenient way to see `einops` in action

- read it using [`vignette("basics", package = "einops")`](https://qile0317.github.io/einops/articles/basics.html)

Kapil Sachdeva recorded a small [intro to einops](https://www.youtube.com/watch?v=xGy75Pjsqzo) for the python version.

## Naming

`einops` stands for Einstein-Inspired Notation for operations 
(though "Einstein operations" is more attractive and easier to remember).

Notation was loosely inspired by Einstein summation (in particular by `numpy.einsum` operation).

## Why use `einops` notation

### Semantic information (being verbose in expectations)

```R
y <- x; dim(y) <- c(dim(y)[1], prod(dim(y)[-1]))
y <- rearrange(x, 'b c h w -> b (c h w)')
```

While these two lines are doing the same job in *some* context,
the second one provides information about the input and output.
In other words, `einops` focuses on interface: *what is the input and output*, not *how* the output is computed.

The next operation looks similar:

```R
y <- rearrange(x, 'time c h w -> time (c h w)')
```

but it gives the reader a hint:
this is not an independent batch of images we are processing,
but rather a sequence (video).

Semantic information makes the code easier to read and maintain.

### Convenient checks

Reconsider the same example:

```R
y <- x; dim(y) <- c(dim(y)[1], prod(dim(y)[-1])) # x: (batch, 256, 19, 19)
y <- rearrange(x, 'b c h w -> b (c h w)')
```

The second line checks that the input has four dimensions,
but you can also specify particular dimensions.
That's opposed to just writing comments about shapes since comments don't prevent mistakes,
not tested, and without code review tend to be outdated

```R
y <- x; dim(y) <- c(dim(y)[1], prod(dim(y)[-1])) # x: (batch, 256, 19, 19)
y = rearrange(x, 'b c h w -> b (c h w)', c = 256, h = 19, w = 19)
```

### Result is strictly determined

Below we have at least two ways to define the depth-to-space operation

```R
# depth-to-space
rearrange(x, 'b c (h h2) (w w2) -> b (c h2 w2) h w', h2 = 2, w2 = 2)
rearrange(x, 'b c (h h2) (w w2) -> b (h2 w2 c) h w', h2 = 2, w2 = 2)
```

There are at least four more ways to do it. Which one is used by the underlying code?

These details are ignored, since *usually* it makes no difference,
but it can make a big difference (e.g. if you use grouped convolutions in the next stage),
and you'd like to specify this in your code.

### Uniformity

```R
reduce(x, 'b c (x dx) -> b c x', 'max', dx = 2)
reduce(x, 'b c (x dx) (y dy) -> b c x y', 'max', dx = 2, dy = 3)
reduce(x, 'b c (x dx) (y dy) (z dz) -> b c x y z', 'max', dx = 2, dy = 3, dz = 4)
```

These examples demonstrated that we don't use separate operations for 1d/2d/3d pooling,
those are all defined in a uniform way.

Space-to-depth and depth-to space are defined in many frameworks but how about width-to-height? Here you go:

```R
rearrange(x, 'b c h (w w2) -> b c (h w2) w', w2 = 2)
```

## Citing Einops (for R)

If using the R implementation of Einops in your research work, please cite the original ICLR paper, in addition to this package with these bibtex entries:

```bibtex
@Misc{yang2025einops,
    title = {Einops for R},
    author = {Qile Yang},
    year = {2025},
    url = {https://qile0317.github.io/einops/},
}

@inproceedings{rogozhnikov2022einops,
    title={Einops: Clear and Reliable Tensor Manipulations with Einstein-like Notation},
    author={Alex Rogozhnikov},
    booktitle={International Conference on Learning Representations},
    year={2022},
    url={https://openreview.net/forum?id=oapKSVM2bcj}
}
```

## Development

A [Devcontainer](.devcontainer/devcontainer.json) and [Dockerfile](./Dockerfile) is provided, this environment can be used locally, or on your server, or within github codespaces. To start with devcontainers in vs code, clone repo, and click 'Reopen in Devcontainer'.

All building, installing, and documentation can be generated with the standard tidyverse [`devtools`](https://devtools.r-lib.org/) pipeline, using functions such as `devtools::document()` and `devtools::test()` 

If you'd like to contribute, Github pull requests from forked branches are more than welcome as it is mostly a solo-project at the moment. For major changes, please open an issue first to discuss what you would like to change. Please also make sure to update tests as appropriate.
