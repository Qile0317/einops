# einops (development version)

## Changes

- For CRAN check compatibility and issues with `torch::torch_install()`, torch-related tests auto skip on CRAN.
- Move `lifecycle` from `imports` to `Suggests` since it is a build time developer dependency instead of a user-required dependency.

# einops 0.2.0

## Additions

- Add backend for `torch`'s `torch_tensor` objects for the three core functions `rearrange()`, `reduce()` and `"repeat"()`
- `zeallot` is now a suggested dependency for users
- `lintr` is now a suggested dependency for developers

## Changes

- Minor updates in documentation and docker infrastructure in the sourcecode

# einops 0.1.0

- Initial release. The only python einops function implementations are `rearrange`, `reduce`, `repeat`, and `parse_shape`, and the only backend supported is for `base::array` objects. The first einops tutorial has also been mostly re-implemented as a vignette
