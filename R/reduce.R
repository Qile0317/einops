#' @title
#' Rearrangement and reduction in one step
#'
#' @description
#' [einops::reduce()] combines rearrangement and reduction using
#' reader-friendly notation.
#'
#' @param x tensor: array, matrix, or list of arrays of the same shape and type
#' @param expr string: reduction pattern
#' @param func string or function: one of available reductions ('min', 'max',
#' 'sum', 'mean', 'prod', 'any', 'all'), or an R function (e.g. max, mean,
#' prod, etc.)
#' @param ... either corresponding axes lengths or a single list of them.
#' @param .row_major logical: whether to use row-major order for the output
#' tensor. If `TRUE`, the *operation* is performed in row-major order, but the
#' output will be in whatever order the parent framework uses (e.g. column-major
#' for [base::array()]).
#'
#' @return tensor of the same type as input, with dimensions according to output
#' pattern
#' @export
#'
#' @examples
#' # Suppose x is a 3D array: 100 x 32 x 64
#' x <- array(rnorm(100 * 32 * 64), dim = c(100, 32, 64))
#'
#' # perform max-reduction on the first axis
#' # Axis t does not appear on RHS - thus we reduced over t
#' y <- reduce(x, 't b c -> b c', 'max')
#'
#' # same as previous, but using verbose names for axes
#' y <- reduce(x, 'time batch channel -> batch channel', 'max')
#'
#' # let's pretend now that x is a batch of images
#' # with 4 dims: batch=10, height=20, width=30, channel=40
#' x <- array(rnorm(10 * 20 * 30 * 40), dim = c(10, 20, 30, 40))
#'
#' # 2d max-pooling with kernel size = 2 * 2 for image processing
#' y1 <- reduce(x, 'b c (h1 h2) (w1 w2) -> b c h1 w1', 'max', h2=2, w2=2)
#'
#' # same as previous, using anonymous axes,
#' # note: only reduced axes can be anonymous
#' y1 <- reduce(x, 'b c (h1 2) (w1 2) -> b c h1 w1', 'max')
#'
#' # adaptive 2d max-pooling to 3 * 4 grid,
#' # each element is max of 10x10 tile in the original tensor.
#' dim(reduce(x, 'b c (h1 h2) (w1 w2) -> b c h1 w1', 'max', h1=3, w1=4))
#' # (10, 20, 3, 4)
#'
#' # Global average pooling
#' dim(reduce(x, 'b c h w -> b c', 'mean'))
#' # (10, 20)
#'
#' # subtracting mean over batch for each channel;
#' # similar to x - apply(x, c(2,3,4), mean)
#' y <- x - reduce(x, 'b c h w -> 1 c 1 1', 'mean')
#'
#' # Subtracting per-image mean for each channel
#' y <- x - reduce(x, 'b c h w -> b c 1 1', 'mean')
#'
#' # same as previous, but using empty compositions
#' y <- x - reduce(x, 'b c h w -> b c () ()', 'mean')
#'
reduce <- function(
    x, expr, func, ..., .row_major = getOption("einops_row_major", FALSE)
) {
    UseMethod("reduce")
}

#' @rdname reduce
#' @export
einops.reduce <- reduce # nolint: object_name_linter.

#' @export
reduce.default <- function(
    x, expr, func, ..., .row_major = getOption("einops_row_major", FALSE)
) {
    if (identical(Sys.getenv("TESTTHAT"), "true")) {
        return(.reduce(x, expr, func, ..., .row_major = .row_major))
    }
    tryCatch(
        .reduce(x, expr, func, ..., .row_major = .row_major),
        error = function(e) {
            stop("In Einops - ", conditionMessage(e), call. = FALSE)
        }
    )
}

#' @export
reduce.list <- function(
    x, expr, func, ..., .row_major = getOption("einops_row_major", FALSE)
) {
    if (length(x) == 0) {
        stop("Rearrange/Reduce/Repeat can't be applied to an empty list")
    }
    backend <- get_backend(x[[1]])
    reduce(
        backend$stack_on_zeroth_dimension(x),
        expr,
        func,
        ...,
        .row_major = .row_major
    )
}

.reduce <- function(x, expr, func, ..., .row_major) {

    axes_lengths <- list(...)
    if (length(axes_lengths) == 1L && is.list(axes_lengths[[1]])) {
        axes_lengths <- axes_lengths[[1]]
    }
    
    backend <- get_backend(x)

    hashable_axes_lengths <- axes_lengths

    axes_names <- if (length(axes_lengths) == 0L)
        character()
    else
        names(axes_lengths)

    recipe <- prepare_transformation_recipe(
        expr, func, axes_names = axes_names, ndim = length(backend$shape(x))
    )

    apply_recipe(
        backend,
        recipe,
        x,
        reduction_type = func,
        axes_lengths = hashable_axes_lengths
    )
}
