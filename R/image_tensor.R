#' @name image_tensor
#' @title
#' Image Tensor: A thin wrapper around 2-4D arrays
#'
#' @description
#' The `image_tensor` class provides a convenient way to work with image data
#' in tensor format. It extends the base `array` class and provides methods
#' for conversion to/from various image formats, plotting, and printing.
#'
#' An `image_tensor` object represents image data in the format "h w c"
#' (height, width, channels) for single images, or "b h w c"
#' (batch, height, width, channels) for batches of images, which is a common
#' format for deep learning frameworks. It also can be a 2D array, in which
#' case it is treated as a black and white image and shown as such.
#'
#' The main utility of wrapping image data in the `image_tensor` class is that
#' printing of the object will automatically display the image as a plot,
#' as long as the `imager` package is installed. Otherwise it will simply
#' print the dimension of the image.
#'
#' @param x An object to convert to or from `image_tensor` format.
#' @param ... Additional arguments passed to underlying methods. For `[` and
#'   these are indexing arguments.
#' @param as_image Logical. Whether to print the image as a plot. Default is
#'   controlled by the option `print_image_tensor_as_plot` (default: TRUE).
#'
#' @details
#' The `image_tensor` class provides the following methods:
#'
#' - `as_image_tensor()`: Generic function to convert objects to
#'   `image_tensor` format. Takes in array-like objects of 2-4 dimensions.
#'   for 2 dimensional objects, it will convert them to 3D by repeating the
#'   data across 3 channels, essentially converting grayscale images to RGB.
#' - `as_image_tensor.default()`: Default method that converts arrays to
#'   `image_tensor`
#' - `as_image_tensor.cimg()`: Method to convert `cimg` objects (from imager
#'   package) to `image_tensor`
#' - `as.cimg.image_tensor()`: Method to convert `image_tensor` objects back
#'   to `cimg` format
#' - `[.image_tensor()`: Subset method for `image_tensor` objects
#' - `plot.image_tensor()`: Plot method for `image_tensor` objects
#' - `print.image_tensor()`: Print method for `image_tensor` objects
#'
#' @section Format:
#' An `image_tensor` object is an array with dimensions in "h w c" format for
#' single images, or "b h w c" format for batches of images:
#'
#' - **h**: height dimension (image height in pixels)
#' - **w**: width dimension (image width in pixels)
#' - **c**: channel dimension (e.g., 3 for RGB, 1 for grayscale)
#' - **b**: batch dimension (number of images, only for 4D arrays)
#'
#' @section Options:
#' The following options control the behavior of `image_tensor` methods:
#'
#' - `plot_image_tensor_axes`: Whether to show axes in plots (default: FALSE)
#' - `print_image_tensor_as_plot`: Whether to print images as plots (default:
#'   TRUE)
#'
#' @return
#' - `as_image_tensor()`: An object of class `image_tensor`
#' - `as.cimg()`: A `cimg` object (from imager package)
#' - `[.image_tensor()`: A subset of the `image_tensor` object. For 4D arrays
#'   with single index, returns a 3D slice without the batch dimension.
#' - `plot()`: Invisibly returns the input object
#' - `print()`: Invisibly returns the input object
#' @export
as_image_tensor <- function(x) {
    UseMethod("as_image_tensor", x)
}

#' @rdname image_tensor
#' @export
as_image_tensor.default <- function(x) {
    x <- as.array(x)
    if (!FastUtils::isBound(length(dim(x)), 2, 4)) {
        stop("input must be a 2D (h w), 3D (h w c) or 4D (b h w c) array")
    }
    class(x) <- c("image_tensor", "array")
    x
}

#' @rdname image_tensor
#' @export
as_image_tensor.cimg <- function(x) {
    as_image_tensor(rearrange(x, "w h b c -> b h w c"))
}

as.cimg <- function(x) { # nolint: object_name_linter.
    UseMethod("as.cimg", x)
}

#' @rdname image_tensor
#' @export
as.cimg.image_tensor <- function(x) {
    requireNamespace("imager")
    x <- unclass(x)
    if (length(dim(x)) == 2L) x %<>% einops.repeat("h w -> h w 3")
    if (length(dim(x)) == 3L) {
        imager::as.cimg(rearrange(x, "h w c -> w h 1 c"))
    } else if (length(dim(x)) == 4L) {
        imager::as.cimg(rearrange(x, "b h w c -> w h b c"))
    } else {
        stop("image_tensor must be 2-4D")
    }
}

#' @rdname image_tensor
#' @export
"[.image_tensor" <- function(x, ...) {
    original_class <- class(x)
    result <- unclass(x)[...]
    result_dims <- length(dim(result))
    if (FastUtils::isBound(result_dims, 2, 4)) {
        class(result) <- original_class
    } else {
        result <- unclass(result)
    }
    result
}

#' @rdname image_tensor
#' @export
plot.image_tensor <- function(x, ...) {
    x_dims <- length(dim(x))
    if (x_dims == 4L) {
        if (dim(x)[1] > 1) {
            warning("Multiple images in batch, plotting only the first one")
        }
        grid::grid.raster(as.cimg(x[1, , , , drop = FALSE]), ...)
    } else {
        grid::grid.raster(as.cimg(x), ...)
    }
}

#' @rdname image_tensor
#' @export
print.image_tensor <- function(
    x, as_image = getOption("print_image_tensor_as_plot", TRUE), ...
) {
    if (!requireNamespace("imager", quietly = TRUE)) {
        print(glue("<image_tensor array with shape {repr(dim(x))}>"))
        return(invisible(x))
    }
    if (as_image) {
        plot.image_tensor(x, ...)
        return(invisible(x))
    }
    print(as.cimg(x))
}

#' @export
reduce.image_tensor <- function(x, ...) {
    class(x) <- "array"
    result <- reduce(x, ...)
    if (FastUtils::isBound(length(dim(result)), 2, 4)) {
        result <- as_image_tensor(result)
    }
    result
}

#' @export
.reduce.list.image_tensor <- function(
    x, expr, func, ..., .row_major = getOption("einops_row_major", FALSE)
) {
    as_image_tensor(.reduce.list.default(
        x, expr, func, ..., .row_major = .row_major
    ))
}
