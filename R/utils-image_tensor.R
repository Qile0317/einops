#' @name image_tensor
#' @title
#' Image Tensor: A thin wrapper around 4D arrays
#'
#' @description
#' The `image_tensor` class provides a convenient way to work with image data
#' in tensor format. It extends the base `array` class and provides methods
#' for conversion to/from various image formats, plotting, and printing.
#'
#' An `image_tensor` object represents image data in the format "b h w c"
#' (batch, height, width, channels), which is a common format for deep
#' learning frameworks.
#'
#' The main utility of wrapping image data in the `image_tensor` class is that
#' printing of the object will automatically display the image as a plot.
#'
#' @param x An object to convert to or from `image_tensor` format.
#' @param ... Additional arguments passed to underlying methods. For `[` and
#'   these are indexing arguments.
#' @param axes Logical. Whether to show axes in the plot. Default is
#'   controlled by the option `plot_image_tensor_axes` (default: FALSE).
#' @param as_image Logical. Whether to print the image as a plot. Default is
#'   controlled by the option `print_image_tensor_as_plot` (default: TRUE).
#'
#' @details
#' The `image_tensor` class provides the following methods:
#'
#' - `as_image_tensor()`: Generic function to convert objects to
#'   `image_tensor` format
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
#' An `image_tensor` object is an array with dimensions in "b h w c" format:
#'
#' - **b**: batch dimension (number of images)
#' - **h**: height dimension (image height in pixels)
#' - **w**: width dimension (image width in pixels)
#' - **c**: channel dimension (e.g., 3 for RGB, 1 for grayscale)
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
#' - `[.image_tensor()`: A subset of the `image_tensor` object, also of class
#'   `image_tensor`
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
    imager::as.cimg(rearrange(x, "b h w c -> w h b c"))
}

#' @rdname image_tensor
#' @export
"[.image_tensor" <- function(x, ...) {

    original_class <- class(x)
    args <- list(...)

    if (length(args) == 1) {
        return(as_image_tensor(unclass(x)[args[[1]], , , , drop = FALSE]))
    }

    result <- NextMethod("[", drop = FALSE)
    result_dims <- dim(result)
    if (length(result_dims) != 4) {
        stop("Subsetting resulted in unexpected dimensionality")
    }

    class(result) <- original_class
    result
}

#' @rdname image_tensor
#' @export
plot.image_tensor <- function(
    x, axes = getOption("plot_image_tensor_axes", FALSE), ...
) {
    grid::grid.raster(as.cimg(x), ...)
}

#' @rdname image_tensor
#' @export
print.image_tensor <- function(
    x, as_image = getOption("print_image_tensor_as_plot", TRUE), ...
) {
    if (as_image) {
        plot(x, ...)
        return(invisible(x))
    }
    print(imager::as.cimg(unclass(x)))
}
