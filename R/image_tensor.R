#' @name image_tensor
#' @title
#' Image Tensor: A thin wrapper around 3D and 4D arrays
#'
#' @description
#' The `image_tensor` class provides a convenient way to work with image data
#' in tensor format. It extends the base `array` class and provides methods
#' for conversion to/from various image formats, plotting, and printing.
#'
#' An `image_tensor` object represents image data in the format "h w c"
#' (height, width, channels) for single images, or "b h w c"
#' (batch, height, width, channels) for batches of images, which is a common
#' format for deep learning frameworks.
#'
#' The main utility of wrapping image data in the `image_tensor` class is that
#' printing of the object will automatically display the image as a plot.
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
    dims <- length(dim(x))

    if (dims == 2L) x %<>% einops.repeat(x, "h w -> h w 3")
    if (dims != 3 && dims != 4) {
        stop("image_tensor objects must be 3D (h w c) or 4D (b h w c) arrays")
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
    x_dims <- length(dim(x))
    x <- unclass(x)
    
    if (x_dims == 3) {
        # For 3D arrays (h w c), add batch dimension before rearranging
        imager::as.cimg(rearrange(x, "h w c -> w h 1 c"))
    } else if (x_dims == 4) {
        # For 4D arrays (b h w c)
        imager::as.cimg(rearrange(x, "b h w c -> w h b c"))
    } else {
        stop("image_tensor must be 3D or 4D")
    }
}

#' @rdname image_tensor
#' @export
"[.image_tensor" <- function(x, ...) {

    original_class <- class(x)
    args <- list(...)
    x_dims <- length(dim(x))

    # Handle single argument subsetting for 4D arrays
    if (length(args) == 1 && x_dims == 4) {
        # Return 3D slice without batch dimension
        result <- unclass(x)[args[[1]], , , , drop = FALSE]
        result <- array(result, dim = dim(result)[-1])  # Remove first dimension
        class(result) <- original_class
        return(result)
    }

    result <- NextMethod("[", drop = FALSE)
    result_dims <- length(dim(result))
    
    # Validate dimensionality
    if (result_dims != 3 && result_dims != 4) {
        stop("Subsetting resulted in unexpected dimensionality")
    }

    class(result) <- original_class
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
    if (as_image) {
        plot.image_tensor(x, ...)
        return(invisible(x))
    }
    print(as.cimg(x))
}

#' @export
reduce.image_tensor <- function(x, expr, func, ...) {
    class(x) <- "array"
    result <- reduce(x, expr, func, ...)
    if (FastUtils::isBound(length(dim(result)), 3, 4)) {
        result <- as_image_tensor(result)
    }
    result
}
