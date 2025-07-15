#' @name image_tensor
#' @title Image Tensor S3 Class
#'
#' @description
#' The `image_tensor` class provides a convenient way to work with image data
#' in tensor format. It extends the base `array` class and provides methods
#' for conversion to/from various image formats, plotting, and printing.
#'
#' An `image_tensor` object represents image data in the format "b h w c"
#' (batch, height, width, channels), which is a common format for deep learning
#' frameworks.
#'
#' @param x An object to convert to or from `image_tensor` format.
#' @param axes Logical. Whether to show axes in the plot. Default is controlled
#'   by the option `plot_image_tensor_axes` (default: FALSE).
#' @param as_image Logical. Whether to print the image as a plot. Default is
#'   controlled by the option `print_image_tensor_as_plot` (default: TRUE).
#' @param ... Additional arguments passed to underlying methods.
#'
#' @details
#' The `image_tensor` class provides the following methods:
#'
#' \describe{
#'   \item{`as_image_tensor()`}{Generic function to convert objects to `image_tensor` format}
#'   \item{`as_image_tensor.default()`}{Default method that converts arrays to `image_tensor`}
#'   \item{`as_image_tensor.cimg()`}{Method to convert `cimg` objects (from imager package) to `image_tensor`}
#'   \item{`as.cimg.image_tensor()`}{Method to convert `image_tensor` objects back to `cimg` format}
#'   \item{`plot.image_tensor()`}{Plot method for `image_tensor` objects}
#'   \item{`print.image_tensor()`}{Print method for `image_tensor` objects}
#' }
#'
#' @section Format:
#' An `image_tensor` object is an array with dimensions in "b h w c" format:
#' \itemize{
#'   \item \strong{b}: batch dimension (number of images)
#'   \item \strong{h}: height dimension (image height in pixels)
#'   \item \strong{w}: width dimension (image width in pixels)
#'   \item \strong{c}: channel dimension (e.g., 3 for RGB, 1 for grayscale)
#' }
#'
#' @section Options:
#' The following options control the behavior of `image_tensor` methods:
#' \itemize{
#'   \item \code{plot_image_tensor_axes}: Whether to show axes in plots (default: FALSE)
#'   \item \code{print_image_tensor_as_plot}: Whether to print images as plots (default: TRUE)
#' }
#'
#' @return 
#' \itemize{
#'   \item \code{as_image_tensor()}: An object of class `image_tensor`
#'   \item \code{as.cimg()}: A `cimg` object (from imager package)
#'   \item \code{plot()}: Invisibly returns the input object
#'   \item \code{print()}: Invisibly returns the input object
#' }
#'
#' @examples
#' \dontrun{
#' # Create an image tensor from an array
#' arr <- array(runif(2 * 100 * 100 * 3), dim = c(2, 100, 100, 3))
#' img_tensor <- as_image_tensor(arr)
#' 
#' # Print the image tensor (will plot by default)
#' print(img_tensor)
#' 
#' # Print as array structure instead of plot
#' print(img_tensor, as_image = FALSE)
#' 
#' # Plot with axes
#' plot(img_tensor, axes = TRUE)
#' 
#' # Convert to cimg format (requires imager package)
#' if (requireNamespace("imager", quietly = TRUE)) {
#'   cimg_obj <- as.cimg(img_tensor)
#' }
#' }
#' @keywords internal
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

as.cimg <- function(x) {
    UseMethod("as.cimg", x)
}

#' @rdname image_tensor
#' @export
as.cimg.image_tensor <- function(x) {
    imager::as.cimg(rearrange(x, "b h w c -> w h b c"))
}

#' @rdname image_tensor
#' @export
plot.image_tensor <- function(
    x, axes = getOption("plot_image_tensor_axes", FALSE), ...
) {
    plot(as.cimg(x), axes = axes, ...)
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
