as_image_tensor <- function(x) {
    UseMethod("as_image_tensor", x)
}

#' @export
as_image_tensor.default <- function(x) {
    x <- as.array(x)
    class(x) <- c("image_tensor", "array")
    x
}

#' @export
as_image_tensor.cimg <- function(x) {
    as_image_tensor(rearrange(x, "w h b c -> b h w c"))
}

as.cimg <- function(x) {
    UseMethod("as.cimg", x)
}

#' @export
as.cimg.image_tensor <- function(x) {
    imager::as.cimg(rearrange(x, "b h w c -> w h b c"))
}

#' @export
plot.image_tensor <- function(
    x, axes = getOption("plot_image_tensor_axes", FALSE), ...
) {
    plot(as.cimg(x), axes = axes, ...)
}

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
