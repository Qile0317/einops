#' @title
#' Allows reordering elements and repeating them in arbitrary combinations.
#'
#' @description
#' This operation includes functionality of repeat, tile, and broadcast
#' functions.
#'
#' @inherit rearrange details
#' @inheritParams reduce
#' @inherit reduce return
#' @export
#'
#' @section Why can't the function be called as `repeat()`?:
#' `repeat` is a reserved keyword in R that acts the same as
#' `while(TRUE)`, and has no way of being overridden. Hence,
#' this function can only be called as `einops.repeat()` or
#' using backticks as \code{`repeat`()}.
#'
#' @examples
#' set.seed(42)
#' # a grayscale image (of shape height x width)
#' image <- array(rnorm(30 * 40), dim = c(30, 40))
#'
#' # change it to RGB format by repeating in each channel
#' output <- einops.repeat(image, 'h w -> h w c', c=3)
#' # Visualize the output
#' as_image_tensor(output)
#'
#' # repeat image 2 times along height (vertical axis)
#' output <- einops.repeat(image, 'h w -> (r h) w', r=2)
#' as_image_tensor(output)
#'
#' # repeat image 2 times along height and 3 times along width
#' output <- einops.repeat(image, 'h w -> (h2 h) (w3 w)', h2=2, w3=3)
#' as_image_tensor(output)
#'
#' # convert each pixel to a small square 2x2, i.e. upsample an image by 2x
#' output <- einops.repeat(image, 'h w -> (h h2) (w w2)', h2=2, w2=2)
#' as_image_tensor(output)
#'
#' # 'pixelate' an image first by downsampling by 2x, then upsampling
#' # downsampled <- reduce(image, '(h h2) (w w2) -> h w', 'mean', h2=2, w2=2)
#' # output <- einops.repeat(downsampled, 'h w -> (h h2) (w w2)', h2=2, w2=2)
#' # as_image_tensor(output)
#'
einops.repeat <- function( # nolint: object_name_linter.
    x, expr, ..., .row_major = getOption("einops_row_major", FALSE)
) {
    reduce(x, expr, "repeat", ..., .row_major = .row_major)
}

#' @rdname einops.repeat
#' @export
`repeat` <- einops.repeat
