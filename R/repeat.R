#' einops.repeat allows reordering elements and repeating them in arbitrary combinations.
#' This operation includes functionality of repeat, tile, and broadcast functions.
#'
#' When composing axes, C-order enumeration is used (consecutive elements have different last axis).
#' Find more examples in the vignettes.
#'
#' @param x tensor of any supported library.
#'          list of tensors is also accepted, those should be of the same type and shape
#' @param expr string, rearrangement pattern
#' @param ... any additional specifications for dimensions
#'
#' @return tensor of the same type as input. If possible, a view to the original tensor is returned.
#' @export
#'
#' @examples
#' # a grayscale image (of shape height x width)
#' image <- array(rnorm(30*40), dim = c(30, 40))
#'
#' # change it to RGB format by repeating in each channel
#' einops_repeat(image, 'h w -> h w c', c=3)
#'
#' # repeat image 2 times along height (vertical axis)
#' einops_repeat(image, 'h w -> (repeat h) w', repeat=2)
#'
#' # repeat image 2 times along height and 3 times along width
#' einops_repeat(image, 'h w -> (h2 h) (w3 w)', h2=2, w3=3)
#'
#' # convert each pixel to a small square 2x2, i.e. upsample an image by 2x
#' einops_repeat(image, 'h w -> (h h2) (w w2)', h2=2, w2=2)
#'
#' # 'pixelate' an image first by downsampling by 2x, then upsampling
#' # downsampled <- reduce(image, '(h h2) (w w2) -> h w', 'mean', h2=2, w2=2)
#' # einops_repeat(downsampled, 'h w -> (h h2) (w w2)', h2=2, w2=2)
einops_repeat <- function(x, expr, ...) {
    UseMethod("einops_repeat")
}
