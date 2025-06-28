#' @title
#' Reader-friendly smart element reordering for multidimensional tensors.
#'
#' @description
#' This operation includes functionality of transpose (axes permutation),
#' reshape (view), squeeze, unsqueeze, stack, concatenate and other operations.
#'
#' @details
#' When composing axes, C-order enumeration is used (consecutive elements
#' have different last axis). Find more examples in the vignettes.
#'
#' @inheritParams reduce
#' @inherit reduce return
#' @export
#'
#' @examples
#' # suppose we have a set of 32 images in "h w c" format (height-width-channel)
#' images <- lapply(1:32, function(i) array(rnorm(30*40*3), dim = c(30, 40, 3)))
#'
#' # stack along first (batch) axis, output is a single array
#' rearrange(images, 'b h w c -> b h w c')
#'
#' # stacked and reordered axes to "b c h w" format
#' rearrange(images, 'b h w c -> b c h w')
#'
#' # concatenate images along height (vertical axis), 960 = 32 * 30
#' rearrange(images, 'b h w c -> (b h) w c')
#'
#' # concatenated images along horizontal axis, 1280 = 32 * 40
#' rearrange(images, 'b h w c -> h (b w) c')
#'
#' # flattened each image into a vector, 3600 = 30 * 40 * 3
#' rearrange(images, 'b h w c -> b (c h w)')
#'
#' # split each image into 4 smaller (top-left, top-right, bottom-left, bottom-right), 128 = 32 * 2 * 2
#' rearrange(images, 'b (h1 h) (w1 w) c -> (b h1 w1) h w c', h1=2, w1=2)
#'
#' # space-to-depth operation
#' rearrange(images, 'b (h h1) (w w1) c -> b h w (c h1 w1)', h1=2, w1=2)
rearrange <- function(x, expr, ...) {
    reduce(x, expr, "rearrange", ...)
}
