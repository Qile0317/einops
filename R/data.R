#' @title Example 4D Image Tensor for Einops
#' @description
#' An [image_tensor()] object that is a super thin wrapper around a 4D
#' [base::array()], representing image data in the format "b h w c"
#' (batch, height, width, channels). The actual image data is 6 black letters
#' on differently colored backgrounds, spelling out the word "einops". When
#' printing this object in the terminal it will automatically plot the images.
#' To subset, use the `[` operator, and when used with a single index, it
#' will return a single image tensor.
#' @format An [image_tensor()] object with 6 images, each of size 96 x 96
#' pixels, with 3 color channels (RGB). The images are stored in a 4D array
#' with dimensions (6, 96, 96, 3).
#' @seealso [as.cimg()]
#' @examples
#' data("einops_image")
#' for (i in seq_len(6)) print(einops_image[i, , , ])
"einops_image"
