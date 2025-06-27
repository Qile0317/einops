# # Test suite for reduce() function with arrays

# create_test_array <- function(dims, fill_value = NULL) {
#   if (is.null(fill_value)) {
#     arr <- array(1:prod(dims), dim = dims)
#   } else {
#     arr <- array(fill_value, dim = dims)
#   }
#   return(arr)
# }

# test_that("reduce works for arrays", {
#   # 2D reductions
#   arr <- create_test_array(c(3, 4))
  
#   result1 <- reduce(arr, "h w -> w", "sum")
#   expect_equal(dim(result1), 4)
#   expect_equal(as.vector(result1), colSums(arr))
  
#   result2 <- reduce(arr, "h w -> h", "sum")
#   expect_equal(dim(result2), 3)
#   expect_equal(as.vector(result2), rowSums(arr))
  
#   # 3D reductions
#   arr3d <- create_test_array(c(2, 3, 4))
  
#   result4 <- reduce(arr3d, "b h w -> h w", "sum")
#   expect_equal(dim(result4), c(3, 4))
#   expect_equal(result4, apply(arr3d, c(2, 3), sum))
  
#   result5 <- reduce(arr3d, "b h w -> b w", "sum")
#   expect_equal(dim(result5), c(2, 4))
#   expect_equal(result5, apply(arr3d, c(1, 3), sum))
  
#   result6 <- reduce(arr3d, "b h w -> b h", "sum")
#   expect_equal(dim(result6), c(2, 3))
#   expect_equal(result6, apply(arr3d, c(1, 2), sum))
  
#   result7 <- reduce(arr3d, "b h w -> b", "sum")
#   expect_equal(dim(result7), 2)
#   expect_equal(as.vector(result7), apply(arr3d, 1, sum))
  
#   # Different reduction functions
#   result_mean <- reduce(arr, "h w -> h", "mean")
#   expect_equal(as.vector(result_mean), rowMeans(arr))
  
#   result_max <- reduce(arr, "h w -> h", "max")
#   expect_equal(as.vector(result_max), apply(arr, 1, max))
  
#   result_min <- reduce(arr, "h w -> h", "min")
#   expect_equal(as.vector(result_min), apply(arr, 1, min))
  
#   # Parentheses grouping
#   arr4d <- create_test_array(c(2, 3, 4, 5))
  
#   result8 <- reduce(arr4d, "(b h) w c -> w c", "sum")
#   expect_equal(dim(result8), c(4, 5))
#   reshaped <- array(arr4d, dim = c(6, 4, 5))
#   expected8 <- apply(reshaped, c(2, 3), sum)
#   expect_equal(result8, expected8)
  
#   result9 <- reduce(arr4d, "b h (w c) -> b h", "sum")
#   expect_equal(dim(result9), c(2, 3))
#   reshaped2 <- array(arr4d, dim = c(2, 3, 20))
#   expected9 <- apply(reshaped2, c(1, 2), sum)
#   expect_equal(result9, expected9)
  
#   # Ellipsis
#   result10 <- reduce(arr3d, "... w ->...", "sum")
#   expect_equal(dim(result10), c(2, 3))
#   expect_equal(result10, apply(arr3d, c(1, 2), sum))
  
#   arr5d <- create_test_array(c(2, 3, 4, 5))
#   result11 <- reduce(arr5d, "... c -> ...", "sum")
#   expect_equal(dim(result11), c(2, 3, 4))
#   expect_equal(result11, apply(arr5d, c(1, 2, 3), sum))
  
#   result12 <- reduce(arr5d, "b ... -> ...", "sum")
#   expect_equal(dim(result12), c(3, 4, 5))
#   expect_equal(result12, apply(arr5d, c(2, 3, 4), sum))
  
#   # Mixed patterns
#   arr6d <- create_test_array(c(2, 3, 4, 5, 6))
  
#   result13 <- reduce(arr6d, "b c h w d -> c (h w)", "sum")
#   expect_equal(dim(result13), c(3, 20))
#   temp <- apply(arr6d, c(2, 3, 4), sum)
#   expected13 <- array(temp, dim = c(3, 20))
#   expect_equal(result13, expected13)
  
#   result14 <- reduce(arr6d, "b c h w d -> (b c) d", "sum")
#   expect_equal(dim(result14), c(6, 6))
#   temp2 <- apply(arr6d, c(1, 2, 5), sum)
#   expected14 <- array(temp2, dim = c(6, 6))
#   expect_equal(result14, expected14)
  
#   # 1D arrays
#   arr1d <- create_test_array(10)
#   result15 <- reduce(arr1d, "n ->", "sum")
#   expect_equal(length(result15), 1)
#   expect_equal(result15, sum(arr1d))
  
#   # High dimensional arrays
#   arr7d <- create_test_array(c(2, 2, 2, 2, 2, 2))
#   result16 <- reduce(arr7d, "a b c d e f -> a c e", "sum")
#   expect_equal(dim(result16), c(2, 2, 2))
#   expected16 <- apply(arr7d, c(1, 3, 5), sum)
#   expect_equal(result16, expected16)
  
#   # Array attributes preservation
#   arr_named <- create_test_array(c(3, 4))
#   dimnames(arr_named) <- list(paste0("row", 1:3), paste0("col", 1:4))
#   result17 <- reduce(arr_named, "h w -> h", "sum")
#   expect_equal(names(dimnames(result17)), "row")
#   expect_equal(dimnames(result17)[[1]], paste0("row", 1:3))
  
#   # Different storage modes
#   arr_int <- array(1:12, dim = c(3, 4))
#   result_int <- reduce(arr_int, "h w -> h", "sum")
#   expect_type(result_int, "integer")
  
#   arr_logical <- array(c(TRUE, FALSE), dim = c(2, 3))
#   result_logical <- reduce(arr_logical, "h w -> h", "sum")
#   expect_type(result_logical, "integer")
  
#   arr_complex <- array(1:6 + 1i, dim = c(2, 3))
#   result_complex <- reduce(arr_complex, "h w -> h", "sum")
#   expect_type(result_complex, "complex")
  
#   # Edge cases
#   arr_single <- array(42, dim = c(1, 1))
#   result_single <- reduce(arr_single, "h w ->", "sum")
#   expect_equal(result_single, 42)
  
#   arr_unit <- create_test_array(c(1, 5))
#   result_unit <- reduce(arr_unit, "h w -> w", "sum")
#   expect_equal(dim(result_unit), 5)
#   expect_equal(as.vector(result_unit), as.vector(arr_unit))
  
#   # Additional function arguments
#   arr_na <- create_test_array(c(3, 4))
#   arr_na[1, 1] <- NA
  
#   result_na_rm <- reduce(arr_na, "h w ->", "sum", na.rm = TRUE)
#   expect_equal(result_na_rm, sum(arr_na, na.rm = TRUE))
  
#   result_na <- reduce(arr_na, "h w ->", "sum")
#   expect_true(is.na(result_na))
  
#   # Function validation
#   expect_error(reduce(arr, "h w -> h", 123), "func must be a character string")
#   expect_error(reduce(arr, "h w -> h", c("sum", "mean")), "func must be a character string")
# })

# test_that("reduce works for image processing patterns", {
#   # Simulate batch of images: batch=10, height=20, width=30, channel=40
#   x <- create_test_array(c(10, 20, 30, 40))
  
#   # 2D max-pooling with kernel size 2x2
#   y1 <- reduce(x, 'b c (h1 h2) (w1 w2) -> b c h1 w1', 'max', h2=2, w2=2)
#   expect_equal(dim(y1), c(10, 20, 15, 20))
  
#   # Same pooling using anonymous axes
#   y1_anon <- reduce(x, 'b c (h1 2) (w1 2) -> b c h1 w1', 'max')
#   expect_equal(dim(y1_anon), c(10, 20, 15, 20))
#   expect_equal(y1, y1_anon)
  
#   # Adaptive 2D max-pooling to 3x4 grid
#   y_adaptive <- reduce(x, 'b c (h1 h2) (w1 w2) -> b c h1 w1', 'max', h1=3, w1=4)
#   expect_equal(dim(y_adaptive), c(10, 20, 3, 4))
  
#   # Global average pooling
#   y_global <- reduce(x, 'b c h w -> b c', 'mean')
#   expect_equal(dim(y_global), c(10, 20))
  
#   # Channel-wise mean for broadcasting (across batch, height, width)
#   channel_mean <- reduce(x, 'b c h w -> 1 c 1 1', 'mean')
#   expect_equal(dim(channel_mean), c(1, 20, 1, 1))
  
#   # Per-image channel mean for broadcasting
#   per_image_mean <- reduce(x, 'b c h w -> b c 1 1', 'mean')
#   expect_equal(dim(per_image_mean), c(10, 20, 1, 1))
  
#   # Same as previous using empty compositions
#   per_image_mean_empty <- reduce(x, 'b c h w -> b c () ()', 'mean')
#   expect_equal(dim(per_image_mean_empty), c(10, 20, 1, 1))
#   expect_equal(per_image_mean, per_image_mean_empty)
# })
