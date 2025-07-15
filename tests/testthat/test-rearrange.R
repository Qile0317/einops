# TODO actually check values

identity_patterns <- c(
    "...->...",
    "a b c d e-> a b c d e",
    "a b c d e ...-> ... a b c d e",
    "a b c d e ...-> a ... b c d e",
    "... a b c d e -> ... a b c d e",
    "a ... e-> a ... e",
    "a ... -> a ... "#,
    # "a ... c d e -> a (...) c d e" # FIXME this breaks rearrange
)

# equivalent_rearrange_patterns <- list(
#     list("a b c d e -> (a b) c d e", "a b ... -> (a b) ... "),
#     list("a b c d e -> a b (c d) e", "... c d e -> ... (c d) e"),
#     list("a b c d e -> a b c d e", "... -> ... "),
#     list("a b c d e -> (a b c d e)", "... ->  (...)"),
#     list("a b c d e -> b (c d e) a", "a b ... -> b (...) a"),
#     list("a b c d e -> b (a c d) e", "a b ... e -> b (a ...) e")
# )

for (pattern in identity_patterns) {
    test_in_all_tensor_types_that(glue("rearrange('{pattern}') works"), {
        x <- create_tensor(1:(10 * 20 * 30 * 40 * 50), c(10, 20, 30, 40, 50))
        expect_identical(rearrange(x,  pattern), x)
    })
}

test_in_all_tensor_types_that("rearrange() works", {

    # test 0: matrix transpose
    x <- as.matrix(mtcars)
    expect_equal(
        rearrange(x, "row col -> col row"),
        t(x)
    )
    expect_equal(
        rearrange(x, "... col -> col ..."),
        t(x)
    )

    x <- create_tensor(1:(10 * 20 * 30 * 40), c(10, 20, 30, 40))
    
    # Test 1: transpose
    y1 <- rearrange(x, "b c h w -> b h w c")
    expect_equal(dim(y1), c(10, 30, 40, 20))
    
    # Test 2: view/reshape
    y2 <- rearrange(x, "b c h w -> b (c h w)")
    expect_equal(dim(y2), c(10, 20 * 30 * 40))
    
    # Test 3: depth-to-space
    y3 <- rearrange(x, "b (c h1 w1) h w -> b c (h h1) (w w1)", h1 = 2, w1 = 2)
    expect_equal(dim(y3), c(10, 5, 30 * 2, 40 * 2))
    
    # Test 4: space-to-depth
    y4 <- rearrange(x, "b c (h h1) (w w1) -> b (h1 w1 c) h w", h1 = 2, w1 = 2)
    expect_equal(dim(y4), c(10, 20 * 4, 30 / 2, 40 / 2))
    
    # Test 5: simple transposition
    y5 <- rearrange(x, "b1 sound b2 letter -> b1 b2 sound letter")
    expect_equal(dim(y5), c(10, 30, 20, 40))
    
    # Test 6: parsing parameters
    t <- rearrange(x, "b c h w -> (b h w) c")
    t <- t[, seq(1, ncol(t), by = 2)]
    expect_equal(dim(t), c(10 * 30 * 40, 10))
    y6 <- rearrange(t, "(b h w) c2 -> b c2 h w", parse_shape(x, "b _ h w"))
    expect_equal(dim(y6), c(10, 10, 30, 40))
    
    # Test 7: split of embedding into groups
    # Note: R doesn't have tuple unpacking like Python, so we need to handle differently
    y7_result <- rearrange(x, "b (c g) h w -> g b c h w", g = 2)
    expect_equal(dim(y7_result), c(2, 10, 10, 30, 40))
    
    # Test 10: stack
    x_list <- lapply(1:10, function(i) {
        x[i, , , ] # TODO this may not work for all tensors in the future - may need to define S3 method for a wrapped version
    })
    y10 <- rearrange(x_list, "b c h w -> b h w c")
    expect_equal(dim(y10), c(10, 30, 40, 20))
    
    # Test 11: concatenate
    y11 <- rearrange(x_list, "b c h w -> h (b w) c")
    expect_equal(dim(y11), c(30, 10 * 40, 20))
    
    # TODO rest of tests

})
