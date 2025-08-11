identity_patterns <- c(
    "...->...",
    "a b c d e-> a b c d e",
    "a b c d e ...-> ... a b c d e",
    "a b c d e ...-> a ... b c d e",
    "... a b c d e -> ... a b c d e",
    "a ... e-> a ... e",
    "a ... -> a ... ",
    "a ... c d e -> a (...) c d e"
)

equivalent_rearrange_patterns <- list(
    list("a b c d e -> (a b) c d e", "a b ... -> (a b) ... "),
    list("a b c d e -> a b (c d) e", "... c d e -> ... (c d) e"),
    list("a b c d e -> a b c d e", "... -> ... "),
    list("a b c d e -> (a b c d e)", "... ->  (...)"),
    list("a b c d e -> b (c d e) a", "a b ... -> b (...) a"),
    list("a b c d e -> b (a c d) e", "a b ... e -> b (a ...) e")
)

for (pattern in identity_patterns) {
    test_in_all_tensor_types_that(glue("rearrange(x, '{pattern}') returns x"), {
        x <- create_seq_tensor(10 * 1:5)
        expect_no_error(y <- rearrange(x, pattern))
        expect_identical(dim(y), dim(x))
        expect_identical(y, x)
    })
}

for (pattern in equivalent_rearrange_patterns) {
    test_in_all_tensor_types_that(glue(
        "rearrange(x, '{pattern[[1]]}') is equivalent to ",
        "rearrange(x, '{pattern[[2]]}')"
    ), {
        x <- create_seq_tensor(10 * 1:5)
        expect_no_error(y1 <- rearrange(x, pattern[[1]]))
        expect_no_error(y2 <- rearrange(x, pattern[[2]]))
        expect_identical(dim(y1), dim(y2))
        expect_identical(y1, y2)
    })
}

test_in_all_tensor_types_that("rearrange() is consistent", {
    shape <- c(1, 2, 3, 5, 7, 11)
    x <- create_seq_tensor(shape)
    for (pattern in c(
        "a b c d e f -> a b c d e f",
        "b a c d e f -> a b d e f c",
        "a b c d e f -> f e d c b a",
        "a b c d e f -> (f e) d (c b a)",
        "a b c d e f -> (f e d c b a)"
    )) {
        expect_no_error(result <- rearrange(x, pattern))
        expect_identical(
            length(setdiff(as_base_array(x), as_base_array(result))), 0L
        )
    }

    # FIXME: for torch_tensor, this fails but only due to tensor -> array
    # conversion
    # using x$flatten() we see that they are actually identical, so this either
    # we
    # need to change the conversion and NOT use torch::as_array() or we need to
    # change the test to use x$flatten() instead of as_base_array(). Neither
    # feel that right nor intuitive.
    # result <- rearrange(x, "a b c d e f -> a (b) (c d e) f")
    # expect_identical(
    #     as.numeric(as_base_array(result)), as.numeric(as_base_array(x))
    # )

    result <- rearrange(x, "a aa aa1 a1a1 aaaa a11 -> a aa aa1 a1a1 aaaa a11")
    expect_identical(x, result)

    result1 <- rearrange(x, "a b c d e f -> f e d c b a")
    result2 <- rearrange(x, "f e d c b a -> a b c d e f")
    expect_identical(result1, result2)

    result <- rearrange(
        rearrange(x, "a b c d e f -> (f d) c (e b) a"),
        "(f d) c (e b) a -> a b c d e f",
        b = 2,
        d = 5
    )
    expect_identical(x, result)

    sizes <- setNames(as.list(shape), letters[1:6])
    temp <- rearrange(x, "a b c d e f -> (f d) c (e b) a", sizes)
    result <- rearrange(temp, "(f d) c (e b) a -> a b c d e f", sizes)
    expect_identical(x, result)

    x2 <- create_seq_tensor(2:4)
    result <- rearrange(x2, "a b c -> b c a")
    x2_array <- as_base_array(x2)
    result_array <- as_base_array(result)
    expect_equal(x2_array[2, 3, 4], result_array[3, 4, 2])
    expect_equal(x2_array[1, 2, 3], result_array[2, 3, 1])
})

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

    x <- create_seq_tensor(10 * 1:4)

    # TODO actually check values
    
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
    y7_result <- rearrange(x, "b (c g) h w -> g b c h w", g = 2)
    expect_equal(dim(y7_result), c(2, 10, 10, 30, 40))
    
    # Test 10: stack
    x_list <- lapply(1:10, function(i) {
        x[i, , , ]
    })
    y10 <- rearrange(x_list, "b c h w -> b h w c")
    expect_equal(dim(y10), c(10, 30, 40, 20))
    
    # Test 11: concatenate
    y11 <- rearrange(x_list, "b c h w -> h (b w) c")
    expect_equal(dim(y11), c(30, 10 * 40, 20))
    
    # TODO rest of tests

})

test_in_all_tensor_types_that("rearrange() works on lists", {

    x <- create_seq_tensor(2:6)
    
    expect_identical(
        rearrange(
            lapply(seq_len(dim(x)[1]), function(i) x[i, , , , ]),
            "... -> (...)"
        ),
        rearrange(x, "... -> (...)")
    )

})
