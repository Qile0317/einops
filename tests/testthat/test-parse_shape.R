test_that("Basic shape parsing works for base::array", {
    expect_identical(
        parse_shape(array(1:16, dim = c(4, 4)), "height width"),
        list(height = 4L, width = 4L)
    )

    expect_identical(
        parse_shape(array(1:12, dim = c(3, 4)), "height width"),
        list(height = 3L, width = 4L)
    )

    expect_identical(
        parse_shape(array(1:360, dim = 3:6), "b c h w"),
        list(b = 3L, c = 4L, h = 5L, w = 6L)
    )

    expect_identical(
        parse_shape(array(1:360, dim = 3:6), "c h w1 w2"),
        list(c = 3L, h = 4L, w1 = 5L, w2 = 6L)
    )

    expect_identical(
        parse_shape(array(1:360, dim = 3:6), "b _ h w"),
        list(b = 3L, h = 5L, w = 6L)
    )
})
