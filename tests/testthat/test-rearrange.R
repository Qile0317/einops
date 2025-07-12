# TODO loop test over all backends

test_that("rearrange() works", {

    # test matrix transpose
    x <- as.matrix(mtcars)
    expect_equal(
        rearrange(x, "row col -> col row"),
        t(x)
    )
    expect_equal(
        rearrange(x, "... col -> col ..."),
        t(x)
    )

})
