test_that("preprocess_shape_ast expands ellipsis correctly", {

    shape <- c(1, 2, 3, 4)
    ast <- OneSidedAstNode(
        ConstantAstNode(
            count = 3,
            src = list(start = 1)
        ),
        NamedAxisAstNode(
            name = "c",
            src = list(start = 3)
        ),
        EllipsisAstNode(
            src = list(start = 5)
        )
    )
    expect_identical(
        preprocess_shape_ast(ast, shape),
        OneSidedAstNode(
            ConstantAstNode(
                count = 3,
                src = list(start = 1)
            ),
            NamedAxisAstNode(
                name = "c",
                src = list(start = 3)
            ),
            UnderscoreAstNode(src = list()),
            UnderscoreAstNode(src = list())
        )
    )

    shape <- 1:8
    ast <- OneSidedAstNode(
        NamedAxisAstNode(
            name = "a",
            src = list(start = 1)
        ),
        NamedAxisAstNode(
            name = "b",
            src = list(start = 3)
        ),
        NamedAxisAstNode(
            name = "c",
            src = list(start = 5)
        ),
        UnderscoreAstNode(
            src = list(start = 7)
        ),
        UnderscoreAstNode(
            src = list(start = 9)
        ),
        EllipsisAstNode(
            src = list(start = 11)
        )
    )
    expect_identical(
        preprocess_shape_ast(ast, shape),
        OneSidedAstNode(
            NamedAxisAstNode(
                name = "a",
                src = list(start = 1)
            ),
            NamedAxisAstNode(
                name = "b",
                src = list(start = 3)
            ),
            NamedAxisAstNode(
                name = "c",
                src = list(start = 5)
            ),
            UnderscoreAstNode(
                src = list(start = 7)
            ),
            UnderscoreAstNode(
                src = list(start = 9)
            ),
            UnderscoreAstNode(src = list()),
            UnderscoreAstNode(src = list()),
            UnderscoreAstNode(src = list())
        )
    )
})

test_that("parse_shape works for base::array", {
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
        parse_shape(array(1:360, dim = 3:6), "... c h w"),
        list(c = 4L, h = 5L, w = 6L)
    )

    expect_identical(
        parse_shape(array(1:360, dim = 3:6), "3 c h w"),
        list(c = 4L, h = 5L, w = 6L)
    )

    expect_identical(
        parse_shape(array(1:360, dim = 3:6), "3 c 5 w"),
        list(c = 4L, w = 6L)
    )

    expect_identical(
        parse_shape(array(1:360, dim = 3:6), "3 c ..."),
        list(c = 4L)
    )

    expect_identical(
        parse_shape(array(1:360, dim = 3:6), "c h w1 w2"),
        list(c = 3L, h = 4L, w1 = 5L, w2 = 6L)
    )

    expect_identical(
        parse_shape(array(1:360, dim = 3:6), "b _ h w"),
        list(b = 3L, h = 5L, w = 6L)
    )

    expect_identical(
        parse_shape(array(1:360, dim = 3:6), "b _ _ w"),
        list(b = 3L, w = 6L)
    )

    expect_identical(
        parse_shape(array(1:360, dim = 3:6), "b _ _ _"),
        list(b = 3L)
    )

    expect_identical(
        parse_shape(array(1:360, dim = 3:6), "b ..."),
        list(b = 3L)
    )

    expect_identical(
        parse_shape(array(1:360, dim = 3:6), "... h _ w"),
        list(h = 4L, w = 6L)
    )

    expect_identical(
        parse_shape(array(1:360, dim = 3:6), "a ... w"),
        list(a = 3L, w = 6L)
    )

    expect_identical(
        parse_shape(array(1:360, dim = 3:6), "a ... _ w"),
        list(a = 3L, w = 6L)
    )

    expect_identical(
        parse_shape(array(1:720, dim = 2:6), "a ... _ w _"),
        list(a = 2L, w = 5L)
    )
})
