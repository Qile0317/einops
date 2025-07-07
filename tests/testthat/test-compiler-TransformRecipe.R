test_that("expand_ellipsis works", {

    ast <- EinopsAst(
        input_axes = OneSidedAstNode(
            NamedAxisAstNode(
                name = "a",
                src = list(start = 1)
            ),
            NamedAxisAstNode(
                name = "b",
                src = list(start = 3)
            )
        ),
        output_axes = OneSidedAstNode(
            NamedAxisAstNode(
                name = "b",
                src = list(start = 8)
            )
        ),
        src = list(start = 1)
    )
    expect_identical(expand_ellipsis(ast, 2), ast)

    ast <- EinopsAst(
        input_axes = OneSidedAstNode(
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
            )
        ),
        output_axes = OneSidedAstNode(
            NamedAxisAstNode(
                name = "b",
                src = list(start = 10)
            ),
            NamedAxisAstNode(
                name = "c",
                src = list(start = 12)
            )
        ),
        src = list(start = 1)
    )

    expect_identical(expand_ellipsis(ast, 3), ast)

    # check that simple flat left and right ellipses are expanded correctly

    ast <- EinopsAst(
        input_axes = OneSidedAstNode(
            NamedAxisAstNode(
                name = "a",
                src = list(start = 1)
            ),
            EllipsisAstNode(
                src = list(start = 3)
            )
        ),
        output_axes = OneSidedAstNode(
            EllipsisAstNode(
                src = list(start = 10)
            )
        ),
        src = list(start = 1)
    )

    expected_ast <- EinopsAst(
        input_axes = OneSidedAstNode(
            NamedAxisAstNode(
                name = "a",
                src = list(start = 1)
            ),
            NamedAxisAstNode(
                name = "...1",
                src = list()
            ),
            NamedAxisAstNode(
                name = "...2",
                src = list()
            )
        ),
        output_axes = OneSidedAstNode(
            NamedAxisAstNode(
                name = "...1",
                src = list()
            ),
            NamedAxisAstNode(
                name = "...2",
                src = list()
            )
        ),
        src = list(start = 1)
    )

    expect_identical(expand_ellipsis(ast, 3), expected_ast)

})

test_that("prepare_transformation_recipe works", {

    # sanity check
    expect_no_error(prepare_transformation_recipe(
        "b c h w -> c h w",
        "mean",
        list(),
        4L
    ))

    # TODO test expr above's output

    # TODO test prepare_transformation_recipe(
    #     "... c h w -> ... h w",
    #     "mean",
    #     list(),
    #     4L
    # )

    # TODO test prepare_transformation_recipe(
    #     "... c h w -> ... h w",
    #     "mean",
    #     list(c = 4L),
    #     4L
    # )

    # TODO test prepare_transformation_recipe(
    #     "... 4 h w -> ... h w",
    #     "mean",
    #     list(),
    #     4L
    # )

    # TODO some test with 1's

    # TODO some test with brackets

    # TODO some test with 1's, brackets, anonymous axes, ellipses,
    # bracketted axes, etc.

    # TODO some tests with ADDED axes
})
