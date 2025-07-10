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

    # utility function to create a list of unknown axis lengths
    make_unknown_composition <- function(x) {
        lapply(seq_along(x), function(i) list(known = integer(), unknown = i))
    }

    # sanity check
    expect_no_error(prepare_transformation_recipe(
        "a b -> b", "mean", character(), 2L
    ))

    expect_identical(
        prepare_transformation_recipe(
            "a b -> b", "mean", character(), 2L
        ),
        TransformRecipe(
            elementary_axes_lengths = rep(unknown_axis_length(), 2L),
            axis_name2elementary_axis = r2r::hashmap(),
            input_composition_known_unknown = make_unknown_composition(1:2),
            axes_permutation = c(2L, 1L),
            first_reduced_axis = 2L,
            added_axes = r2r::hashmap(),
            output_composite_axes = list(2L)
        )
    )

    expect_identical(
        prepare_transformation_recipe(
            "... c h w -> ... h w", "mean", character(), 4L
        ),
        TransformRecipe(
            elementary_axes_lengths = rep(unknown_axis_length(), 4L),
            axis_name2elementary_axis = r2r::hashmap(),
            input_composition_known_unknown = make_unknown_composition(1:4),
            axes_permutation = c(1L, 3L, 4L, 2L),
            first_reduced_axis = 4L,
            added_axes = r2r::hashmap(),
            output_composite_axes = list(1L, 3L, 4L)
        )
    )

    expect_identical(
        prepare_transformation_recipe(
            "... c h w -> ... h w", "mean", character(), 5L
        ),
        TransformRecipe(
            elementary_axes_lengths = rep(unknown_axis_length(), 5L),
            axis_name2elementary_axis = r2r::hashmap(),
            input_composition_known_unknown = make_unknown_composition(1:5),
            axes_permutation = c(1L, 2L, 4L, 5L, 3L),
            first_reduced_axis = 5L,
            added_axes = r2r::hashmap(),
            output_composite_axes = list(1L, 2L, 4L, 5L)
        )
    )

    expect_identical(
        prepare_transformation_recipe(
            "... 4 h w -> ... h w", "mean", character(), 4L
        ),
        TransformRecipe(
            elementary_axes_lengths = c(
                unknown_axis_length(), 4L, rep(unknown_axis_length(), 2L)
            ),
            axis_name2elementary_axis = r2r::hashmap(),
            input_composition_known_unknown = list(
                list(known = integer(), unknown = 1L),
                list(known = 2L, unknown = integer()),
                list(known = integer(), unknown = 3L),
                list(known = integer(), unknown = 4L)
            ),
            axes_permutation = c(1L, 3L, 4L, 2L),
            first_reduced_axis = 4L,
            added_axes = r2r::hashmap(),
            output_composite_axes = list(1L, 3L, 4L)
        )
    )

    expect_identical(
        prepare_transformation_recipe(
            "b c h w -> 1 c 1 1", "mean", character(), 4L
        ),
        TransformRecipe(
            elementary_axes_lengths = rep(unknown_axis_length(), 4L),
            axis_name2elementary_axis = r2r::hashmap(),
            input_composition_known_unknown = make_unknown_composition(1:4),
            axes_permutation = c(2L, 1L, 3L, 4L),
            first_reduced_axis = 2L,
            added_axes = r2r::hashmap(),
            output_composite_axes = list(integer(), 2L, integer(), integer())
        )
    )

    # TODO some test with brackets

    # TODO some test with 1's, brackets, anonymous axes, ellipses,
    # bracketted axes, etc.

    expect_identical(
        prepare_transformation_recipe(
            "b (h1 h2 h3) (w1 w2 w3) c -> (h1 w2 h3) (b w1 h2 w3) c",
            "rearrange",
            c("h2", "w2", "w3", "h3"),
            4L
        ),
        TransformRecipe(
            elementary_axes_lengths = c(
                rep(unknown_axis_length(), 2L),
                rep(expected_axis_length(), 2L),
                unknown_axis_length(),
                rep(expected_axis_length(), 2L),
                unknown_axis_length()
            ),
            axis_name2elementary_axis = r2r::hashmap(
                list("h2", 3L), list("w2", 6L), list("w3", 7L), list("h3", 4L)
            ),
            input_composition_known_unknown = list(
                list(known = integer(), unknown = 1L),
                list(known = c(3L, 4L), unknown = 2L),
                list(known = c(6L, 7L), unknown = 5L),
                list(known = integer(), unknown = 8L)
            ),
            axes_permutation = c(2L, 6L, 4L, 1L, 5L, 3L, 7L, 8L),
            first_reduced_axis = 9L,
            added_axes = r2r::hashmap(),
            output_composite_axes = list(
                c(2L, 6L, 4L), c(1L, 5L, 3L, 7L), 8L
            )
        )
    )

    expect_identical(
        prepare_transformation_recipe(
            "h w c -> h 5 w c", "repeat", character(), 3L
        ),
        TransformRecipe(
            elementary_axes_lengths = c(
                rep(unknown_axis_length(), 3L), 5L
            ),
            axis_name2elementary_axis = r2r::hashmap(),
            input_composition_known_unknown = make_unknown_composition(1:3),
            axes_permutation = c(1L, 2L, 3L),
            first_reduced_axis = 4L,
            added_axes = r2r::hashmap(list(2L, 4L)), # FIXME: outputs NULL inplace of 4
            output_composite_axes = list(1L, 4L, 2L, 3L)
        )
    )
})
