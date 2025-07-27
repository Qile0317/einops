test_that("create_execution_plan works", {

    # rearrange, "b c h w -> b h w c"

    recipe <- TransformRecipe(
        elementary_axes_lengths = rep(unknown_axis_length(), 4L),
        axis_name2elementary_axis = r2r::hashmap(),
        input_composition_known_unknown = make_unknown_composition(1:4),
        axes_permutation = c(1L, 3L, 4L, 2L),
        first_reduced_axis = 5L,
        added_axes = r2r::hashmap(),
        output_composite_axes = list(1L, 3L, 4L, 2L)
    )

    expect_identical(
        create_execution_plan(recipe, as.integer(10 * 1:4), list()),
        EinopsExecutionPlan(
            init_shapes = integer(),
            axes_reordering = c(1L, 3L, 4L, 2L),
            reduced_axes = integer(),
            added_axes = r2r::hashmap(),
            final_shapes = integer(),
            n_axes_w_added = 4L
        )
    )

    # rearrange, "b (c h1 w1) h w -> b c (h h1) (w w1)", h1 = 2, w1 = 2

    recipe <- TransformRecipe(
        elementary_axes_lengths = c(
            rep(unknown_axis_length(), 2L),
            rep(expected_axis_length(), 2L),
            rep(unknown_axis_length(), 2L)
        ),
        axis_name2elementary_axis = r2r::hashmap(
            list("h1", 3L), list("w1", 4L)
        ),
        input_composition_known_unknown = list(
            list(known = integer(), unknown = 1L),
            list(known = c(3L, 4L), unknown = 2L),
            list(known = integer(), unknown = 5L),
            list(known = integer(), unknown = 6L)
        ),
        axes_permutation = c(1L, 2L, 5L, 3L, 6L, 4L),
        first_reduced_axis = 7L,
        added_axes = r2r::hashmap(),
        output_composite_axes = list(1L, 2L, c(5L, 3L), c(6L, 4L))
    )

    expect_identical(
        create_execution_plan(
            recipe, as.integer(10 * 1:4), list(h1 = 2, w1 = 2)
        ),
        EinopsExecutionPlan(
            init_shapes = c(10L, 5L, 2L, 2L, 30L, 40L),
            axes_reordering = c(1L, 2L, 5L, 3L, 6L, 4L),
            reduced_axes = integer(),
            added_axes = r2r::hashmap(),
            final_shapes = c(10L, 5L, 60L, 80L),
            n_axes_w_added = 6L
        )
    )

    # rearrange(x, "a b c d e f -> a (b) (c d e) f")

    recipe <- TransformRecipe(
        elementary_axes_lengths = rep(unknown_axis_length(), 6L),
        axis_name2elementary_axis = r2r::hashmap(),
        input_composition_known_unknown = make_unknown_composition(1:6),
        axes_permutation = 1:6,
        first_reduced_axis = 7L,
        added_axes = r2r::hashmap(),
        output_composite_axes = list(1L, 2L, 3:5, 6L)
    )

    expect_identical(
        create_execution_plan(recipe, c(1L, 2L, 3L, 5L, 7L, 11L), list()),
        EinopsExecutionPlan(
            init_shapes = integer(),
            axes_reordering = integer(),
            reduced_axes = integer(),
            added_axes = r2r::hashmap(),
            final_shapes = c(1L, 2L, 105L, 11L),
            n_axes_w_added = 6L
        )
    )

})
