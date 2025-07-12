test_that("create_execution_plan works", {

    # "b c h w -> b h w c" rearrange

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

})
