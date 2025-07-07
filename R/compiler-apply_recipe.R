#' @title
#' Apply a TransformRecipe to a Tensor
#'
#' @description
#' This function applies a TransformRecipe to a tensor, performing rearrangement
#' and reduction as specified by the recipe.
#'
#' TODO docs for which parts of the compilation pipeline this is.
#'
#' @param backend The [EinopsBackend()] to use for tensor operations.
#' @param recipe A [TransformRecipe()] that specifies how to transform the
#' tensor.
#' @param tensor A tensor to be transformed
#' @param reduction_type A character of length 1 that specifies the type of
#' reduction to apply.
#' @param axes_lengths TODO check - I think its just an integer vector?
#' @return A tensor that has been transformed according to the recipe, with the
#' same type (if possible) as the input tensor.
#' @keywords internal
#'
apply_recipe <- function(
    backend, recipe, tensor, reduction_type, axes_lengths
) {

    execution_plan <- create_execution_plan(
        recipe, backend$shape(tensor), axes_lengths
    )

    if (!is.null(execution_plan$init_shapes)) {
        tensor <- backend$reshape(tensor, execution_plan$init_shapes)
    }
    if (!is.null(execution_plan$axes_reordering)) {
        tensor <- backend$transpose(tensor, execution_plan$axes_reordering)
    }
    if (length(execution_plan$reduced_axes) > 0) {
        tensor %<>% reduce_axes(
            reduction_type = reduction_type,
            reduced_axes = execution_plan$reduced_axes,
            backend = backend
        )
    }
    if (length(execution_plan$added_axes) > 0) {
        tensor %<>% backend$add_axes(
            n_axes = execution_plan$n_axes_w_added,
            pos2len = execution_plan$added_axes
        )
    }
    if (!is.null(execution_plan$final_shapes)) {
        tensor <- backend$reshape(tensor, execution_plan$final_shapes)
    }

    tensor
}

#' @title
#' Constructor for an Execution Plan (`CookedRecipe` in the python sourcecode)
#'
#' @param init_shapes Optional list of integers specifying initial tensor shapes
#' for reshaping.
#' @param axes_reordering Optional list of integers specifying the order for
#' transposing tensor axes.
#' @param reduced_axes List of integers specifying which axes to reduce during
#' operations.
#' @param added_axes [r2r::hashmap()] mapping axis positions (int) to their
#' lengths (int) for axes to be added.
#' @param final_shapes Optional list of integers specifying final tensor shapes
#' for reshaping.
#' @param n_axes_w_added Integer specifying the total number of axes after
#' adding new axes.
#' @return An object of class `EinopsExecutionPlan`, which is a list containing
#' the execution plan for transforming tensors according to the specified
#' recipe.
#' @keywords internal
#'
EinopsExecutionPlan <- function(
    init_shapes,
    axes_reordering,
    reduced_axes,
    added_axes,
    final_shapes,
    n_axes_w_added
) {

    assert_that(
        is.null(init_shapes) || is.integer(init_shapes),
        is.null(axes_reordering) || is.integer(axes_reordering),
        is.integer(reduced_axes),
        inherits(added_axes, "r2r_hashmap"),
        is.null(final_shapes) || is.integer(final_shapes),
        is.integer(n_axes_w_added) &&
            length(n_axes_w_added) == 1L &&
            n_axes_w_added >= 0L
    )

    structure(
        list(
            init_shapes = init_shapes,
            axes_reordering = axes_reordering,
            reduced_axes = reduced_axes,
            added_axes = added_axes,
            final_shapes = final_shapes,
            n_axes_w_added = n_axes_w_added
        ),
        class = c("EinopsExecutionPlan", "s3list", "list")
    )
}

create_execution_plan <- function(recipe, shape, axes_lengths) {
    # TODO try to do a cached version (`reconstruct_from_shape` in the python impl)
    # this is _reconstruct_from_shape_uncached
    EinopsExecutionPlan(
        init_shapes = NULL,
        axes_reordering = NULL,
        reduced_axes = integer(),
        added_axes = r2r::hashmap(),
        final_shapes = NULL,
        n_axes_w_added = 0L
    )
}

reduce_axes <- function(tensor, reduction_type, reduced_axes, backend) {
    if (is.function(reduction_type)) {
        return(reduction_type(tensor, reduced_axes))
    }
    if (identical(reduction_type, "mean")) {
        if (!backend$is_float_type(tensor)) {
            stop("reduce mean is not available for non-floating tensors")
        }
    }
    backend$reduce(tensor, reduction_type, reduced_axes)
    tensor
}
