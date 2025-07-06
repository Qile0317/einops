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
        tensor <- reduce_axes(
            tensor,
            reduction_type = reduction_type,
            reduced_axes = execution_plan$reduced_axes,
            backend = backend
        )
    }
    if (length(execution_plan$added_axes) > 0) {
        tensor <- backend$add_axes(
            tensor,
            n_axes = execution_plan$n_axes_w_added,
            pos2len = execution_plan$added_axes
        )
    }
    if (!is.null(execution_plan$final_shapes)) {
        tensor <- backend$reshape(tensor, execution_plan$final_shapes)
    }

    tensor
}

EinopsExecutionPlan <- function(
    init_shapes = NULL,
    axes_reordering = NULL,
    reduced_axes = NULL,
    added_axes = NULL,
    final_shapes = NULL,
    n_axes_w_added = NULL
) {
    # TODO typecheck inputs
    structure(
        as.list(match.call())[-1],
        class = c("EinopsExecutionPlan", "s3list", "list")
    )
}

create_execution_plan <- function(recipe, shape, axes_lengths) {
    # TODO implement this function
    # This function should create an execution plan based on the recipe and the
    # shape of the tensor.

    #     try:
    #         init_shapes, axes_reordering, reduced_axes, added_axes, final_shapes, n_axes_w_added = _reconstruct_from_shape(
    #             recipe, backend.shape(tensor), axes_lengths
    #         )
    #     except TypeError:
    #         # shape or one of passed axes lengths is not hashable (i.e. they are symbols)
    #         _result = _reconstruct_from_shape_uncached(recipe, backend.shape(tensor), axes_lengths)
    #         (init_shapes, axes_reordering, reduced_axes, added_axes, final_shapes, n_axes_w_added) = _result

    EinopsExecutionPlan()
}

reduce_axes <- function(tensor, reduction_type, reduced_axes, backend) {
    # TODO implement this function
    # This function should apply the specified reduction to the tensor along the
    # specified axes.
    tensor
}
