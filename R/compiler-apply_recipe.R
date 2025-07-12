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

    if (length(execution_plan$init_shapes) > 0) {
        tensor <- backend$reshape(tensor, execution_plan$init_shapes)
    }
    if (length(execution_plan$axes_reordering) > 0) {
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
    if (length(execution_plan$final_shapes) > 0) {
        tensor <- backend$reshape(tensor, execution_plan$final_shapes)
    }

    tensor
}

#' @title
#' Constructor for an Execution Plan (`CookedRecipe` in the python sourcecode)
#'
#' @param init_shapes integer vector specifying initial tensor shapes
#' for reshaping. Length 0 signifies nullness.
#' @param axes_reordering integer vector specifying the order for
#' transposing tensor axes. Length 0 signifies nullness.
#' @param reduced_axes List of integers specifying which axes to reduce during
#' operations. Length 0 signifies nullness.
#' @param added_axes [r2r::hashmap()] mapping axis positions (int) to their
#' lengths (int) for axes to be added.
#' @param final_shapes list of integers specifying final tensor shapes
#' for reshaping. Length 0 signifies nullness.
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
        is.integer(init_shapes),
        is.integer(axes_reordering),
        is.integer(reduced_axes),
        inherits(added_axes, "r2r_hashmap"),
        is.integer(final_shapes),
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

#' @title
#' Reconstruct all actual parameters using shape.
#' @description
#' This implements `_reconstruct_from_shape_uncached` from the python
#' implementation of einops. In the future, a cached version may be
#' implemented.
#' @param recipe a populated [TransformRecipe()] object
#' @param shape A vector of integers representing the shape of the tensor.
#' @param axes_dims A named list of axes names to their dimensions/lengths.
#' May also be an empty list.
#' @return An `EinopsExecutionPlan` object that contains the execution plan for
#' transforming tensors according to the specified recipe and shape.
#' @keywords internal
#'
create_execution_plan <- function(recipe, shape, axes_dims) {

    assert_that(
        inherits(recipe, "TransformRecipe"),
        is.integer(shape),
        is.list(axes_dims)
    )

    need_init_reshape <- FALSE

    # last axis is allocated for collapsed ellipsis
    axes_lengths <- recipe$elementary_axes_lengths
    for (axis in names(axes_dims)) {
        dim <- axes_dims[[axis]]
        axes_lengths[[recipe$axis_name2elementary_axis[[axis]]]] <- dim
    }

    # Process input composition known/unknown
    for (el in FastUtils::enumerateit(recipe$input_composition_known_unknown)) {
        input_axis <- FastUtils::ind(el)
        known_axes <- FastUtils::val1(el)$known
        unknown_axes <- FastUtils::val1(el)$unknown
        
        if (length(known_axes) == 0L && length(unknown_axes) == 1L) {
            axes_lengths[[unknown_axes[1]]] <- shape[input_axis]
            next
        }

        known_product <- 1L
        for (axis in known_axes) {
            known_product %*=% axes_lengths[axis]
        }

        if (length(unknown_axes) == 0L) {
            if (is.integer(shape[input_axis]) && is.integer(known_product) && shape[input_axis] != known_product) {
                stop(glue("Shape mismatch, {shape[input_axis]} != {known_product}"))
            }
        } else {
            # assert len(unknown_axes) == 1, 'this is enforced when recipe is created'
            if (is.integer(shape[input_axis]) && is.integer(known_product) && shape[input_axis] %% known_product != 0L) {
                stop(glue("Shape mismatch, can't divide axis of length {shape[input_axis]} in chunks of {known_product}"))
            }

            unknown_axis <- unknown_axes[1]
            inferred_length <- as.integer(length %/% known_product)
            axes_lengths[[unknown_axis]] <- inferred_length
        }

        if (length(known_axes) + length(unknown_axes) != 1L) {
            need_init_reshape <- TRUE
        }
    }

    # at this point all axes_lengths are computed (either have values or
    # variables, but not NULLs)

    # elementary axes are ordered as they appear in input, then all added axes
    init_shapes <- if (need_init_reshape) {
        as.integer(axes_lengths[seq_len(length(recipe$axes_permutation))])
    } else {
        integer()
    }

    need_final_reshape <- FALSE
    final_shapes <- list()
    for (grouping in recipe$output_composite_axes) {
        lengths <- axes_lengths[grouping]
        final_shapes %<>% c(.product(lengths))
        if (length(lengths) != 1L) {
            need_final_reshape <- TRUE
        }
    }

    # Create added_axes hashmap
    added_axes <- r2r::hashmap()
    for (pos in r2r::keys(recipe$added_axes)) {
        pos_in_elementary <- recipe$added_axes[[pos]]
        r2r::insert(added_axes, pos, axes_lengths[pos_in_elementary])
    }

    # this list can be empty
    reduced_axes <- if (recipe$first_reduced_axis <= length(recipe$axes_permutation)) {
        as.integer(seq(recipe$first_reduced_axis, length(recipe$axes_permutation)))
    } else {
        integer(0)
    }

    n_axes_after_adding_axes <- length(recipe$added_axes) + length(recipe$axes_permutation)

    axes_reordering <- recipe$axes_permutation
    if (identical(recipe$axes_permutation, seq_len(length(recipe$axes_permutation)))) {
        axes_reordering <- integer()
    }

    final_shapes_result <- if (need_final_reshape)
        as.integer(final_shapes)
    else
        integer()

    EinopsExecutionPlan(
        init_shapes = init_shapes,
        axes_reordering = axes_reordering,
        reduced_axes = reduced_axes,
        added_axes = added_axes,
        final_shapes = final_shapes_result,
        n_axes_w_added = n_axes_after_adding_axes
    )
}

# minimalistic product that works both with numbers and symbols. Supports empty
# lists
.product <- function(sequence) {
    if (length(sequence) == 0L) return(1L)
    Reduce(prod, sequence, init = 1L)
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
