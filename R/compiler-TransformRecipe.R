#' @title TransformRecipe S3 constructor
#' @description Recipe describes actual computation pathway. Can be applied to a
#' tensor or variable.
#' @param elementary_axes_lengths Integer vector. List of sizes for elementary
#' axes as they appear in left expression. This is what (after computing unknown
#' parts) will be a shape after first transposition. This does not include any
#' ellipsis dimensions.
#' @param axis_name2elementary_axis [r2r::hashmap()] Mapping from name to
#' position. if additional axes are provided, they should be set in prev array.
#' The keys are unclassed [AxisNames()] objects, and the values are
#' integer positions of the elementary axes.
#' @param input_composition_known_unknown List of list(known, unknown) [AxisNames()].
#' known and unknown are integer vectors, but this may also be fully empty.
#' @param axes_permutation Integer vector. Permutation applied to elementary
#' axes, if ellipsis is absent. This is ONE INDEXED!
#' @param first_reduced_axis Integer of length 1. First position of reduced axes.
#' Permutation puts reduced axes in the end, we only need to know the first position.
#' @param added_axes [r2r::hashmap()]. Axis position -> axis index. At which positions
#' which of elementary axes should appear.
#' @param output_composite_axes List of integer vectors. Ids of axes as they
#' appear in result. Again pointers to elementary_axes_lengths, only used to infer
#' result dimensions.
#' @return An object of class 'TransformRecipe'.
#' @keywords internal
TransformRecipe <- function(
    elementary_axes_lengths,
    axis_name2elementary_axis,
    input_composition_known_unknown,
    axes_permutation,
    first_reduced_axis,
    added_axes,
    output_composite_axes
) {
    assert_that(
        is.integer(elementary_axes_lengths),
        inherits(axis_name2elementary_axis, "r2r_hashmap"),
        all(sapply(input_composition_known_unknown, function(x) {
            is.list(x) &&
                length(x) == 2L &&
                identical(names(x), c("known", "unknown")) &&
                inherits(x$known, "AxisNames") &&
                inherits(x$unknown, "AxisNames")
        })),
        is.integer(axes_permutation),
        is.count(first_reduced_axis),
        inherits(added_axes, "r2r_hashmap"),
        is.list(output_composite_axes) &&
            all(sapply(output_composite_axes, is.integer))
    )
    structure(
        list(
            elementary_axes_lengths = elementary_axes_lengths,
            axis_name2elementary_axis = axis_name2elementary_axis,
            input_composition_known_unknown = input_composition_known_unknown,
            axes_permutation = axes_permutation,
            first_reduced_axis = first_reduced_axis,
            added_axes = added_axes,
            output_composite_axes = output_composite_axes
        ),
        class = c("TransformRecipe", "s3list", "list")
    )
}

#' @title
#' Create the Transformation Recipe for an einops call
#'
#' @description
#' This function does the following parts of the einops 'compilation' pipeline:
#' 1. Lexing: tokenizing the input expression string
#' 2. Parsing: converting the tokens into an Abstract Syntax Tree (AST)
#' 3. Syntactic Analysis:
#'     - operation-based AST validation pass
#'     - [TODO] Compile syntactic info for intermediate representation (IR).
#' 4. [TODO] IR generation: return the [TransformRecipe()] object, acting as
#'    the IR for the einops.
#'
#' @param expr The input einops expression string
#' @param func The string/function indicating the reduction operation
#' @param axes_names the user defined keyword args for dims as a [list()].
#' Names correspond to axes variable names, and values are their dimension.
#' @param ndim count for the number of dimensions of the input tensor
#' @return a populated [TransformRecipe()] object
#' @keywords internal
prepare_transformation_recipe <- function(expr, func, axes_names, ndim) {

    tokens <- lex(expr)

    ast <- parse_einops_ast(tokens) %>%
        validate_reduction_operation(func) %>%
        expand_ellipsis(ndim)
    
    UNKNOWN_AXIS_LENGTH = -999999
    EXPECTED_AXIS_LENGTH = -99999

    # the keyset are unclassed [AxisNames()] objects.
    # the keys represent unique axes, where named axes are just their
    # names, but constant axes are represented by the ConstantAstNode
    # the values are the known lengths of the axes, if unknown, then
    # the value is UNKNOWN_AXIS_LENGTH
    axis_name2known_length <- AddOnlyOrderedMap()
    for (axis_node in get_ungrouped_nodes(ast$input_axes)) {
        if (inherits(axis_node, "ConstantAstNode")) {
            if (axis_node$count == 1L) next
            axis_name2known_length[[axis_node]] <- axis_node$count
        } else {
            axis_name2known_length[[axis_node$name]] <- UNKNOWN_AXIS_LENGTH
        }
    }

    repeat_axes_names <- AxisNames()
    for (ast_key in get_identifiers(ast$output_axes)) {
        if (has_key(axis_name2known_length, ast_key)) next
        if (inherits(ast_key, "ConstantAstNode")) {
            axis_name2known_length[[ast_key]] <- ast_key$count
        } else {
            axis_name2known_length[[ast_key]] <- UNKNOWN_AXIS_LENGTH
        }
        repeat_axes_names %<>% c(ast_key)
    }

    axis_name2position <- get_key_to_index_map(axis_name2known_length)

    for (elementary_axis in axes_names) {
        # TODO check the axis name
        if (!has_key(axis_name2known_length, elementary_axis)) {
            stop(glue(
                "Axis {repr(elementary_axis, indent = 0L)} is not used in transform"
            ))
        }
        axis_name2known_length[[elementary_axis]] <- EXPECTED_AXIS_LENGTH
    }

    input_axes_known_unknown <- list()
    for (composite_axis in as_iterables(as_axis_names(ast$input_axes))) {

        known <- r2r::hashset()
        unknown <- r2r::hashset()

        for (axis in composite_axis) {
            if (axis_name2known_length[[axis]] == UNKNOWN_AXIS_LENGTH) {
                r2r::insert(unknown, axis)
            } else {
                r2r::insert(known, axis)
            }
        }

        if (length(unknown) > 1) stop(glue("Could not infer sizes"))
        if (length(unknown) + length(known) != length(composite_axis)) {
            stop(glue(
                "The input axes {to_expression(composite_axis_node)} ",
                "do not match the expected axes {repr(axes_names)}."
            ))
        }

        input_axes_known_unknown %<>% append(list(list(
            known = AxisNames(lapply(known, function(x) axis_name2position[[x]])),
            unknown = AxisNames(lapply(unknown, function(x) axis_name2position[[x]]))
        )))
    }

    axis_position_after_reduction <- r2r::hashmap()
    rght_identifiers_hashset <- get_identifiers_hashset(ast$output_axes)
    for (axis_name in get_ordered_axis_names(ast$output_axes)) {
        if (r2r::has_key(rght_identifiers_hashset, axis_name)) {
            axis_position_after_reduction[[axis_name]] <- length(
                axis_position_after_reduction
            )
        }
    }

    result_axes_grouping <- lapply(
        as_iterables(as_axis_names(ast$output_axes)),
        function(composite_axis) {
            as.integer(sapply(
                composite_axis, function(axis) axis_name2position[[axis]]
            ))
        }
    )

    ordered_axis_left <- add_relative_pos(get_ordered_axis_names(ast$input_axes))
    ordered_axis_rght <- add_relative_pos(get_ordered_axis_names(ast$output_axes))
    reduced_axes <- get_reduced_axis_names(ordered_axis_left, ordered_axis_rght)
    relative_output_identifiers <- get_identifiers_hashset(
        ast$output_axes, add_relative_pos = TRUE
    )
    order_after_transposition <- c(
        ordered_axis_rght[sapply(
            ordered_axis_rght,
            function(axis) r2r::has_key(relative_output_identifiers, axis)
        )],
        reduced_axes
    )

    axes_permutation <- as.integer(sapply(order_after_transposition, function(axis) {
        which(sapply(ordered_axis_left, function(x) identical(x, axis)))
    }))

    added_axes <- r2r::hashmap()
    left_identifiers <- get_identifiers_hashset(ast$input_axes)
    for (i in seq_along(ordered_axis_rght)) {
        axis_name <- ordered_axis_rght[i]
        if (!r2r::has_key(left_identifiers, axis_name)) {
            r2r::insert(added_axes, i, axis_name2position[[axis_name]])
        }
    }

    TransformRecipe(
        elementary_axes_lengths = as.integer(values(axis_name2known_length)),
        axis_name2elementary_axis = if (length(axes_names) == 0L)
            r2r::hashmap()
        else
            do.call(
                r2r::hashmap,
                FastUtils::zipit(names(axes_names), axis_name2position[names(axes_names)])
            ),
        input_composition_known_unknown = input_axes_known_unknown,
        axes_permutation = axes_permutation,
        first_reduced_axis = length(order_after_transposition) - length(reduced_axes),
        added_axes = added_axes,
        output_composite_axes = result_axes_grouping
    )
}

#' @title
#' Expand ellipses of an EinopsAst
#'
#' @description
#' Helper for [prepare_transformation_recipe()].
#'
#' This function expands each relevant ellipsis ast node inplace into a
#' sequence of `NamedAxisAstNode` nodes, where each node will have
#' a name like `"...1"`, `"...2"`, etc. and an empty `src` list.
#'
#' Also does some further validation of the ellipses syntax using `ndim`.
#'
#' @param einops_ast an EinopsAst
#' @param ndim integer. Number of dimensions in the input tensor
#' @return an expanded EinopsAst with ellipses expanded
#' @keywords internal
expand_ellipsis <- function(einops_ast, ndim) {

    if (!has_ellipsis(einops_ast$input_axes)) {
        if (ndim != length(einops_ast$input_axes)) {
            stop(glue(
                "Wrong shape: expected {length(einops_ast$input_axes)} dims. ",
                "Received {ndim}-dim tensor."
            ))
        }
        return(einops_ast)
    }

    n_other_dims <- length(einops_ast$input_axes) - 1
    if (ndim < n_other_dims) {
        stop(glue(
            "Wrong shape: expected >={n_other_dims} dims. Received {ndim}-dim ",
            "tensor."
        ))
    }

    replace_ellipsis <- function(ast) {
        ellipsis_index <- get_ellipsis_index(ast)
        append(
            x = ast[-ellipsis_index],
            values = lapply(seq_len(ndim - n_other_dims), function(i) {
                NamedAxisAstNode(paste0("...", i))
            }),
            after = ellipsis_index - 1
        )
    }

    einops_ast$input_axes %<>% replace_ellipsis()

    # expand the output ellipsis

    if (has_ellipsis(einops_ast$output_axes)) {
        einops_ast$output_axes %<>% replace_ellipsis()
        return(einops_ast)
    }

    for (i in seq_len(einops_ast$output_axes)) {
        if (!inherits(einops_ast$output_axes[[i]], "GroupAstNode")) next
        ellipsis_index <- get_ellipsis_index(einops_ast$output_axes[[i]])
        if (length(ellipsis_index) == 0) next
        einops_ast$output_axes[[i]] %<>% replace_ellipsis()
        return(einops_ast)
    }

    stop(
        "No ellipsis found in the output axes. ",
        "This is a bug in the einops parser. Please report as issue.",
    )

}
