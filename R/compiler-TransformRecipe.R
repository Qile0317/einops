#' @title TransformRecipe S3 constructor
#' @description Recipe describes actual computation pathway. Can be applied to a
#' tensor or variable.
#' @param elementary_axes_lengths Integer vector. List of sizes for elementary
#' axes as they appear in left expression.
#' @param axis_name2elementary_axis Named integer vector. Mapping from name to
#' position.
#' @param input_composition_known_unknown List of list(present, unknown) integer
#' vectors. Each element is a tuple of known and unknown indices.
#' @param axes_permutation Integer vector. Permutation applied to elementary
#' axes.
#' @param first_reduced_axis Integer. First position of reduced axes.
#' @param added_axes Named integer vector. Axis position -> axis index.
#' @param output_composite_axes List of integer vectors. Ids of axes as they
#' appear in result.
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
    structure(
        as.list(match.call())[-1],
        class = c("TransformRecipe", "s3list", "list")
    )
}

#' @title
#' Construct an instance of an `AxisNames` class
#' @description
#' This is a wrapper for a [list()], but the elements may only be singular
#' [character()] or [ConstantAstNode()] objects.
#' @param ... a list of elements or arbitrary number of elements
AxisNames <- function(...) {
    input <-
        if (nargs() == 1 && is.list(..1) && !inherits(..1, "ConstantAstNode"))
            ..1
        else
            list(...)
    # TODO type check element
    structure(input, class = c("AxisNames", "s3list", "list"))
}

#' @title
#' Create the Transformation Recipe for an einops call
#'
#' @description
#' This function does the following parts of the einops 'compilation' pipeline:
#' 1. Lexing: tokenizing the input expression string
#' 2. Parsing: converting the tokens into an Abstract Syntax Tree (AST)
#' 3. Syntactic Analysis:
#'     - [IN PROGRESS] operation-based AST validation pass
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

    # the keys represent unique axes, where named axes are just their
    # names, but constant axes are represented by the ConstantAstNode
    # the values are the known lengths of the axes, if unknown, then
    # the value is UNKNOWN_AXIS_LENGTH
    axis_name2known_length <- AddOnlyOrderedMap()
    for (axis_node in ast$input_axes) {
        if (inherits(axis_node, "ConstantAstNode")) {
            axis_name2known_length[[axis_node]] <- axis_node$count
        } else {
            axis_name2known_length[[axis_node$name]] <- UNKNOWN_AXIS_LENGTH
        }
    }

    repeat_axes_names <- list()
    for (ast_node in ast$output_axes) {
        ast_key <- if (!inherits(ast_node, "ConstantAstNode")) {
            ast_node$name
        } else {
            ast_node
        }
        if (has_key(axis_name2known_length, ast_key)) next
        if (inherits(ast_key, "ConstantAstNode")) {
            axis_name2known_length[[ast_key]] <- ast_key$count
        } else {
            axis_name2known_length[[ast_key]] <- UNKNOWN_AXIS_LENGTH
        }
        repeat_axes_names %<>% append(list(ast_key))
    }

    axis_name2position <- make_addonlyorderedmap_bypairs(
        FastUtils::enumerateit(axis_name2known_length), .reverse = TRUE
    )

    for (elementary_axis in axes_names) {
        # TODO check the axis name
        if (!has_key(axis_name2known_length, elementary_axis)) {
            stop(glue("Axis {elementary_axis} is not used in transform"))
        }
        axis_name2known_length[[elementary_axis]] <- EXPECTED_AXIS_LENGTH
    }

    input_axes_known_unknown <- list()
    for (composite_axis_node in ast$input_axes) {
        known <- r2r::hashset()
        unknown <- r2r::hashset()
        # note that in the logic below, it handles assuming 1's are translated to []
    #     known: Set[str] = {axis for axis in composite_axis if axis_name2known_length[axis] != _unknown_axis_length}
    #     unknown: Set[str] = {axis for axis in composite_axis if axis_name2known_length[axis] == _unknown_axis_length}
        if (length(unknown) > 1) stop(glue("Could not infer sizes for {to_expression(unknown)}")) # to_expression here isnt implemented
        if (length(unknown) + length(known) != length(composite_axis_node)) {
            stop(glue(
                "The input axes {to_expression(composite_axis_node)} ",
                "do not match the expected axes {to_expression(axes_names)}."
            ))
        }
    #     input_axes_known_unknown.append(
    #         ([axis_name2position[axis] for axis in known], [axis_name2position[axis] for axis in unknown]))
    }

    # axis_position_after_reduction: Dict[str, int] = {}
    # for axis_name in itertools.chain(*left_composition):
    #     if axis_name in rght.identifiers:
    #         axis_position_after_reduction[axis_name] = len(axis_position_after_reduction)

    # result_axes_grouping: List[List[int]] = [
    #     [axis_name2position[axis] for axis in composite_axis] for i, composite_axis in enumerate(rght_composition)
    # ]

    # ordered_axis_left = list(itertools.chain(*left_composition))
    # ordered_axis_rght = list(itertools.chain(*rght_composition))
    # reduced_axes = [axis for axis in ordered_axis_left if axis not in rght.identifiers]
    # order_after_transposition = [axis for axis in ordered_axis_rght if axis in left.identifiers] + reduced_axes
    # axes_permutation = [ordered_axis_left.index(axis) for axis in order_after_transposition]
    # added_axes = {
    #     i: axis_name2position[axis_name]
    #     for i, axis_name in enumerate(ordered_axis_rght)
    #     if axis_name not in left.identifiers
    # }

    TransformRecipe(
        elementary_axes_lengths = as.integer(values(axis_name2known_length)),
        axis_name2elementary_axis = AddOnlyOrderedMap(
            names(axes_names), axis_name2position[names(axes_names)]
        ),
        input_composition_known_unknown = input_axes_known_unknown,
        axes_permutation = axes_permutation,
        first_reduced_axis = length(order_after_transposition) - length(reduced_axes),
        added_axes = added_axes,
        output_composite_axes = result_axes_grouping,
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
