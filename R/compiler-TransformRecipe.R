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
    assert_that(
        is.numeric(elementary_axes_lengths),
        is.integer(first_reduced_axis) || is.numeric(first_reduced_axis),
        is.list(input_composition_known_unknown),
        is.numeric(axes_permutation),
        is.list(output_composite_axes),
        is.list(added_axes) || is.numeric(added_axes) || is.integer(added_axes),
        is.list(axis_name2elementary_axis) ||
            is.integer(axis_name2elementary_axis) ||
            is.numeric(axis_name2elementary_axis)
    )

    structure(as.list(match.call()), class = c("TransformRecipe", "list"))
}

#' @export
print.TransformRecipe <- function(x, ...) {
    pprint(x, indent = 4L, s3_cons = TRUE, ...)
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
#' @param axes_names the user defined keyword args for dims as a [list()]
#' @param ndim count for the number of dimensions of the input tensor
#' @return a populated [TransformRecipe()] object
#' @keywords internal
prepare_transformation_recipe <- function(expr, func, axes_names, ndim) {

    tokens <- lex(expr)

    ast <- parse_einops_ast(tokens) %>%
        validate_reduction_operation(func) %>%
        expand_ellipsis(ndim)
    
    #axis_name2known_length <- # use get_ungrouped_nodes(ast$input_axes)

    # axis_name2known_length: Dict[Union[str, AnonymousAxis], int] = OrderedDict()
    # for composite_axis in left_composition:
    #     for axis_name in composite_axis:
    #         if isinstance(axis_name, AnonymousAxis):
    #             axis_name2known_length[axis_name] = axis_name.value
    #         else:
    #             axis_name2known_length[axis_name] = _unknown_axis_length

    # TODO -> more processing
    TransformRecipe(
        # TODO
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
