#' @title TransformRecipe S3 constructor
#' @description Recipe describes actual computation pathway. Can be applied to a tensor or variable.
#' @param elementary_axes_lengths Integer vector. List of sizes for elementary axes as they appear in left expression.
#' @param axis_name2elementary_axis Named integer vector. Mapping from name to position.
#' @param input_composition_known_unknown List of list(present, unknown) integer vectors. Each element is a tuple of known and unknown indices.
#' @param axes_permutation Integer vector. Permutation applied to elementary axes.
#' @param first_reduced_axis Integer. First position of reduced axes.
#' @param added_axes Named integer vector. Axis position -> axis index.
#' @param output_composite_axes List of integer vectors. Ids of axes as they appear in result.
#' @return An object of class 'TransformRecipe'.
#' @export
#' @importFrom assertthat assert_that is.string is.count is.flag
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
        is.list(axis_name2elementary_axis) || is.integer(axis_name2elementary_axis) || is.numeric(axis_name2elementary_axis)
    )

    structure(list(
        elementary_axes_lengths = elementary_axes_lengths,
        axis_name2elementary_axis = axis_name2elementary_axis,
        input_composition_known_unknown = input_composition_known_unknown,
        axes_permutation = axes_permutation,
        first_reduced_axis = first_reduced_axis,
        added_axes = added_axes,
        output_composite_axes = output_composite_axes
    ), class = "TransformRecipe")
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
    ast <- parse_einops_ast(tokens)
    validate_reduction_operation(func, ast)
    # TODO
}
