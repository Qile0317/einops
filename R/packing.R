#' Given an ast for pack/unpack, return
#' c(n_axes_before, n_axes_after, min_axes)
#' @noRd
analyze_ast <- function(ast) {

    assert_that(inherits(ast, "OneSidedAstNode"))
    axes_set <- unique(lapply(ast, clear_srcs))

    if (length(axes_set) != length(ast)) {
        stop(glue("Duplicates in axes names"), call. = FALSE)
    }

    asterisk_index <- find_node_types_indices(ast, "AsteriskAstNode")
    if (length(asterisk_index) == 0L) {
        stop(glue("No *-axis"), call. = FALSE)
    }
    if (length(asterisk_index) > 1L) {
        stop(glue("More than one *-axis')"), call. = FALSE)
    }
    # TODO more syntactic checking

    c(n_axes_before = asterisk_index,
      n_axes_after = length(ast) - asterisk_index,
      min_axes = length(ast))
}

#' @title
#' Packs several tensors into one.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function can pack a list of tensors into a single tensor based
#' on the `pattern`, comprising of a sequence of letters and one asterisk.
#' See the vignette for more information on how this all works.
#'
#' This function's output is a list of length 2, with the first element
#' being the actually packed tensor, and the second being a list of
#' integers indicating the packed shapes, a nice piece of syntactic sugar
#' for assigning these results is to use the `zeallot::"%<-%"` operator
#' to assign 2 variables like such: `c(packed, ps) %<-% pack(x, "a b *")`
#'
#' @param tensors Tensors to be packed. Can be of different dimensionality.
#' @param pattern Pattern shared for all inputs and output, e.g. `"i j * k"`
#'   or `"batch seq *"`.
#'
#' @return A list containing the packed tensor and packed shapes (PS).
#' @export
#' @keywords internal
#' @seealso [unpack()]
#' @examples
#' library(zeallot)
#' inputs <- list(
#'     array(0, c(2, 3, 5)),
#'     array(0, c(2, 3, 7, 5)),
#'     array(0, c(2, 3, 7, 9, 5))
#' )
#' packed, ps %<-% pack(inputs, 'i j * k')
#' # the line above is identical to assigning each of those
#' # variables to the first and second elements of the output
#'
#' print(dim(packed))
#' #> c(2, 3, 71, 5)
#' print(ps)
#' #> list(integer(), 7, c(7, 9))
#'
#' # In this example, axes were matched to: i=2, j=3, k=5 based on order
#' # (first, second, and last). All other axes were 'packed' and
#' # concatenated. PS (packed shapes) contains information about axes
#' # matched to '*' in every input. The resulting tensor has as many
#' # elements as all inputs in total.
#'
#' # Packing can be reversed with unpack, which additionally needs PS
#' # (packed shapes) to reconstruct order.
#'
#' # inputs_unpacked <- unpack(packed, ps, 'i j * k')
#' # [x.shape for x in inputs_unpacked]
#' # [(2, 3, 5), (2, 3, 7, 5), (2, 3, 7, 9, 5)]
#'
#' # Read the tutorial for introduction and application
pack <- function(
    tensors, pattern, return_ps = getOption("einops_pack_return_ps", TRUE), ...
) {
    UseMethod("pack")
}

#' @export
pack.default <- function(
    tensors, pattern, return_ps = getOption("einops_pack_return_ps", TRUE), ...
) {
    pack.list(list(tensors), pattern, return_ps, ...)
}

#' @export
pack.list <- function(
    tensors, pattern, return_ps = getOption("einops_pack_return_ps", TRUE), ...
) {

    assert_that(is.string(pattern), is.flag(return_ps))

    onesided_ast <- lex(pattern) %>%
        parse_onesided_ast() %>%
        validate_pack_ast()

    c(n_axes_before, n_axes_after, min_axes) %<-% analyze_ast(onesided_ast)

    backend <- get_backend(tensors[[1]])

    reshaped_tensors <- list()
    if (return_ps) packed_shapes <- list()
    for (i in seq_along(tensors)) {
        shape <- backend$shape(tensors[[i]])
        if (length(shape) < min_axes) {
            stop(glue(
                "packed tensor #{i} has shape {repr(shape)}, ",
                "while pattern '{pattern}' assumes at least {min_axes} ",
                "axes"
            ))
        }
        axis_after_packed_axes = length(shape) - n_axes_after
        if (return_ps) {
            packed_shapes %<>% append(list(
                shape[n_axes_before:(axis_after_packed_axes - 1)]
            ))
        }
        reshaped_tensors %<>% append(list( # FIXME probably wrong
            backend$reshape(
                tensors[[i]],
                c(shape[1:(n_axes_before - 1)],
                  -1,
                  shape[axis_after_packed_axes: length(shape)])
            )
        ))
    }
    
    reshaped_tensors %<>% backend$concat(n_axes_before)
    if (!return_ps) return(reshaped_tensors)
    list(reshaped_tensors, packed_shapes)
}

validate_pack_ast <- function(onesided_ast) {
    valid_node_types <- c("NamedAxisAstNode", "AsteriskAstNode")
    for (node in onesided_ast) {
        is_valid <- any(sapply(
            valid_node_types, function(type) inherits(node, type)
        ))
        if (is_valid) next
        stop("Invalid input token(s)")
    }
    onesided_ast
}

#' @title
#' Unpack a single tensor to several in a list
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Unpacks a single tensor into several by splitting over a selected axes.
#' See einops tutorial for introduction into packing (and how it replaces
#' stack and concatenation).
#'
#' If framework supports views, results are views to the original tensor.
#'
#' @param tensor tensor to be unpacked
#' @param packed_shapes list of shapes that take the place of `'*'` in each
#' output. will contain a single tensor for every provided shape
#' @param pattern a string pattern that is shared for input and all outputs,
#' e.g. `"i j * k"` or `"batch seq *"`, where * designates an axis to be
#' unpacked.
#' @return [list()] of tensors of the same type
#' @keywords internal
#' @seealso [pack()]
#' @export
#' @inherit pack examples
unpack <- function(tensor, packed_shapes, pattern) {

    assert_that(is.list(packed_shapes), is.string(pattern))

    onesided_ast <- lex(pattern) %>%
        parse_onesided_ast() %>%
        validate_pack_ast()
    
    c(n_axes_before, n_axes_after, min_axes) %<-% analyze_ast(onesided_ast)

    backend <- get_backend(tensor)
    input_shape <- backend$shape(tensor)
    if (length(input_shape) != n_axes_before + 1 + n_axes_after) {
        stop("Received input of wrong dim with shape {repr(input_shape)}")
    }

    unpacked_axis <- n_axes_before
    
#     lengths_of_composed_axes: List[int] = [-1 if -1 in p_shape else prod(p_shape) for p_shape in packed_shapes]

#     n_unknown_composed_axes = sum(int(x == -1) for x in lengths_of_composed_axes)
#     if n_unknown_composed_axes > 1:
#         raise EinopsError(
#             f"unpack(..., {pattern}) received more than one -1 in {packed_shapes} and can't infer dimensions"
#         )

#     # following manipulations allow to skip some shape verifications
#     # and leave it to backends

#     # [[], [2, 3], [4], [-1, 5], [6]] < examples of packed_axis
#     # split positions when computed should be
#     # [0,   1,      7,   11,      N-6 , N ], where N = length of axis
#     split_positions = [0] * len(packed_shapes) + [input_shape[unpacked_axis]]
#     if n_unknown_composed_axes == 0:
#         for i, x in enumerate(lengths_of_composed_axes[:-1]):
#             split_positions[i + 1] = split_positions[i] + x
#     else:
#         unknown_composed_axis: int = lengths_of_composed_axes.index(-1)
#         for i in range(unknown_composed_axis):
#             split_positions[i + 1] = split_positions[i] + lengths_of_composed_axes[i]
#         for j in range(unknown_composed_axis + 1, len(lengths_of_composed_axes))[::-1]:
#             split_positions[j] = split_positions[j + 1] - lengths_of_composed_axes[j]

#     shape_start = input_shape[:unpacked_axis]
#     shape_end = input_shape[unpacked_axis + 1 :]
#     slice_filler = (slice(None, None),) * unpacked_axis
#     try:
#         return [
#             backend.reshape(
#                 # shortest way slice arbitrary axis
#                 tensor[(*slice_filler, slice(split_positions[i], split_positions[i + 1]))],
#                 (*shape_start, *element_shape, *shape_end),
#             )
#             for i, element_shape in enumerate(packed_shapes)
#         ]
#     except Exception as e:
#         # this hits if there is an error during reshapes, which means passed shapes were incorrect
#         raise EinopsError(
#             f'Error during unpack(..., "{pattern}"): could not split axis of size {split_positions[-1]}'
#             f" into requested {packed_shapes}"
#         ) from e
    stop("Not implemented")
}
