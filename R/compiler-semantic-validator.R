# for all core operations (reduce, rearrrange, repeat), they are all
# implemented as different reduction operations

validate_reduction_operation <- function(operation, einops_ast) {

    left <- einops_ast$input_axes
    rght <- einops_ast$output_axes

    if (!has_ellipsis(left) && has_ellipsis(rght)) {
        stop(glue(
            "Ellipsis found in right side, but not left side of a ",
            "pattern: {to_expression(einops_ast)}"
        ))
    }

    if (has_ellipsis(left) && has_ellipsis_parenthesized(left)) {
        stop(glue(
            "Ellipsis inside parenthesis in the left side is not allowed: ",
            "{to_expression(einops_ast)}"
        ))
    }

    if (operation == "rearrange") {
        
    }

}

#     if operation == "rearrange":
#         if left.has_non_unitary_anonymous_axes or rght.has_non_unitary_anonymous_axes:
#             raise EinopsError("Non-unitary anonymous axes are not supported in rearrange (exception is length 1)")
#         difference = set.symmetric_difference(left.identifiers, rght.identifiers)
#         if len(difference) > 0:
#             raise EinopsError(f"Identifiers only on one side of expression (should be on both): {difference}")
#     elif operation == "repeat":
#         difference = set.difference(left.identifiers, rght.identifiers)
#         if len(difference) > 0:
#             raise EinopsError(f"Unexpected identifiers on the left side of repeat: {difference}")
#         axes_without_size = set.difference(
#             {ax for ax in rght.identifiers if not isinstance(ax, AnonymousAxis)},
#             {*left.identifiers, *axes_names},
#         )
#         if len(axes_without_size) > 0:
#             raise EinopsError(f"Specify sizes for new axes in repeat: {axes_without_size}")
#     elif operation in _reductions or callable(operation):
#         difference = set.difference(rght.identifiers, left.identifiers)
#         if len(difference) > 0:
#             raise EinopsError(f"Unexpected identifiers on the right side of reduce {operation}: {difference}")
#     else:
#         raise EinopsError(f"Unknown reduction {operation}. Expect one of {_reductions}.")