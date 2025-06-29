# for all core operations (reduce, rearrrange, repeat), they are all
# implemented as different reduction operations

valid_reduction_strings <- function() {
    c("min", "max", "sum", "mean", "prod", "any", "all")
}

validate_reduction_operation <- function(operation, einops_ast) {

    left <- einops_ast$input_axes
    rght <- einops_ast$output_axes

    # check for nested brackets
    for (axes in list(left, rght)) {
        group_nodes <- axes[find_node_types_indices(axes, "GroupAstNode")]
        for (grp_node in group_nodes) {
            if (contains_node(grp_node$children, "GroupAstNode")) {
                stop(glue(
                    "Nested brackets are not allowed in the expression: ",
                    "{to_expression(einops_ast)}"
                ))
            }
        }
    }

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

    # TODO throw error on symmetric difference of unique identifier lengths

    if (operation == "rearrange") {
        if (has_non_unitary_anonymous_axes(einops_ast)) {
            stop(
                "Non-unitary anonymous axes are not supported in rearrange ",
                "(exception is length 1)"
            )
        }
        # TODO throw error on symmetric difference of unique identifier lengths
        return()
    }
    if (operation == "repeat") {
        # TODO throw error on symmetric difference of unique identifier lengths
        # TODO axes_without_size <- set difference of
        #   {ax for ax in rght.identifiers if not isinstance(ax, AnonymousAxis)},
        #   {*left.identifiers, *axes_names}
        # TODO if (length(axes_without_size) > 0) return error
        return()
    }
    if (is.function(operation) ||
        operation %in% valid_reduction_strings()) { # nolint
        # TODO throw error on symmetric difference of unique identifier lengths
        return()
    }

    stop(glue(
        "Unknown reduction {capture.output(print(operation))}. ",
        "Expect one of {valid_reduction_strings()} or a function."
    ))

}

contains_node <- function(x, node_type, ...) {
    UseMethod("contains_node", x)
}

#' @export
contains_node.OneSidedAstNode <- function(x, node_type, ...) {
    any(sapply(x, function(child) inherits(child, node_type)))
}

has_ellipsis <- function(onesided_ast) {
    contains_node(onesided_ast, "EllipsisAstNode")
}

has_ellipsis_parenthesized <- function(onesided_ast) {
    if (!contains_node(onesided_ast, "GroupAstNode")) return(FALSE)
    any(sapply(
        onesided_ast, function(child) inherits(child, "EllipsisAstNode")
    ))
}

has_non_unitary_anonymous_axes <- function(x, ...) {
    UseMethod("has_non_unitary_anonymous_axes", x)
}

#' @export
has_non_unitary_anonymous_axes.OneSidedAstNode <- function(x, ...) {
    any(sapply(x, function(child) {
        if (inherits(child, "ConstantAstNode")) {
            child$count > 1L
        }
    }))
}

#' @export
has_non_unitary_anonymous_axes.EinopsAst <- function(x, ...) {
    has_non_unitary_anonymous_axes(x$input_axes) ||
        has_non_unitary_anonymous_axes(x$output_axes)
}

find_node_types_indices <- function(x, node_type, ...) {
    UseMethod("find_node_types_indices", x)
}

#' @export
find_node_types_indices.OneSidedAstNode <- function(x, node_type, ...) {
    indices <- which(sapply(x, function(child) inherits(child, node_type)))
    if (length(indices) == 0) {
        return(integer(0))
    }
    indices
}

#' Determine whether any composed axes are present in
#' the AST, IGNORING any 1's within brackets
#' @noRd
has_composed_axes <- function(x, ...) {
    UseMethod("has_composed_axes", x)
}

#' @export
has_composed_axes.OneSidedAstNode <- function(x, ...) {
    if (!contains_node(x, "GroupAstNode")) return(FALSE)
    group_indices <- find_node_types_indices(x, "GroupAstNode")
    any(sapply(group_indices, function(grp_node) {
        any(sapply(grp_node$children, function(node) {
            inherits(node, "ConstantAstNode") && node$count > 1L
        }))
    }))
}

# #' Get unique identifier set (axes and ellipses, excluding 1 and _)
# #' as a list
# #' @noRd
# identifiers <- function(x, ...) {
#     UseMethod("identifiers", x)
# }

# #' @export
# identifiers.OneSidedAstNode <- function(x, ...) {
#     ast_node_set <- list()
#     for (child_node in x) {

#     }
# }
