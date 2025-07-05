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
#' Given a `OneSidedAstNode` object, get unique identifiers
#' @description
#' get an unordered list, representing a set of all unique
#' identifiers on one side of the expression. Named nodes become
#' the character representing its name, ConstantAstNode nodes
#' are just themselves, aside from 1 which is ignored. GroupAstNode
#' nodes are flattened, and EllipsisAstNode nodes are ignored.
#' @param ast the Abstract Syntax Tree (AST) of the einops expression
#' @param ... additional arguments (not used)
#' @return an [AxisNames()] of unique identifiers
#' @keywords internal
get_identifiers <- function(ast, ...) {
    assert_that(inherits(ast, "OneSidedAstNode"))
    identifiers <- r2r::hashset()
    for (node in get_ungrouped_nodes(ast)) {
        if (inherits(node, "EllipsisAstNode")) next
        if (inherits(node, "NamedAxisAstNode")) {
            r2r::insert(identifiers, node$name)
            next
        }
        if (inherits(node, "ConstantAstNode")) {
            if (identical(node$count, 1L)) next
            r2r::insert(identifiers, node)
            next
        }
        stop("Sourcecode bug: Unexpected node type in get_identifiers: ", class(node)[1])
    }
    AxisNames(r2r::keys(identifiers))
}
