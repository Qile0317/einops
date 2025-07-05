#' @title
#' Construct an instance of an `AxisNames` class
#' @description
#' This is a wrapper for a [list()], but the elements may only be singular
#' [character()], [ConstantAstNode()] or other AstNode objects. (only 1 level
#' of nesting allowed, as its essentially another representation of a
#' [OneSidedAstNode()].
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

#' @title Convert an AstNode into an [AxisNames()] object
#' @param ast the AstNode input
#' @param ... other args
#' @return an [AxisNames()] object of the axes in order
#' @keywords internal
#' @examples
#' get_axis_names(parse_onesided_ast(lex('a (b c 1) 2 d')))
#' # should output AxisNames("a", AxisNames("b", "c", ConstantAstNode(1)), ConstantAstNode(2), "d")
as_axis_names <- function(ast, ...) UseMethod("as_axis_names")

#' @export
as_axis_names.GroupAstNode <- function(ast, ...) {
    AxisNames(lapply(ast$children, function(x) {
        if (inherits(x, "ConstantAstNode")) return(x)
        if (inherits(x, "NamedAxisAstNode")) return(x$name)
        stop("Not supported")
    }))
}

#' @export
as_axis_names.OneSidedAstNode <- function(ast, ...) {
    AxisNames(lapply(unclass(ast), function(x) {
        if (inherits(x, "ConstantAstNode")) return(x)
        if (inherits(x, "NamedAxisAstNode")) return(x$name)
        if (inherits(x, "GroupAstNode")) return(as_axis_names(x))
        stop("Not supported")
    }))
}
