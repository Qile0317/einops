#' @title
#' Construct an instance of an `AxisNames` class
#' @description
#' This is a wrapper for a [list()], but the elements may only be singular
#' [character()], [ConstantAstNode()] or other AstNode objects. (only 1 level
#' of nesting allowed, as its essentially another representation of a
#' [OneSidedAstNode()].
#'
#' Note that when using [c()] on an `AxisNames` object, when the other object is
#' another [AxisNames()], the elements of that will simply be appended to the
#' first object. To nest (in the case of a `GroupAstNode`), you must append
#' a `list(AxisNames(...))` to the first object instead.
#' @param ... a list of elements or arbitrary number of elements
#' @return an `AxisNames` object, which is a list with a specific class
#' @keywords internal
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
#' Convert an object to a list of iterables (lists)
#' @description
#' This function converts an object into a list of iterables, where each
#' iterable is a list of elements. The function is generic and can be
#' extended for different classes.
#' @param x the input object to be converted
#' @param ... additional arguments (not used)
#' @return a list of iterables, where each iterable is a list of elements
#' @section AxisNames:
#' This method converts an `AxisNames` object into a list of iterables.
#' Each element in the `AxisNames` is processed to extract its name or
#' constant value. If the element is a `ConstantAstNode` with a count of
#' 1, it is converted to an empty list. Otherwise, it is wrapped in
#' a list. If the element is a `NamedAxisAstNode`, its name is extracted.
#' Regular AxisNames that are nested will be unclassed
#' @keywords internal
as_iterables <- function(x, ...) UseMethod("as_iterables")

#' @export
as_iterables.AxisNames <- function(x, ...) {
    if (length(x) == 0L) return(list())
    lapply(x, function(y) {
        if (inherits(y, "AxisNames")) return(unclass(y))
        if (inherits(y, "ConstantAstNode")) {
            if (y$count == 1L) return(list())
            return(list(y))
        }
        if (!inherits(y, "character")) {
            stop(
                "Sourcecode bug: Unexpected node type in as_iterables: ",
                class(y)[1]
            )
        }
        list(y)
    })
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
#' positions to the `ConstantAstNode` objects in the output. This will
#' REMOVE all other elements in the src list.
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
        stop(
            "Sourcecode bug: Unexpected node type in get_identifiers: ",
            class(node)[1]
        )
    }
    AxisNames(r2r::keys(identifiers))
}

get_identifiers_hashset <- function(ast, ...) {
    do.call(r2r::hashset, get_identifiers(ast, ...))
}

#' @title Convert an AstNode into an [AxisNames()] object
#' @param ast the AstNode input
#' @param ... other args
#' @return an [AxisNames()] object of the axes in order
#' @keywords internal
#'
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
    if (length(ast) == 0L) return(AxisNames())
    AxisNames(lapply(unclass(ast), function(x) {
        if (inherits(x, "ConstantAstNode")) return(x)
        if (inherits(x, "NamedAxisAstNode")) return(x$name)
        if (inherits(x, "GroupAstNode")) return(as_axis_names(x))
        stop("Not supported")
    }))
}

#' Get the ordered axis names from a OneSidedAstNode, removing
#' all nesting.
#' @param ast OneSidedAstNode object
#' @param ... additional arguments (not used)
#' @return a [list()] of axis names
#' @keywords internal
get_ordered_axis_names <- function(ast, ...) {
    assert_that(inherits(ast, "OneSidedAstNode"))
    if (length(ast) == 0L) return(AxisNames())
    AxisNames(unlist(as_iterables(as_axis_names(ast)), recursive = FALSE))
}

# check of an object can be a single element within a flat [AxisNames()]
# object. This is used for code readability.
is_flat_axis_names_element <- function(x) {
    inherits(x, "ConstantAstNode") || is.string(x)
}
