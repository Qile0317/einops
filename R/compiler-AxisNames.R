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
    do.call(r2r::hashset, get_identifiers(ast))
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

#' Get the ordered axis names from a OneSidedAstNode, removing
#' all nesting.
#' @param ast OneSidedAstNode object
#' @param ... additional arguments (not used)
#' @return a [list()] of axis names
#' @keywords internal
get_ordered_axis_names <- function(ast, ...) {
    assert_that(inherits(ast, "OneSidedAstNode"))
    AxisNames(unlist(as_iterables(as_axis_names(ast)), recursive = FALSE))
}

#' Get reduced axis names by removing axes present in y from x
#' @param x AxisNames object to reduce
#' @param y AxisNames object containing axes to remove
#' @param ... additional arguments (not used)
#' @return AxisNames object with axes from x that are not in y
#' @keywords internal
get_reduced_axis_names <- function(x, y, ...) {
    assert_that(inherits(x, "AxisNames"))
    assert_that(inherits(y, "AxisNames"))
    
    # Add relative_pos to ConstantAstNode src for proper comparison
    add_relative_pos <- function(axes) {
        const_positions <- list()
        
        for (i in seq_along(axes)) {
            if (inherits(axes[[i]], "ConstantAstNode")) {
                count <- axes[[i]]$count
                if (is.null(const_positions[[as.character(count)]])) {
                    const_positions[[as.character(count)]] <- 1
                } else {
                    const_positions[[as.character(count)]] <- const_positions[[as.character(count)]] + 1
                }
                axes[[i]]$src$relative_pos <- const_positions[[as.character(count)]]
            }
        }
        axes
    }
    
    x_axes <- add_relative_pos(x)
    y_axes <- add_relative_pos(y)
    y_identifiers <- do.call(r2r::hashset, y_axes)
    
    result_axes <- Filter(function(axis) !r2r::has_key(y_identifiers, axis), x_axes)
    AxisNames(result_axes)
}
