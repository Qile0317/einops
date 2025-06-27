#' @title AST Node Constructors for Einops Parser
#' @description Functions to create AST nodes for the einops pattern parser

#' @title Create a NamedAxisAstNode
#' @param name Character string, the name of the axis
#' @param count Integer or NULL, optional count/size for the axis
#' @param src List with start and end positions
#' @return NamedAxisAstNode object
#' @keywords internal
NamedAxisAstNode <- function(name, count, src) {
  structure(list(
    name = name,
    count = count,
    src = src
  ), class = c("NamedAxisAstNode", "AstNode"))
}

#' @title Create an EllipsisAstNode
#' @param src List with start and end positions
#' @return EllipsisAstNode object
#' @keywords internal
EllipsisAstNode <- function(src) {
  structure(list(
    src = src
  ), class = c("EllipsisAstNode", "AstNode"))
}

#' @title Create a GroupAstNode
#' @param children List of axis nodes contained in this group
#' @param src List with start and end positions
#' @return GroupAstNode object
#' @keywords internal
GroupAstNode <- function(children, src) {
  structure(list(
    children = children,
    src = src
  ), class = c("GroupAstNode", "AstNode"))
}

#' @title Create an EinopsAst root node
#' @param input_axes List of axis nodes for the input pattern
#' @param output_axes List of axis nodes for the output pattern
#' @param src List with start and end positions covering the full pattern
#' @return EinopsAst object
#' @keywords internal
EinopsAst <- function(input_axes, output_axes, src) {
  structure(list(
    input_axes = input_axes,
    output_axes = output_axes,
    src = src
  ), class = c("EinopsAst", "AstNode"))
}

#' @title Merge source position information
#' @param src_a First source position (list with start, end)
#' @param src_b Second source position (list with start, end)
#' @return Combined source position with earliest start and latest end
#' @keywords internal
merge_src <- function(src_a, src_b) {
  list(
    start = min(src_a$start, src_b$start),
    end = max(src_a$end, src_b$end)
  )
}

#' @title Print method for EinopsAst
#' @param x EinopsAst object
#' @param ... Additional arguments (unused)
#' @export
print.EinopsAst <- function(x, ...) {
  cat("EinopsAst:\n")
  cat("  Input axes: ", length(x$input_axes), " nodes\n")
  cat("  Output axes: ", length(x$output_axes), " nodes\n")
  cat("  Source span: ", x$src$start, "-", x$src$end, "\n")
  invisible(x)
}
