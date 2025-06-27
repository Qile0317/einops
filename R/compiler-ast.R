#' @title AST Node Constructors and Einops Parser
#' @description Functions to create AST nodes and parse einops patterns

#' @title Create a NamedAxisAstNode
#' @param name Character string, the name of the axis
#' @param src List with start position and length
#' @return NamedAxisAstNode object
#' @keywords internal
NamedAxisAstNode <- function(name, src) {
    structure(list(
        name = name,
        src = src
    ), class = c("NamedAxisAstNode", "AstNode"))
}

#' @title Create a ConstantAstNode
#' @param count Character string representing the constant value
#' @param src List with start position and length
#' @return ConstantAstNode object
#' @keywords internal
ConstantAstNode <- function(count, src) {
    structure(list(
        count = count,
        src = src
    ), class = c("ConstantAstNode", "AstNode"))
}

#' @title Create an EllipsisAstNode
#' @param src List with start position and length
#' @return EllipsisAstNode object
#' @keywords internal
EllipsisAstNode <- function(src) {
    structure(list(
        src = src
    ), class = c("EllipsisAstNode", "AstNode"))
}

#' @title Create a GroupAstNode
#' @param children List of axis nodes contained in this group
#' @param src List with start position and length
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
#' @param src List with start position and length covering the full pattern
#' @return EinopsAst object
#' @keywords internal
EinopsAst <- function(input_axes, output_axes, src) {
    structure(list(
        input_axes = input_axes,
        output_axes = output_axes,
        src = src
    ), class = c("EinopsAst", "AstNode"))
}

#' @title Print method for EinopsAst
#' @param x EinopsAst object
#' @param ... Additional arguments (unused)
#' @export
print.EinopsAst <- function(x, ...) {
    # Internal function to reconstruct node text
    reconstruct_node <- function(node) {
        if (inherits(node, "NamedAxisAstNode")) {
            return(node$name)
        } else if (inherits(node, "ConstantAstNode")) {
            return(node$count)
        } else if (inherits(node, "EllipsisAstNode")) {
            return("...")
        } else if (inherits(node, "GroupAstNode")) {
            children_text <- sapply(node$children, reconstruct_node)
            return(paste0("(", paste(children_text, collapse = " "), ")"))
        } else {
            return("?")
        }
    }
    
    # Internal function to generate constructor code
    generate_constructor <- function(node, indent_level = 0) {
        src_str <- if (!is.null(node$src)) {
            paste0("list(start = ", node$src$start, ", length = ", node$src$length, ")")
        } else ""
        
        if (inherits(node, "NamedAxisAstNode")) {
            return(paste0("NamedAxisAstNode(\"", node$name, "\", ", src_str, ")"))
        } else if (inherits(node, "ConstantAstNode")) {
            return(paste0("ConstantAstNode(\"", node$count, "\", ", src_str, ")"))
        } else if (inherits(node, "EllipsisAstNode")) {
            return(paste0("EllipsisAstNode(", src_str, ")"))
        } else if (inherits(node, "GroupAstNode")) {
            child_indent <- paste(rep("    ", indent_level + 1), collapse = "")
            children_constructors <- sapply(node$children, function(child) 
                generate_constructor(child, indent_level + 2))
            
            if (length(children_constructors) == 1) {
                children_str <- paste0("list(", children_constructors[1], ")")
            } else {
                children_str <- paste0("list(\n", child_indent, "    ",
                                     paste(children_constructors, collapse = paste0(",\n", child_indent, "    ")), 
                                     "\n", child_indent, ")")
            }
            return(paste0("GroupAstNode(", children_str, ", ", src_str, ")"))
        } else {
            return("UnknownNode()")
        }
    }
    
    # Internal function to format list of nodes
    format_list <- function(nodes, indent_level = 0) {
        constructors <- sapply(nodes, function(n) generate_constructor(n, indent_level))
        if (length(constructors) == 1) {
            return(paste0("list(", constructors[1], ")"))
        } else {
            indent_str <- paste(rep("    ", indent_level + 1), collapse = "")
            return(paste0("list(\n", indent_str, "    ", 
                         paste(constructors, collapse = paste0(",\n", indent_str, "    ")), 
                         "\n", indent_str, ")"))
        }
    }
    
    # Reconstruct the expression
    input_text <- sapply(x$input_axes, reconstruct_node)
    output_text <- sapply(x$output_axes, reconstruct_node)
    reconstructed <- paste0(paste(input_text, collapse = " "), " -> ", paste(output_text, collapse = " "))
    
    cat("Reconstructed expression:", reconstructed, "\n")
    
    # Generate formatted lists
    input_list_str <- format_list(x$input_axes, indent_level = 1)
    output_list_str <- format_list(x$output_axes, indent_level = 1)
    src_str <- paste0("list(start = ", x$src$start, ", length = ", x$src$length, ")")
    
    cat("EinopsAst(\n")
    cat("    input_axes = ", input_list_str, ",\n")
    cat("    output_axes = ", output_list_str, ",\n")
    cat("    src = ", src_str, "\n")
    cat(")\n")
    
    invisible(x)
}
