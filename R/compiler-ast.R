#' @keywords internal
to_tokens <- function(x, ...) {
    UseMethod("to_tokens", x)
}

#' @title Create a NamedAxisAstNode
#' @param name Character string, the name of the axis
#' @param src List with start position
#' @return NamedAxisAstNode object
#' @keywords internal
NamedAxisAstNode <- function(name, src) {
    structure(list(
        name = name,
        src = src
    ), class = c("NamedAxisAstNode", "AstNode"))
}

#' @export
#' @keywords internal
to_tokens.NamedAxisAstNode <- function(x, ...) {
    list(NameToken(x$name, x$src$start))
}

#' @title Create a ConstantAstNode
#' @param count Character string representing the constant value
#' @param src List with start position
#' @return ConstantAstNode object
#' @keywords internal
ConstantAstNode <- function(count, src) {
    structure(list(
        count = count,
        src = src
    ), class = c("ConstantAstNode", "AstNode"))
}
    
#' @export
#' @keywords internal
to_tokens.ConstantAstNode <- function(x, ...) {
    list(IntToken(x$count, x$src$start))
}

#' @title Create an EllipsisAstNode
#' @param src List with start position
#' @return EllipsisAstNode object
#' @keywords internal
EllipsisAstNode <- function(src) {
    structure(list(
        src = src
    ), class = c("EllipsisAstNode", "AstNode"))
}

#' @export
#' @keywords internal
to_tokens.EllipsisAstNode <- function(x, ...) {
    list(EllipsisToken(x$src$start))
}

#' @title Create a GroupAstNode
#' @param children List of axis nodes contained in this group
#' @param src List with start position
#' @return GroupAstNode object
#' @keywords internal
GroupAstNode <- function(children, src) {
    structure(list(
        children = children,
        src = src
    ), class = c("GroupAstNode", "AstNode"))
}

#' @export
#' @keywords internal
to_tokens.GroupAstNode <- function(x, ...) {
    lparen_token <- LParenToken(x$src$start - 1)
    last_child_astnode <- tail(x$children, 1)[[1]]
    # Get the appropriate field based on the node type (TODO just make this take printed output of others)
    text_content <- if (inherits(last_child_astnode, "NamedAxisAstNode")) {
        last_child_astnode$name
    } else if (inherits(last_child_astnode, "ConstantAstNode")) {
        last_child_astnode$count
    } else {
        ""
    }
    rparen_token <- RParenToken(last_child_astnode$src$start + nchar(text_content))
    child_tokens <- lapply(x$children, to_tokens)
    c(lparen_token, unlist(child_tokens, recursive = FALSE), rparen_token)
}

#' @title Create an EinopsAst root node
#' @param input_axes List of axis nodes for the input pattern
#' @param output_axes List of axis nodes for the output pattern
#' @param src List with start position covering the full pattern
#' @return EinopsAst object
#' @keywords internal
EinopsAst <- function(input_axes, output_axes, src) {
    structure(list(
        input_axes = input_axes,
        output_axes = output_axes,
        src = src
    ), class = c("EinopsAst", "AstNode"))
}

#' @export
#' @keywords internal
to_tokens.EinopsAst <- function(x, ...) {
    input_tokens <- unlist(lapply(x$input_axes, to_tokens), recursive = FALSE)
    output_tokens <- unlist(lapply(x$output_axes, to_tokens), recursive = FALSE)
    last_input_astnode <- tail(x$input_axes, 1)[[1]]
    # Get the appropriate field based on the node type - TODO should make this take printed output of others
    text_content <- if (inherits(last_input_astnode, "NamedAxisAstNode")) {
        last_input_astnode$name
    } else if (inherits(last_input_astnode, "ConstantAstNode")) {
        last_input_astnode$count
    } else {
        ""
    }
    arrow_token <- ArrowToken(last_input_astnode$src$start + nchar(text_content) + 1)
    args <- c(input_tokens, list(arrow_token), output_tokens)
    do.call(TokenSequence, args)
}

#' @title Print method for AstNode
#' @param x AstNode object
#' @param ... Additional arguments (unused)
#' @export
print.AstNode <- function(x, ...) {
    format_value <- function(value, indent = 0) {
        indent_str <- paste(rep("  ", indent), collapse = "")
        
        if (is.character(value)) {
            return(paste0('"', value, '"'))
        } else if (is.numeric(value)) {
            return(as.character(value))
        } else if (is.list(value)) {
            if (inherits(value, "AstNode")) {
                # Recursively format nested AST nodes
                class_name <- class(value)[1]
                if (length(value) == 0) {
                    return(paste0(class_name, "()"))
                }
                
                params <- sapply(names(value), function(name) {
                    paste0("\n", indent_str, "  ", name, " = ", format_value(value[[name]], indent + 1))
                })
                return(paste0(class_name, "(", paste(params, collapse = ","), "\n", indent_str, ")"))
            } else if (length(value) == 0) {
                return("list()")
            } else if (all(sapply(value, function(x) inherits(x, "AstNode")))) {
                # List of AST nodes
                formatted_items <- sapply(value, function(item) {
                    paste0("\n", indent_str, "  ", format_value(item, indent + 1))
                })
                return(paste0("list(", paste(formatted_items, collapse = ","), "\n", indent_str, ")"))
            } else {
                # Regular list
                formatted_items <- sapply(names(value), function(name) {
                    paste0(name, " = ", format_value(value[[name]], indent))
                })
                return(paste0("list(", paste(formatted_items, collapse = ", "), ")"))
            }
        } else {
            return(as.character(value))
        }
    }
    
    class_name <- class(x)[1]
    if (length(x) == 0) {
        cat(class_name, "()\n", sep = "")
    } else {
        params <- sapply(names(x), function(name) {
            paste0("\n  ", name, " = ", format_value(x[[name]], 1))
        })
        cat(class_name, "(", paste(params, collapse = ","), "\n)\n", sep = "")
    }
    
    invisible(x)
}

#' @export
print.EinopsAst <- function(x, ...) {
    cat(glue::glue(
        "Einops Abstract Syntax Tree for '{to_expression(to_tokens(x))}':\n\n"
    ))
    print.AstNode(x, ...)
}
