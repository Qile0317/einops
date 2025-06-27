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
    EinopsTokenSequence(NameToken(x$name, x$src$start))
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
    EinopsTokenSequence(IntToken(x$count, x$src$start))
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
    EinopsTokenSequence(EllipsisToken(x$src$start))
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

#' Get the last n children of a GroupAstNode as a list of AstNodes.
#' @keywords internal
#' @export
tail.GroupAstNode <- function(x, n = 1) {
    if (n < 1) {
        stop("n must be at least 1")
    }
    tail(x$children, n)
}

#' @export
#' @keywords internal
to_tokens.GroupAstNode <- function(x, ...) {
    lparen_token <- LParenToken(x$src$start - 1)
    last_child_astnode <- tail(x, 1)[[1]]
    last_child_tokens <- to_tokens(last_child_astnode)
    last_token <- tail(last_child_tokens, 1)[[1]]
    rparen_token <- RParenToken(last_token$start + nchar(last_token$value))
    child_tokens <- lapply(x$children, to_tokens)
    asEinopsTokenSequence(
        c(lparen_token, unlist(child_tokens, recursive = FALSE), rparen_token)
    )
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
    print("=========================")
    print(last_input_astnode)
    last_input_tokens <- to_tokens(last_input_astnode)
    print("=========================")
    print(last_input_tokens)
    last_token <- tail(last_input_tokens, 1)[[1]]
    print("=========================")
    print(last_token)
    arrow_token <- ArrowToken(last_token$start + nchar(last_token$value) + 1)
    asEinopsTokenSequence(c(input_tokens, list(arrow_token), output_tokens))
}

#' @title Print method for AstNode
#' @param x AstNode object
#' @param ... Additional arguments (unused)
#' @export
print.AstNode <- function(x, ...) {
    format_value <- function(value, indent = 0) {
        indent_str <- paste(rep("    ", indent), collapse = "")
        
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
                    paste0("\n", indent_str, "    ", name, " = ", format_value(value[[name]], indent + 1))
                })
                return(paste0(class_name, "(", paste(params, collapse = ","), "\n", indent_str, ")"))
            } else if (length(value) == 0) {
                return("list()")
            } else if (all(sapply(value, function(x) inherits(x, "AstNode")))) {
                # List of AST nodes
                formatted_items <- sapply(value, function(item) {
                    paste0("\n", indent_str, "    ", format_value(item, indent + 1))
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
            paste0("\n    ", name, " = ", format_value(x[[name]], 1))
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
