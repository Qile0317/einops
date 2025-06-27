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

#' @title Merge source position information
#' @param src_a First source position (list with start, length)
#' @param src_b Second source position (list with start, length)
#' @return Combined source position with earliest start and combined length
#' @keywords internal
merge_src <- function(src_a, src_b) {
    start_a <- src_a$start
    end_a <- src_a$start + src_a$length - 1
    start_b <- src_b$start
    end_b <- src_b$start + src_b$length - 1
    
    new_start <- min(start_a, start_b)
    new_end <- max(end_a, end_b)
    
    list(
        start = new_start,
        length = new_end - new_start + 1
    )
}

#' @title Reconstruct expression text from AST node
#' @param node AST node (NamedAxisAstNode, ConstantAstNode, EllipsisAstNode, or GroupAstNode)
#' @return Character string representation
#' @keywords internal
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

#' @title Generate constructor code for AST node
#' @param node AST node
#' @return Character string with constructor call
#' @keywords internal
generate_constructor <- function(node) {
    if (inherits(node, "NamedAxisAstNode")) {
        src_str <- paste0("list(start = ", node$src$start, ", length = ", node$src$length, ")")
        return(paste0("NamedAxisAstNode(\"", node$name, "\", ", src_str, ")"))
    } else if (inherits(node, "ConstantAstNode")) {
        src_str <- paste0("list(start = ", node$src$start, ", length = ", node$src$length, ")")
        return(paste0("ConstantAstNode(\"", node$count, "\", ", src_str, ")"))
    } else if (inherits(node, "EllipsisAstNode")) {
        src_str <- paste0("list(start = ", node$src$start, ", length = ", node$src$length, ")")
        return(paste0("EllipsisAstNode(", src_str, ")"))
    } else if (inherits(node, "GroupAstNode")) {
        children_constructors <- sapply(node$children, generate_constructor)
        children_str <- paste0("list(\n        ", paste(children_constructors, collapse = ",\n        "), "\n    )")
        src_str <- paste0("list(start = ", node$src$start, ", length = ", node$src$length, ")")
        return(paste0("GroupAstNode(", children_str, ", ", src_str, ")"))
    } else {
        return("UnknownNode()")
    }
}

#' @title Print method for EinopsAst
#' @param x EinopsAst object
#' @param ... Additional arguments (unused)
#' @export
print.EinopsAst <- function(x, ...) {
    # Reconstruct the expression
    input_text <- sapply(x$input_axes, reconstruct_node)
    output_text <- sapply(x$output_axes, reconstruct_node)
    reconstructed <- paste0(paste(input_text, collapse = " "), " -> ", paste(output_text, collapse = " "))
    
    cat("Reconstructed expression:", reconstructed, "\n")
    
    # Generate constructor code
    input_constructors <- sapply(x$input_axes, generate_constructor)
    output_constructors <- sapply(x$output_axes, generate_constructor)
    
    input_list_str <- paste0("list(\n    ", paste(input_constructors, collapse = ",\n    "), "\n)")
    output_list_str <- paste0("list(\n    ", paste(output_constructors, collapse = ",\n    "), "\n)")
    src_str <- paste0("list(start = ", x$src$start, ", length = ", x$src$length, ")")
    
    cat("EinopsAst(\n")
    cat("    input_axes = ", input_list_str, ",\n")
    cat("    output_axes = ", output_list_str, ",\n")
    cat("    src = ", src_str, "\n")
    cat(")\n")
    
    invisible(x)
}

#' @title Find the arrow in a token sequence
#' @param tokens EinopsTokenSequence object
#' @return one-indexed Integer position of the arrow token
#' @keywords internal
find_top_level_arrow_index <- function(tokens) {
    arrow_positions <- which(sapply(tokens, function(x) x$type == "ARROW"))
    if (length(arrow_positions) == 0) stop("No '->' found in expression")
    if (length(arrow_positions) > 1) stop("Multiple '->' found in expression")
    arrow_positions[1]
}

#' @title Parse a sequence of axis tokens 
#' @param tokens List of tokens representing one side of the pattern
#' @return List of AST nodes
#' @keywords internal
parse_axes_iter <- function(tokens) {
    if (length(tokens) == 0) {
        stop("Empty axis pattern")
    }
    
    result <- list()
    has_ellipsis <- FALSE
    i <- 1
    
    while (i <= length(tokens)) {
        token <- tokens[[i]]
        
        if (token$type == "NAME") {
            src <- list(start = token$start, length = nchar(token$value))
            node <- NamedAxisAstNode(token$value, src)
            result <- append(result, list(node))
            
        } else if (token$type == "INT") {
            src <- list(start = token$start, length = nchar(token$value))
            node <- ConstantAstNode(token$value, src)
            result <- append(result, list(node))
            
        } else if (token$type == "ELLIPSIS") {
            if (has_ellipsis) {
                stop("Multiple ellipses found in axis pattern at position ", token$start)
            }
            has_ellipsis <- TRUE
            
            src <- list(start = token$start, length = nchar(token$value))
            node <- EllipsisAstNode(src)
            result <- append(result, list(node))
            
        } else if (token$type == "LPAREN") {
            # Find matching closing paren
            paren_depth <- 1
            group_start <- i + 1
            group_end <- i + 1
            
            while (group_end <= length(tokens) && paren_depth > 0) {
                if (tokens[[group_end]]$type == "LPAREN") {
                    paren_depth <- paren_depth + 1
                    # Check for nesting - not allowed in einops
                    if (paren_depth > 1) {
                        stop("Groups cannot be nested at position ", tokens[[group_end]]$start)
                    }
                } else if (tokens[[group_end]]$type == "RPAREN") {
                    paren_depth <- paren_depth - 1
                }
                if (paren_depth > 0) {
                    group_end <- group_end + 1
                }
            }
            
            if (paren_depth > 0) {
                stop("Unmatched opening parenthesis '(' at position ", token$start)
            }
            
            # Parse group contents
            group_tokens <- tokens[group_start:(group_end - 1)]
            if (length(group_tokens) == 0) {
                stop("Empty group '()' at position ", token$start)
            }
            
            group_children <- parse_axes_iter(group_tokens)
            
            # Calculate group source span
            rparen_token <- tokens[[group_end]]
            src <- merge_src(
                list(start = token$start, length = nchar(token$value)),
                list(start = rparen_token$start, length = nchar(rparen_token$value))
            )
            
            group_node <- GroupAstNode(group_children, src)
            result <- append(result, list(group_node))
            
            # Skip to after the closing paren
            i <- group_end
            
        } else if (token$type == "RPAREN") {
            stop("Unmatched closing parenthesis ')' at position ", token$start)
            
        } else {
            stop("Unexpected token type '", token$type, "' at position ", token$start)
        }
        
        i <- i + 1
    }
    
    result
}

#' @title Parse einops pattern into AST
#' @param tokens EinopsTokenSequence object from the lexer
#' @return EinopsAst object
#' @export
parse_einops_ast <- function(tokens) {
    if (length(tokens) == 0) {
        stop("Empty token sequence")
    }
    
    # Find the top-level arrow
    arrow_pos <- find_top_level_arrow_index(tokens)
    
    # Split into input and output slices
    if (arrow_pos == 1) {
        input_tokens <- list()
    } else {
        input_tokens <- tokens[1:(arrow_pos - 1)]
    }
    
    if (arrow_pos == length(tokens)) {
        output_tokens <- list()
    } else {
        output_tokens <- tokens[(arrow_pos + 1):length(tokens)]
    }
    
    # Parse each side
    if (length(input_tokens) == 0) {
        stop("Empty input pattern before arrow")
    }
    if (length(output_tokens) == 0) {
        stop("Empty output pattern after arrow")
    }
    
    input_axes <- parse_axes_iter(input_tokens)
    output_axes <- parse_axes_iter(output_tokens)
    
    # Create the root AST node
    first_token <- tokens[[1]]
    last_token <- tokens[[length(tokens)]]
    
    full_src <- merge_src(
        list(start = first_token$start, length = nchar(first_token$value)),
        list(start = last_token$start, length = nchar(last_token$value))
    )
    
    EinopsAst(input_axes, output_axes, full_src)
}
