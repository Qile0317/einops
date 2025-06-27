#' @title AST Node Constructors and Einops Parser
#' @description Functions to create AST nodes and parse einops patterns

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

#' @title Find the top-level arrow in a token sequence
#' @param tokens EinopsTokenSequence object
#' @return Integer position of the top-level arrow token
#' @keywords internal
find_top_level_arrow <- function(tokens) {
    depth <- 0
    arrow_positions <- c()
    
    for (i in seq_along(tokens)) {
        token <- tokens[[i]]
        
        if (token$type == "LPAREN") {
            depth <- depth + 1
        } else if (token$type == "RPAREN") {
            depth <- depth - 1
        } else if (token$type == "ARROW" && depth == 0) {
            arrow_positions <- c(arrow_positions, i)
        }
    }
    
    if (length(arrow_positions) == 0) {
        stop("Missing arrow (->) in einops pattern at position ", 
             if (length(tokens) > 0) tokens[[length(tokens)]]$end else 0)
    }
    
    if (length(arrow_positions) > 1) {
        first_extra <- arrow_positions[2]
        stop("Multiple top-level arrows found in einops pattern at position ", 
             tokens[[first_extra]]$start)
    }
    
    arrow_positions[1]
}

#' @title Parse a sequence of axis tokens iteratively
#' @param tokens List of tokens representing one side of the pattern
#' @return List of AST nodes
#' @keywords internal
parse_axes_iter <- function(tokens) {
    if (length(tokens) == 0) {
        stop("Empty axis pattern")
    }
    
    # Stack of axis lists, one per nesting level
    stack <- list(list())
    has_ellipsis <- FALSE
    i <- 1
    
    while (i <= length(tokens)) {
        token <- tokens[[i]]
        current_frame <- length(stack)
        
        if (token$type == "NAME") {
            # Check if next token is an INT (count)
            count <- NULL
            if (i < length(tokens) && tokens[[i + 1]]$type == "INT") {
                count <- tokens[[i + 1]]$value
                src <- merge_src(token, tokens[[i + 1]])
                i <- i + 1  # consume the INT token
            } else {
                src <- list(start = token$start, end = token$end)
            }
            
            node <- NamedAxisAstNode(token$value, count, src)
            stack[[current_frame]] <- append(stack[[current_frame]], list(node))
            
        } else if (token$type == "ELLIPSIS") {
            if (has_ellipsis) {
                stop("Multiple ellipses found in axis pattern at position ", token$start)
            }
            has_ellipsis <- TRUE
            
            node <- EllipsisAstNode(list(start = token$start, end = token$end))
            stack[[current_frame]] <- append(stack[[current_frame]], list(node))
            
        } else if (token$type == "LPAREN") {
            # Push new empty frame onto stack
            stack <- append(stack, list(list()))
            
        } else if (token$type == "RPAREN") {
            if (length(stack) <= 1) {
                stop("Unmatched closing parenthesis ')' at position ", token$start)
            }
            
            # Pop the top frame
            popped_frame <- stack[[length(stack)]]
            stack <- stack[-length(stack)]
            
            if (length(popped_frame) == 0) {
                stop("Empty group '()' at position ", token$start)
            }
            
            # Find the source span for this group
            if (i > 1 && tokens[[i - 1]]$type != "LPAREN") {
                # Find the matching LPAREN
                lparen_pos <- i - 1
                depth <- 1
                while (lparen_pos > 0 && depth > 0) {
                    lparen_pos <- lparen_pos - 1
                    if (tokens[[lparen_pos]]$type == "RPAREN") {
                        depth <- depth + 1
                    } else if (tokens[[lparen_pos]]$type == "LPAREN") {
                        depth <- depth - 1
                    }
                }
                
                if (lparen_pos > 0) {
                    src <- merge_src(tokens[[lparen_pos]], token)
                } else {
                    src <- list(start = token$start, end = token$end)
                }
            } else {
                src <- list(start = token$start, end = token$end)
            }
            
            # Create group node and add to current frame
            group_node <- GroupAstNode(popped_frame, src)
            current_frame <- length(stack)
            stack[[current_frame]] <- append(stack[[current_frame]], list(group_node))
            
        } else {
            stop("Unexpected token type '", token$type, "' at position ", token$start)
        }
        
        i <- i + 1
    }
    
    # Final checks
    if (length(stack) != 1) {
        stop("Unmatched opening parenthesis '(' - missing closing parenthesis")
    }
    
    result <- stack[[1]]
    if (length(result) == 0) {
        stop("Empty axis pattern")
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
    arrow_pos <- find_top_level_arrow(tokens)
    
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
    full_src <- merge_src(
        list(start = tokens[[1]]$start, end = tokens[[1]]$end),
        list(start = tokens[[length(tokens)]]$start, end = tokens[[length(tokens)]]$end)
    )
    
    EinopsAst(input_axes, output_axes, full_src)
}
