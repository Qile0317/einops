#' @title Parse einops pattern into AST
#' @param tokens EinopsTokenSequence object from the lexer
#' @return EinopsAst object
#' @keywords internal
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
        output_axes <- list(NothingAstNode())
    } else {
        output_axes <- parse_axes_iter(output_tokens)
    }
    input_axes <- parse_axes_iter(input_tokens)
    # Create the root AST node
    first_token <- tokens[[1]]
    full_src <- list(start = first_token$start)
    EinopsAst(input_axes, output_axes, full_src)
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

#' @title Parse a one-sided einops pattern into OneSidedAstNode
#' @param tokens EinopsTokenSequence object from the lexer (no arrow expected)
#' @return OneSidedAstNode object
#' @keywords internal
parse_onesided_ast <- function(tokens) {
    if (length(tokens) == 0) {
        stop("Empty token sequence")
    }
    axes <- parse_axes_iter(tokens)
    OneSidedAstNode(axes)
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
            src <- list(start = token$start)
            node <- NamedAxisAstNode(token$value, src)
            result <- append(result, list(node))
            
        } else if (token$type == "INT") {
            src <- list(start = token$start)
            node <- ConstantAstNode(token$value, src)
            result <- append(result, list(node))
            
        } else if (token$type == "ELLIPSIS") {
            if (has_ellipsis) {
                stop("Multiple ellipses found in axis pattern at position ", token$start)
            }
            has_ellipsis <- TRUE
            
            src <- list(start = token$start)
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
            if (group_start > group_end - 1) {
                # Empty group
                group_children <- list()
            } else {
                group_tokens <- tokens[group_start:(group_end - 1)]
                group_children <- parse_axes_iter(group_tokens)
            }
            
            # Use opening paren position as group source
            src <- list(start = token$start)
            
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

