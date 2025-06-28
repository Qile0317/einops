#' @title
#' Parse a tensor shape to dictionary mapping axes names to their lengths.
#'
#' @description
#' Use underscore to skip the dimension in parsing.
#'
#' @param x tensor of any supported framework
#' @param expr character of length 1, space separated names for axes,
#' underscore means skip axis
#' @param ... additional arguments passed to methods
#'
#' @return named list, maps axes names to their lengths
#' @export
#'
#' @examples
#' # Use underscore to skip the dimension in parsing.
#' x <- array(0, dim = c(2, 3, 5, 7))
#' parse_shape(x, 'batch _ h w')
#' # $batch
#' # [1] 2
#' # $h
#' # [1] 5
#' # $w
#' # [1] 7
#'
#' # `parse_shape` output can be used to specify axes_lengths for other operations:
#' y <- array(0, dim = 700)
#' shape_info <- parse_shape(x, 'b _ h w')
#' # rearrange(y, '(b c h w) -> b c h w', shape_info) would give shape (2, 10, 5, 7)
#'
parse_shape <- function(x, expr, ...) {
    backend <- get_backend(x)
    shape <- backend$shape(x)
    tokens <- lex(expr)
    onesided_ast <- parse_onesided_ast(tokens) %>%
        validate_shape_ast(shape) %>%
        preprocess_shape_ast(onesided_ast)
    # TODO
}

validate_shape_ast <- function(onesided_ast, shape) {
    throw_cannot_parse <- function() {
        stop(glue("can't parse expression with composite axes: {expr} {shape}"))
    }
    if (length(onesided_ast) == 0) {
        stop("Parsed AST is empty. Please check your expression.")
    }
    if (contains_node(onesided_ast, "GroupAstNode")) throw_cannot_parse()
    if (length(shape) != length(onesided_ast)) {
        if (contains_node(onesided_ast, "EllipsisAstNode")) {
            if (length(shape) < length(onesided_ast) - 1) {
                throw_cannot_parse()
            }
            ellipsis_index <- find_node_types_indices(
                onesided_ast, "EllipsisAstNode"
            )
            if (ellipsis_index != 1L && ellipsis_index != length(shape)) {
                throw_cannot_parse()
            }
        }
        throw_cannot_parse()
    }
    onesided_ast
}

preprocess_shape_ast <- function(onesided_ast) {
    if (!contains_node(onesided_ast, "EllipsisAstNode")) return(onesided_ast)

    ellipsis_index <- find_node_types_indices(onesided_ast, "EllipsisAstNode")
    ellipsis_side <- ifelse(ellipsis_index == 1L, "left", "right")

    onesided_ast <- onesided_ast[-ellipsis_index]
    missing_dim_count <- length(onesided_ast) - length(onesided_ast)
    new_underscore_nodes <- OneSidedAstNode(replicate(
        missing_dim_count, UnderscoreAstNode(NULL)
    ))

    if (ellipsis_side == "left") {
        onesided_ast <- append(onesided_ast, new_underscore_nodes)
    } else {
        onesided_ast <- append(new_underscore_nodes, onesided_ast)
    }

    onesided_ast
}

#' @title Parse shape tokens into AST, then does semantic Analysis
#' @param ast OneSidedAstNode
#' @param dimensions numeric vector
#' @return EinopsShapeSemanticAst object
#' @keywords internal
parse_shape_tokens <- function(ast, dimensions) {
    EinopsShapeSemanticAst(ast, dimensions)
}

#' @title EinopsShapeSemanticAst constructor
#' @param ast OneSidedAstNode
#' @param dimensions numeric vector of array dimensions
#' @return EinopsShapeSemanticAst object
#' @keywords internal
EinopsShapeSemanticAst <- function(ast, dimensions) {
    tokens <- unlist(lapply(ast, to_tokens), recursive = FALSE)
    validate_syntax(tokens)
    validate_dimension_count(tokens, dimensions)
    dimension_map <- match_dimensions(tokens, dimensions)
    structure(
        list(
            tokens = tokens,
            dimensions = dimensions,
            dimension_map = dimension_map
        ),
        class = "EinopsShapeSemanticAst"
    )
}

#' @title Validate token syntax
#' @param tokens EinopsTokenSequence
#' @keywords internal
validate_syntax <- function(tokens) {
    allowed_types <- c("NAME", "UNDERSCORE")
    for (i in seq_along(tokens)) {
        token <- tokens[[i]]
        if (!token$type %in% allowed_types) {
            stop(glue("Invalid token type '{token$type}' at position {i}. Only Name and Underscore tokens are allowed."))
        }
    }
}

#' @title Validate dimension count matches token count
#' @param tokens EinopsTokenSequence
#' @param dimensions numeric vector
#' @keywords internal
validate_dimension_count <- function(tokens, dimensions) {
    n_tokens <- length(tokens)
    n_dims <- length(dimensions)
    
    if (n_tokens != n_dims) {
        stop(glue("Number of tokens ({n_tokens}) does not match number of dimensions ({n_dims})"))
    }
}

#' @title Match tokens to their dimensional values
#' @param tokens EinopsTokenSequence
#' @param dimensions numeric vector
#' @return named list mapping dimension names to values
#' @keywords internal
match_dimensions <- function(tokens, dimensions) {
    dimension_map <- list()
    
    for (i in seq_along(tokens)) {
        token <- tokens[[i]]
        
        # Skip underscore tokens
        if (token$type == "UNDERSCORE") {
            next
        }
        
        dim_name <- token$value
        dim_value <- dimensions[i]
        
        # Check if we've seen this dimension name before
        if (dim_name %in% names(dimension_map)) {
            # Verify the dimension value matches
            expected_value <- dimension_map[[dim_name]]
            if (dim_value != expected_value) {
                stop(glue("Dimension mismatch for '{dim_name}': expected {expected_value}, got {dim_value}"))
            }
        } else {
            # First time seeing this dimension name
            dimension_map[[dim_name]] <- dim_value
        }
    }
    
    dimension_map
}

#' @export
print.EinopsShapeSemanticAst <- function(x, ...) {
    cat(glue("EinopsShapeSemanticAst for: {to_expression(x$tokens)}\n"))
    cat("Dimension mapping:\n")
    for (name in names(x$dimension_map)) {
        cat(glue("  {name}: {x$dimension_map[[name]]}\n"))
    }
    invisible(x)
}

shape_ir_then_execute <- function(shape_semantic_ast) {
    shape_semantic_ast$dimension_map
}
