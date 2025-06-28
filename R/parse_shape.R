#' @title Parse shape expression
#' @description This function is the entry point for parsing a shape expression.
#' @param expr expression to parse
#' @return named list of tokens to their mapped dimensions
parse_shape <- function(x, expr, ...) {
    UseMethod("parse_shape", x)
}

#' @export
parse_shape.array <- function(x, expr, ...) {
    tokens <- lex(expr)
    semantic_ast <- parse_shape_tokens(tokens, dim(x))
    shape_ir_then_execute(semantic_ast)
}

#' @title Parse shape tokens into AST, then does semantic Analysis
#' @param tokens EinopsTokenSequence
#' @return EinopsShapeSemanticAst object
#' @keywords internal
parse_shape_tokens <- function(tokens, dimensions) {
    EinopsShapeSemanticAst(tokens, dimensions)
}

#' @title EinopsShapeSemanticAst constructor
#' @param tokens parsed EinopsTokenSequence
#' @param dimensions numeric vector of array dimensions
#' @return EinopsShapeSemanticAst object
#' @keywords internal
EinopsShapeSemanticAst <- function(tokens, dimensions) {
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
            stop(glue::glue("Invalid token type '{token$type}' at position {i}. Only Name and Underscore tokens are allowed."))
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
        stop(glue::glue("Number of tokens ({n_tokens}) does not match number of dimensions ({n_dims})"))
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
        if (token$type == "Underscore") {
            next
        }
        
        dim_name <- token$value
        dim_value <- dimensions[i]
        
        # Check if we've seen this dimension name before
        if (dim_name %in% names(dimension_map)) {
            # Verify the dimension value matches
            expected_value <- dimension_map[[dim_name]]
            if (dim_value != expected_value) {
                stop(glue::glue("Dimension mismatch for '{dim_name}': expected {expected_value}, got {dim_value}"))
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
    cat(glue::glue("EinopsShapeSemanticAst for: {to_expression(x$tokens)}\n"))
    cat("Dimension mapping:\n")
    for (name in names(x$dimension_map)) {
        cat(glue::glue("  {name}: {x$dimension_map[[name]]}\n"))
    }
    invisible(x)
}

shape_ir_then_execute <- function(shape_semantic_ast) {
    shape_semantic_ast$dimension_map
}
