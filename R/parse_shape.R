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
#'
#' # `parse_shape` output can be used to specify axes_lengths for other
#' # operations:
#' y <- array(0, dim = 700)
#' shape_info <- parse_shape(x, 'b _ h w')
#' # rearrange(y, '(b c h w) -> b c h w', shape_info) would give shape
#' # (2, 10, 5, 7)
parse_shape <- function(x, expr, ...) {
    tryCatch(
        .parse_shape(x, expr, ...),
        error = function(e) {
            stop("In parse_shape - ", conditionMessage(e), call. = FALSE)
        }
    )
}

.parse_shape <- function(x, expr, ...) {

    backend <- get_backend(x)
    shape <- backend$shape(x)
    tokens <- lex(expr)
    onesided_ast <- parse_onesided_ast(tokens) %>%
        validate_shape_ast(shape, expr) %>%
        preprocess_shape_ast(shape)
    
    result <- list()
    for (i in seq_along(shape)) {

        axes_node <- onesided_ast[[i]]

        if (inherits(axes_node, "UnderscoreAstNode")) next

        axis_length <- shape[i]

        if (inherits(axes_node, "NamedAxisAstNode")) {
            axis_name <- axes_node$name
            result[[axis_name]] <- axis_length
            next
        }

        if (inherits(axes_node, "ConstantAstNode")) {
            if (axes_node$count != axis_length) {
                stop(glue(
                    "Length of anonymous axis does not match: '{expr}' ",
                    "{repr(shape, indent = 0L)}"
                ))
            }
            next
        }

        stop(glue(
            "Unexpected node type in shape AST: ",
            "`{repr(axes_node, indent = 0L)}` ",
            "for expression: '{expr}' and shape: {repr(shape, indent = 0L)}. ",
            "Please report this as a bug.",
        ))

    }
    result
}

validate_shape_ast <- function(onesided_ast, shape, expr) {

    if (length(onesided_ast) == 0) {
        stop("Parsed AST is empty. Please check your expression.")
    }

    if (has_composed_axes(onesided_ast)) {
        stop(glue(
            "can't parse expression with composite axes: '{expr}' ",
            "{repr(shape, indent = 0L)}"
        ))
    }

    throw_length_error <- function() {
        stop(glue(
            "Shape length {length(shape)} does not match the number of ",
            "axes in the expression {length(onesided_ast)}. ",
            "Please check your expression: '{expr}'."
        ))
    }

    if (length(shape) != length(onesided_ast)) {
        if (!has_ellipsis(onesided_ast)) throw_length_error()
        if (length(shape) < length(onesided_ast) - 1) throw_length_error()
    }

    onesided_ast
}

# process OneSidedAstNode for parse_shape by replacing ellipses w/underscore(s)
preprocess_shape_ast <- function(onesided_ast, shape) {

    if (!contains_node(onesided_ast, "EllipsisAstNode")) return(onesided_ast)

    ellipsis_idx <- find_node_types_indices(onesided_ast, "EllipsisAstNode")
    if (is.null(shape)) {
        stop(
            "Shape must be provided when ellipsis is present in the expression."
        )
    }

    n_shape <- length(shape)
    missing_dim_count <- n_shape - (length(onesided_ast) - 1)
    if (missing_dim_count < 0) {
        stop("Too many axes in expression for the shape.")
    }
    new_underscore_nodes <- replicate(
        missing_dim_count, UnderscoreAstNode(), simplify = FALSE
    )
    
    result <- OneSidedAstNode()
    
    # Add nodes before ellipsis
    if (ellipsis_idx > 1) {
        result <- append(result, onesided_ast[seq_len(ellipsis_idx - 1)])
    }
    
    # Add new underscore nodes
    result <- append(result, new_underscore_nodes)
    
    # Add nodes after ellipsis
    if (ellipsis_idx < length(onesided_ast)) {
        result <- append(
            result, onesided_ast[(ellipsis_idx + 1):length(onesided_ast)]
        )
    }
    
    result
}
