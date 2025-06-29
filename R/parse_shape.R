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
#' # `parse_shape` output can be used to specify axes_lengths for other
#' # operations:
#' y <- array(0, dim = 700)
#' shape_info <- parse_shape(x, 'b _ h w')
#' # rearrange(y, '(b c h w) -> b c h w', shape_info) would give shape
#' # (2, 10, 5, 7)
#'
parse_shape <- function(x, expr, ...) {

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
                    "{capture.output(print(shape))}"
                ))
            }
            next
        }

        stop(glue(
            "Unexpected node type in shape AST: ",
            "{capture.output(print(class(axes_node)))} ",
            "for expression: {expr} and shape: ",
            "{capture.output(print(shape))}. ",
            "Please report this as a bug.",
        ))

    }
    return(result)
}

validate_shape_ast <- function(onesided_ast, shape, expr) {
    throw_cannot_parse <- function() {
        stop(glue(
            "can't parse expression with composite axes: {expr} ",
            "{capture.output(print(shape))}"
        ))
    }
    if (length(onesided_ast) == 0) {
        stop("Parsed AST is empty. Please check your expression.")
    }
    if (contains_node(onesided_ast, "GroupAstNode")) throw_cannot_parse()
    if (length(shape) != length(onesided_ast)) {
        if (has_ellipsis(onesided_ast)) {
            if (length(shape) < length(onesided_ast) - 1) {
                stop(glue(
                    "Shape length {length(shape)} is < the number of axes ",
                    "in the expression {length(onesided_ast)}. ",
                    "Please check your expression: {expr}."
                ))
            }
        } else {
            stop(glue(
                "Shape length {length(shape)} does not match the number of ",
                "axes in the expression {length(onesided_ast)}. ",
                "Please check your expression: {expr}."
            ))
        }
    }
    onesided_ast
}

# process OneSidedAstNode for parse_shape by replacing ellipses w/underscore(s)
preprocess_shape_ast <- function(onesided_ast, shape) {
    if (!contains_node(onesided_ast, "EllipsisAstNode")) return(onesided_ast)

    ellipsis_index <- find_node_types_indices(onesided_ast, "EllipsisAstNode")
    if (is.null(shape)) {
        stop("Shape must be provided when ellipsis is present in the expression.")
    }
    n_ast <- length(onesided_ast)
    n_shape <- length(shape)
    missing_dim_count <- n_shape - (n_ast - 1)
    if (missing_dim_count < 0) {
        stop("Too many axes in expression for the shape.")
    }
    new_underscore_nodes <- replicate(
        missing_dim_count, UnderscoreAstNode(list()), simplify = FALSE
    )
    before <- if (ellipsis_index > 1) onesided_ast[seq_len(ellipsis_index - 1)] else list()
    after <- if (ellipsis_index < n_ast) onesided_ast[(ellipsis_index + 1):n_ast] else list()
    onesided_ast <- OneSidedAstNode(c(before, new_underscore_nodes, after))
    onesided_ast
}
