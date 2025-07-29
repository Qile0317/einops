#' Perform Einstein-style tensor operations
#'
#' @description
#' A unified interface for rearranging, reducing, and repeating tensor
#' dimensions using Einstein notation-inspired expressions. This was directly
#' adapted from the python `einop` package: https://github.com/cgarciae/einop
#'
#' `einop()` auto detects the operation type based on the provided expression:
#' - [rearrange()] when all input dimensions are present in output
#' - [reduce()] when some input dimensions are missing from output
#' - [einops.repeat()] when output contains dimensions not in input
#'
#' Note that there are ongoing debates about the use of this function purely
#' from the perspective of code readability and maintainability:
#' <https://github.com/arogozhnikov/einops/issues/84>. Generally, some argue
#' that the descriptive names of `rearrange`, `reduce`, and `repeat`
#' encourage good practices, while others think that semantically `einop()`
#' actually makes it *clearer* what the operation
#' is doing, as opposed to mandating the use of these commonly used function
#' names across many packages.
#'
#' @param reduction A string specifying the reduction operation (e.g.,
#' "mean", "sum", "max"). Required for reduce operations, ignored for
#' rearrange and repeat operations.
#' @inheritParams reduce
#'
#' @return A tensor with dimensions transformed according to the expression
#' @export
#' @examples
#' if (requireNamespace("abind", quietly = TRUE)) {
#'
#' # load a 3d tensor representing an rgb image
#' x <- get(data("einops_image"))[1, , , ]
#'
#' # Rearrange dimensions
#' einop(x, "h w c -> c h w")
#'
#' # Reduce dimensions
#' einop(x, "h w c -> h w", "mean")
#'
#' # Repeat dimensions
#' einop(x[, , 1], "h w -> h w c", c = 3)
#'
#' }
#'
einop <- function(
    x,
    expr,
    reduction = NULL,
    ...,
    .row_major = getOption("einops_row_major", FALSE)
) {
    
    op <- match_einop(expr, reduction, ...)

    if (op == "rearrange") {
        if (!is.null(reduction)) {
            stop(
                "Got reduction operation but there is no dimension to reduce ",
                "in pattern: ", expr
            )
        }
        return(rearrange(x, expr, ..., .row_major = .row_major))
    } else if (op == "reduce") {
        if (is.null(reduction)) {
            stop("Missing reduction operation for reduce pattern: ", expr)
        }
        return(reduce(x, expr, reduction, ..., .row_major = .row_major))
    } else if (op == "repeat") {
        if (!is.null(reduction)) {
            stop("Do not pass reduction for repeat pattern: ", expr)
        }
        return(einops.repeat(x, expr, ..., .row_major = .row_major))
    } else {
        stop("Unknown operation: ", op)
    }
}

match_einop <- function(expr, reduction = NULL, ...) {

    assert_that(
        is.string(expr),
        is.string(reduction) || is.function(reduction) || is.null(reduction)
    )
    
    ast <- parse_einops_ast(lex(expr))
    input_identifiers_hashset <- get_identifiers_hashset(ast$input_axes)
    output_identifiers_hashset <- get_identifiers_hashset(ast$output_axes)
    op <- "rearrange"

    for (index in get_identifiers(ast$input_axes)) {
        if (!r2r::query(output_identifiers_hashset, index)) {
            op <- "reduce"
            break
        }
    }

    for (index in get_identifiers(ast$output_axes)) {
        if (r2r::query(input_identifiers_hashset, index)) next
        if (!is.string(op)) next
        if (op != "rearrange") {
            stop("You must perform a reduce and repeat separately: ", expr)
        }
        op <- "repeat"
        break
    }

    op
}
