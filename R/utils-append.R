#' @inheritParams base::append
#' @param ... Additional arguments passed to or from other methods.
#' @inherit base::append title description details return references examples
#' @keywords internal
append <- function(x, values, after = length(x), ...) {
    UseMethod("append")
}

#' @export
append.default <- function(x, values, after = length(x), ...) {
    base::append(x, values, after = length(x))
}
