is_sorted <- function(x, ...) {
    UseMethod("is_sorted", x)
}

assertthat::on_failure(is_sorted) <- function(call, env) {
    paste0(deparse(call$x), " is not sorted.")
}

#' @export
is_sorted.numeric <- function(x, ...) {
    all(diff(x) >= 0)
}
