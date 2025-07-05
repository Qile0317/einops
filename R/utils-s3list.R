#' @export
print.s3list <- pprint

#' @export
repr.s3list <- function(x, indent = 4L, s3_cons = TRUE, ...) {
    repr.list(x, indent = indent, s3_cons = s3_cons, ...)
}

#' @export
c.s3list <- function(...) {

    args <- list(...)
    current_class <- class(..1)
    
    output <- lapply(args, function(arg) {
        processor <- if (inherits(arg, "s3list") && !(any(setdiff(current_class, "s3list") %in% setdiff(class(arg), "s3list")))) list else identity
        processor(arg)
    })
    output <- unlist(list(list(list()), output), recursive = FALSE)
    output <- do.call(c, output)
    class(output) <- current_class
    output
}
