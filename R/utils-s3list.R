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
        if (!inherits(arg, "s3list")) return(arg)

        # Check if classes are compatible for conversion
        current_non_s3 <- setdiff(current_class, "s3list")
        arg_non_s3 <- setdiff(class(arg), "s3list")
        classes_overlap <- any(current_non_s3 %in% arg_non_s3)
        
        processor <- if (!classes_overlap) list else identity
        processor(arg)
    })
    output <- unlist(list(list(list()), output), recursive = FALSE)
    output <- do.call(c, output)
    class(output) <- current_class
    output
}
