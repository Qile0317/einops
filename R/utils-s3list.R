#' @export
print.s3list <- pprint

#' @export
repr.s3list <- function(x, indent = 4L, s3_cons = TRUE, ...) {
    repr.list(x, indent = indent, s3_cons = s3_cons, ...)
}

# #' @export
# c.s3list <- function(...) {
#     structure(c(unlist(lapply(list(...), unclass), recursive = FALSE)), class = class(..1))
# }
