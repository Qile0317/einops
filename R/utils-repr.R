#' Create a string representation of an object, as a vector of lines
#'
#' @param x Object to represent
#' @param indent Indentation level (number of spaces)
#' @param ... Additional arguments passed to methods
#' @return A character vector of class c("repr_output", "character"), each element is a line
#' @keywords internal
repr <- function(x, indent = 0L, ...) {
    UseMethod("repr", x)
}

as_repr <- function(x) {
    assert_that(is.character(x))
    structure(x, class = c("repr_output", class(x)))
}

#' @export
print.repr_output <- function(x, ...) {
    cat(paste0(x, collapse = "\n"), "\n")
}

repr_print <- function(x, ...) print(repr(x, ...))

#' @export
repr.default <- function(x, indent = 0L, ...) {
    if (is.null(x)) return(as_repr("NULL"))
    if (is.atomic(x)) {
        if (length(x) == 0) return(as_repr(glue("{class(x)[1]}()")))
        return(repr.character(x, indent = indent, quote = FALSE, ...))
    }
    # fallback
    as_repr(capture.output(print(x, ...)))
}

# Helper for named/unnamed contents
.repr_named_contents <- function(x, value_fun, collapse, quote_char = NULL) {
    nms <- names(x)
    s <- sapply(seq_along(x), function(i) {
        val <- value_fun(i)
        if (!is.null(nms) && !(is.na(nms[i]) || nms[i] == "")) {
            paste0(nms[i], " = ", val)
        } else {
            val
        }
    })
    paste(s, collapse = collapse)
}

#' @export
repr.character <- function(
    x, indent = 0L, quote = TRUE, wrap_single = FALSE, ...
) {
    # Only allow logical TRUE/FALSE for quote
    if (!is.logical(quote) || length(quote) != 1L || is.na(quote)) stop("'quote' must be TRUE or FALSE")
    if (length(x) == 0) return(as_repr(paste0(class(x)[1], "()")))
    if (!wrap_single && length(x) == 1L) return(as_repr(x[1]))
    quote_char <- if (quote) '"' else ""
    collapse <- if (indent > 0L) paste0(",\n", strrep(" ", indent)) else ", "
    contents <- .repr_named_contents(
        x,
        function(i) paste0(quote_char, x[i], quote_char),
        collapse
    )
    out <- paste0("c(", contents, ")")
    as_repr(strsplit(out, "\n")[[1]])
}

#' @export
repr.list <- function(x, indent = 0L, ...) {
    if (length(x) == 0) return(as_repr("list()"))
    inner_indent <- indent + 2L
    inner_pad <- strrep(" ", inner_indent)
    sep <- if (indent > 0L) paste0(",\n", inner_pad) else ", "
    pad <- strrep(" ", indent)
    contents <- .repr_named_contents(
        x,
        function(i) paste0(repr(x[[i]], indent = inner_indent, ...), collapse = "\n"),
        sep
    )
    if (indent > 0L) {
        out <- c(
            paste0("list(", if (nchar(contents) > 0) "\n" else ""),
            paste0(inner_pad, contents),
            paste0("\n", pad, ")")
        )
    } else {
        out <- paste0("list(", sub(paste0("^", inner_pad), "", contents), ")")
    }
    as_repr(strsplit(out, "\n")[[1]])
}
