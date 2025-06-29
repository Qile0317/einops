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

pprint <- function(x, ...) print(repr(x, ...))

#' @export
repr.repr_output <- function(x, ...) x

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
    assert_that(is.flag(quote), is.flag(wrap_single))
    if (length(x) == 0) return(as_repr(glue("{class(x)[1]}()")))
    if (!wrap_single && length(x) == 1L) return(as_repr(as.character(x[1])))
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

    if (indent == 0L) {
        nms <- names(x)
        contents <- vapply(seq_along(x), function(i) {
            name_part <- ifelse(!is.null(nms) && nms[i] != "", paste0(nms[i], " = "), "")
            paste0(name_part, paste0(repr(x[[i]], indent = 0L, ...), collapse = ""))
        }, character(1))
        return(as_repr(paste0("list(", paste(contents, collapse = ", "), ")")))
    }
    
    indent      <- as.integer(indent)
    indent_str  <- strrep(" ", indent)          # current level
    inner_str   <- strrep(" ", indent + 4L)     # one level deeper
    nms         <- names(x)
    
    out <- c(paste0(indent_str, "list("))
    
    for (i in seq_along(x)) {
        # build element representation without extra indent
        elem_lines <- repr(x[[i]], indent = 0L, ...)
        
        # prefix first line (with optional name)
        name_prefix <- if (!is.null(nms) && !(is.na(nms[i]) || nms[i] == "")) {
            paste0(nms[i], " = ")
        } else {
            ""
        }
        elem_lines[1] <- paste0(inner_str, name_prefix, elem_lines[1])
        
        # prefix any following lines
        if (length(elem_lines) > 1L) {
            elem_lines[-1] <- paste0(inner_str, elem_lines[-1])
        }
        
        # comma after each element except the last
        if (i < length(x)) {
            elem_lines[length(elem_lines)] <- paste0(elem_lines[length(elem_lines)], ",")
        }
        
        out <- c(out, elem_lines)
    }
    
    out <- c(out, paste0(indent_str, ")"))
    as_repr(out)
}
