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
    as_repr(trimws(capture.output(print(x, ...))))
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

#' @param incl_nm Whether to include names in the representation
#' @param s3_cons If there's an S3 class, and a constructor is found, use
#' `constructor()` instead of `list(`
#' @noRd
#' @export
repr.list <- function(x, indent = 0L, incl_nm = TRUE, s3_cons = FALSE, ...) {

    constructor_str <- if (s3_cons) {
        class_names <- class(x)
        found <- FALSE
        for (class_name in class_names) {
            if (is.function(get(class_name))) {
                constructor_str <- class_name
                found <- TRUE
                break
            }
        }
        if (!found) "list(" else paste0(constructor_str, "(")
    } else {
        "list("
    }

    if (length(x) == 0) return(as_repr(paste0(constructor_str, ")")))

    if (indent == 0L) {
        nms <- names(x)
        contents <- vapply(seq_along(x), function(i) {
            name_part <- ifelse(!is.null(nms) && nms[i] != "" && incl_nm, paste0(nms[i], " = "), "")
            paste0(name_part, paste0(repr(x[[i]], indent = 0L, ...), collapse = ""))
        }, character(1))
        return(as_repr(paste0(constructor_str, paste(contents, collapse = ", "), ")")))
    }

    indent_str <- strrep(" ", indent)
    nms <- names(x)

    elems <- lapply(seq_along(x), function(i) {
        # recurse to obtain the element's own repr
        elem_lines <- as.character(repr(x[[i]], indent = indent, ...))

        # attach name (if any) to the first line
        name_part <- if (!is.null(nms) && incl_nm && nms[i] != "")
            paste0(nms[i], " = ")
        else ""

        elem_lines[1] <- paste0(indent_str, name_part, elem_lines[1])
        if (length(elem_lines) > 1)
            elem_lines[-1] <- paste0(indent_str, elem_lines[-1])

        elem_lines
    })

    if (length(elems) > 1) {
        for (i in seq_len(length(elems) - 1L)) {
            last <- length(elems[[i]])
            elems[[i]][last] <- paste0(elems[[i]][last], ",")
        }
    }

    out <- c(constructor_str, unlist(elems, use.names = FALSE), ")")
    as_repr(out)
}
