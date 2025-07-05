#' @title Pretty Print
#'
#' @description
#' This is a convenience function that prints the [repr()] of an object.
#' It is similar to python's `pprint.pprint()`. Usually, to ensure that
#' an object is displayed in the terminal using its pprint format, just
#' define a `print.that_object_class` method that calls `pprint()`.
#'
#' @param x Object to pretty print
#' @param ... Additional arguments passed to `repr()`
#' @return The input object, invisibly
#' @keywords internal
pprint <- function(x, ...) {
    content_to_print <- if (inherits(x, "s3list")) {
        repr.list(x, s3_cons = TRUE, indent = 4L, ...)
    } else {
        repr(x, ...)
    }
    print(content_to_print)
    invisible(x)
}

#' @title Python-like Representation of Objects as Strings
#' @description
#' This is an R implementation of python's `repr()` function.
#' All objects will have a default repr that uses its print method.
#' This is important because the [pprint()] function will always
#' be able to give a repr-esque output of any object.
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

    quote_char <- if (quote) '"' else ""

    if (!wrap_single && length(x) == 1L) {
        return(as_repr(paste0(quote_char, x, quote_char, collapse = "")))
    }
    
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
            if (!exists(class_name)) next
            if (!is.function(get(class_name))) next
            constructor_str <- class_name
            found <- TRUE
            break
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
            paste0(name_part, paste0(repr(x[[i]], indent = 0L, incl_nm = incl_nm, s3_cons = s3_cons, ...), collapse = ""))
        }, character(1))
        return(as_repr(paste0(constructor_str, paste(contents, collapse = ", "), ")")))
    }

    indent_str <- strrep(" ", indent)
    nms <- names(x)

    elems <- lapply(seq_along(x), function(i) {
        # recurse to obtain the element's own repr
        elem_lines <- as.character(repr(x[[i]], indent = indent, incl_nm = incl_nm, s3_cons = s3_cons, ...))

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
