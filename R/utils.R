repr <- function(x, indent, ...) {
    UseMethod("repr", x)
}

#' @export
repr.default <- function(x, indent = 0L, ...) {

    if (is.null(x)) return("NULL\n")
    if (is.atomic(x)) {
        if (length(x) == 0) return(glue("{class(x)[1]}()\n"))
        return(
            repr(as.character(x), indent = indent, quote = FALSE, ...)
        )
    }

    if (is.list(x)) {
        return(paste0("list(", paste(sapply(x, repr), collapse = ", "), ")"))
    }

    # fallback
    capture.output(print(x, ...))
}

#' @export
repr.character <- function(
    x, indent = 0L, quote = TRUE, wrap_single = FALSE, ...
) {
    assert_that(is.flag(quote))
    if (length(x) == 0) return(glue("{class(x)[1]}()\n"))
    if (!wrap_single && length(x) == 1L) return(glue("{x[1]}\n"))
    quote_char <- if (quote) '"' else ""
    collapse <- if (indent > 0L) paste0(",\n", strrep(" ", indent)) else ", "
    paste0("c(", paste0(quote_char, x, quote_char, collapse = collapse), ")\n")
}

#' @export
repr.list <- function(x, indent = 0L, ...) {
    if (length(x) == 0) return("list()\n")

    inner_indent <- indent + 2L
    inner_pad <- strrep(" ", inner_indent)
    sep <- if (indent > 0L) paste0(",\n", inner_pad) else ", "
    pad <- strrep(" ", indent)

    if (is.null(names(x))) {
        s <- sapply(x, function(item) repr(item, indent = inner_indent, ...))
    } else {
        nms <- names(x)
        s <- sapply(seq_along(x), function(i) {
            val <- repr(x[[i]], indent = inner_indent, ...)
            if (is.na(nms[i]) || nms[i] == "") {
                val
            } else {
                paste0(nms[i], " = ", val)
            }
        })
    }
    contents <- paste0(inner_pad, paste(s, collapse = sep))
    if (indent > 0L) {
        return(paste0("list(\n", contents, "\n", pad, ")\n"))
    } else {
        # Remove leading pad for flat output
        return(paste0("list(", sub(paste0("^", inner_pad), "", contents), ")\n"))
    }
}
