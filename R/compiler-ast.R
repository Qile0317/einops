#' @keywords internal
to_tokens <- function(x, ...) {
    UseMethod("to_tokens", x)
}

#' @title Create a NamedAxisAstNode
#' @param name Character string, the name of the axis
#' @param src List with start position
#' @return NamedAxisAstNode object
#' @keywords internal
NamedAxisAstNode <- function(name, src) {
    structure(list(
        name = name,
        src = src
    ), class = c("NamedAxisAstNode", "AstNode"))
}

#' @export
#' @keywords internal
to_tokens.NamedAxisAstNode <- function(x, ...) {
    EinopsTokenSequence(NameToken(x$name, x$src$start))
}

#' @title Create a ConstantAstNode
#' @param count Character string representing the constant value
#' @param src List with start position
#' @return ConstantAstNode object
#' @keywords internal
ConstantAstNode <- function(count, src) {
    structure(list(
        count = count,
        src = src
    ), class = c("ConstantAstNode", "AstNode"))
}
    
#' @export
#' @keywords internal
to_tokens.ConstantAstNode <- function(x, ...) {
    EinopsTokenSequence(IntToken(x$count, x$src$start))
}

#' @title Create an EllipsisAstNode
#' @param src List with start position
#' @return EllipsisAstNode object
#' @keywords internal
EllipsisAstNode <- function(src) {
    structure(list(
        src = src
    ), class = c("EllipsisAstNode", "AstNode"))
}

#' @export
#' @keywords internal
to_tokens.EllipsisAstNode <- function(x, ...) {
    EinopsTokenSequence(EllipsisToken(x$src$start))
}

#' @title Create a NothingAstNode
#' @param src List with start position
#' @return NothingAstNode object
#' @keywords internal
NothingAstNode <- function() {
    structure(list(), class = c("NothingAstNode", "AstNode"))
}

#' @export
#' @keywords internal
to_tokens.NothingAstNode <- function(x, ...) {
    EinopsTokenSequence()
}

#' @title Create an UnderscoreAstNode
#' @param src List with start position
#' @return UnderscoreAstNode object
#' @keywords internal
UnderscoreAstNode <- function(src) {
    structure(list(
        src = src
    ), class = c("UnderscoreAstNode", "AstNode"))
}

#' @export
#' @keywords internal
to_tokens.UnderscoreAstNode <- function(x, ...) {
    EinopsTokenSequence(UnderscoreToken(x$src$start))
}

#' @title Create a GroupAstNode
#' @param children List of axis nodes contained in this group, potentially empty
#' @param src List with start position
#' @return GroupAstNode object
#' @keywords internal
GroupAstNode <- function(children, src) {
    structure(list(
        children = children,
        src = src
    ), class = c("GroupAstNode", "AstNode"))
}

#' Get the last n children of a GroupAstNode as a list of AstNodes.
#' @keywords internal
#' @export
tail.GroupAstNode <- function(x, n = 1) {
    if (n < 1) {
        stop("n must be at least 1")
    }
    tail(x$children, n)
}

#' @export
#' @keywords internal
to_tokens.GroupAstNode <- function(x, ...) {
    lparen_token <- LParenToken(x$src$start)
    
    # Handle empty groups
    if (length(x$children) == 0) {
        rparen_token <- RParenToken(x$src$start + 1)
        return(EinopsTokenSequence(lparen_token, rparen_token))
    }
    
    last_child_astnode <- tail(x, 1)[[1]]
    last_child_tokens <- to_tokens(last_child_astnode)
    last_token <- tail(last_child_tokens, 1)[[1]]
    rparen_token <- RParenToken(last_token$start + nchar(last_token$value))
    child_tokens <- do.call(EinopsTokenSequence, lapply(x$children, to_tokens))
    EinopsTokenSequence(lparen_token, child_tokens, rparen_token)
}

#' @title Create a OneSidedAstNode (wrapper for input/output axes lists)
#' @param ... Axis nodes (or a single list of axis nodes)
#' @return OneSidedAstNode object
#' @keywords internal
OneSidedAstNode <- function(...) {
    args <- list(...)
    # If a single argument and it's a list, treat as list of nodes
    if (length(args) == 1 && is.list(args[[1]]) && !inherits(args[[1]], "AstNode")) {
        axes <- args[[1]]
    } else {
        axes <- args
    }
    structure(axes, class = c("OneSidedAstNode", "AstNode"))
}

#' @export
print.OneSidedAstNode <- function(x, ...) {
    cat("OneSidedAstNode(")

    if (length(x) == 0) {
        cat(")\n")
        return(invisible(x))
    }

    for (i in seq_along(x)) {
        child_lines <- capture.output(print(x[[i]], ...))
        cat("\n    ", paste(child_lines, collapse = "\n    "), sep = "")
        if (i < length(x)) cat(",")
    }

    cat("\n)\n")
    invisible(x)
}

#' @export
to_tokens.OneSidedAstNode <- function(x, ...) {
    tokens <- unlist(lapply(x, to_tokens), recursive = FALSE)
    do.call(EinopsTokenSequence, tokens)
}

#' @export
"[.OneSidedAstNode" <- function(x, ...) {
    structure(unclass(x)[...], class = class(x))
}

#' @export
append.OneSidedAstNode <- function(x, values, after = length(x), ...) {
    if (!is.list(values) || inherits(values, "AstNode")) {
        values <- list(values)
    }
    new_x <- append(unclass(x), values, after = after, ...)
    structure(new_x, class = class(x))
}

contains_node <- function(x, node_type, ...) {
    UseMethod("contains_node", x)
}

#' @export
contains_node.OneSidedAstNode <- function(x, node_type, ...) {
    any(sapply(x, function(child) inherits(child, node_type)))
}

find_node_types_indices <- function(x, node_type, ...) {
    UseMethod("find_node_types_indices", x)
}

#' @export
find_node_types_indices.OneSidedAstNode <- function(x, node_type, ...) {
    indices <- which(sapply(x, function(child) inherits(child, node_type)))
    if (length(indices) == 0) {
        return(integer(0))
    }
    indices
}

#' @title Create an EinopsAst root node
#' @param input_axes List of axis nodes for the input pattern
#' @param output_axes List of axis nodes for the output pattern
#' @param src List with start position covering the full pattern
#' @return EinopsAst object
#' @keywords internal
EinopsAst <- function(input_axes, output_axes, src) {
    structure(list(
        input_axes = if (!inherits(input_axes, "OneSidedAstNode")) OneSidedAstNode(input_axes) else input_axes,
        output_axes = if (!inherits(output_axes, "OneSidedAstNode")) OneSidedAstNode(output_axes) else output_axes,
        src = src
    ), class = c("EinopsAst", "AstNode"))
}

#' @export
#' @keywords internal
to_tokens.EinopsAst <- function(x, ...) {
    input_tokens <- unlist(lapply(x$input_axes, to_tokens), recursive = FALSE)
    output_tokens <- unlist(lapply(x$output_axes, to_tokens), recursive = FALSE)
    last_input_astnode <- tail(x$input_axes, 1)[[1]]
    last_input_tokens <- to_tokens(last_input_astnode)
    last_token <- tail(last_input_tokens, 1)[[1]]
    arrow_token <- ArrowToken(last_token$start + nchar(last_token$value) + 1)
    asEinopsTokenSequence(c(input_tokens, list(arrow_token), output_tokens))
}

#' @export
print.AstNode <- function(x, ...) {

    format_value <- function(val, indent = 0) {
        ind <- paste(rep("    ", indent), collapse = "")

        if (is.character(val)) return(paste0('"', val, '"'))
        if (is.numeric(val))   return(as.character(val))

        if (is.list(val)) {

            if (inherits(val, "OneSidedAstNode")) {
                raw <- capture.output(print(val, ...))
                if (length(raw) == 0) return("")

                first <- raw[1]
                if (length(raw) == 1) return(first)

                rest  <- paste(raw[-1], collapse = paste0("\n", ind))
                return(paste0(first, "\n", ind, rest))
            }

            if (inherits(val, "AstNode")) {
                cls <- class(val)[1]
                if (length(val) == 0) return(paste0(cls, "()"))

                nms <- names(val)
                parts <- mapply(
                    function(el, nm, idx) {
                        lbl <- if (!is.null(nm) && nzchar(nm)) nm else paste0("[[", idx, "]]")
                        paste0("\n", ind, "    ", lbl, " = ", format_value(el, indent + 1))
                    },
                    val, nms, seq_along(val), SIMPLIFY = FALSE
                )
                return(paste0(cls, "(", paste(parts, collapse = ","), "\n", ind, ")"))
            }

            if (length(val) == 0) return("list()")

            nms <- names(val)
            parts <- mapply(
                function(el, nm) {
                    lbl <- if (!is.null(nm) && nzchar(nm)) paste0(nm, " = ") else ""
                    paste0(lbl, format_value(el, indent))
                },
                val, nms, SIMPLIFY = FALSE
            )
            return(paste0("list(", paste(parts, collapse = ", "), ")"))
        }
        as.character(val)  # fallback
    }

    cls <- class(x)[1]
    if (length(x) == 0) {
        cat(cls, "()\n", sep = "")
    } else {
        nms <- names(x)
        items <- mapply(
            function(el, nm, idx) {
                lbl <- if (!is.null(nm) && nzchar(nm)) nm else paste0("[[", idx, "]]")
                paste0("\n    ", lbl, " = ", format_value(el, 1))
            },
            x, nms, seq_along(x), SIMPLIFY = FALSE
        )
        cat(cls, "(", paste(items, collapse = ","), "\n)\n", sep = "")
    }

    invisible(x)
}

#' @export
print.EinopsAst <- function(x, ...) {
    cat(glue(
        "Einops Abstract Syntax Tree for '{to_expression(to_tokens(x))}':\n\n"
    ))
    print.AstNode(x, ...)
}
