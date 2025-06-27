create_token <- function(type, value, start) {
    structure(list(
        type = type,
        value = value,
        start = start
    ), class = "EinopsToken")
}

ArrowToken <- function(start) {
    create_token("ARROW", "->", start)
}

EllipsisToken <- function(start) {
    create_token("ELLIPSIS", "...", start)
}

LParenToken <- function(start) {
    create_token("LPAREN", "(", start)
}

RParenToken <- function(start) {
    create_token("RPAREN", ")", start)
}

IntToken <- function(value, start) {
    create_token("INT", value, start)
}

NameToken <- function(value, start) {
    create_token("NAME", value, start)
}

#' @title TokenSequence
#' @description Helper to build a token sequence (list of tokens)
#' @param ... tokens to include
#' @return list of tokens
#' @keywords internal
EinopsTokenSequence <- function(...) {
    tokens <- list(...)
    tokens <- tokens[!vapply(tokens, is.null, logical(1))]
    structure(tokens, class = c("EinopsTokenSequence", "list"))
}

asEinopsTokenSequence <- function(x) {
    if (inherits(x, "EinopsTokenSequence")) {
        return(x)
    }
    if (is.list(x)) {
        return(structure(x, class = c("EinopsTokenSequence", "list")))
    }
    warning("asEinopsTokenSequence called on non-list object, returning input")
    x
}

#' @export
tail.EinopsTokenSequence <- function(x, n = 1) { # nolint: object_name_linter.
    if (n < 1) {
        stop("n must be at least 1")
    }
    asEinopsTokenSequence(tail(x, n))
}

#' @title Print method for EinopsToken
#' @description Print EinopsToken objects in a clean format showing construction
#' @param x EinopsToken object
#' @param ... additional arguments (unused)
#' @return invisible x
#' @export
print.EinopsToken <- function(x, ...) {
    constructor_call <- switch(x$type,
        "ARROW" = glue::glue("ArrowToken({x$start})"),
        "ELLIPSIS" = glue::glue("EllipsisToken({x$start})"),
        "LPAREN" = glue::glue("LParenToken({x$start})"),
        "RPAREN" = glue::glue("RParenToken({x$start})"),
        "INT" = glue::glue("IntToken(\"{x$value}\", {x$start})"),
        "NAME" = glue::glue("NameToken(\"{x$value}\", {x$start})"),
        glue::glue("create_token(\"{x$type}\", \"{x$value}\", {x$start})")
    )
    cat(constructor_call, "\n")
    invisible(x)
}

#' @title Print method for EinopsTokenSequence
#' Print EinopsTokenSequences w/reconstructed expression & constructor
#' @param x EinopsTokenSequence object
#' @param ... additional arguments (unused)
#' @return invisible x
#' @export
print.EinopsTokenSequence <- function(x, ...) {

    if (length(x) == 0) {
        cat("Empty EinopsTokenSequence()\n")
        return(invisible(x))
    }

    cat(glue::glue("Einops Lexed Token Sequence for '{to_expression(x)}':\n\n"))
    
    constructor_calls <- sapply(x, function(x) trimws(capture.output(print(x))))
    tokens_string <- paste(constructor_calls, collapse = ",\n    ")
    cat("EinopsTokenSequence(\n   ", tokens_string, "\n)\n")
    invisible(x)
}

to_expression <- function(x, ...) {
    total_length <- 0
    for (token in x) {
        token_end <- token$start + nchar(token$value) - 1
        total_length <- max(total_length, token_end)
    }
    
    chars <- rep(" ", total_length)
    for (token in x) {
        token_chars <- strsplit(token$value, "")[[1]]
        for (i in seq_along(token_chars)) {
            chars[token$start + i - 1] <- token_chars[i]
        }
    }

    paste(chars, collapse = "")
}
