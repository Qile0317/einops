# create a rust-like enum for each token type

create_token <- function(type, value, start, end) {
    structure(list(
        type = type,
        value = value,
        start = start,
        end = end
    ), class = "EinopsToken")
}

ArrowToken <- function(start, end) {
    create_token("ARROW", "->", start, end)
}

EllipsisToken <- function(start, end) {
    create_token("ELLIPSIS", "...", start, end)
}

LParenToken <- function(start, end) {
    create_token("LPAREN", "(", start, end)
}

RParenToken <- function(start, end) {
    create_token("RPAREN", ")", start, end)
}

IntToken <- function(value, start, end) {
    create_token("INT", value, start, end)
}

NameToken <- function(value, start, end) {
    create_token("NAME", value, start, end)
}

#' @title TokenSequence
#' @description Helper to build a token sequence (list of tokens)
#' @param ... tokens to include
#' @return list of tokens
#' @keywords internal
TokenSequence <- function(...) {
    tokens <- list(...)
    tokens <- tokens[!vapply(tokens, is.null, logical(1))]
    structure(tokens, class = c("EinopsTokenSequence", "list"))
}

#' @title Print method for EinopsToken
#' @description Print EinopsToken objects in a clean format showing construction
#' @param x EinopsToken object
#' @param ... additional arguments (unused)
#' @return invisible x
#' @export
print.EinopsToken <- function(x, ...) {
    constructor_call <- switch(x$type,
        "ARROW" = glue::glue("ArrowToken({x$start}, {x$end})"),
        "ELLIPSIS" = glue::glue("EllipsisToken({x$start}, {x$end})"),
        "LPAREN" = glue::glue("LParenToken({x$start}, {x$end})"),
        "RPAREN" = glue::glue("RParenToken({x$start}, {x$end})"),
        "INT" = glue::glue("IntToken(\"{x$value}\", {x$start}, {x$end})"),
        "NAME" = glue::glue("NameToken(\"{x$value}\", {x$start}, {x$end})"),
        glue::glue("create_token(\"{x$type}\", \"{x$value}\", {x$start}, {x$end})")
    )
    cat(constructor_call, "\n")
    invisible(x)
}

#' @title Print method for EinopsTokenSequence
#' @description Print EinopsTokenSequence objects showing reconstructed expression and construction
#' @param x EinopsTokenSequence object
#' @param ... additional arguments (unused)
#' @return invisible x
#' @export
print.EinopsTokenSequence <- function(x, ...) {

    if (length(x) == 0) {
        cat("Empty EinopsTokenSequence()\n")
        return(invisible(x))
    }
    
    last_token <- x[[length(x)]]
    total_length <- last_token$end
    chars <- rep(" ", total_length)
    for (token in x) {
        if (inherits(token, "EinopsToken")) {
            token_chars <- strsplit(token$value, "")[[1]]
            for (i in seq_along(token_chars)) {
                chars[token$start + i - 1] <- token_chars[i]
            }
        }
    }
    reconstructed <- paste(chars, collapse = "")
    cat("Reconstructed expression:", reconstructed, "\n")
    
    # Generate constructor calls for each token by capturing print output
    constructor_calls <- sapply(x, function(x) trimws(capture.output(print(x))))
    tokens_string <- paste(constructor_calls, collapse = ",\n    ")
    cat("TokenSequence(\n   ", tokens_string, "\n)\n")
    invisible(x)
}

#' @title Lexically analyze einops pattern into structured tokens
#' @description Lexically analyze einops pattern into structured tokens
#' @param pattern character string with einops pattern
#' @return list of token objects with type, value, start, end fields
#' @keywords internal
lex <- function(pattern) {
    if (!is.character(pattern) || length(pattern) != 1) {
        stop("Pattern must be a single character string")
    }

    tokens <- list()
    pos <- 1
    ellipsis_count <- 0
    paren_stack <- 0

    # Remove leading/trailing whitespace but track original positions
    pattern_chars <- strsplit(pattern, "")[[1]]
    n <- length(pattern_chars)

    while (pos <= n) {
        char <- pattern_chars[pos]

        # Skip whitespace
        if (char %in% c(" ", "\t", "\n")) {
            pos <- pos + 1
            next
        }

        start_pos <- pos

        # Arrow operator
        if (char == "-" && pos < n && pattern_chars[pos + 1] == ">") {
            tokens <- append(tokens, list(ArrowToken(start_pos, pos + 1)))
            pos <- pos + 2
            next
        }

        # Ellipsis
        if (char == "." && pos + 2 <= n && 
            pattern_chars[pos + 1] == "." && 
            pattern_chars[pos + 2] == ".") {
            ellipsis_count <- ellipsis_count + 1
            tokens <- append(tokens, list(EllipsisToken(start_pos, pos + 2)))
            pos <- pos + 3
            next
        }

        # Left parenthesis
        if (char == "(") {
            paren_stack <- paren_stack + 1
            tokens <- append(tokens, list(LParenToken(start_pos, pos)))
            pos <- pos + 1
            next
        }

        # Right parenthesis
        if (char == ")") {
            paren_stack <- paren_stack - 1
            tokens <- append(tokens, list(RParenToken(start_pos, pos)))
            pos <- pos + 1
            next
        }

        # Numbers
        if (char %in% as.character(0:9)) {
            end_pos <- pos
            while (end_pos <= n && pattern_chars[end_pos] %in% as.character(0:9)) {
                end_pos <- end_pos + 1
            }
            value <- paste(pattern_chars[pos:(end_pos-1)], collapse = "")
            tokens <- append(tokens, list(IntToken(value, start_pos, end_pos - 1)))
            pos <- end_pos
            next
        }

        # Names (letters and underscores)
        if (char %in% c(letters, LETTERS, "_")) {
            end_pos <- pos
            while (end_pos <= n && pattern_chars[end_pos] %in% c(letters, LETTERS, "_", as.character(0:9))) {
                end_pos <- end_pos + 1
            }
            value <- paste(pattern_chars[pos:(end_pos-1)], collapse = "")
            tokens <- append(tokens, list(NameToken(value, start_pos, end_pos - 1)))
            pos <- end_pos
            next
        }

        # Skip any character we don't recognize
        pos <- pos + 1
    }

    # Return tokens with a custom class for better printing
    structure(tokens, class = c("EinopsTokenSequence", "list"))
}

#' @title .next_token
#' @description Internal helper to get next token from stream
#' @param tokens list of tokens
#' @param pos current position
#' @return list with token and new position
#' @keywords internal
.next_token <- function(tokens, pos) {
    if (pos > length(tokens)) {
        return(list(token = NULL, pos = pos))
    }
    list(token = tokens[[pos]], pos = pos + 1)
}
