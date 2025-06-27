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
            tokens <- append(tokens, list(ArrowToken(start_pos)))
            pos <- pos + 2
            next
        }

        # Ellipsis
        if (char == "." && pos + 2 <= n && 
            pattern_chars[pos + 1] == "." && 
            pattern_chars[pos + 2] == ".") {
            ellipsis_count <- ellipsis_count + 1
            tokens <- append(tokens, list(EllipsisToken(start_pos)))
            pos <- pos + 3
            next
        }

        # Left parenthesis
        if (char == "(") {
            paren_stack <- paren_stack + 1
            tokens <- append(tokens, list(LParenToken(start_pos)))
            pos <- pos + 1
            next
        }

        # Right parenthesis
        if (char == ")") {
            paren_stack <- paren_stack - 1
            tokens <- append(tokens, list(RParenToken(start_pos)))
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
            tokens <- append(tokens, list(IntToken(value, start_pos)))
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
            tokens <- append(tokens, list(NameToken(value, start_pos)))
            pos <- end_pos
            next
        }

        # Skip any character we don't recognize
        pos <- pos + 1
    }

    asTokenSequence(tokens)
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
