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

    # Define simple token mappings
    simple_tokens <- list(
        "(" = list(constructor = LParenToken, standalone = FALSE),
        ")" = list(constructor = RParenToken, standalone = FALSE),
        "_" = list(constructor = UnderscoreToken, standalone = TRUE),
        "*" = list(constructor = AsteriskToken, standalone = TRUE),
        "," = list(constructor = CommaToken, standalone = TRUE)
    )

    while (pos <= n) {
        char <- pattern_chars[pos]

        # Skip whitespace
        if (char %in% c(" ", "\t", "\n")) {
            pos <- pos + 1
            next
        }

        start_pos <- pos

        # Arrow and Comma operator (can be spaced or not)
        if ((char == "-" && pos < n && pattern_chars[pos + 1] == ">") || char == ",") {
            if (char == "-" && pattern_chars[pos + 1] == ">") {
                tokens <- append(tokens, list(ArrowToken(start_pos)))
                pos <- pos + 2
            } else if (char == ",") {
                tokens <- append(tokens, list(CommaToken(start_pos)))
                pos <- pos + 1
            }
            next
        }

        # Ellipsis
        if (char == "." && pos + 2 <= n &&
            pattern_chars[pos + 1] == "." && # nolint: indentation_linter.
            pattern_chars[pos + 2] == "."
        ) {
            ellipsis_count <- ellipsis_count + 1
            tokens <- append(tokens, list(EllipsisToken(start_pos)))
            pos <- pos + 3
            next
        }

        # Handle simple tokens
        simple_result <- handle_simple_token(
            char, pos, pattern_chars, n, simple_tokens
        )
        if (!is.null(simple_result)) {
            if (char == "(") paren_stack <- paren_stack + 1
            if (char == ")") paren_stack <- paren_stack - 1
            tokens <- append(tokens, list(simple_result$token))
            pos <- pos + simple_result$advance
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
        warning(glue(
            "Invalid character '{char}' at position {pos}, skipping..."
        ))
        pos <- pos + 1
    }

    asEinopsTokenSequence(tokens)
}

# Helper function to check if a character is a standalone token
# (not part of an identifier or number)
is_standalone_char <- function(char, pos, pattern_chars, n) {
    alphanumeric <- c(letters, LETTERS, "_", as.character(0:9))
    prev_is_alnum <- pos > 1 && pattern_chars[pos-1] %in% alphanumeric
    next_is_alnum <- pos < n && pattern_chars[pos+1] %in% alphanumeric
    !prev_is_alnum && !next_is_alnum
}

# Helper function to handle simple character tokens
handle_simple_token <- function(char, pos, pattern_chars, n, token_mapping) {
    if (char %in% names(token_mapping)) {
        token_info <- token_mapping[[char]]
        if (token_info$standalone && !is_standalone_char(char, pos, pattern_chars, n)) {
            return(NULL)
        }
        return(list(token = token_info$constructor(pos), advance = 1))
    }
    NULL
}
