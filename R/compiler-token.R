create_token <- function(type, value, start) {
    structure(list(
        type = type,
        value = value,
        start = start
    ), class = "EinopsToken")
}

create_simple_token <- function(type, value, start) {
    structure(list(
        type = type,
        value = value,
        start = start
    ), class = c("SimpleEinopsToken", "EinopsToken"))
}

create_parameterized_token <- function(type, value, start) {
    structure(list(
        type = type,
        value = value,
        start = start
    ), class = c("ParameterizedEinopsToken", "EinopsToken"))
}

ArrowToken <- function(start) {
    create_simple_token("ARROW", "->", start)
}

EllipsisToken <- function(start) {
    create_simple_token("ELLIPSIS", "...", start)
}

# The underscore token is only used in parse_shape()
UnderscoreToken <- function(start) {
    create_simple_token("UNDERSCORE", "_", start)
}

# This token will be used in the future for pack()/unpack()
AsteriskToken <- function(start) {
    create_simple_token("ASTERISK", "*", start)
}

# this token will be used in the future for einsum()
CommaToken <- function(start) {
    create_simple_token("COMMA", ",", start)
}

LParenToken <- function(start) {
    create_simple_token("LPAREN", "(", start)
}

RParenToken <- function(start) {
    create_simple_token("RPAREN", ")", start)
}

IntToken <- function(value, start) {
    create_parameterized_token("INT", value, start)
}

NameToken <- function(value, start) {
    create_parameterized_token("NAME", value, start)
}

#' @title EinopsTokenSequence constructor
#' @description Helper to build a token sequence (list of tokens)
#' @param ... tokens or EinopsTokenSequences to include
#' @return list of tokens
#' @keywords internal
EinopsTokenSequence <- function(...) {
    inputs <- list(...)
    inputs <- inputs[!vapply(inputs, is.null, logical(1))]
    tokens <- list()
    for (input in inputs) {
        if (inherits(input, "EinopsTokenSequence")) {
            tokens <- c(tokens, unclass(input))
        } else {
            tokens <- c(tokens, list(input))
        }
    }
    structure(tokens, class = c("EinopsTokenSequence", "list"))
}

asEinopsTokenSequence <- function(x) {
    if (inherits(x, "EinopsTokenSequence")) {
        return(x)
    }
    if (is.list(x)) {
        return(structure(unclass(x), class = c("EinopsTokenSequence", "list")))
    }
    stop("asEinopsTokenSequence called on non-list object")
}

#' @export
tail.EinopsTokenSequence <- function(x, n = 1) { # nolint: object_name_linter.
    assert_that(is.count(n))
    if (n < 1) {
        stop("n must be at least 1")
    }
    result <- utils::tail(unclass(x), n)
    asEinopsTokenSequence(result)
}

type_to_function_name <- function(type) {
    type_lower <- tolower(type)
    if (type == "LPAREN") return("LParenToken")
    if (type == "RPAREN") return("RParenToken")
    paste0(
        toupper(substring(type_lower, 1, 1)),
        substring(type_lower, 2),
        "Token"
    )
}

#' @export
print.SimpleEinopsToken <- function(x, ...) {
    func_name <- type_to_function_name(x$type)
    constructor_call <- glue("{func_name}({x$start})")
    cat(constructor_call, "\n")
    invisible(x)
}

#' @export
print.ParameterizedEinopsToken <- function(x, ...) {
    func_name <- type_to_function_name(x$type)
    constructor_call <- glue("{func_name}(\"{x$value}\", {x$start})")
    cat(constructor_call, "\n")
    invisible(x)
}

#' @export
print.EinopsTokenSequence <- function(x, ...) {

    if (length(x) == 0) {
        cat("EinopsTokenSequence()\n")
        return(invisible(x))
    }

    cat(glue("Einops Lexed Token Sequence for '{to_expression(x)}':\n\n"))
    
    constructor_calls <- sapply(x, function(x) trimws(capture.output(print(x))))
    tokens_string <- paste(constructor_calls, collapse = ",\n    ")
    cat("EinopsTokenSequence(\n   ", tokens_string, "\n)\n")
    invisible(x)
}

to_expression <- function(x, ...) {
    UseMethod("to_expression", x)
}

#' @export
to_expression.EinopsTokenSequence <- function(x, ...) {
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
