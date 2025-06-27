#' @title Tokenize einops pattern into structured tokens
#' @description Tokenize einops pattern into structured tokens
#' @param pattern character string with einops pattern
#' @return list of token objects with type, value, start, end fields
#' @keywords internal
tokenize <- function(pattern) {
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
      if (ellipsis_count > 1) {
        stop("Only one ellipsis (...) is allowed in the pattern")
      }
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
      if (paren_stack == 0) {
        stop("Unmatched closing parenthesis at position ", pos)
      }
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

    # Unsupported operators
    if (char %in% c("+", "*", "/", "%", "^", "&", "|")) {
      stop("Unsupported operator '", char, "' at position ", pos)
    }

    # Illegal characters
    stop("Illegal character '", char, "' at position ", pos)
  }

  # Check for unclosed parentheses
  if (paren_stack > 0) {
    stop("Unclosed parenthesis")
  }

  tokens
}

#' @title parse_einops
#' @description Parse einops pattern into terms structure
#' @param pattern character string with einops pattern
#' @param operation optional operation type ("rearrange", "reduce", "repeat")
#' @return list containing terms and metadata
#' @keywords internal
parse_einops <- function(pattern, operation = NULL) {
  # Validate operation if provided
  if (!is.null(operation)) {
    valid_ops <- c("rearrange", "reduce", "repeat")
    if (!operation %in% valid_ops) {
      stop("Invalid operation: ", operation, ". Must be one of: ", paste(valid_ops, collapse = ", "))
    }
  }

  tokens <- tokenize(pattern)
  terms <- list()
  pos <- 1
  current_side <- "input"  # "input" or "output"

  while (pos <= length(tokens)) {
    result <- .next_token(tokens, pos)
    token <- result$token
    pos <- result$pos

    if (is.null(token)) break

    if (token$type == "ARROW") {
      current_side <- "output"
      next
    }

    if (token$type == "NAME") {
      terms <- append(terms, list(list(
        type = "name",
        value = token$value,
        is_output = (current_side == "output")
      )))
    } else if (token$type == "ELLIPSIS") {
      terms <- append(terms, list(list(
        type = "ellipsis",
        value = token$value,
        is_output = (current_side == "output")
      )))
    } else if (token$type == "LPAREN") {
      # Parse grouped expression
      group_terms <- list()
      paren_count <- 1

      while (pos <= length(tokens) && paren_count > 0) {
        result <- .next_token(tokens, pos)
        inner_token <- result$token
        pos <- result$pos

        if (is.null(inner_token)) {
          stop("Unclosed parenthesis in group")
        }

        if (inner_token$type == "LPAREN") {
          paren_count <- paren_count + 1
        } else if (inner_token$type == "RPAREN") {
          paren_count <- paren_count - 1
        }

        if (paren_count > 0) {
          if (inner_token$type == "NAME") {
            group_terms <- append(group_terms, list(list(
              type = "name",
              value = inner_token$value
            )))
          } else if (inner_token$type == "INT") {
            group_terms <- append(group_terms, list(list(
              type = "int",
              value = as.integer(inner_token$value)
            )))
          }
        }
      }

      terms <- append(terms, list(list(
        type = "group",
        value = group_terms,
        is_output = (current_side == "output")
      )))
    }
  }

  list(
    terms = terms,
    operation = operation,
    pattern = pattern
  )
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
