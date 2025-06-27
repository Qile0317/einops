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

  tokens <- lex(pattern)
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
