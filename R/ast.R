#' EinopsAst S4 Class for Representing Einops Expressions
#'
#' This class represents parsed einops expressions as Abstract Syntax Trees (ASTs).
#' It separates input and output patterns and handles nested structures with brackets
#' and ellipsis tokens.
#'
#' @slot input_pattern list representing the input pattern structure
#' @slot output_pattern list representing the output pattern structure  
#' @slot ellipsis_axes character vector of axis names that contain ellipsis
#' @slot known_axes character vector of all known axis names
#' @keywords internal
#'
setClass("EinopsAst",
  slots = list(
    input_pattern = "list",
    output_pattern = "list", 
    ellipsis_axes = "character",
    known_axes = "character"
  )
)

#' Tokenize an einops expression string
#'
#' @param expr character string containing the einops expression
#' @return list of tokens
#' @keywords internal
tokenize_einops <- function(expr) {
  # Remove extra whitespace and split by spaces
  tokens <- strsplit(trimws(gsub("\\s+", " ", expr)), " ")[[1]]
  
  # Handle parentheses by splitting them into separate tokens
  result <- list()
  for (token in tokens) {
    if (grepl("^\\(", token) || grepl("\\)$", token)) {
      # Extract parentheses and content
      chars <- strsplit(token, "")[[1]]
      current <- ""
      for (char in chars) {
        if (char %in% c("(", ")")) {
          if (nchar(current) > 0) {
            result <- append(result, current)
            current <- ""
          }
          result <- append(result, char)
        } else {
          current <- paste0(current, char)
        }
      }
      if (nchar(current) > 0) {
        result <- append(result, current)
      }
    } else {
      result <- append(result, token)
    }
  }
  
  return(result)
}

#' Parse tokens into a nested structure
#'
#' @param tokens list of tokens
#' @return list representing the parsed structure
#' @keywords internal
parse_tokens <- function(tokens) {
  result <- list()
  i <- 1
  
  while (i <= length(tokens)) {
    token <- tokens[[i]]
    
    if (token == "(") {
      # Find matching closing parenthesis
      depth <- 1
      j <- i + 1
      while (j <= length(tokens) && depth > 0) {
        if (tokens[[j]] == "(") depth <- depth + 1
        if (tokens[[j]] == ")") depth <- depth - 1
        j <- j + 1
      }
      
      if (depth > 0) {
        stop("Unmatched opening parenthesis in expression")
      }
      
      # Parse the content inside parentheses
      inner_tokens <- tokens[(i + 1):(j - 2)]
      result <- append(result, list(parse_tokens(inner_tokens)), after = length(result))
      i <- j
    } else if (token == ")") {
      stop("Unmatched closing parenthesis in expression")
    } else {
      result <- append(result, token, after = length(result))
      i <- i + 1
    }
  }
  
  return(result)
}

#' Extract axis names from a parsed pattern
#'
#' @param pattern list representing parsed pattern
#' @return character vector of axis names
#' @keywords internal
extract_axis_names <- function(pattern) {
  axes <- character(0)
  
  for (item in pattern) {
    if (is.list(item)) {
      # Recursively extract from nested lists
      axes <- c(axes, extract_axis_names(item))
    } else if (is.character(item) && item != "...") {
      axes <- c(axes, item)
    }
  }
  
  return(unique(axes))
}

#' Check for ellipsis in pattern
#'
#' @param pattern list representing parsed pattern
#' @return character vector of axes that contain ellipsis
#' @keywords internal
find_ellipsis_axes <- function(pattern) {
  ellipsis_axes <- character(0)
  
  for (item in pattern) {
    if (is.list(item)) {
      ellipsis_axes <- c(ellipsis_axes, find_ellipsis_axes(item))
    } else if (is.character(item) && item == "...") {
      # Mark the position where ellipsis occurs
      ellipsis_axes <- c(ellipsis_axes, "...")
    }
  }
  
  return(ellipsis_axes)
}

#' Parse an einops expression into an EinopsAst object
#'
#' @param expr character string containing the einops expression
#' @return EinopsAst object
#' @keywords internal
#'
#' @examples
#' # Parse a simple reduction
#' ast <- parse_einops("h w c -> h w")
#' 
#' # Parse with parentheses  
#' ast <- parse_einops("(h w) c -> h w c")
#'
#' # Parse with ellipsis
#' ast <- parse_einops("... h w -> ...")
parse_einops <- function(expr) {
  assertthat::assert_that(
    assertthat::is.string(expr),
    msg = "Expression must be a single character string"
  )
  
  # Split on arrow to get input and output
  parts <- strsplit(expr, "->")[[1]]
  
  if (length(parts) != 2) {
    stop("Expression must contain exactly one '->' arrow")
  }
  
  input_str <- trimws(parts[1])
  output_str <- trimws(parts[2])
  
  # Tokenize and parse both sides
  input_tokens <- tokenize_einops(input_str)
  output_tokens <- tokenize_einops(output_str)
  
  input_pattern <- parse_tokens(input_tokens)
  output_pattern <- parse_tokens(output_tokens)
  
  # Extract axis information
  input_axes <- extract_axis_names(input_pattern)
  output_axes <- extract_axis_names(output_pattern)
  known_axes <- unique(c(input_axes, output_axes))
  
  # Find ellipsis
  input_ellipsis <- find_ellipsis_axes(input_pattern)
  output_ellipsis <- find_ellipsis_axes(output_pattern)
  ellipsis_axes <- unique(c(input_ellipsis, output_ellipsis))
  
  # Create and return AST object
  new("EinopsAst",
    input_pattern = input_pattern,
    output_pattern = output_pattern,
    ellipsis_axes = ellipsis_axes,
    known_axes = known_axes
  )
}

#' Print method for EinopsAst
#'
#' @param x EinopsAst object
#' @keywords internal
setMethod("show", "EinopsAst", function(object) {
  cat("EinopsAst:\n")
  cat("  Input pattern: ", deparse(object@input_pattern), "\n")
  cat("  Output pattern:", deparse(object@output_pattern), "\n")
  cat("  Known axes:    ", paste(object@known_axes, collapse = ", "), "\n")
  cat("  Ellipsis axes: ", paste(object@ellipsis_axes, collapse = ", "), "\n")
})
