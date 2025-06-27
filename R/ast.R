#' S4 class for Einops Abstract Syntax Tree
#' 
#' @slot text character string of original pattern
#' @slot axes list of axis descriptors
#' @slot ellipsis_pos integer position of ellipsis or NA
setClass("EinopsAst",
  slots = list(
    text = "character",
    axes = "list", 
    ellipsis_pos = "integer"
  ),
  validity = function(object) {
    # Check that text is non-empty
    if (length(object@text) == 0 || nchar(object@text) == 0) {
      return("Pattern text cannot be empty")
    }
    
    # Check axes structure
    if (length(object@axes) == 0) {
      return("Must have at least one axis")
    }
    
    # Each axis must have required fields
    for (i in seq_along(object@axes)) {
      axis <- object@axes[[i]]
      required_fields <- c("name", "size_expr", "is_new", "is_reduced")
      
      if (!all(required_fields %in% names(axis))) {
        return(paste("Axis", i, "missing required fields"))
      }
      
      if (!is.character(axis$name) || length(axis$name) != 1) {
        return(paste("Axis", i, "name must be a single character string"))
      }
    }
    
    # Check for duplicate axis names
    axis_names <- sapply(object@axes, function(x) x$name)
    if (any(duplicated(axis_names))) {
      return("Duplicate axis names not allowed")
    }
    
    # Check ellipsis position
    if (!is.na(object@ellipsis_pos)) {
      if (object@ellipsis_pos < 1 || object@ellipsis_pos > length(object@axes) + 1) {
        return("Invalid ellipsis position")
      }
    }
    
    TRUE
  }
)

#' Create axis descriptor
#' 
#' @param name character string axis name
#' @param size_expr character string size expression (e.g., "2", "h", "h*2")
#' @param is_new logical whether axis is newly created
#' @param is_reduced logical whether axis is reduced
#' @param op character reduction operation if applicable
.axis_descriptor <- function(name, size_expr = NULL, is_new = FALSE, is_reduced = FALSE, op = NULL) {
  # Canonicalize size expression
  if (is.null(size_expr)) {
    size_expr <- name  # Default to axis name
  }
  
  # For grouped expressions, convert to canonical form
  if (is.list(size_expr)) {
    # Convert list of terms to string representation
    parts <- sapply(size_expr, function(term) {
      if (term$type == "name") {
        term$value
      } else if (term$type == "int") {
        as.character(term$value)
      } else {
        term$value
      }
    })
    size_expr <- paste(parts, collapse = " ")
  }
  
  list(
    name = name,
    size_expr = size_expr,
    is_new = is_new,
    is_reduced = is_reduced,
    op = op
  )
}

#' Make AST from parsed terms
#' 
#' @param terms list of term objects from parser
#' @param pattern original pattern string
#' @param op reduction operation
#' @return EinopsAst object
make_ast <- function(terms, pattern, op) {
  if (length(terms) == 0) {
    stop("Empty pattern not allowed")
  }
  
  axes <- list()
  ellipsis_pos <- NA_integer_
  input_axes <- character()
  output_axes <- character()
  
  # Collect all axis names and their contexts
  for (i in seq_along(terms)) {
    term <- terms[[i]]
    
    if (term$type == "ellipsis") {
      if (!is.na(ellipsis_pos)) {
        stop("Multiple ellipses not allowed")
      }
      ellipsis_pos <- length(axes) + 1L
    } else if (term$type == "name") {
      if (term$is_output) {
        output_axes <- c(output_axes, term$value)
      } else {
        input_axes <- c(input_axes, term$value)
      }
      
      # Add axis if not already present
      if (!term$value %in% sapply(axes, function(x) x$name)) {
        axes <- append(axes, list(.axis_descriptor(term$value)))
      }
    } else if (term$type == "group") {
      # Handle grouped expression
      group_names <- character()
      size_parts <- list()
      
      for (group_term in term$value) {
        if (group_term$type == "name") {
          group_names <- c(group_names, group_term$value)
          size_parts <- append(size_parts, list(group_term))
        } else if (group_term$type == "int") {
          size_parts <- append(size_parts, list(group_term))
        }
      }
      
      # For grouped expressions, each name becomes an axis
      for (name in group_names) {
        if (term$is_output) {
          output_axes <- c(output_axes, name)
        } else {
          input_axes <- c(input_axes, name)
        }
        
        if (!name %in% sapply(axes, function(x) x$name)) {
          # Determine if this is a new axis (appears in output but not input)
          is_new <- term$is_output && !name %in% input_axes
          axes <- append(axes, list(.axis_descriptor(name, size_parts, is_new)))
        }
      }
    }
  }
  
  # Check for duplicate axis names across input/output
  all_names <- c(input_axes, output_axes)
  if (any(duplicated(all_names))) {
    dup_names <- unique(all_names[duplicated(all_names)])
    stop("Duplicate axis name: ", paste(dup_names, collapse = ", "))
  }
  
  # Mark reduced axes (appear in input but not output)
  for (i in seq_along(axes)) {
    axis_name <- axes[[i]]$name
    is_reduced <- axis_name %in% input_axes && !axis_name %in% output_axes
    axes[[i]]$is_reduced <- is_reduced
    if (is_reduced) {
      axes[[i]]$op <- op
    }
  }
  
  # Check if reduction operation is needed but missing
  has_reduced_axes <- any(sapply(axes, function(x) x$is_reduced))
  if (has_reduced_axes && (is.null(op) || is.na(op))) {
    stop("Reduction operation required when axes are removed")
  }
  
  # Create and validate AST
  ast <- new("EinopsAst",
    text = pattern,
    axes = axes,
    ellipsis_pos = ellipsis_pos
  )
  
  validObject(ast)
  ast
}

#' Convert EinopsAst back to string representation
#' 
#' @param x EinopsAst object
#' @return character string canonical representation
setMethod("as.character", "EinopsAst", function(x) {
  # Reconstruct canonical pattern
  input_parts <- character()
  output_parts <- character()
  
  pos <- 1
  for (axis in x@axes) {
    if (pos == x@ellipsis_pos) {
      input_parts <- c(input_parts, "...")
      pos <- pos + 1
    }
    
    if (!axis$is_new) {
      input_parts <- c(input_parts, axis$name)
    }
    if (!axis$is_reduced) {
      output_parts <- c(output_parts, axis$name)
    }
    pos <- pos + 1
  }
  
  if (pos == x@ellipsis_pos) {
    input_parts <- c(input_parts, "...")
  }
  
  input_str <- paste(input_parts, collapse = " ")
  output_str <- paste(output_parts, collapse = " ")
  
  paste(input_str, "->", output_str)
})

#' Print method for EinopsAst
#' 
#' @param object EinopsAst object
setMethod("show", "EinopsAst", function(object) {
  cat("EinopsAst: ", object@text, "\n")
  cat("Axes (", length(object@axes), "):\n")
  for (i in seq_along(object@axes)) {
    axis <- object@axes[[i]]
    flags <- character()
    if (axis$is_new) flags <- c(flags, "new")
    if (axis$is_reduced) flags <- c(flags, "reduced")
    flag_str <- if (length(flags) > 0) paste0(" [", paste(flags, collapse = ", "), "]") else ""
    cat("  ", axis$name, ": ", axis$size_expr, flag_str, "\n")
  }
  if (!is.na(object@ellipsis_pos)) {
    cat("Ellipsis at position: ", object@ellipsis_pos, "\n")
  }
})
