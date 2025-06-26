#' Internal utilities for einops operations
#'
#' These functions handle the core logic for parsing einops expressions
#' and planning array transformations.
#'
#' @keywords internal

#' Resolve einops pattern against actual array dimensions
#'
#' @param ast EinopsAst object
#' @param input_dims integer vector of input array dimensions
#' @return list with resolved input/output patterns and dimension mappings
#' @keywords internal
resolve_array_pattern <- function(ast, input_dims) {
  input_pattern <- ast@input_pattern
  output_pattern <- ast@output_pattern
  
  # Handle ellipsis by figuring out how many dimensions it represents
  ellipsis_count <- 0
  if ("..." %in% ast@ellipsis_axes) {
    # Count non-ellipsis axes in input pattern
    input_axes_count <- count_non_ellipsis_axes(input_pattern)
    ellipsis_count <- length(input_dims) - input_axes_count
    
    if (ellipsis_count < 0) {
      stop("Input array has fewer dimensions than specified in pattern")
    }
  }
  
  # Create dimension mapping
  dim_mapping <- create_dimension_mapping(input_pattern, input_dims, ellipsis_count)
  
  # Validate that all dimensions are accounted for
  if (length(dim_mapping$axis_to_dims) != sum(sapply(dim_mapping$axis_to_dims, length))) {
    stop("Dimension mapping failed - not all dimensions accounted for")
  }
  
  return(list(
    input_pattern = input_pattern,
    output_pattern = output_pattern,
    dim_mapping = dim_mapping,
    ellipsis_count = ellipsis_count
  ))
}

#' Count non-ellipsis axes in a pattern
#'
#' @param pattern list representing parsed pattern
#' @return integer count of non-ellipsis axes
#' @keywords internal
count_non_ellipsis_axes <- function(pattern) {
  count <- 0
  for (item in pattern) {
    if (is.list(item)) {
      count <- count + count_non_ellipsis_axes(item)
    } else if (is.character(item) && item != "...") {
      count <- count + 1
    }
  }
  return(count)
}

#' Create mapping from axes to array dimensions
#'
#' @param pattern list representing input pattern
#' @param dims integer vector of array dimensions
#' @param ellipsis_count integer number of dimensions represented by ellipsis
#' @return list with axis mappings
#' @keywords internal
create_dimension_mapping <- function(pattern, dims, ellipsis_count) {
  axis_to_dims <- list()
  dim_index <- 1
  
  for (item in pattern) {
    if (is.list(item)) {
      # Handle grouped dimensions - they get combined
      group_dims <- c()
      for (sub_item in item) {
        if (is.character(sub_item) && sub_item != "...") {
          group_dims <- c(group_dims, dim_index)
          dim_index <- dim_index + 1
        } else if (sub_item == "...") {
          group_dims <- c(group_dims, dim_index:(dim_index + ellipsis_count - 1))
          dim_index <- dim_index + ellipsis_count
        }
      }
      # Create a group name for the combined dimensions
      group_name <- paste0("group_", length(axis_to_dims) + 1)
      axis_to_dims[[group_name]] <- group_dims
    } else if (is.character(item) && item != "...") {
      axis_to_dims[[item]] <- dim_index
      dim_index <- dim_index + 1
    } else if (item == "...") {
      axis_to_dims[["..."]] <- dim_index:(dim_index + ellipsis_count - 1)
      dim_index <- dim_index + ellipsis_count
    }
  }
  
  return(list(axis_to_dims = axis_to_dims))
}

#' Create a plan for reduction operations
#'
#' @param resolved_pattern list from resolve_array_pattern
#' @return list with reduction plan
#' @keywords internal
create_reduction_plan <- function(resolved_pattern) {
  input_pattern <- resolved_pattern$input_pattern
  output_pattern <- resolved_pattern$output_pattern
  dim_mapping <- resolved_pattern$dim_mapping
  
  # Extract axis names from patterns
  input_axes <- extract_axis_names_flat(input_pattern)
  output_axes <- extract_axis_names_flat(output_pattern)
  
  # Determine which axes to keep and which to reduce
  axes_to_keep <- intersect(input_axes, output_axes)
  axes_to_reduce <- setdiff(input_axes, output_axes)
  
  # Handle ellipsis
  if ("..." %in% axes_to_keep || "..." %in% axes_to_reduce) {
    if ("..." %in% output_pattern && "..." %in% input_pattern) {
      axes_to_keep <- c(axes_to_keep, "...")
      axes_to_reduce <- setdiff(axes_to_reduce, "...")
    } else if ("..." %in% input_pattern && !("..." %in% output_pattern)) {
      axes_to_reduce <- c(axes_to_reduce, "...")
      axes_to_keep <- setdiff(axes_to_keep, "...")
    }
  }
  
  # Map to actual dimension indices
  dims_to_keep <- c()
  dims_to_reduce <- c()
  
  for (axis in axes_to_keep) {
    if (axis %in% names(dim_mapping$axis_to_dims)) {
      dims_to_keep <- c(dims_to_keep, dim_mapping$axis_to_dims[[axis]])
    }
  }
  
  for (axis in axes_to_reduce) {
    if (axis %in% names(dim_mapping$axis_to_dims)) {
      dims_to_reduce <- c(dims_to_reduce, dim_mapping$axis_to_dims[[axis]])
    }
  }
  
  return(list(
    dims_to_keep = sort(dims_to_keep),
    dims_to_reduce = sort(dims_to_reduce),
    output_pattern = output_pattern,
    dim_mapping = dim_mapping
  ))
}

#' Extract axis names from pattern (flattened)
#'
#' @param pattern list representing pattern
#' @return character vector of axis names
#' @keywords internal
extract_axis_names_flat <- function(pattern) {
  axes <- character(0)
  for (item in pattern) {
    if (is.list(item)) {
      axes <- c(axes, extract_axis_names_flat(item))
    } else if (is.character(item)) {
      axes <- c(axes, item)
    }
  }
  return(unique(axes))
}

#' Execute the reduction operation
#'
#' @param x input array
#' @param reduction_plan list from create_reduction_plan
#' @param func reduction function
#' @param ... additional arguments for func
#' @return reduced array
#' @keywords internal
execute_reduction <- function(x, reduction_plan, func, ...) {
  if (length(reduction_plan$dims_to_reduce) == 0) {
    # No reduction needed, just reshaping
    return(x)
  }
  
  if (length(reduction_plan$dims_to_keep) == 0) {
    # Reduce to scalar
    return(func(x, ...))
  }
  
  # Use apply to reduce along specified dimensions
  result <- apply(x, reduction_plan$dims_to_keep, func, ...)
  
  # Ensure result is an array with proper dimensions
  if (!is.array(result)) {
    result_dims <- dim(x)[reduction_plan$dims_to_keep]
    result <- array(result, dim = result_dims)
  }
  
  return(result)
}

#' Reshape result according to output pattern
#'
#' @param result reduced array
#' @param reduction_plan list from create_reduction_plan
#' @return reshaped array
#' @keywords internal
reshape_result <- function(result, reduction_plan) {
  output_pattern <- reduction_plan$output_pattern
  
  # If output pattern is empty (scalar reduction), return as-is
  if (length(output_pattern) == 0) {
    return(as.vector(result))
  }
  
  # Calculate new dimensions based on output pattern
  current_dims <- dim(result)
  if (is.null(current_dims)) {
    current_dims <- length(result)
  }
  
  # Handle grouped dimensions in output
  new_dims <- calculate_output_dimensions(output_pattern, current_dims)
  
  if (length(new_dims) > 0 && prod(new_dims) == length(result)) {
    result <- array(result, dim = new_dims)
  }
  
  return(result)
}

#' Calculate output dimensions from pattern
#'
#' @param pattern output pattern list
#' @param current_dims current array dimensions
#' @return integer vector of new dimensions
#' @keywords internal
calculate_output_dimensions <- function(pattern, current_dims) {
  new_dims <- c()
  dim_index <- 1
  
  for (item in pattern) {
    if (is.list(item)) {
      # Grouped dimensions - multiply together
      group_size <- 1
      for (sub_item in item) {
        if (is.character(sub_item) && sub_item != "...") {
          group_size <- group_size * current_dims[dim_index]
          dim_index <- dim_index + 1
        }
      }
      new_dims <- c(new_dims, group_size)
    } else if (is.character(item) && item != "...") {
      new_dims <- c(new_dims, current_dims[dim_index])
      dim_index <- dim_index + 1
    } else if (item == "...") {
      # For ellipsis, keep remaining dimensions as-is
      remaining_dims <- current_dims[dim_index:length(current_dims)]
      new_dims <- c(new_dims, remaining_dims)
      dim_index <- length(current_dims) + 1
    }
  }
  
  return(new_dims)
}
