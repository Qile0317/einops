#' Reduce operation for einops expressions
#'
#' Reduces tensor dimensions according to einops notation. This function
#' applies a reduction function (like sum, mean, max) along specified dimensions
#' while potentially reshaping the tensor according to the einops pattern.
#'
#' @param x object to reduce (array, matrix, etc.)
#' @param expr character string with einops expression (e.g., "h w c -> h w")
#' @param func character string naming the reduction function (e.g., "sum", "mean", "max")
#' @param ... additional arguments passed to the reduction function
#'
#' @return reduced object with dimensions according to output pattern
#' @export
#'
#' @examples
#' # 2D reduction
#' arr <- array(1:12, dim = c(3, 4))
#' reduce(arr, "h w -> h", "sum")
#' reduce(arr, "h w -> w", "sum")
#' reduce(arr, "h w ->", "sum")
#'
#' # 3D reduction with grouping
#' arr3d <- array(1:24, dim = c(2, 3, 4))
#' reduce(arr3d, "b h w -> h w", "sum")
#' reduce(arr3d, "(b h) w -> w", "sum")
#' 
#' # Using ellipsis
#' reduce(arr3d, "... w -> ...", "sum")
reduce <- function(x, expr, func, ...) {
    UseMethod("reduce")
}

#' Reduce operation for arrays
#'
#' @rdname reduce
#' @export
reduce.array <- function(x, expr, func, ...) {
  # Validate that func is a character string
  if (!is.character(func) || length(func) != 1) {
    stop("func must be a character string naming a function")
  }
  
  # Get the function object
  func_obj <- match.fun(func)
  
  # Parse the einops expression
  ast <- parse_einops(expr)
  
  # Get the dimensions of the input array
  input_dims <- dim(x)
  if (is.null(input_dims)) {
    input_dims <- length(x)
  }
  
  # Validate and resolve the pattern against input dimensions
  resolved_pattern <- resolve_array_pattern(ast, input_dims)
  
  # Determine which dimensions to reduce and which to keep
  reduction_plan <- create_reduction_plan(resolved_pattern)
  
  # Apply the reduction
  result <- execute_reduction(x, reduction_plan, func_obj, ...)
  
  # Reshape result according to output pattern
  final_result <- reshape_result(result, reduction_plan)
  
  return(final_result)
}

