#' Run a test across all supported tensor types
#'
#' This helper function executes a test for each tensor type supported by the
#' backends registered in `BackendRegistry`. It ensures that the test code is
#' run for every tensor type, skipping tests if required backend packages are
#' not installed.
#'
#' @param desc Description of the test (character string).
#' @param code Test code to be evaluated. Should use `create_tensor` to create
#' tensors in a backend-agnostic way.
#' @details
#' For each tensor type, the function:
#'   - Retrieves dependencies and skips the test if any are missing.
#'   - Defines a `create_tensor` function for the current backend.
#'   - Evaluates the provided test code in the calling environment context.
#' This allows writing tests that are portable across different tensor backends
#' while having access to variables defined in the test frame.
#' @return [logical()] of length 1 indicating whether the test passed or failed.
test_in_all_tensor_types_that <- function(desc, code) {

    assert_that(is.string(desc))

    tensor_types <- get_backend_registry()$get_supported_types()
    substituted_code <- substitute(code)
    backend_registry <- get_backend_registry()
    parent_env <- parent.frame()

    for (tensor_type in tensor_types) {

        test_that(glue("{desc} for [{tensor_type}]"), {

            for (pkg in backend_registry$get_dependencies(tensor_type)) {
                skip_if_not_installed(pkg)
            }

            skip_if_not(
                backend_registry$is_loadable(tensor_type),
                glue("Tensor type {tensor_type}'s backend is not loadable")
            )

            backend <- backend_registry$get_backend_from_type(tensor_type)
            
            eval_env <- new.env(parent = parent_env)
            eval_env$create_tensor <- function(values, dims, ...) {
                backend$create_tensor(values, dims, ...)
            }
            eval_env$as_base_array <- function(x) backend$as_array(x)
            
            eval(substituted_code, envir = eval_env)
        })
    }
}

#' Interface function for creation of tensors
#'
#' This function is a placeholder that should be defined in the test context.
#' It is used to create tensors in a backend-agnostic way.
#' @param values A vector of values to fill the tensor.
#' @param dims A vector of dimensions for the tensor.
#' @param ... Additional arguments passed to the backend's tensor creation
#' method.
#' @return A tensor object created by the backend.
create_tensor <- function(values, dims, ...) {
    stop("create_tensor() must be defined in the test context")
}

#' Interface function to convert a tensor to a base array
#'
#' This function is a placeholder that should be defined in the test context.
#' It is used to convert a tensor to a base R array.
#' @param x A tensor object to be converted.
#' @return A base R array representation of the tensor.
as_base_array <- function(x) {
    stop("as_base_array must be defined in the test context")
}
