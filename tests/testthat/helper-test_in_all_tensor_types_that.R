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
#'   - Evaluates the provided test code in a local context.
#' This allows writing tests that are portable across different tensor backends.
#' @return [logical()] of length 1 indicating whether the test passed or failed.
test_in_all_tensor_types_that <- function(desc, code) {

    tensor_types <- get_backend_registry()$get_supported_types()
    substituted_code <- substitute(code)
    backend_registry <- get_backend_registry()

    for (tensor_type in tensor_types) {

        test_that(glue("{desc} for [{tensor_type}]"), {

            for (pkg in backend_registry$get_dependencies(tensor_type)) {
                skip_if_not_installed(pkg)
            }

            backend <- backend_registry$get_backend_from_type(tensor_type)
            
            # Create evaluation environment with create_tensor function
            test_env <- new.env(parent = parent.frame())
            test_env[["create_tensor"]] <- function(values, dims, ...) { # nolint: object_usage_linter, line_length_linter.
                backend$create_tensor(values, dims, ...)
            }
            
            eval(substituted_code, envir = test_env)
        })
    }
}

create_tensor <- function(values, dims, ...) {
    stop("create_tensor must be defined in the test context")
}
