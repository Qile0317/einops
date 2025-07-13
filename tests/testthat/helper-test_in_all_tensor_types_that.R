test_in_all_tensor_types_that <- function(desc, code) {

    tensor_types <- BackendRegistry$new()$get_supported_types()

    for (tensor_type in tensor_types) {

        test_that(glue("{desc} for [{tensor_type}]"), {

            for (pkg in BackendRegistry$new()$get_dependencies(tensor_type)) {
                skip_if_not_installed(pkg)
            }
            
            local({
                create_tensor <- function(values, dims, ...) { # nolint: object_usage_linter, line_length_linter.
                    get_backend(tensor_type)$create_tensor(values, dims, ...)
                }
                eval(substitute(code))
            })
        })
    }
}

create_tensor <- function(...) {
    stop("create_tensor must be defined in the test context")
}
