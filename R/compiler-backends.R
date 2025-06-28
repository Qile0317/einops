# nolint start: indentation_linter.

#' Main function to detect and return backend
get_backend <- function(tensor) {
    BackendRegistry$new()$get_backend(tensor)
}

#' @title
#' Singleton Backend Registry, managing all available backends.
#' @description
#' Contains global backend pool
#' @keywords internal
BackendRegistry <- R6Class("BackendRegistry", inherit = R6P::Singleton, cloneable=FALSE,

private = list(
    # A mapping of types to backend class generators
    type2backend = new.env(parent = emptyenv()),
    # A mapping of types to backend instances
    loaded_backends = new.env(parent = emptyenv()),
    debug_importing = FALSE
),

public = list(

    #' @description detect the return relevant backend from the input
    #' @param tensor any support tensor-like class
    #' @return A singleton instance of a [BackendRegistry()] object
    get_backend = function(tensor) {
        tensor_classes <- class(tensor)
        for (tensor_class in tensor_classes) {
            # Check if backend instance is already loaded
            if (exists(tensor_class, envir = private$loaded_backends, inherits = FALSE)) {
                if (private$debug_importing) {
                    message(sprintf("[einops] Using loaded backend for class: %s", tensor_class))
                }
                return(get(tensor_class, envir = private$loaded_backends, inherits = FALSE))
            }
            # Otherwise, check if a backend generator is registered
            if (exists(tensor_class, envir = private$type2backend, inherits = FALSE)) {
                backend_generator <- get(tensor_class, envir = private$type2backend, inherits = FALSE)
                if (private$debug_importing) {
                    message(sprintf("[einops] Initializing backend for class: %s", tensor_class))
                }
                backend_instance <- backend_generator$new()
                assign(tensor_class, backend_instance, envir = private$loaded_backends)
                return(backend_instance)
            }
        }
        stop(sprintf("Tensor type unknown to einops: %s", paste(tensor_classes, collapse = ", ")))
    },

    #' @description Set whether debug messages should be displayed
    #' @param flag boolean
    #' @return this object
    set_debug = function(flag = TRUE) {
        assert_that(is.flag(flag))
        private$debug_importing <- flag
        return(self)
    },

    #' @description Register a new backend singleton
    #' @param backend_class an EinopsBackend subclass generator
    #' @return this object
    register_backend = function(backend_class) {
        assert_that(inherits(backend_class, "R6ClassGenerator"))
        backend_singleton_instance <- backend_class$new()
        if (!inherits(backend_singleton_instance, "EinopsBackend")) {
            stop(glue::glue("{backend_class} is not an EinopsBackend"))
        }
        tensor_type_name <- backend_singleton_instance$tensor_type()
        if (private$debug_importing) {
            message(sprintf("[einops] Registering backend for tensor type: %s", tensor_type_name))
        }
        assign(
            x = tensor_type_name,
            value = backend_class,
            envir = private$type2backend
        )
        return(self)
    }
))

EinopsBackend <- R6Class("EinopsBackend", inherit = R6P::Singleton, cloneable=FALSE,

public = list(
    tensor_type = function() {
        stop("Not implemented")
    },
    is_appropriate_type = function(tensor) {
        stop("Not implemented")
    },
    create_symbol = function(shape) {
        stop("framework doesn't support symbolic computations")
    },
    eval_symbol = function(symbol, symbol_value_pairs) {
        stop("framework doesn't support symbolic computations")
    },
    arange = function(start, stop) {
        stop("framework doesn't implement arange")
    },
    shape = function(x) {
        stop("Not implemented")
    },
    reshape = function(x, shape) {
        stop("Not implemented")
    },
    transpose = function(x, axes) {
        stop("Not implemented")
    },
    reduce = function(x, operation, axes) {
        stop("Not implemented")
    },
    stack_on_zeroth_dimension = function(tensors) {
        stop("Not implemented")
    },
    add_axis = function(x, new_position) {
        stop("Not implemented")
    },
    add_axes = function(x, n_axes, pos2len) {
        stop("Not implemented")
    },
    tile = function(x, repeats) {
        stop("Not implemented")
    },
    concat = function(tensors, axis) {
        stop("Not implemented")
    },
    is_float_type = function(x) {
        stop("Not implemented")
    },
    layers = function() {
        stop("backend does not provide layers")
    },
    repr = function() {
        sprintf("<einops backend for %s>", self$framework_name)
    },
    einsum = function(pattern, ...) {
        stop("backend does not support einsum")
    }
))

BaseArrayBackend <- R6Class("BaseArrayBackend", inherit = EinopsBackend, cloneable = FALSE,
public = list(
    tensor_type = function() "array"
))

# nolint end: indentation_linter.
