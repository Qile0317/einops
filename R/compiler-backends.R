# nolint start: indentation_linter.

#' Main function to detect and return backend
get_backend <- function(tensor) {
    BackendRegistry$new()$get_backend(tensor)
}

#' @title
#' Singleton Backend Registry, managing all available backends.
#' @description
#' This class should never be instantiated directly, and only used internally
#' via get_backend_registry()
#' @keywords internal
BackendRegistry <- R6Class("BackendRegistry", inherit = R6P::Singleton, cloneable=FALSE,

private = list(
    loaded_backends = new.env(parent = emptyenv()),
    type2backend = list2env(parent = emptyenv(), x = list(

    )),
    debug_importing = FALSE
),

public = list(

    #' @description detect the return relevant backend from the input
    #' @param tensor any support tensor-like class
    #' @return A singleton instance of a [BackendRegistry()] object
    get_backend = function(tensor) {
        tensor_class <- class(tensor)[1]
        if (!exists(
            tensor_class, envir = private$type2backend, inherits = FALSE
        )) {
            stop(sprintf("Tensor type unknown to einops: %s", tensor_class))
        }
        get(tensor_class, envir = private$type2backend, inherits = FALSE)
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
    #' @param backend an EinopsBackend subclass
    #' @return this object
    register_backend = function(backend) {
        assert_that(inherits(backend, "EinopsBackend") == TRUE)
        assign(envir = private$type2backend, backend$tensor_type(), backend)
        return(self)
    }
))

#' @title
#' Abstract EinopsBackend base class
#' @noRd
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

# nolint end: indentation_linter.
