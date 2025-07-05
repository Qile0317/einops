#' Main function to detect and return backend
#' @param tensor any support tensor-like class
#' @return An instance of a `BackendRegistry` class
#' @keywords internal
get_backend <- function(tensor) {
    BackendRegistry$new()$get_backend(tensor)
}

register_backend <- function(backend_class) {
    BackendRegistry$new()$register_backend(backend_class)
}

# nolint start: indentation_linter.

#' @title
#' Singleton Backend Registry, managing all available backends.
#' @description
#' Contains global backend pool
#' @keywords internal
BackendRegistry <- R6Class("BackendRegistry", inherit = Singleton, cloneable = FALSE,

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
            stop(glue("{backend_class} is not an EinopsBackend"))
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

#' @noRd
EinopsBackend <- R6Class("EinopsBackend", inherit = Singleton, cloneable = FALSE,

public = list(

    tensor_type = function() {
        stop("Not implemented")
    },

    #' @param start integer, inclusive
    #' @param stop integer, inclusive
    #' @return a sequence from start to stop
    arange = function(start, stop) {
        stop("framework doesn't implement arange")
    },

    #' @description
    #' shape should return a tuple with integers or "shape symbols"
    #' (which will evaluate to actual size)
    shape = function(x) {
        tryCatch(dim(x), error = function(e) stop("Not implemented"))
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

    #' @description
    #' Add multiple axes to a tensor and tile along specified axes.
    #'
    #' This function adds new axes to the input tensor at the specified
    #' positions and tiles the tensor along those axes according to the
    #' provided lengths.
    #'
    #' @param x The input tensor/array.
    #' @param n_axes The total number of axes after addition.
    #' @param pos2len A named list or vector mapping axis positions
    #' (1-based) to their lengths (number of repeats).
    #' @return The tensor/array with new axes added and tiled as specified.
    add_axes = function(x, n_axes, pos2len) {
        repeats <- rep(1, n_axes)
        if (length(pos2len) > 0) {
            for (axis_position in as.integer(names(pos2len))) {
                x <- self$add_axis(x, axis_position)
                repeats[axis_position] <- pos2len[[as.character(axis_position)]]
            }
        }
        self$tile(x, repeats)
    },

    #' @description
    #' repeats - same lengths as x.shape
    tile = function(x, repeats) {
        stop("Not implemented")
    },

    #' @description
    #' concatenates tensors along axis. Assume identical across tensors:
    #' devices, dtypes and shapes except selected axis.
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
        sprintf("<einops backend for %s>", self$tensor_type())
    },
    
    einsum = function(pattern, ...) {
        stop("backend does not support einsum")
    }
))

BaseArrayBackend <- R6Class("BaseArrayBackend", inherit = EinopsBackend, cloneable = FALSE,
public = list(

    initialize = function() {
        if (!requireNamespace("abind", quietly = TRUE)) {
            stop("abind package required for array operations")
        }
    },

    tensor_type = function() "array",

    arange = function(start, stop) seq(from = start, to = stop),

    reshape = function(x, shape) array(x, dim = shape),

    transpose = function(x, axes) aperm(x, perm = axes),

    reduce = function(x, operation, axes) {
        op_fun <- switch(operation,
            sum  = sum,
            mean = mean,
            max  = max,
            min  = min,
            prod = prod,
            operation
        )
        keep <- setdiff(seq_along(dim(x)), axes)
        if (length(keep) == 0) return(op_fun(x))
        res <- apply(x, keep, op_fun)
        if (!is.array(res))
            res <- array(res, dim = length(res))
        res
    },

    stack_on_zeroth_dimension = function(tensors) {
        abind::abind(tensors, along = 1)
    },

    tile = function(x, repeats) {
        assert_that(length(dim(x)) == length(repeats))
        old_dims <- dim(x)
        new_dims <- old_dims * repeats
        x <- array(rep(x, times = prod(repeats)), dim = new_dims)
        x
    },

    concat = function(tensors, axis) {
        abind::abind(tensors, along = axis)
    },

    is_float_type = function(x) is.numeric(x),

    add_axis = function(x, new_position) {
        array(x, dim = append(dim(x), 1, after = new_position - 1))
    }
))

register_backend(BaseArrayBackend)

TorchBackend <- R6Class("TorchBackend", inherit = EinopsBackend, cloneable = FALSE,
public = list(

    initialize = function() {
        if (!requireNamespace("torch", quietly = TRUE)) {
            stop("torch package required for TorchBackend")
        }
    },

    tensor_type = function() "torch_tensor"
))

register_backend(TorchBackend)

# nolint end: indentation_linter.
