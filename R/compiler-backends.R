#' Main function to detect and return backend
#' @param tensor any support tensor-like class
#' @return An instance of a `BackendRegistry` class
#' @keywords internal
get_backend <- function(tensor) {
    BackendRegistry$new()$get_backend(tensor)
}

register_backend <- function(tensor_type, backend_class) {
    BackendRegistry$new()$register_backend(tensor_type, backend_class)
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
    type2backend = r2r::hashmap(),
    # A mapping of types to backend instances
    loaded_backends = r2r::hashmap(),

    get_backend_from_type = function(tensor_class) {
        assert_that(is.string(tensor_class))
        if (r2r::has_key(private$loaded_backends, tensor_class)) {
            return(private$loaded_backends[[tensor_class]])
        }
        if (r2r::has_key(private$type2backend, tensor_class)) {
            backend_class <- private$type2backend[[tensor_class]]
            backend_instance <- backend_class$new()
            private$loaded_backends[[tensor_class]] <- backend_instance
            return(backend_instance)
        }
        NullEinopsBackend$new()
    }
),

public = list(

    #' @description detect the return relevant backend from the input
    #' @param tensor any support tensor-like class
    #' @return A singleton instance of a [BackendRegistry()] object
    get_backend = function(tensor) {
        tensor_classes <- class(tensor)
        for (tensor_class in tensor_classes) {
            backend <- private$get_backend_from_type(tensor_class)
            if (!inherits(backend, "NullEinopsBackend")) return(backend)
        }
        stop(glue("Tensor type unknown to einops: {repr(tensor_classes)})"))
    },

    #' @description Register a new backend singleton
    #' @param tensor_type a string with the tensor type the backend supports
    #' @param backend_class an EinopsBackend subclass generator
    #' @return this object
    register_backend = function(tensor_type, backend_class) {
        assert_that(inherits(backend_class, "R6ClassGenerator"))
        private$type2backend[[tensor_type]] <- backend_class
        return(self)
    },

    #' @description
    #' Unregister a backend for a specific tensor type.
    #' @param tensor_type a string with the tensor type
    #' @return this object
    unregister_backend = function(tensor_type) {
        assert_that(is.string(tensor_type))
        r2r::delete(private$type2backend, tensor_type)
        r2r::delete(private$loaded_backends, tensor_type)
        return(self)
    },

    #' @description
    #' Get a list of all registered backend types.
    #' @return A character vector of backend types.
    get_supported_types = function() {
        sort(as.character(r2r::keys(private$type2backend)))
    },

    #' @description
    #' given a tensor type, return the required packages
    #' @param tensor_type a string with the tensor type
    #' @return a character vector with required packages. Length 0 if
    #' no packages are required.
    get_dependencies = function(tensor_type) {
        assert_that(is.string(tensor_type))
        private$get_backend_from_type(tensor_type)$required_pkgs()
    }
))

#' @noRd
EinopsBackend <- R6Class("EinopsBackend", inherit = Singleton, cloneable = FALSE,

public = list(

    initialize = function() {
        super$initialize()
        for (pkg in self$required_pkgs()) {
            if (!requireNamespace(pkg, quietly = TRUE)) {
                stop(glue("Package '{pkg}' is required for this tensor."))
            }
        }
    },

    tensor_type = function() {
        stop("Not implemented")
    },

    required_pkgs = function() {
        character(0)
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
        glue("<einops backend for {self$tensor_type()}>")
    },
    
    einsum = function(pattern, ...) {
        stop("backend does not support einsum")
    }
))

NullEinopsBackend <- R6Class(
    "NullEinopsBackend", inherit = EinopsBackend, cloneable = FALSE
)

BaseArrayBackend <- R6Class("BaseArrayBackend", inherit = EinopsBackend, cloneable = FALSE,
public = list(

    required_pkgs = function() "abind",

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

register_backend("array", BaseArrayBackend)

TorchBackend <- R6Class("TorchBackend", inherit = EinopsBackend, cloneable = FALSE,
public = list(

    required_pkgs = function() "torch"
))

register_backend("torch_tensor", TorchBackend)

# nolint end: indentation_linter.
