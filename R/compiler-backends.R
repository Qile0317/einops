#' Main function to detect and return backend
#' @param tensor any supported tensor-like object
#' @return An instance of a [EinopsBackend()] class. Every returned object
#' is a singleton, so the same object will be returned for the same tensor type.
#' @keywords internal
get_backend <- function(tensor) {
    get_backend_registry()$get_backend(tensor)
}

#' Get the singleton backend registry instance
#'
#' This function retrieves the singleton instance of the backend registry.
#' If the instance does not exist, it creates a new one.
#' If `refresh` is set to TRUE, it will create a new instance even if
#' one already exists.
#'
#' @param clear_testing Logical indicating whether to wipe all backends assigned
#' from tests
#' @return A singleton instance of [BackendRegistry()].
#' @keywords internal
get_backend_registry <- function(
    clear_testing = !identical(Sys.getenv("TESTTHAT"), "true")
) {
    assert_that(is.flag(clear_testing))
    if (!exists(".backend_singleton", envir = globalenv())) {
        assign(".backend_singleton", BackendRegistry$new(), envir = globalenv())
    }
    registry <- get(".backend_singleton", envir = globalenv())
    if (clear_testing) {
        registry$clear_testing_backends()
    }
    registry
}

register_backend <- function(
    tensor_type,
    backend_class,
    dependencies = character(0),
    testing = FALSE,
    aliases = character(0)
) {
    get_backend_registry()$register_backend(
        tensor_type, backend_class, dependencies, testing, aliases
    )
}

unregister_backend <- function(tensor_type) {
    get_backend_registry()$unregister_backend(tensor_type)
}

# nolint start: indentation_linter, line_length_linter

#' @title
#' Singleton Backend Registry, managing all available backends.
#' @description
#' Contains global backend pool, ensuring backends are only loaded if
#' actually required.
#' @keywords internal
BackendRegistry <- R6Class("BackendRegistry", cloneable = FALSE,

private = list(
    # A mapping of types to backend class generators
    type2backend = new.env(parent = emptyenv()),
    # A mapping of types to backend instances
    loaded_backends = new.env(parent = emptyenv()),
    # A mapping of types to their required dependencies
    type2dependencies = new.env(parent = emptyenv()),
    # A set of testing-only backend types
    testing_types = new.env(parent = emptyenv()),
    # A mapping of aliases to their canonical types
    alias2type = new.env(parent = emptyenv())
),

public = list(

    #' @description detect the return relevant backend from the input
    #' @param tensor any supported tensor-like class
    #' @return A singleton instance of a [BackendRegistry()] object
    get_backend = function(tensor) {
        tensor_classes <- class(tensor)
        for (tensor_class in tensor_classes) {
            backend <- self$get_backend_from_type(tensor_class)
            if (!inherits(backend, "NullEinopsBackend")) return(backend)
        }
        stop(glue("Tensor type unknown to einops: {repr(tensor_classes)})"))
    },

    #' @description
    #' Get a backend instance for a specific tensor type.
    #' If the backend is not loaded, it will be instantiated.
    #' @param tensor_class A string representing the tensor type.
    #' @return An instance of the backend class for the specified tensor type.
    get_backend_from_type = function(tensor_class) {
        assert_that(is.string(tensor_class))
        
        # Check if it's an alias first
        if (exists(tensor_class, envir = private$alias2type)) {
            tensor_class <- private$alias2type[[tensor_class]]
        }
        
        if (exists(tensor_class, envir = private$loaded_backends)) {
            return(private$loaded_backends[[tensor_class]])
        }
        if (exists(tensor_class, envir = private$type2backend)) {
            backend_class <- private$type2backend[[tensor_class]]
            backend_instance <- backend_class$new()
            private$loaded_backends[[tensor_class]] <- backend_instance
            return(backend_instance)
        }
        NullEinopsBackend$new()
    },

    #' @description Register a new backend singleton
    #' @param tensor_type a string with the tensor type the backend supports
    #' @param backend_class an EinopsBackend subclass generator
    #' @param dependencies a character vector of required package names
    #' @param testing logical flag indicating if this is a testing-only backend
    #' @param aliases a character vector of aliases for the tensor type
    #' @return this object
    register_backend = function(
        tensor_type,
        backend_class,
        dependencies = character(0),
        testing = FALSE,
        aliases = character(0)
    ) {
        assert_that(
            inherits(backend_class, "R6ClassGenerator"), 
            is.character(dependencies), 
            is.flag(testing),
            is.character(aliases)
        )
        private$type2backend[[tensor_type]] <- backend_class
        private$type2dependencies[[tensor_type]] <- dependencies
        if (testing) {
            private$testing_types[[tensor_type]] <- TRUE
        }
        
        # Add aliases if provided
        if (length(aliases) > 0) {
            for (alias in aliases) {
                self$add_backend_alias(alias, tensor_type)
            }
        }
        
        invisible(self)
    },

    #' @description
    #' Unregister a backend for a specific tensor type.
    #' @param tensor_type a string with the tensor type
    #' @return this object
    unregister_backend = function(tensor_type) {
        assert_that(is.string(tensor_type))
        if (exists(tensor_type, envir = private$type2backend)) {
            rm(list = tensor_type, envir = private$type2backend)
        }
        if (exists(tensor_type, envir = private$loaded_backends)) {
            rm(list = tensor_type, envir = private$loaded_backends)
        }
        if (exists(tensor_type, envir = private$type2dependencies)) {
            rm(list = tensor_type, envir = private$type2dependencies)
        }
        if (exists(tensor_type, envir = private$testing_types)) {
            rm(list = tensor_type, envir = private$testing_types)
        }
        
        # Remove aliases pointing to this type
        aliases_to_remove <- character(0)
        for (alias in ls(envir = private$alias2type)) {
            if (private$alias2type[[alias]] == tensor_type) {
                aliases_to_remove <- c(aliases_to_remove, alias)
            }
        }
        if (length(aliases_to_remove) > 0) {
            rm(list = aliases_to_remove, envir = private$alias2type)
        }
        
        invisible(self)
    },

    #' @description
    #' Add an alias for a backend type.
    #' @param alias a string with the alias name
    #' @param tensor_type a string with the canonical tensor type
    #' @return this object
    #' @keywords internal
    add_backend_alias = function(alias, tensor_type) {
        assert_that(is.string(alias), is.string(tensor_type))
        
        # Check if the canonical type is registered
        if (!exists(tensor_type, envir = private$type2backend)) {
            stop(glue("Cannot add alias '{alias}' for unregistered tensor type '{tensor_type}'"))
        }
        
        # Check if alias already exists
        if (exists(alias, envir = private$alias2type)) {
            existing_type <- private$alias2type[[alias]]
            if (existing_type != tensor_type) {
                stop(glue("Alias '{alias}' already exists for tensor type '{existing_type}'"))
            }
        }
        
        private$alias2type[[alias]] <- tensor_type
        invisible(self)
    },

    #' @description
    #' Clear all testing-only backends.
    #' @return this object
    clear_testing_backends = function() {
        testing_types <- ls(envir = private$testing_types)
        for (tensor_type in testing_types) {
            self$unregister_backend(tensor_type)
        }
        invisible(self)
    },

    #' @description
    #' Get a list of all registered backend types.
    #' @return A character vector of backend types.
    get_supported_types = function() {
        sort(ls(envir = private$type2backend))
    },

    #' @description
    #' given a tensor type, return the required packages
    #' @param tensor_type a string with the tensor type
    #' @return a character vector with required packages. Length 0 if
    #' no packages are required.
    get_dependencies = function(tensor_type) {
        assert_that(is.string(tensor_type))
        if (exists(tensor_type, envir = private$alias2type)) {
            tensor_type <- private$alias2type[[tensor_type]]
        }
        if (exists(tensor_type, envir = private$type2dependencies)) {
            return(private$type2dependencies[[tensor_type]])
        }
        character(0)
    },

    # TODO potentially just return err message

    #' @description
    #' Check if a tensor type is truly loadable,
    #' i.e., if it is registered and has no missing dependencies.
    #' @param tensor_type a string with the tensor type
    #' @return TRUE if the tensor type is loadable, FALSE otherwise.
    is_loadable = function(tensor_type) {
        assert_that(is.string(tensor_type))
        
        # Check if it's an alias first
        canonical_type <- tensor_type
        if (exists(tensor_type, envir = private$alias2type)) {
            canonical_type <- private$alias2type[[tensor_type]]
        }
        
        if (!(canonical_type %in% self$get_supported_types())) {
            return(FALSE)
        }
        dependencies <- self$get_dependencies(tensor_type)
        for (pkg in dependencies) {
            if (!requireNamespace(pkg, quietly = TRUE)) {
                return(FALSE)
            }
        }
        tryCatch({
            self$get_backend_from_type(tensor_type)
            if (exists(canonical_type, envir = private$loaded_backends)) {
                rm(list = canonical_type, envir = private$loaded_backends)
            }
            TRUE
        }, warning = function(w) {
            FALSE
        }, error = function(e) {
            FALSE
        })
    }
))

#' @title
#' Base Backend Class for Einops Tensor Operations
#' @description
#' Abstract base class that defines the interface for tensor operations
#' across different frameworks. All backend implementations must inherit
#' from this class and implement the required methods.
#' @keywords internal
EinopsBackend <- R6Class("EinopsBackend", cloneable = FALSE,

public = list(

    #' @description
    #' Initialize the backend and check for required packages.
    #' It is assumed that the constructor will fully load and
    #' setup all dependencies and error otherwise.
    #' @return A new EinopsBackend instance.
    initialize = function() {
        for (pkg in get_backend_registry()$get_dependencies(self$tensor_type())) {
            if (!requireNamespace(pkg, quietly = TRUE)) {
                stop(glue("Package '{pkg}' is required for this tensor."))
            }
        }
    },

    #' @description
    #' Get the type of tensor this backend supports.
    #' This method should be overridden in subclasses to return the specific
    #' tensor type (e.g., "torch_tensor", "array").
    #' @return A string representing the tensor type.
    tensor_type = function() {
        stop("Not implemented")
    },

    #' @description
    #' Get a string representation of this backend.
    #' @return A character string describing the backend.
    repr = function() {
        glue("<einops backend for {self$tensor_type()}>")
    },

    #' @description
    #' Create a tensor of the specified type with given values and dimensions.
    #' @param values A vector of values to initialize the tensor.
    #' @param dims A numeric vector specifying the dimensions of the tensor.
    #' @param ... Additional arguments for specific backend implementations.
    #' @return A tensor of the specified type.
    create_tensor = function(values, dims, ...) {
        stop("Not implemented")
    },

    #' @description
    #' Convert a tensor to a standard [base::array()]
    #' @param x The input tensor/array.
    #' @return A standard array representation of the tensor.
    as_array = function(x) {
        stop("Not implemented")
    },

    #' @param start integer, inclusive
    #' @param stop integer, inclusive
    #' @return a sequence from start to stop
    arange = function(start, stop) {
        stop("Not Implemented")
    },

    #' @description
    #' Get the shape of a tensor.
    #' Shape should return a tuple with integers or "shape symbols"
    #' (which will evaluate to actual size).
    #' @param x The input tensor/array.
    #' @return A numeric vector representing the tensor shape.
    shape = function(x) {
        tryCatch(dim(x), error = function(e) stop("Not implemented"))
    },

    #' @description
    #' Reshape a tensor to the specified dimensions.
    #' @param x The input tensor/array.
    #' @param shape A numeric vector specifying the new shape.
    #' @return The reshaped tensor/array.
    reshape = function(x, shape) {
        stop("Not implemented")
    },

    #' @description
    #' Transpose a tensor along the specified axes.
    #' @param x The input tensor/array.
    #' @param axes A numeric vector specifying the new axis order.
    #' @return The transposed tensor/array.
    transpose = function(x, axes) {
        stop("Not implemented")
    },

    #' @description
    #' Reduce a tensor along specified axes using the given operation.
    #' @param x The input tensor/array.
    #' @param operation A character string specifying the reduction operation
    #' (e.g., "sum", "mean", "max", "min", "prod").
    #' @param axes A numeric vector specifying which axes to reduce over.
    #' @return The reduced tensor/array.
    reduce = function(x, operation, axes) {
        stop("Not implemented")
    },

    #' @description
    #' Stack multiple tensors along a new zeroth dimension.
    #' @param tensors A list of tensors/arrays to stack.
    #' @return A tensor/array with the input tensors stacked along dimension 1.
    stack_on_zeroth_dimension = function(tensors) {
        stop("Not implemented")
    },

    #' @description
    #' Add a new axis to a tensor at the specified position.
    #' @param x The input tensor/array.
    #' @param new_position The position (1-based) where to insert the new axis.
    #' @return The tensor/array with a new axis added.
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
    #' @param pos2len int->int [r2r::hashmap()] mapping positions to lengths.
    #' @return The tensor/array with new axes added and tiled as specified.
    add_axes = function(x, n_axes, pos2len) {
        assert_that(
            is.count(n_axes),
            inherits(pos2len, "r2r_hashmap"),
            all(sapply(r2r::keys(pos2len), is.count))
        )
        repeats <- rep(1L, n_axes)
        for (axis_position in r2r::keys(pos2len)) {
            x <- self$add_axis(x, axis_position)
            repeats[axis_position] <- pos2len[[axis_position]]
        }
        self$tile(x, as.integer(repeats))
    },

    #' @description
    #' Tile (repeat) a tensor along each axis according to the repeat counts.
    #' The repeats vector should have the same length as the tensor's shape.
    #' @param x The input tensor/array.
    #' @param repeats A numeric vector specifying how many times to repeat
    #' along each axis. Must have same length as x.shape.
    #' @return The tiled tensor/array.
    tile = function(x, repeats) {
        stop("Not implemented")
    },

    #' @description
    #' Concatenate tensors along the specified axis.
    #' Assumes identical devices, dtypes and shapes except for the selected axis.
    #' @param tensors A list of tensors/arrays to concatenate.
    #' @param axis The axis along which to concatenate (1-based).
    #' @return The concatenated tensor/array.
    concat = function(tensors, axis) {
        stop("Not implemented")
    },

    #' @description
    #' Check if the tensor has a floating point data type.
    #' @param x The input tensor/array.
    #' @return A logical value indicating if the tensor is of float type.
    is_float_type = function(x) {
        stop("Not implemented")
    },

    #' @description
    #' Get neural network layers specific to this backend.
    #' @return Backend-specific layer implementations.
    layers = function() {
        stop("backend does not provide layers")
    },
    
    #' @description
    #' Perform Einstein summation on tensors.
    #' @param pattern A character string specifying the einsum pattern.
    #' @param ... Additional tensors to operate on.
    #' @return The result of the einsum operation.
    einsum = function(pattern, ...) {
        stop("backend does not support einsum")
    }
))

NullEinopsBackend <- R6Class("NullEinopsBackend", inherit = EinopsBackend, cloneable = FALSE,
    public = list(initialize = function() invisible(self), tensor_type = function() "NULL")
)

BaseArrayBackend <- R6Class("BaseArrayBackend", inherit = EinopsBackend, cloneable = FALSE,
public = list(

    tensor_type = function() "array",

    create_tensor = function(values, dims) array(values, dim = dims),

    as_array = function(x) x,

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
        assert_that(is.list(tensors))
        if (length(tensors) == 1L) return(tensors[[1]])
        unname(abind::abind(tensors, along = 0))
    },

    tile = function(x, repeats) {
        assert_that(
            is.integer(repeats),
            length(self$shape(x)) == length(repeats),
            all(repeats >= 1L)
        )
        for (i in seq_len(length(self$shape(x)))) {
            if (repeats[i] == 1L) next
            x <- abind::abind(
                replicate(repeats[i], x, simplify = FALSE), along = i
            )
        }
        unname(x)
    },

    concat = function(tensors, axis) {
        unname(abind::abind(tensors, along = axis))
    },

    is_float_type = function(x) is.numeric(x),

    add_axis = function(x, new_position) {
        assert_that(is.count(new_position))
        dim(x) <- append(dim(x), 1, after = new_position - 1)
        x
    }
))

register_backend("array", BaseArrayBackend, "abind", aliases = "numeric")

# TorchBackend <- R6Class("TorchBackend", inherit = EinopsBackend, cloneable = FALSE,
# public = list(

#     initialize = function() {
#         super$initialize()
#         tryCatch(
#             torch::torch_tensor(0),
#             error = function(e) {
#                 stop("Error initializing torch backend. ", conditionMessage(e))
#             }
#         )
#     },

#     tensor_type = function() "torch_tensor",

#     create_tensor = function(values, dims, ...) {
#         torch::torch_tensor(array(values, dim = dims), ...)
#     },

#     as_array = function(x) {
#         torch::as_array(x)
#     }
# ))

# register_backend("torch_tensor", TorchBackend, "torch")

# TensorflowBackend <- R6Class("TensorflowBackend", inherit = EinopsBackend, cloneable = FALSE,
# public = list(

#     initialize = function() {
#         super$initialize()
#         self$tf <- tensorflow::tf
#     },

#     tensor_type = function() "tensorflow.python.types.core.Tensor"
# ))

# register_backend(
#     "tensorflow.python.types.core.Tensor",
#     TensorflowBackend,
#     dependencies = c("reticulate", "tensorflow"),
#     aliases = c(
#         "tensorflow.tensor",
#         "tensorflow.python.framework.ops.EagerTensor",
#         "tensorflow.python.framework.ops._EagerTensorBase",
#         "tensorflow.python.framework.tensor.Tensor",
#         "tensorflow.python.types.internal.NativeObject",
#         "tensorflow.python.types.core.Symbol",
#         "tensorflow.python.types.core.Value"
#     )
# )

# nolint end: indentation_linter, line_length_linter
