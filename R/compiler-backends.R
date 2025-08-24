#' Main function to detect and return backend
#' @param tensor any supported tensor-like object
#' @return An instance of a [EinopsBackend()] class. Every returned object
#' is a singleton, so the same object will be returned for the same tensor type.
#' @keywords internal
get_backend <- function(tensor) {
    get_backend_registry()$get_backend(tensor)
}

#' Simple thunk: wraps an input in a no-argument function
#' @param input Any R object or expression
#' @return A thunk that returns the object when called with
#' class `c("thunk", "function")`
#' @keywords internal
thunk <- function(input) {
    structure(function() input, class = c("thunk", "function"))
}

#' Register a new backend for a tensor type
#'
#' Registers a backend implementation for a specific tensor type, along with any
#' required dependencies, testing flag, and optional aliases. This function
#' wraps the backend class in a [thunk()] and registers it with the backend
#' registry.
#'
#' @param tensor_type A string specifying the tensor type the backend supports.
#' @param backend_class An R6Class generator for the backend (subclass of
#' [EinopsBackend]). Note that this binding does not necessarily have to have a
#' defined value at the time of calling this function.
#' @param dependencies Optional character vector of required package names.
#' @param testing If TRUE, indicates its only used for testing.
#' @param aliases Optional character vector of aliases for the tensor type.
#' @return Invisibly returns the backend registry object.
#' @keywords internal
register_backend <- function(
    tensor_type,
    backend_class,
    dependencies = character(0),
    testing = FALSE,
    aliases = character(0)
) {
    get_backend_registry()$register_backend(
        tensor_type, thunk(backend_class), dependencies, testing, aliases
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
    # A mapping of types to backend class thunks
    type2backend_thunk = new.env(parent = emptyenv()),
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
    #' @return A singleton instance of a [EinopsBackend()] object
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
        if (exists(tensor_class, envir = private$alias2type)) {
            tensor_class <- private$alias2type[[tensor_class]]
        }
        if (exists(tensor_class, envir = private$loaded_backends)) {
            return(private$loaded_backends[[tensor_class]])
        }
        if (exists(tensor_class, envir = private$type2backend_thunk)) {
            backend_thunk <- private$type2backend_thunk[[tensor_class]]
            backend_class <- backend_thunk()
            backend_instance <- backend_class$new()
            private$loaded_backends[[tensor_class]] <- backend_instance
            return(backend_instance)
        }
        NullEinopsBackend$new()
    },

    #' @description Register a new backend singleton
    #' @param tensor_type a string with the tensor type the backend supports
    #' @param backend_class_thunk a [thunk()]'ed EinopsBackend subclass generator
    #' @param dependencies a character vector of required package names
    #' @param testing logical flag indicating if this is a testing-only backend
    #' @param aliases a character vector of aliases for the tensor type
    #' @return this object
    register_backend = function(
        tensor_type,
        backend_class_thunk,
        dependencies = character(0),
        testing = FALSE,
        aliases = character(0)
    ) {
        assert_that(
            inherits(backend_class_thunk, "thunk"), 
            is.character(dependencies), 
            is.flag(testing),
            is.character(aliases)
        )
        private$type2backend_thunk[[tensor_type]] <- backend_class_thunk
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
        if (exists(tensor_type, envir = private$type2backend_thunk)) {
            rm(list = tensor_type, envir = private$type2backend_thunk)
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
        if (!exists(tensor_type, envir = private$type2backend_thunk)) {
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
        sort(ls(envir = private$type2backend_thunk))
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

.einops_backend_registry <- BackendRegistry$new()
get_backend_registry <- function(
    clear_testing = !identical(Sys.getenv("TESTTHAT"), "true")
) {
    assert_that(is.flag(clear_testing))
    if (clear_testing) {
        .einops_backend_registry$clear_testing_backends()
    }
    .einops_backend_registry
}

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
    #' The print method for EinopsBackend instances
    #' @param ... arguments passed to [pprint()]
    #' @return This object, invisibly
    print = function(...) pprint(self, ...),

    #' @description
    #' Get a string representation of this backend.
    #' @return A character string describing the backend.
    repr = function() {
        glue("<einops backend for {self$tensor_type()}>")
    },

    #' @description
    #' Get the type of tensor this backend supports.
    #' This method should be overridden in subclasses to return the specific
    #' tensor type (e.g., "torch_tensor", "array").
    #' @return A string representing the tensor type.
    tensor_type = function() throw_not_implemented(),

    #' @description
    #' Do any relevant preprocessing of a tensor before any operations are
    #' done on it. This should always be called before running any backend
    #' operations on a tensor
    #' @param x The input raw tensor-like object
    #' @return A preprocessed version of the input, may or may not have changed
    #' classes
    preprocess = function(x) x,

    #' @description
    #' Create a tensor of the specified type with given values and dimensions.
    #' @param values A vector of values to initialize the tensor.
    #' @param dims A numeric vector specifying the dimensions of the tensor.
    #' @param ... Additional arguments for specific backend implementations.
    #' @return A tensor of the specified type.
    create_tensor = function(values, dims, ...) throw_not_implemented(),

    #' @description
    #' Convert a tensor to a standard [base::array()]
    #' @param x The input tensor/array.
    #' @return A standard array representation of the tensor.
    as_array = function(x) throw_not_implemented(),

    #' @description
    #' Return a flattened version of the tensor. Note that the
    #' order of calling as_array and flatten does matter because
    #' different frameworks may store data differently.
    #' @param x The input tensor/array
    #' @return A 1 dimensional tensor
    flatten = function(x) throw_not_implemented(),

    #' @param start integer, inclusive
    #' @param stop integer, exclusive
    #' @return a sequence from start to stop - 1
    arange = function(start, stop) throw_not_implemented(),

    #' @description
    #' Get the shape of a tensor.
    #' Shape should return a tuple with integers or "shape symbols"
    #' (which will evaluate to actual size).
    #' @param x The input tensor/array.
    #' @return A numeric vector representing the tensor shape.
    shape = function(x) {
        tryCatch(dim(x), error = function(e) throw_not_implemented())
    },

    #' @description
    #' Reshape a tensor to the specified dimensions.
    #' @param x The input tensor/array.
    #' @param shape A numeric vector specifying the new shape.
    #' @return The reshaped tensor/array.
    reshape = function(x, shape) throw_not_implemented(),

    #' @description
    #' Transpose a tensor along the specified axes.
    #' @param x The input tensor/array.
    #' @param axes A numeric vector specifying the new axis order.
    #' @return The transposed tensor/array.
    transpose = function(x, axes) throw_not_implemented(),

    #' @description
    #' Reduce a tensor along specified axes using the given operation.
    #' @param x The input tensor/array.
    #' @param operation A character string specifying the reduction operation
    #' (e.g., "sum", "mean", "max", "min", "prod", "all", "any"), OR
    #' a two argument function, with the first argument being the tensor
    #' to modify, and the second argument being an integer list of axes
    #' to perform the reduction over.
    #' @param axes A numeric vector specifying which axes to reduce over.
    #' @return The reduced tensor/array.
    reduce = function(x, operation, axes) throw_not_implemented(),

    #' @description
    #' Stack multiple tensors along a new zeroth dimension.
    #' @param tensors A list of tensors/arrays to stack.
    #' @return A tensor/array with the input tensors stacked along dimension 1.
    stack_on_zeroth_dimension = function(tensors) throw_not_implemented(),

    #' @description
    #' Add a new axis to a tensor at the specified position.
    #' @param x The input tensor/array.
    #' @param new_position The position (1-based) where to insert the new axis.
    #' @return The tensor/array with a new axis added.
    add_axis = function(x, new_position) throw_not_implemented(),

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
    tile = function(x, repeats) throw_not_implemented(),

    #' @description
    #' Concatenate tensors along the specified axis.
    #' Assumes identical devices, dtypes and shapes except for the selected axis.
    #' @param tensors A list of tensors/arrays to concatenate.
    #' @param axis The axis along which to concatenate (1-based).
    #' @return The concatenated tensor/array.
    concat = function(tensors, axis) throw_not_implemented(),

    #' @description
    #' Check if the tensor has a floating point data type.
    #' @param x The input tensor/array.
    #' @return A logical value indicating if the tensor is of float type.
    is_float_type = function(x) throw_not_implemented(),

    #' @description
    #' Get neural network layers specific to this backend.
    #' @return Backend-specific layer implementations.
    layers = function() throw_not_implemented(),
    
    #' @description
    #' Perform Einstein summation on tensors.
    #' @param pattern A character string specifying the einsum pattern.
    #' @param ... Additional tensors to operate on.
    #' @return The result of the einsum operation.
    einsum = function(pattern, ...) throw_not_implemented()
))

NullEinopsBackend <- R6Class("NullEinopsBackend", inherit = EinopsBackend, cloneable = FALSE,
    public = list(initialize = function() invisible(self), tensor_type = function() "NULL")
)

register_backend(
    tensor_type = "array",
    backend_class = BaseArrayBackend,
    dependencies = "abind",
    aliases = c("integer", "numeric", "character")
)

# TODO utility function to handle names. abind assigns empty names to each dim
# if original had no dimnames. so if there were names we shouldn't unname, but
# for the case that the original array had empty names, they should be untouched.
BaseArrayBackend <- R6Class("BaseArrayBackend", inherit = EinopsBackend, cloneable = FALSE,
public = list(

    tensor_type = function() "array",

    preprocess = function(x) as.array(x),

    create_tensor = function(values, dims, ...) array(values, dim = dims),

    as_array = function(x) x,

    flatten = function(x) as.vector(x),

    arange = function(start, stop) seq(start, stop - 1),

    reshape = function(x, shape) array(x, dim = shape),

    transpose = function(x, axes) aperm(x, perm = axes),

    reduce = function(x, operation, axes) {

        if (is.function(operation)) return(operation(x, axes))

        make_reducer <- function(base_op) {
            function(arr, dims) {
                keep <- setdiff(seq_along(dim(arr)), dims)
                if (length(keep) == 0) return(base_op(arr))
                res <- apply(arr, keep, base_op)
                if (!is.array(res)) res <- array(res, dim = length(res))
                res
            }
        }
        
        op_fun <- switch(operation,
            sum = make_reducer(sum),
            mean = make_reducer(mean),
            max = make_reducer(max),
            min = make_reducer(min),
            prod = make_reducer(prod),
            any = make_reducer(any),
            all = make_reducer(all),
            stop("Invalid operation")
        )
        
        op_fun(x, axes)
    },

    stack_on_zeroth_dimension = function(tensors) {
        if (length(tensors) == 1L) return(tensors[[1]])
        unname(abind::abind(tensors, along = 0))
    },

    tile = function(x, repeats) {
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
        dim(x) %<>% append(1L, after = new_position - 1L)
        x
    }
))

register_backend(
    tensor_type = "torch_tensor",
    backend_class = TorchBackend,
    dependencies = "torch"
)

TorchBackend <- R6Class("TorchBackend", inherit = EinopsBackend, cloneable = FALSE,
public = list(

    tensor_type = function() "torch_tensor",

    create_tensor = function(values, dims, ...) torch::torch_tensor(array(values, dim = dims), ...),

    as_array = function(x) torch::as_array(x),

    flatten = function(x) x$flatten(),

    reshape = function(x, shape) x$reshape(shape),

    transpose = function(x, axes) x$permute(axes),

    reduce = function(x, operation, axes) {

        if (is.function(operation)) return(operation(x, axes))

        # pytorch supports reducing only one operation at a time for prod, any, all
        if (operation %in% c("prod", "any", "all")) {
            for (i in sort(axes, decreasing = TRUE)) {
                x <- switch(operation,
                    prod = x$prod(dim = i),
                    any = x$any(dim = i),
                    all = x$all(dim = i)
                )
            }
            return(x)
        }
        
        switch(operation,
            sum = x$sum(axes),
            mean = x$mean(axes),
            max = x$amax(axes),
            min = x$amin(axes),
            throw_not_implemented(glue("reducing with `{operation}` is not implemented"))
        )
    },

    stack_on_zeroth_dimension = function(tensors) {
        if (length(tensors) == 1L) return(tensors[[1]])
        torch::torch_stack(tensors, 1)
    },

    tile = function(x, repeats) {
        x$`repeat`(repeats)
    },

    concat = function(tensors, axis) {
        torch::torch_cat(tensors, axis)
    },

    is_float_type = function(x) torch::torch_is_floating_point(x),

    add_axis = function(x, new_position) {
        x$unsqueeze(new_position)
    }

))

register_backend(
    tensor_type = "tensorflow.python.types.core.Tensor",
    backend_class = TensorflowBackend
)

# FIXME: need to make doubly sure of indexing discrepancies
TensorflowBackend <- R6Class("TensorflowBackend", inherit = EinopsBackend, cloneable = FALSE,

private = list(
    tf = NULL # TODO check if numerical indexing is different in R's tf wrapper
),

public = list(

    initialize = function() {
        super$initialize() # FIXME is this correct?

        # TODO maybe this should be done with an global option and/or env var
        tryCatch(
            if (requireNamespace("tensorflow", quietly = TRUE)) {
                private$tf <- tensorflow::tf
            } else {
                private$tf <- reticulate::py_suppress_warnings(
                    reticulate::import("tensorflow")
                )
            },
            error = function(e) {
                stop("Error loading tensorflow: ", e, stop. = FALSE)
            }
        )
    },

    tensor_type = function() "tensorflow.python.types.core.Tensor",

    create_tensor = function(values, dims, ...) {
        private$tf$Variable(array(values, dim = dims), ...)
    },

    as_array = function(x) as.array(x),

    flatten = function(x) private$tf$reshape(x, -1),

    arange = function(start, stop) private$tf$range(start, stop),

    shape = function(x) {
        if (private$tf$executing_eagerly()) {
            if (any(sapply(x$shape, is.null))) throw_not_implemented()
            return(as.integer(x$shape))
        }
        static_shape <- x$shape$as_list()
        tf_shape <- private$tf$shape(x)
        throw_not_implemented() # TODO setup a convention for symbolic elements so a pure primitive vector is returned. likely NA or NaN is the best candidate here
    },

    reduce = function(x, operation, axes) {
        if (is.function(operation)) return(operation(x, axes))
        private$tf[[glue("reduce_{operation}")]](x, axis = axes - 1L)
    },

    reshape = function(x, shape) private$tf$reshape(x, as.integer(shape)),

    transpose = function(x, axes) private$tf$transpose(x, axes - 1L),

    stack_on_zeroth_dimension = function(tensors) {
        if (length(tensors) == 1L) return(tensors[[1]])
        private$tf$stack(tensors)
    },

    tile = function(x, repeats) {
        private$tf$tile(x, repeats)
    },

    concat = function(tensors, axis) {
        private$tf$concat(tensors, axis = axis - 1L)
    },

    add_axis = function(x, new_position) {
        private$tf$expand_dims(x, new_position - 1L)
    },

    is_float_type = function(x) {
        x$dtype %in% c("float16", "float32", "float64", "float128", "bfloat16")
    }

))

# nolint end: indentation_linter, line_length_linter
