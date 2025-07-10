#' @title
#' Create an `AddOnlyOrderedMap` instance
#'
#' @description
#' This function initializes a new list-like object using the
#' specified keys and values if provided. The resulting map preserves the
#' order of insertion and does not allow modification or removal of existing
#' entries.
#'
#' The `AddOnlyOrderedMap` can be interacted with exactly like a regular
#' list, possessing methods for `[`, `[[`, `[<-`, and `[[<-` with the same
#' behaviour, except that NULL cannot be passed in since removal is not
#' permitted.
#'
#' The [keys()] generic is defined for this class, which will return a list
#' of the keys in their insertion order. The [has_key()] generic is also defined
#' for this class, returning TRUE/FALSE if a key exists. Lastly, the [values()]
#' generic is defined to get all values in insertion order.
#'
#' @param keys Optional list. A vector of keys to initialize the map with. Can
#' be any R object. It is assumed that all keys are unique, otherwise the
#' behaviour is undefined. This CANNOT be a scalar value. If that is desired,
#' wrap it in a [list()].
#' @param values Optional list. An iterable vector of values corresponding to
#' the keys. This CANNOT be a scalar value. If that is desired, wrap it in a
#' [list()].
#' @param key_validator Optional function. A function that validates
#' *individual* keys before insertion, returning TRUE if valid, FALSE otherwise.
#' @param val_validator Optional function. A function that validates
#' *individual* values before insertion, returning TRUE if valid, FALSE
#' otherwise.
#'
#' @return An `AddOnlyOrderedMap` instance
#' @keywords internal
#'
#' @details
#' The average time complexity of all operations are linear with respect to
#' the number of insertion/query inputs, in contrast to R lists which has
#' quadratic time complexity for the same operations.
#'
AddOnlyOrderedMap <- function(
    keys = NULL,
    values = NULL,
    key_validator = Negate(is.null),
    val_validator = Negate(is.null)
) {
    assert_that(
        (is.null(keys) && is.null(values)) || (
            !is.null(keys) && !is.null(values) &&
                (length(keys) == length(values) || length(values) == 1L)
        ),
        is.function(key_validator),
        is.function(val_validator)
    )
    .AddOnlyOrderedMap$new(keys, values, key_validator, val_validator)
}

#' @export
"[.AddOnlyOrderedMap" <- function(x, i) {
    x$query(i, vectorize = TRUE)
}

#' @export
"[[.AddOnlyOrderedMap" <- function(x, i) {
    x$query(i, vectorize = FALSE)
}

#' @export
"[<-.AddOnlyOrderedMap" <- function(x, i, value) {
    x$insert(i, value, vectorize = TRUE)
}

#' @export
"[[<-.AddOnlyOrderedMap" <- function(x, i, value) {
    x$insert(i, value, vectorize = FALSE)
}

#' @export
length.AddOnlyOrderedMap <- function(x) {
    x$size()
}

keys <- function(x, ...) UseMethod("keys")

#' @export
keys.AddOnlyOrderedMap <- function(x, ...) x$get_keys_in_order()

has_key <- function(x, key, ...) UseMethod("has_key")

#' @export
has_key.AddOnlyOrderedMap <- function(x, key, ...) x$has_key(key)

values <- function(x, ...) UseMethod("values")

#' @export
values.AddOnlyOrderedMap <- function(x, ...) x$get_values_in_order()

#' @export
as.list.AddOnlyOrderedMap <- function(x, ...) {
    FastUtils::setNames(
        values(x),
        as.character(sapply(keys(x), repr, indent = 0L))
    )
}

get_key_to_index_map <- function(x, ...) UseMethod("get_key_to_index_map")

#' @export
get_key_to_index_map.AddOnlyOrderedMap <- function(x, ...) {
    x$get_key_to_index_map()
}

# this is essentially a LinkedHashMap without the ability to remove
.AddOnlyOrderedMap <- R6Class("AddOnlyOrderedMap",
private = list( # nolint start: indentation_linter

    key2value = NULL,
    key2index = NULL,
    highest_index = 0L,
    key_validator = NULL,
    val_validator = NULL,

    validate_key = function(key, vectorize = FALSE) {
        if (!vectorize) return(private$key_validator(key))
        all(sapply(key, private$key_validator))
    },

    validate_val = function(value, vectorize = FALSE) {
        if (!vectorize) return(private$val_validator(value))
        all(sapply(value, private$val_validator))
    },
    
    validate_inputs = function(keys, values, vectorize = FALSE) {
        if (!private$validate_key(keys, vectorize)) {
            stop("Key validation failed")
        }
        if (!private$validate_val(values, vectorize)) {
            stop("Value validation failed")
        }
    }
),
public = list(

    initialize = function(keys, values, key_validator, val_validator) {

        private$key_validator <- key_validator
        private$val_validator <- val_validator

        if (!is.null(keys) && !is.null(values)) {
            private$validate_inputs(keys, values, vectorize = TRUE)
            assert_that(length(keys) == length(values) || length(values) == 1L)
            private$key2value <- do.call(r2r::hashmap, FastUtils::zipit(keys, values))
            private$key2index <- do.call(r2r::hashmap, FastUtils::zipit(keys, seq_along(keys)))
            private$highest_index <- length(keys)
        } else {
            private$key2value <- r2r::hashmap()
            private$key2index <- r2r::hashmap()
            private$highest_index <- 0L
        }
    },


    print = function(...) {
        cat("AddOnlyOrderedMap with", self$size(), "elements:\n")
        if (self$size() == 0) return(invisible(self))
        keys <- self$get_keys_in_order()
        values <- self$query(keys, vectorize = TRUE)
        key_str_reprsentations <- sapply(keys, repr)
        names(values) <- key_str_reprsentations
        repr_lines <- repr(values, indent = 2L, s3_cons = TRUE, ...)
        cat(repr_lines[c(-1, -length(repr_lines))], sep = "\n")
        invisible(self)
    },

    insert = function(key, value, vectorize = FALSE) {

        assert_that(!is.null(key), !is.null(value))
        private$validate_inputs(key, value, vectorize)
        
        if (vectorize) {
            is_existing <- r2r::has_key(private$key2value, key)
            new_keys <- key[!is_existing]
            existing_keys <- key[is_existing]
            n_new <- length(new_keys)
            if (n_new > 0) {
                private$key2value[new_keys] <- value[match(new_keys, key)]
                private$key2index[new_keys] <- (private$highest_index + 1):(private$highest_index + n_new)
                private$highest_index <- private$highest_index + n_new
            }
            if (length(existing_keys) > 0) {
                private$key2value[existing_keys] <- value[match(existing_keys, key)]
            }
        } else {
            if (!r2r::has_key(private$key2value, key)) {
                private$key2value[[key]] <- value
                private$key2index[[key]] <- private$highest_index + 1L
                private$highest_index %+=% 1L
            } else {
                private$key2value[[key]] <- value
            }
        }

        invisible(self)
    },

    query = function(key, vectorize = FALSE) {
        if (vectorize) return(private$key2value[key])
        private$key2value[[key]]
    },

    get_keys_in_order = function() {
        all_keys <- r2r::keys(private$key2index)
        all_keys[order(as.integer(private$key2index[all_keys]))]
    },

    has_key = function(key) {
        r2r::has_key(private$key2value, key)
    },

    get_values_in_order = function() {
        self$query(self$get_keys_in_order(), vectorize = TRUE)
    },

    get_key_to_index_map = function() {
        private$key2index
    },

    size = function() {
        length(private$key2index)
    }
)) # nolint end: indentation_linter
