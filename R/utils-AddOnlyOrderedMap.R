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
#' behaviour is undefined.
#' @param values Optional list. A vector of values corresponding to the keys. If
#' length of values is one, all inserted keys will have that value.
#'
#' @return An `AddOnlyOrderedMap` instance
#' @keywords internal
#'
#' @details
#' The average time complexity of all operations are linear with respect to
#' the number of insertion/query inputs, in contrast to R lists which has
#' quadratic time complexity for the same operations.
#'
#' @examples
#' map <- AddOnlyOrderedMap(keys = c("a", "b"), values = c(1, 2))
#'
AddOnlyOrderedMap <- function(keys = NULL, values = NULL) {
    assert_that(
        is.null(keys) && is.null(values) || (
            !is.null(keys) && !is.null(values) &&
                (length(keys) == length(values) || length(values) == 1L)
        )
    )
    .AddOnlyOrderedMap$new(keys, values)
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
    FastUtils::setNames(values(x), sapply(keys(x), repr, indent = 0L))
}

get_key_to_index_map <- function(x, ...) UseMethod("get_key_to_index_map")

#' @export
get_key_to_index_map.AddOnlyOrderedMap <- function(x, ...) {
    x$get_key_to_index_map()
}

.AddOnlyOrderedMap <- R6Class("AddOnlyOrderedMap",
private = list( # nolint start: indentation_linter
    key2value = NULL,
    key2index = NULL,
    highest_index = NA
),
public = list(

    initialize = function(keys = NULL, values = NULL) {
        if (!is.null(keys) && !is.null(values)) {
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
        assert_that(!is.null(value))
        if (vectorize) {
            private$key2value[key] <- value
            private$highest_index <- private$highest_index + length(key)
            private$key2index[key] <-
                (private$highest_index - length(key) + 1):private$highest_index
        } else {
            private$key2value[[key]] <- value
            private$key2index[[key]] <- private$highest_index + 1
            private$highest_index <- private$highest_index + 1
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
        self$query(private$get_keys_in_order(), vectorize = TRUE)
    },

    get_key_to_index_map = function() {
        private$key2index
    },

    size = function() {
        length(private$key2index)
    }
)) # nolint end: indentation_linter
