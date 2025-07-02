AddOnlyOrderedMap <- function(keys = NULL, values = NULL) {
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
        keys <- self$keys_in_order()
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

    keys_in_order = function() {
        all_keys <- r2r::keys(private$key2index)
        all_keys[order(as.integer(private$key2index[all_keys]))]
    },

    size = function() {
        length(r2r::keys(private$key2index))
    }
)) # nolint end: indentation_linter
