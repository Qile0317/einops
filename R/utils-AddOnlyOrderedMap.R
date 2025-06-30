AddOnlyOrderedMap <- function() {
    .AddOnlyOrderedMap$new()
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

.AddOnlyOrderedMap <- R6Class("AddOnlyOrderedMap",
private = list(
    key2value = NULL,
    key2index = NULL,
    highest_index = NA
),
public = list(
    initialize = function() {
        private$key2value <- r2r::hashmap()
        private$key2index <- r2r::hashmap()
        private$highest_index <- 0L
    },
    print = function() {
        cat("AddOnlyOrderedMap with", self$size(), "elements:\n")
        for (key in self$keys_in_order()) {
            value <- self$query(key)
            cat(sprintf("  %s: %s\n", key, ifelse(is.null(value), "NULL", value)))
        }
        invisible(self)
    },
    insert = function(key, value, vectorize = FALSE) {
        assert_that(!is.null(value))
        if (vectorize) {
            private$key2value[key] <- value
            private$highest_index <- private$highest_index + length(key)
            private$key2index[key] <-
                (private$highest_index - length(key)):private$highest_index
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
        all_keys[order(private$key2index[all_keys])]
    },
    size = function() {
        length(r2r::keys(private$key2index))
    }
)) # nolint end: indentation_linter
