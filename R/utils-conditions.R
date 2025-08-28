throw_not_implemented <- function(
    message = "Not Implemented", call = NULL, ...
) {
    stop(structure(
        list(message = message, call = call, ...),
        class = c("NotImplementedError", "error", "condition")
    ))
}
