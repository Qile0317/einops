pack <- function(..., expr) {

    assert_that(is.string(expr))
    
    arglist <- list(...)

    if (length(arglist) == 0) {
        warning("No arguments provided to pack()")
        return(NULL)
    }

    if (length(arglist) == 1) {
        if (is.list(arglist[[1]])) {
            arglist <- arglist[[1]]
        } else {
            warning("Only one argument provided to pack(), returning input")
            return(arglist[[1]])
        }
    }

    # TODO main logic

}
