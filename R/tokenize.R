tokenize <- function(expr) {
    assert_that(is.string(expr))
    return(strsplit(expr, " ")[[1]])
}