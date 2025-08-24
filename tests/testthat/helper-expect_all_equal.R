expect_all_equal <- function(x, y) {
    are_both_convertable_to_arr <- tryCatch(
        {
            as.array(x)
            as.array(y)
            TRUE
        },
        error = function(e) {
            FALSE
        }
    )
    if (are_both_convertable_to_arr) {
        return(testthat::expect_equal(as.array(x), as.array(y)))
    }
    testthat::expect_true(all.equal(x, y))
}
