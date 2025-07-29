repeat_test_cases = list(
    # all assume that input has shape [2, 3, 5]
    list("a b c -> c a b", list()),
    list("a b c -> (c copy a b)", list(copy = 2, a = 2, b = 3, c = 5)),
    list("a b c -> (a copy) b c ", list(copy = 1)),
    list("a b c -> (c a) (copy1 b copy2)", list(a = 2, copy1 = 1, copy2 = 2)),
    list("a ...  -> a ... copy", list(copy = 4)),
    list("... c -> ... (copy1 c copy2)", list(copy1 = 1, copy2 = 2)),
    list("...  -> ... ", list()),
    list(" ...  -> copy1 ... copy2 ", list(copy1 = 2, copy2 = 3)),
    list("a b c  -> copy1 a copy2 b c () ", list(copy1 = 2, copy2 = 1))
)

test_cases_repeat_anonymous = list(
    # all assume that input has shape [1, 2, 4, 6]
    list("a b c d -> c a d b", list()),
    list("a b c d -> (c 2 d a b)", list(a = 1, c = 4, d = 6)),
    list("1 b c d -> (d copy 1) 3 b c ", list(copy = 3)),
    list("1 ...  -> 3 ... ", list()),
    list("() ... d -> 1 (copy1 d copy2) ... ", list(copy1 = 2, copy2 = 3)),
    list("1 b c d -> (1 1) (1 b) 2 c 3 d (1 1)", list())
)

# TODO check_reversion function

test_in_all_tensor_types_that("repeat() works", {

    x <- create_seq_tensor(c(2, 3, 5))
    
    for (i in seq_along(repeat_test_cases)) {
        pattern <- repeat_test_cases[[i]][[1]]
        axis_dimensions <- repeat_test_cases[[i]][[2]]
        expect_no_error(expected <- einops.repeat(x, pattern, axis_dimensions))
        # TODO ACTUAL TESTS
    }

})

test_in_all_tensor_types_that("repeat() works on lists", {

    x <- create_seq_tensor(2:6)
    
    expect_identical(
        einops.repeat(
            lapply(seq_len(dim(x)[1]), function(i) x[i, , , , ]),
            "... -> b (...)",
            b = 3
        ),
        einops.repeat(x, "... -> b (...)", b = 3)
    )

})
