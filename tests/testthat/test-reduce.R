equivalent_reduction_patterns <- list(
    list("a b c d e -> ", "... ->  "),
    list("a b c d e -> (e a)", "a ... e -> (e a)"),
    list("a b c d e -> d (a e)", " a b c d e ... -> d (a e) "),
    list("a b c d e -> (a b)", "... c d e  -> (...) ")
)

for (pattern in equivalent_reduction_patterns) {
    for (reduction in c("min", "max", "sum")) {
        test_in_all_tensor_types_that(glue(
            "reduce(x, '{pattern[[1]]}', '{reduction}') is equivalent to ",
            "reduce(x, '{pattern[[2]]}', '{reduction}')"
        ), {
            x <- create_seq_tensor(c(10, 20, 30, 40, 50))
            if (inherits(x, "torch_tensor")) x <- x$to(dtype = torch::torch_float64())
            expect_no_error(out1 <- reduce(x, pattern[[1]], reduction))
            expect_no_error(out2 <- reduce(x, pattern[[2]], reduction))
            expect_identical(out1, out2)
        })
    }
}

# TODO more tests
