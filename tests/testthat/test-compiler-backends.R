test_that("get_backend_registry() returns a singleton instance", {
    registry1 <- get_backend_registry()
    registry2 <- get_backend_registry()
    expect_identical(registry1, registry2)
    expect_identical(lobstr::obj_addr(registry1), lobstr::obj_addr(registry2))
    expect_identical(lobstr::obj_addrs(registry1), lobstr::obj_addrs(registry2))
})

test_that("get_backend() return unique singletons", {

    for (i in 1:2) { # TODO use some with() like function
        tensor_class <- glue("DummyTensor{i}")
        register_backend(tensor_class, R6Class(
            glue("DummyTensorBackend{i}"),
            inherit = EinopsBackend,
            cloneable = FALSE,
            public = list(tensor_type = function() tensor_class)
        ), testing = TRUE)
    }

    expect_contains(
        get_backend_registry()$get_supported_types(),
        c("DummyTensor1", "DummyTensor2")
    )

    backend1_1 <- get_backend(structure(1:10, class = "DummyTensor1"))
    backend2_1 <- get_backend(structure(1:10, class = "DummyTensor2"))
    backend1_2 <- get_backend(structure(1:10, class = "DummyTensor1"))
    backend2_2 <- get_backend(structure(1:10, class = "DummyTensor2"))

    expect_identical(
        lobstr::obj_addr(backend1_1), lobstr::obj_addr(backend1_2)
    )
    expect_identical(
        lobstr::obj_addrs(backend1_1), lobstr::obj_addrs(backend1_2)
    )
    expect_identical(
        lobstr::obj_addr(backend2_1), lobstr::obj_addr(backend2_2)
    )
    expect_identical(
        lobstr::obj_addrs(backend2_1), lobstr::obj_addrs(backend2_2)
    )

    # Clean up
    unregister_backend("DummyTensor1")
    unregister_backend("DummyTensor2")
})

test_that("each backend's stack_on_zeroth_dimension() works", {

    x <- array(1:(10 * 20 * 30 * 40), dim = c(10, 20, 30, 40))
    x_list <- lapply(1:10, function(i) {
        x[i, , , ]
    })

    expect_equal(x, get_backend(x)$stack_on_zeroth_dimension(x_list))
})

test_in_all_tensor_types_that("add_axis() and tile() works", {

    x <- create_tensor(1:prod(2, 3, 5), c(2, 3, 5))
    backend <- get_backend(x)

    expect_no_error(x <- backend$add_axis(x, 3))

    expect_identical(
        as_base_array(backend$tile(x, c(1L, 1L, 1L, 1L))),
        as_base_array(x)
    )

    expect_identical(
        as_base_array(backend$tile(x, c(1L, 1L, 2L, 1L))),
        array(
            as.integer(c(
                0, 15, 5, 20, 10, 25, 0, 15, 5, 20, 10, 25, 1, 16,
                6, 21, 11, 26, 1, 16, 6, 21, 11, 26, 2, 17, 7, 22, 12, 27, 2,
                17, 7, 22, 12, 27, 3, 18, 8, 23, 13, 28, 3, 18, 8, 23, 13, 28,
                4, 19, 9, 24, 14, 29, 4, 19, 9, 24, 14, 29
            )),
            dim = c(2L, 3L, 2L, 5L)
        )
    )

})
