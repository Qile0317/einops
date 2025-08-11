test_that("get_backend_registry() returns a singleton instance", {
    skip_if_not_installed("lobstr")
    registry1 <- get_backend_registry()
    registry2 <- get_backend_registry()
    expect_identical(registry1, registry2)
    expect_identical(lobstr::obj_addr(registry1), lobstr::obj_addr(registry2))
    expect_identical(lobstr::obj_addrs(registry1), lobstr::obj_addrs(registry2))
})

test_that("get_backend() return unique singletons", {

    skip_if_not_installed("lobstr")

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

test_in_all_tensor_types_that("backend$stack_on_zeroth_dimension() works", {

    x <- create_seq_tensor(10 * 1:4)
    x_list <- lapply(1:10, function(i) {
        x[i, , , ]
    })

    expect_equal(x, get_backend(x)$stack_on_zeroth_dimension(x_list))
})

test_in_all_tensor_types_that("backend$add_axis() and backend$tile() works", {

    x <- create_seq_tensor(c(2, 3, 5))
    backend <- get_backend(x)

    expect_no_error(x <- backend$add_axis(x, 3))

    expect_identical(
        as_base_array(backend$tile(x, c(1L, 1L, 1L, 1L))),
        as_base_array(x)
    )

    expect_identical(
        as_base_array(backend$tile(x, c(1L, 1L, 2L, 1L))),
        array(c(
            1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
            8L, 9L, 10L, 11L, 12L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L,
            16L, 17L, 18L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L,
            23L, 24L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L,
            30L, 25L, 26L, 27L, 28L, 29L, 30L
        ), dim = c(2L, 3L, 2L, 5L))
    )

})

test_in_all_tensor_types_that("backend$reshape() works", {

    x <- create_seq_tensor(2:3)
    backend <- get_backend(x)
    expect_equal(x, backend$reshape(backend$reshape(x, 3:2), 2:3))

    x <- create_seq_tensor(2:4)
    expect_equal(
        x, backend$reshape(backend$reshape(x, c(2, 4, 3)), 2:4)
    )
})
