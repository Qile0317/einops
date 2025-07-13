test_that("get_backend() returns identical singleton objects", {
    
    dummy_tensor <- structure(list(), class = "DummyTensor")
    DummyBackend <- R6::R6Class(
        "DummyBackend", inherit = EinopsBackend, cloneable = FALSE
    )

    # Create and register the singleton instance only once
    backend_singleton <- DummyBackend$new()
    BackendRegistry$new()$register_backend("DummyTensor", DummyBackend)

    backend1 <- get_backend(dummy_tensor)
    backend2 <- get_backend(dummy_tensor)

    # All addresses should be identical
    expect_identical(
        lobstr::obj_addr(backend_singleton), lobstr::obj_addr(backend1)
    )
    expect_identical(
        lobstr::obj_addr(backend1), lobstr::obj_addr(backend2)
    )
    expect_identical(
        lobstr::obj_addrs(backend_singleton), lobstr::obj_addrs(backend1)
    )
    expect_identical(
        lobstr::obj_addrs(backend1), lobstr::obj_addrs(backend2)
    )
    
    # All objects themselves should be identical too
    expect_identical(backend_singleton, backend1)
    expect_identical(backend1, backend2)

    BackendRegistry$new()$unregister_backend("DummyTensor")
})

# TODO use a helper to test all backends like in the other tests

test_that("each backend's stack_on_zeroth_dimension() works", {

    x <- array(1:(10 * 20 * 30 * 40), dim = c(10, 20, 30, 40))
    x_list <- lapply(1:10, function(i) {
        array(i:(i + 20 * 30 * 40 - 1), dim = c(20, 30, 40))
    })

    expect_equal(x, get_backend(x)$stack_on_zeroth_dimension(x_list))
})
