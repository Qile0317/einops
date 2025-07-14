test_that("get_backend() returns identical singleton objects and different backends have different addresses", {
    
    # Set up DummyBackend (first backend type)
    dummy_tensor <- structure(list(), class = "DummyTensor")
    DummyBackend <- R6::R6Class(
        "DummyBackend", inherit = EinopsBackend, cloneable = FALSE
    )

    # Set up DummyBackend2 (second backend type)
    dummy_tensor2 <- structure(list(), class = "DummyTensor2")
    DummyBackend2 <- R6::R6Class(
        "DummyBackend2", inherit = EinopsBackend, cloneable = FALSE
    )

    # Create and register both singleton instances
    backend_singleton <- DummyBackend$new()
    backend2_singleton <- DummyBackend2$new()
    register_backend("DummyTensor", DummyBackend)
    register_backend("DummyTensor2", DummyBackend2)

    # Get backend instances for first type
    backend1 <- get_backend(dummy_tensor)
    backend2 <- get_backend(dummy_tensor)
    
    # Get backend instances for second type
    backend2_1 <- get_backend(dummy_tensor2)
    backend2_2 <- get_backend(dummy_tensor2)

    # Test that instances of the same backend type have identical addresses
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
    
    # Test that instances of the second backend type have identical addresses
    expect_identical(
        lobstr::obj_addr(backend2_singleton), lobstr::obj_addr(backend2_1)
    )
    expect_identical(
        lobstr::obj_addr(backend2_1), lobstr::obj_addr(backend2_2)
    )
    expect_identical(
        lobstr::obj_addrs(backend2_singleton), lobstr::obj_addrs(backend2_1)
    )
    expect_identical(
        lobstr::obj_addrs(backend2_1), lobstr::obj_addrs(backend2_2)
    )
    
    # Test that objects of the same backend type are identical
    expect_identical(backend_singleton, backend1)
    expect_identical(backend1, backend2)
    expect_identical(backend2_singleton, backend2_1)
    expect_identical(backend2_1, backend2_2)
    
    # Test that different backend types have different addresses
    expect_false(identical(
        lobstr::obj_addr(backend_singleton), lobstr::obj_addr(backend2_singleton)
    ))
    expect_false(identical(
        lobstr::obj_addr(backend1), lobstr::obj_addr(backend2_1)
    ))
    expect_false(identical(
        lobstr::obj_addr(backend2), lobstr::obj_addr(backend2_2)
    ))
    
    # Test that different backend types are not identical objects
    expect_false(identical(backend_singleton, backend2_singleton))
    expect_false(identical(backend1, backend2_1))
    expect_false(identical(backend2, backend2_2))
    
    # Test that different backend types have different classes
    expect_false(identical(class(backend1), class(backend2_1)))
    expect_equal(class(backend1)[1], "DummyBackend")
    expect_equal(class(backend2_1)[1], "DummyBackend2")

    # Clean up
    unregister_backend("DummyTensor")
    unregister_backend("DummyTensor2")
})

# TODO use a helper to test all backends like in the other tests

test_that("each backend's stack_on_zeroth_dimension() works", {

    x <- array(1:(10 * 20 * 30 * 40), dim = c(10, 20, 30, 40))
    x_list <- lapply(1:10, function(i) {
        x[i, , , ]
    })

    expect_equal(x, get_backend(x)$stack_on_zeroth_dimension(x_list))
})
