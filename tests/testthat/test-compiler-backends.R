test_that("get_backend returns identical singleton objects", {
    
    dummy_tensor <- structure(list(), class = "DummyTensor")
    DummyBackend <- R6::R6Class(
        "DummyBackend", inherit = EinopsBackend, cloneable = FALSE,
        public = list(tensor_type = function() "DummyTensor")
    )

    # Create and register the singleton instance only once
    backend_singleton <- DummyBackend$new()
    BackendRegistry$new()$register_backend(DummyBackend)

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
})
