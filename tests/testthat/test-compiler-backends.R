test_that("get_backend returns identical singleton objects", {

    dummy_tensor <- structure(list(), class = "DummyTensor")
    DummyBackend <- R6::R6Class("DummyBackend", inherit = EinopsBackend,
        public = list(tensor_type = function() "DummyTensor")
    )

    backend0 <- DummyBackend$new()
    BackendRegistry$new()$register_backend(DummyBackend$new())

    backend1 <- get_backend(dummy_tensor)
    backend2 <- get_backend(dummy_tensor)

    # All addresses should be identical
    expect_identical(lobstr::ref(backend0), lobstr::ref(backend1))
    expect_identical(lobstr::ref(backend1), lobstr::ref(backend2))
    
    # All objects should be identical
    expect_identical(backend0, backend1)
    expect_identical(backend1, backend2)
})
