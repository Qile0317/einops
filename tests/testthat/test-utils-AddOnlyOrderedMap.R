# Tests for AddOnlyOrderedMap and its methods

test_that("AddOnlyOrderedMap basic creation and length", {
    map <- AddOnlyOrderedMap(keys = list("a", "b"), values = list(1, 2))
    expect_s3_class(map, "AddOnlyOrderedMap")
    expect_equal(length(map), 2)
})

test_that("AddOnlyOrderedMap insertion and querying", {
    map <- AddOnlyOrderedMap()
    map[["x"]] <- 10
    map[["y"]] <- 20
    expect_equal(map[["x"]], 10)
    expect_equal(map[["y"]], 20)
    expect_equal(map["x"], list(10))
    expect_equal(map["y"], list(20))
})

test_that("AddOnlyOrderedMap vectorized insertion and update", {
    map <- AddOnlyOrderedMap()
    map[c("a", "b")] <- c(1, 2)
    expect_equal(map[["a"]], 1)
    expect_equal(map[["b"]], 2)
    # Update value
    map[["a"]] <- 100
    expect_equal(map[["a"]], 100)
})

test_that("keys, has_key, values, as.list, get_key_to_index_map", {
    map <- AddOnlyOrderedMap(keys = list("k1", "k2"), values = list(11, 22))
    expect_equal(keys(map), list("k1", "k2"))
    expect_true(has_key(map, "k1"))
    expect_false(has_key(map, "not_in_map"))
    expect_equal(values(map), list(11, 22))
    aslist <- as.list(map)
    expect_true(is.list(aslist))
    expect_equal(unname(aslist), list(11, 22))
    idx_map <- get_key_to_index_map(map)
    expect_true("k1" %in% r2r::keys(idx_map))
    expect_true("k2" %in% r2r::keys(idx_map))
})

test_that("AddOnlyOrderedMap respects key and value validators", {
    key_val <- function(x) is.character(x) && nchar(x) == 1
    val_val <- function(x) is.numeric(x) && x > 0
    map <- AddOnlyOrderedMap(key_validator = key_val, val_validator = val_val)
    map[["z"]] <- 5
    expect_equal(map[["z"]], 5)
    expect_error(map[["zz"]] <- 1, "Key validation failed")
    expect_error(map[["a"]] <- -1, "Value validation failed")
})

test_that("get_keys_in_order and get_values_in_order preserve order", {
    map <- AddOnlyOrderedMap()
    map[["first"]] <- 1
    map[["second"]] <- 2
    map[["third"]] <- 3
    expect_equal(map$get_keys_in_order(), list("first", "second", "third"))
    expect_equal(map$get_values_in_order(), list(1, 2, 3))
})

test_that("size method returns correct size", {
    map <- AddOnlyOrderedMap()
    expect_equal(map$size(), 0)
    map[["a"]] <- 1
    expect_equal(map$size(), 1)
    map[["b"]] <- 2
    expect_equal(map$size(), 2)
})

test_that("modifying a value preserves map integrity", {
    map <- AddOnlyOrderedMap(keys = list("foo", "bar"), values = list(1, 2))
    map[["foo"]] <- 99
    expect_equal(map[["foo"]], 99)
    expect_equal(map[["bar"]], 2)
    expect_equal(keys(map), list("foo", "bar"))
    expect_equal(values(map), list(99, 2))
    expect_equal(unname(as.list(map)), list(99, 2))
    expect_equal(map$size(), 2)
})
