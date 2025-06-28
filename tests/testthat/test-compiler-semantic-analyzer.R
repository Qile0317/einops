# # Mock AST node constructors
# NamedAxisAstNode <- function(name) structure(list(name = name), class = "NamedAxisAstNode")
# ConstantAstNode <- function() structure(list(), class = "ConstantAstNode")
# GroupAstNode <- function(children) structure(list(children = children), class = "GroupAstNode")
# EllipsisAstNode <- function() structure(list(), class = "EllipsisAstNode")

# # Helper to extract axis names from elementaries
# get_axis_names <- function(elementaries) vapply(elementaries, function(ax) if (is.null(ax$name)) NA_character_ else ax$name, character(1))
# get_axis_lengths <- function(elementaries) vapply(elementaries, function(ax) ax$length, integer(1))
# get_axis_ids <- function(elementaries) vapply(elementaries, function(ax) ax$id, integer(1))


# test_that("analyse_semantics works for simple rearrange", {
#     ast <- list(
#         input_axes = list(NamedAxisAstNode("b"), NamedAxisAstNode("c")),
#         output_axes = list(NamedAxisAstNode("c"), NamedAxisAstNode("b"))
#     )
#     sem <- analyse_semantics(ast, op_kind = "rearrange")
#     expect_s3_class(sem, "SemanticInfo")
#     expect_equal(get_axis_names(sem$elementaries), c("b", "c"))
#     expect_equal(sem$name2id, list(b = 0L, c = 1L))
#     expect_equal(sem$axes_permutation, c(2, 1))
#     expect_equal(sem$reduced_axes, integer(0))
#     expect_equal(sem$added_axes, integer(0))
# })

# test_that("analyse_semantics works for reduce", {
#     ast <- list(
#         input_axes = list(NamedAxisAstNode("b"), NamedAxisAstNode("c")),
#         output_axes = list(NamedAxisAstNode("b"))
#     )
#     sem <- analyse_semantics(ast, op_kind = "reduce")
#     expect_equal(get_axis_names(sem$elementaries), c("b", "c"))
#     expect_equal(sem$reduced_axes, 1L) # axis id 1 ("c") is reduced
# })

# test_that("analyse_semantics works for repeat (added axis)", {
#     ast <- list(
#         input_axes = list(NamedAxisAstNode("b")),
#         output_axes = list(NamedAxisAstNode("b"), NamedAxisAstNode("new"))
#     )
#     sem <- analyse_semantics(ast, op_kind = "repeat")
#     expect_equal(get_axis_names(sem$elementaries), c("b", NA))
#     expect_equal(unname(sem$added_axes), 1L)
#     expect_true("new" %in% names(sem$name2id))
# })

# test_that("analyse_semantics handles ConstantAstNode and EllipsisAstNode", {
#     ast <- list(
#         input_axes = list(NamedAxisAstNode("a"), ConstantAstNode(), EllipsisAstNode()),
#         output_axes = list(NamedAxisAstNode("a"), ConstantAstNode(), EllipsisAstNode())
#     )
#     sem <- analyse_semantics(ast, op_kind = "rearrange")
#     # Accept length >= 3, but first three should match input
#     expect_true(length(sem$elementaries) >= 3)
#     expect_equal(get_axis_names(sem$elementaries)[1:3], c("a", NA, NA))
# })

# test_that("analyse_semantics handles GroupAstNode", {
#     ast <- list(
#         input_axes = list(GroupAstNode(list(NamedAxisAstNode("a"), NamedAxisAstNode("b")))),
#         output_axes = list(NamedAxisAstNode("b"), NamedAxisAstNode("a"))
#     )
#     sem <- analyse_semantics(ast, op_kind = "rearrange")
#     expect_equal(get_axis_names(sem$elementaries), c("a", "b"))
#     expect_equal(sem$axes_permutation, c(2, 1))
# })

# test_that("analyse_semantics infers axis lengths and detects conflicts", {
#     ast <- list(
#         input_axes = list(NamedAxisAstNode("a")),
#         output_axes = list(NamedAxisAstNode("a"))
#     )
#     sem <- analyse_semantics(ast, op_kind = "rearrange", axes_lengths_hint = list(a = 5L))
#     expect_equal(get_axis_lengths(sem$elementaries), 5L)
#     # Set length first, then try to set conflicting length
#     sem$elementaries[[1]]$length <- 5L
#     expect_error(infer_lengths(sem$elementaries, sem$name2id, axes_lengths_hint = list(a = 3L)),
#                  "Conflicting length hint")
# })

# test_that("factorise_inputs and output_compositions are correct", {
#     ast <- list(
#         input_axes = list(GroupAstNode(list(NamedAxisAstNode("a"), NamedAxisAstNode("b"))), NamedAxisAstNode("c")),
#         output_axes = list(NamedAxisAstNode("a"), NamedAxisAstNode("b"), NamedAxisAstNode("c"))
#     )
#     sem <- analyse_semantics(ast, op_kind = "rearrange")
#     # input_factorisations: first input is group (a, b), second is c
#     expect_equal(length(sem$input_factorisations), 2)
#     expect_equal(sem$input_factorisations[[1]]$factors, c(0L, 1L))
#     expect_equal(sem$input_factorisations[[2]]$factors, 2L)
#     # output_compositions: each output dim is a single axis
#     expect_equal(length(sem$output_compositions), 3)
#     expect_equal(sem$output_compositions[[1]]$factors, 0L)
#     expect_equal(sem$output_compositions[[2]]$factors, 1L)
#     expect_equal(sem$output_compositions[[3]]$factors, 2L)
# })

# # Edge case: output axis not in input (not allowed except in repeat)
# test_that("analyse_semantics errors for output axis not in input (not repeat)", {
#     ast <- list(
#         input_axes = list(NamedAxisAstNode("a")),
#         output_axes = list(NamedAxisAstNode("b"))
#     )
#     expect_error(analyse_semantics(ast, op_kind = "rearrange"),
#                  "Output axis 'b' not found in input axes")
# })
