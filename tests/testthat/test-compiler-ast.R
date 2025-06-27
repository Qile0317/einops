# test_that("ast_to_tokens converts AST back to tokens correctly", {
#   # Test simple pattern: "h w -> (h w)"
  
#   # Create the expected AST
#   h_axis <- NamedAxisAstNode("h", list(start = 1))
#   w_axis <- NamedAxisAstNode("w", list(start = 3))
  
#   # Output group containing h and w
#   h_axis_out <- NamedAxisAstNode("h", list(start = 7))
#   w_axis_out <- NamedAxisAstNode("w", list(start = 9))
#   group_out <- GroupAstNode(list(h_axis_out, w_axis_out), list(start = 6))
  
#   ast <- EinopsAst(list(h_axis, w_axis), list(group_out), list(start = 1))
  
#   # Convert back to tokens
#   result_tokens <- ast_to_tokens(ast)
  
#   # Create expected token sequence
#   expected_tokens <- TokenSequence(
#     NameToken("h", 1),
#     NameToken("w", 3),
#     ArrowToken(5),
#     LParenToken(8),
#     NameToken("h", 9),
#     NameToken("w", 11),
#     RParenToken(12)
#   )
  
#   # Check we get the right number of tokens
#   expect_length(result_tokens, length(expected_tokens))
  
#   # Check each token matches
#   for (i in seq_along(expected_tokens)) {
#     expect_equal(result_tokens[[i]]$type, expected_tokens[[i]]$type)
#     expect_equal(result_tokens[[i]]$value, expected_tokens[[i]]$value)
#   }
# })

# test_that("ast_to_tokens handles ellipsis and constants", {
#   # Test pattern: "... h 224 -> h 224"
  
#   ellipsis_axis <- EllipsisAstNode(list(start = 1))
#   h_axis <- NamedAxisAstNode("h", list(start = 5))
#   const_axis <- ConstantAstNode("224", list(start = 7))
  
#   h_axis_out <- NamedAxisAstNode("h", list(start = 14))
#   const_axis_out <- ConstantAstNode("224", list(start = 16))
  
#   ast <- EinopsAst(
#     list(ellipsis_axis, h_axis, const_axis),
#     list(h_axis_out, const_axis_out),
#     list(start = 1)
#   )
  
#   # Convert back to tokens
#   result_tokens <- ast_to_tokens(ast)
  
#   # Create expected token sequence
#   expected_tokens <- TokenSequence(
#     EllipsisToken(1),
#     NameToken("h", 5),
#     IntToken("224", 7),
#     ArrowToken(11),
#     NameToken("h", 14),
#     IntToken("224", 16)
#   )
  
#   # Check we get the right number of tokens
#   expect_length(result_tokens, length(expected_tokens))
  
#   # Check each token matches
#   for (i in seq_along(expected_tokens)) {
#     expect_equal(result_tokens[[i]]$type, expected_tokens[[i]]$type)
#     expect_equal(result_tokens[[i]]$value, expected_tokens[[i]]$value)
#   }
# })

# test_that("round trip conversion works", {
#   # Test that lex -> parse -> ast_to_tokens produces equivalent tokens
#   pattern <- "h w c -> (h w) c"
  
#   # Forward conversion
#   original_tokens <- lex(pattern)
#   ast <- parse_einops_ast(original_tokens)
#   result_tokens <- ast_to_tokens(ast)
  
#   # Check that we get the same number of tokens
#   expect_length(result_tokens, length(original_tokens))
  
#   # Check that token types and values match
#   for (i in seq_along(original_tokens)) {
#     expect_equal(result_tokens[[i]]$type, original_tokens[[i]]$type)
#     expect_equal(result_tokens[[i]]$value, original_tokens[[i]]$value)
#   }
# })

# test_that("ast_to_tokens handles nested groups correctly", {
#   # Test pattern: "a (b c) -> (a b) c"
  
#   # Input: a, (b c)
#   a_axis <- NamedAxisAstNode("a", list(start = 1))
#   b_axis <- NamedAxisAstNode("b", list(start = 4))
#   c_axis <- NamedAxisAstNode("c", list(start = 6))
#   input_group <- GroupAstNode(list(b_axis, c_axis), list(start = 3))
  
#   # Output: (a b), c
#   a_axis_out <- NamedAxisAstNode("a", list(start = 12))
#   b_axis_out <- NamedAxisAstNode("b", list(start = 14))
#   output_group <- GroupAstNode(list(a_axis_out, b_axis_out), list(start = 11))
#   c_axis_out <- NamedAxisAstNode("c", list(start = 17))
  
#   ast <- EinopsAst(list(a_axis, input_group), list(output_group, c_axis_out), list(start = 1))
  
#   # Convert back to tokens
#   result_tokens <- ast_to_tokens(ast)
  
#   # Create expected token sequence
#   expected_tokens <- TokenSequence(
#     NameToken("a", 1),
#     LParenToken(3),
#     NameToken("b", 4),
#     NameToken("c", 6),
#     RParenToken(7),
#     ArrowToken(9),
#     LParenToken(12),
#     NameToken("a", 13),
#     NameToken("b", 15),
#     RParenToken(16),
#     NameToken("c", 18)
#   )
  
#   # Check we get the right number of tokens
#   expect_length(result_tokens, length(expected_tokens))
  
#   # Check each token matches
#   for (i in seq_along(expected_tokens)) {
#     expect_equal(result_tokens[[i]]$type, expected_tokens[[i]]$type)
#     expect_equal(result_tokens[[i]]$value, expected_tokens[[i]]$value)
#   }
# })
