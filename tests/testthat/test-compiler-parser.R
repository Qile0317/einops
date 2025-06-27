test_that("AST node constructors work correctly", {
  # Test NamedAxisAstNode
  node <- NamedAxisAstNode("width", 224, list(start = 1, length = 5))
  expect_s3_class(node, c("NamedAxisAstNode", "AstNode"))
  expect_equal(node$name, "width")
  expect_equal(node$count, 224)
  expect_equal(node$src, list(start = 1, length = 5))
  
  # Test EllipsisAstNode
  ellipsis <- EllipsisAstNode(list(start = 10, length = 3))
  expect_s3_class(ellipsis, c("EllipsisAstNode", "AstNode"))
  expect_equal(ellipsis$src, list(start = 10, length = 3))
  
  # Test GroupAstNode
  child1 <- NamedAxisAstNode("h", NULL, list(start = 1, length = 1))
  child2 <- NamedAxisAstNode("w", NULL, list(start = 3, length = 1))
  group <- GroupAstNode(list(child1, child2), list(start = 0, length = 5))
  expect_s3_class(group, c("GroupAstNode", "AstNode"))
  expect_length(group$children, 2)
  expect_equal(group$src, list(start = 0, length = 5))
  
  # Test EinopsAst
  input_axes <- list(child1)
  output_axes <- list(child2)
  ast <- EinopsAst(input_axes, output_axes, list(start = 1, length = 10))
  expect_s3_class(ast, c("EinopsAst", "AstNode"))
  expect_length(ast$input_axes, 1)
  expect_length(ast$output_axes, 1)
  expect_equal(ast$src, list(start = 1, length = 10))
})

test_that("merge_src utility works correctly", {
  src_a <- list(start = 5, length = 5)
  src_b <- list(start = 15, length = 5)
  merged <- merge_src(src_a, src_b)
  expect_equal(merged, list(start = 5, length = 15))
  
  # Test with swapped order
  merged_rev <- merge_src(src_b, src_a)
  expect_equal(merged_rev, list(start = 5, length = 15))
})

test_that("find_top_level_arrow locates arrow correctly", {
  # Simple case: "a -> b"
  tokens <- TokenSequence(
    NameToken("a", 1),
    ArrowToken(3),
    NameToken("b", 6)
  )
  arrow_pos <- find_top_level_arrow(tokens)
  expect_equal(arrow_pos, 2)
  
  # Nested case: "a (b -> c) -> d" - should find position 4 (the outer arrow)
  tokens_nested <- TokenSequence(
    NameToken("a", 1),
    LParenToken(3),
    NameToken("b", 5),
    ArrowToken(7),
    NameToken("c", 10),
    RParenToken(12),
    ArrowToken(14),
    NameToken("d", 17)
  )
  arrow_pos_nested <- find_top_level_arrow(tokens_nested)
  expect_equal(arrow_pos_nested, 7)
})

test_that("find_top_level_arrow errors on missing arrow", {
  tokens <- TokenSequence(
    NameToken("a", 1),
    NameToken("b", 3)
  )
  expect_error(find_top_level_arrow(tokens), "Missing arrow")
})

test_that("find_top_level_arrow errors on multiple top-level arrows", {
  tokens <- TokenSequence(
    NameToken("a", 1),
    ArrowToken(3),
    NameToken("b", 6),
    ArrowToken(8),
    NameToken("c", 11)
  )
  expect_error(find_top_level_arrow(tokens), "Multiple top-level arrows")
})

test_that("parse_axes_iter handles simple axis lists", {
  # Simple case: "a b c"
  tokens <- TokenSequence(
    NameToken("a", 1),
    NameToken("b", 3),
    NameToken("c", 5)
  )
  result <- parse_axes_iter(tokens)
  expect_length(result, 3)
  expect_s3_class(result[[1]], "NamedAxisAstNode")
  expect_equal(result[[1]]$name, "a")
  expect_equal(result[[2]]$name, "b")
  expect_equal(result[[3]]$name, "c")
})

test_that("parse_axes_iter handles named axes with counts", {
  # Case: "batch height 224 width 224"
  tokens <- TokenSequence(
    NameToken("batch", 1),
    NameToken("height", 7),
    IntToken("224", 14),
    NameToken("width", 18),
    IntToken("224", 24)
  )
  result <- parse_axes_iter(tokens)
  expect_length(result, 3)
  expect_equal(result[[1]]$name, "batch")
  expect_null(result[[1]]$count)
  expect_equal(result[[2]]$name, "height")
  expect_equal(result[[2]]$count, "224")
  expect_equal(result[[3]]$name, "width")
  expect_equal(result[[3]]$count, "224")
})

test_that("parse_axes_iter handles ellipsis", {
  # Case: "batch ... channels"
  tokens <- TokenSequence(
    NameToken("batch", 1),
    EllipsisToken(7),
    NameToken("channels", 11)
  )
  result <- parse_axes_iter(tokens)
  expect_length(result, 3)
  expect_s3_class(result[[1]], "NamedAxisAstNode")
  expect_s3_class(result[[2]], "EllipsisAstNode")
  expect_s3_class(result[[3]], "NamedAxisAstNode")
})

test_that("parse_axes_iter handles groups", {
  # Case: "batch (height width) channels"
  tokens <- TokenSequence(
    NameToken("batch", 1),
    LParenToken(7),
    NameToken("height", 9),
    NameToken("width", 16),
    RParenToken(22),
    NameToken("channels", 24)
  )
  result <- parse_axes_iter(tokens)
  expect_length(result, 3)
  expect_s3_class(result[[1]], "NamedAxisAstNode")
  expect_s3_class(result[[2]], "GroupAstNode")
  expect_s3_class(result[[3]], "NamedAxisAstNode")
  expect_length(result[[2]]$children, 2)
  expect_equal(result[[2]]$children[[1]]$name, "height")
  expect_equal(result[[2]]$children[[2]]$name, "width")
})

test_that("parse_axes_iter handles nested groups", {
  # Case: "((a1 2) (b 3))"
  tokens <- TokenSequence(
    LParenToken(1),
    LParenToken(3),
    NameToken("a1", 5),
    IntToken("2", 8),
    RParenToken(10),
    LParenToken(12),
    NameToken("b", 14),
    IntToken("3", 16),
    RParenToken(18),
    RParenToken(20)
  )
  result <- parse_axes_iter(tokens)
  expect_length(result, 1)
  expect_s3_class(result[[1]], "GroupAstNode")
  expect_length(result[[1]]$children, 2)
  
  # Check nested structure
  inner_group1 <- result[[1]]$children[[1]]
  inner_group2 <- result[[1]]$children[[2]]
  expect_s3_class(inner_group1, "GroupAstNode")
  expect_s3_class(inner_group2, "GroupAstNode")
  expect_equal(inner_group1$children[[1]]$name, "a1")
  expect_equal(inner_group1$children[[1]]$count, "2")
  expect_equal(inner_group2$children[[1]]$name, "b")
  expect_equal(inner_group2$children[[1]]$count, "3")
})

test_that("parse_axes_iter errors on multiple ellipses", {
  tokens <- TokenSequence(
    NameToken("a", 1),
    EllipsisToken(3),
    NameToken("b", 7),
    EllipsisToken(9),
    NameToken("c", 13)
  )
  expect_error(parse_axes_iter(tokens), "Multiple ellipses")
})

test_that("parse_axes_iter errors on unmatched parentheses", {
  # Unmatched opening paren
  tokens <- TokenSequence(
    NameToken("a", 1),
    LParenToken(3),
    NameToken("b", 5)
  )
  expect_error(parse_axes_iter(tokens), "Unmatched opening parenthesis")
  
  # Unmatched closing paren
  tokens2 <- TokenSequence(
    NameToken("a", 1),
    RParenToken(3),
    NameToken("b", 5)
  )
  expect_error(parse_axes_iter(tokens2), "Unmatched closing parenthesis")
})

test_that("parse_axes_iter errors on empty group", {
  tokens <- TokenSequence(
    NameToken("a", 1),
    LParenToken(3),
    RParenToken(5),
    NameToken("b", 7)
  )
  expect_error(parse_axes_iter(tokens), "Empty group")
})

test_that("parse_axes_iter errors on unexpected tokens", {
  # This would need a new token type that shouldn't appear in axis lists
  # For now, let's test with a hypothetical case
  tokens <- TokenSequence(
    NameToken("a", 1),
    ArrowToken(3),  # Arrow shouldn't appear in axis list
    NameToken("b", 6)
  )
  expect_error(parse_axes_iter(tokens), "Unexpected token type")
})

test_that("parse_einops_ast handles simple patterns", {
  # "b (h w) c -> b c (h w)"
  tokens <- TokenSequence(
    NameToken("b", 1),
    LParenToken(3),
    NameToken("h", 5),
    NameToken("w", 7),
    RParenToken(9),
    NameToken("c", 11),
    ArrowToken(13),
    NameToken("b", 16),
    NameToken("c", 18),
    LParenToken(20),
    NameToken("h", 22),
    NameToken("w", 24),
    RParenToken(26)
  )
  
  ast <- parse_einops_ast(tokens)
  expect_s3_class(ast, "EinopsAst")
  expect_length(ast$input_axes, 3)
  expect_length(ast$output_axes, 3)
  
  # Check input structure
  expect_equal(ast$input_axes[[1]]$name, "b")
  expect_s3_class(ast$input_axes[[2]], "GroupAstNode")
  expect_equal(ast$input_axes[[3]]$name, "c")
  
  # Check output structure
  expect_equal(ast$output_axes[[1]]$name, "b")
  expect_equal(ast$output_axes[[2]]$name, "c")
  expect_s3_class(ast$output_axes[[3]], "GroupAstNode")
})

test_that("parse_einops_ast handles ellipsis patterns", {
  # "b ... c -> ... c"
  tokens <- TokenSequence(
    NameToken("b", 1),
    EllipsisToken(3),
    NameToken("c", 7),
    ArrowToken(9),
    EllipsisToken(12),
    NameToken("c", 16)
  )
  
  ast <- parse_einops_ast(tokens)
  expect_s3_class(ast, "EinopsAst")
  expect_length(ast$input_axes, 3)
  expect_length(ast$output_axes, 2)
  
  expect_s3_class(ast$input_axes[[2]], "EllipsisAstNode")
  expect_s3_class(ast$output_axes[[1]], "EllipsisAstNode")
})

test_that("parse_einops_ast handles stress test with deep nesting", {
  # Build 100 nested parentheses: "((((...(a)...))))"
  tokens <- list()
  
  # Add 100 opening parens
  for (i in 1:100) {
    tokens <- append(tokens, list(LParenToken(i)))
  }
  
  # Add the axis
  tokens <- append(tokens, list(NameToken("a", 101)))
  
  # Add 100 closing parens
  for (i in 103:202) {
    tokens <- append(tokens, list(RParenToken(i)))
  }
  
  # Add arrow and output
  tokens <- append(tokens, list(ArrowToken(203)))
  tokens <- append(tokens, list(NameToken("a", 206)))
  
  token_seq <- do.call(TokenSequence, tokens)
  
  # This should not cause stack overflow
  ast <- parse_einops_ast(token_seq)
  expect_s3_class(ast, "EinopsAst")
  expect_length(ast$input_axes, 1)
  expect_length(ast$output_axes, 1)
  
  # Traverse the nested structure to ensure it's correct
  current <- ast$input_axes[[1]]
  depth <- 0
  while (inherits(current, "GroupAstNode")) {
    depth <- depth + 1
    expect_length(current$children, 1)
    current <- current$children[[1]]
  }
  expect_equal(depth, 100)
  expect_equal(current$name, "a")
})

test_that("parse_einops_ast errors on unmatched parentheses", {
  # "b (h w -> c" - unmatched paren will cause arrow detection to fail
  tokens <- TokenSequence(
    NameToken("b", 1),
    LParenToken(3),
    NameToken("h", 5),
    NameToken("w", 7),
    ArrowToken(9),
    NameToken("c", 12)
  )
  expect_error(parse_einops_ast(tokens), "Missing arrow")
})

test_that("parse_einops_ast errors on double ellipsis", {
  # "b ... ... -> c"
  tokens <- TokenSequence(
    NameToken("b", 1),
    EllipsisToken(3),
    EllipsisToken(7),
    ArrowToken(11),
    NameToken("c", 14)
  )
  expect_error(parse_einops_ast(tokens), "Multiple ellipses")
})

test_that("parse_einops_ast errors on missing arrow", {
  # "b h w"
  tokens <- TokenSequence(
    NameToken("b", 1),
    NameToken("h", 3),
    NameToken("w", 5)
  )
  expect_error(parse_einops_ast(tokens), "Missing arrow")
})

test_that("parse_einops_ast errors on empty group", {
  # "()-> b"
  tokens <- TokenSequence(
    LParenToken(1),
    RParenToken(3),
    ArrowToken(5),
    NameToken("b", 8)
  )
  expect_error(parse_einops_ast(tokens), "Empty group")
})

test_that("parse_einops_ast errors on empty output", {
  # "b h w ->"
  tokens <- TokenSequence(
    NameToken("b", 1),
    NameToken("h", 3),
    NameToken("w", 5),
    ArrowToken(7)
  )
  expect_error(parse_einops_ast(tokens), "Empty output pattern")
})

test_that("parse_einops_ast errors on empty input", {
  # "-> b"
  tokens <- TokenSequence(
    ArrowToken(1),
    NameToken("b", 4)
  )
  expect_error(parse_einops_ast(tokens), "Empty input pattern")
})
