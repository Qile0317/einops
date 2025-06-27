test_that("AST node constructors work correctly", {
  # Test NamedAxisAstNode
  node <- NamedAxisAstNode("width", list(start = 1, length = 5))
  expect_s3_class(node, c("NamedAxisAstNode", "AstNode"))
  expect_equal(node$name, "width")
  expect_equal(node$src, list(start = 1, length = 5))
  
  # Test ConstantAstNode
  constant <- ConstantAstNode("224", list(start = 7, length = 3))
  expect_s3_class(constant, c("ConstantAstNode", "AstNode"))
  expect_equal(constant$count, "224")
  expect_equal(constant$src, list(start = 7, length = 3))
  
  # Test EllipsisAstNode
  ellipsis <- EllipsisAstNode(list(start = 10, length = 3))
  expect_s3_class(ellipsis, c("EllipsisAstNode", "AstNode"))
  expect_equal(ellipsis$src, list(start = 10, length = 3))
  
  # Test GroupAstNode
  child1 <- NamedAxisAstNode("h", list(start = 1, length = 1))
  child2 <- NamedAxisAstNode("w", list(start = 3, length = 1))
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

test_that("find_arrow locates arrow correctly", {
  # Simple case: "a -> b"
  tokens <- TokenSequence(
    NameToken("a", 1),
    ArrowToken(3),
    NameToken("b", 6)
  )
  arrow_pos <- find_top_level_arrow_index(tokens)
  expect_equal(arrow_pos, 2)
  
  # Case with groups: "a (b c) -> d (e f)"
  tokens_with_groups <- TokenSequence(
    NameToken("a", 1),
    LParenToken(3),
    NameToken("b", 5),
    NameToken("c", 7),
    RParenToken(9),
    ArrowToken(11),
    NameToken("d", 14),
    LParenToken(16),
    NameToken("e", 18),
    NameToken("f", 20),
    RParenToken(22)
  )
  arrow_pos_groups <- find_top_level_arrow_index(tokens_with_groups)
  expect_equal(arrow_pos_groups, 6)
})

test_that("find_arrow errors on missing arrow", {
  tokens <- TokenSequence(
    NameToken("a", 1),
    NameToken("b", 3)
  )
  expect_error(find_top_level_arrow_index(tokens), "No '->' found")
})

test_that("find_arrow errors on multiple arrows", {
  tokens <- TokenSequence(
    NameToken("a", 1),
    ArrowToken(3),
    NameToken("b", 6),
    ArrowToken(8),
    NameToken("c", 11)
  )
  expect_error(find_top_level_arrow_index(tokens), "Multiple '->' found")
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

test_that("parse_axes_iter handles named axes with constants", {
  # Case: "batch height 224 width 224"
  tokens <- TokenSequence(
    NameToken("batch", 1),
    NameToken("height", 7),
    IntToken("224", 14),
    NameToken("width", 18),
    IntToken("224", 24)
  )
  result <- parse_axes_iter(tokens)
  expect_length(result, 5)
  expect_equal(result[[1]]$name, "batch")
  expect_equal(result[[2]]$name, "height")
  expect_s3_class(result[[3]], "ConstantAstNode")
  expect_equal(result[[3]]$count, "224")
  expect_equal(result[[4]]$name, "width")
  expect_s3_class(result[[5]], "ConstantAstNode")
  expect_equal(result[[5]]$count, "224")
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

test_that("parse_axes_iter handles groups with constants", {
  # Case: "(h 224 w 224)" - group with constants
  tokens <- TokenSequence(
    LParenToken(1),
    NameToken("h", 3),
    IntToken("224", 5),
    NameToken("w", 9),
    IntToken("224", 11),
    RParenToken(15)
  )
  result <- parse_axes_iter(tokens)
  expect_length(result, 1)
  expect_s3_class(result[[1]], "GroupAstNode")
  expect_length(result[[1]]$children, 4)  # h, 224, w, 224
  expect_equal(result[[1]]$children[[1]]$name, "h")
  expect_s3_class(result[[1]]$children[[2]], "ConstantAstNode")
  expect_equal(result[[1]]$children[[2]]$count, "224")
  expect_equal(result[[1]]$children[[3]]$name, "w")
  expect_s3_class(result[[1]]$children[[4]], "ConstantAstNode")
  expect_equal(result[[1]]$children[[4]]$count, "224")
})

test_that("parse_axes_iter errors on nesting groups", {
  # Case: "((a b) c)" - nesting not allowed in einops
  tokens <- TokenSequence(
    LParenToken(1),
    LParenToken(3),
    NameToken("a", 5),
    NameToken("b", 7),
    RParenToken(9),
    NameToken("c", 11),
    RParenToken(13)
  )
  expect_error(parse_axes_iter(tokens), "Groups cannot be nested")
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

test_that("parse_einops_ast handles real einops patterns", {
  # "b h w c -> b (h w) c" - common rearrange pattern
  tokens <- TokenSequence(
    NameToken("b", 1),
    NameToken("h", 3),
    NameToken("w", 5),
    NameToken("c", 7),
    ArrowToken(9),
    NameToken("b", 12),
    LParenToken(14),
    NameToken("h", 16),
    NameToken("w", 18),
    RParenToken(20),
    NameToken("c", 22)
  )
  
  ast <- parse_einops_ast(tokens)
  expect_s3_class(ast, "EinopsAst")
  expect_length(ast$input_axes, 4)
  expect_length(ast$output_axes, 3)
  
  # Check that group is parsed correctly
  expect_s3_class(ast$output_axes[[2]], "GroupAstNode")
  expect_length(ast$output_axes[[2]]$children, 2)
})

test_that("parse_einops_ast handles patterns with constants", {
  # "b h w -> b (h 2 w)" - repeat pattern with constant
  tokens <- TokenSequence(
    NameToken("b", 1),
    NameToken("h", 3),
    NameToken("w", 5),
    ArrowToken(7),
    NameToken("b", 10),
    LParenToken(12),
    NameToken("h", 14),
    IntToken("2", 16),
    NameToken("w", 18),
    RParenToken(20)
  )
  
  ast <- parse_einops_ast(tokens)
  expect_s3_class(ast, "EinopsAst")
  expect_length(ast$input_axes, 3)
  expect_length(ast$output_axes, 2)
  
  # Check that group contains constant
  group <- ast$output_axes[[2]]
  expect_s3_class(group, "GroupAstNode")
  expect_length(group$children, 3)  # h, 2, w
  expect_s3_class(group$children[[2]], "ConstantAstNode")
  expect_equal(group$children[[2]]$count, "2")
})

test_that("parse_einops_ast errors on unmatched parentheses", {
  # "b (h w -> c" - unmatched opening paren
  tokens <- TokenSequence(
    NameToken("b", 1),
    LParenToken(3),
    NameToken("h", 5),
    NameToken("w", 7),
    ArrowToken(9),
    NameToken("c", 12)
  )
  expect_error(parse_einops_ast(tokens), "Unmatched opening parenthesis")
  
  # "b h w) -> c" - unmatched closing paren
  tokens2 <- TokenSequence(
    NameToken("b", 1),
    NameToken("h", 3),
    NameToken("w", 5),
    RParenToken(7),
    ArrowToken(9),
    NameToken("c", 12)
  )
  expect_error(parse_einops_ast(tokens2), "Unmatched closing parenthesis")
})

test_that("parse_einops_ast errors on multiple ellipses on one side", {
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
  expect_error(parse_einops_ast(tokens), "No '->' found")
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
