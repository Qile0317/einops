test_that("tokenize produces exact ordered token objects with positions", {
  # Basic pattern tokenization
  tokens <- tokenize("b h w -> b (h w)")
  
  expect_equal(length(tokens), 9)
  expect_equal(tokens[[1]]$type, "NAME")
  expect_equal(tokens[[1]]$value, "b")
  expect_equal(tokens[[1]]$start, 1)
  expect_equal(tokens[[1]]$end, 1)
  
  expect_equal(tokens[[2]]$type, "NAME")
  expect_equal(tokens[[2]]$value, "h")
  
  expect_equal(tokens[[3]]$type, "NAME")
  expect_equal(tokens[[3]]$value, "w")
  
  expect_equal(tokens[[4]]$type, "ARROW")
  expect_equal(tokens[[4]]$value, "->")
  
  expect_equal(tokens[[5]]$type, "NAME")
  expect_equal(tokens[[5]]$value, "b")
  
  expect_equal(tokens[[6]]$type, "LPAREN")
  expect_equal(tokens[[6]]$value, "(")
  
  expect_equal(tokens[[7]]$type, "NAME")
  expect_equal(tokens[[7]]$value, "h")
  
  expect_equal(tokens[[8]]$type, "NAME")
  expect_equal(tokens[[8]]$value, "w")
  
  expect_equal(tokens[[9]]$type, "RPAREN")
  expect_equal(tokens[[9]]$value, ")")
})

test_that("tokenize handles whitespace insensitivity", {
  tokens1 <- tokenize("b h w")
  tokens2 <- tokenize("b  h   w")
  tokens3 <- tokenize(" b h w ")
  
  # Extract just type and value for comparison
  extract_tokens <- function(tokens) {
    lapply(tokens, function(t) list(type = t$type, value = t$value))
  }
  
  expect_equal(extract_tokens(tokens1), extract_tokens(tokens2))
  expect_equal(extract_tokens(tokens1), extract_tokens(tokens3))
})

test_that("tokenize recognizes ellipsis correctly", {
  tokens <- tokenize("... h w")
  
  expect_equal(tokens[[1]]$type, "ELLIPSIS")
  expect_equal(tokens[[1]]$value, "...")
  expect_equal(tokens[[2]]$type, "NAME")
  expect_equal(tokens[[2]]$value, "h")
})

test_that("tokenize fails when two ellipses appear", {
  expect_error(tokenize("... h ... w"), "Only one ellipsis")
})

test_that("tokenize handles numeric literals in parentheses", {
  tokens <- tokenize("(3)")
  
  expect_equal(length(tokens), 3)
  expect_equal(tokens[[1]]$type, "LPAREN")
  expect_equal(tokens[[1]]$value, "(")
  expect_equal(tokens[[2]]$type, "INT")
  expect_equal(tokens[[2]]$value, "3")
  expect_equal(tokens[[3]]$type, "RPAREN")
  expect_equal(tokens[[3]]$value, ")")
})

test_that("tokenize handles complex numeric expressions", {
  tokens <- tokenize("(h 2)")
  
  expect_equal(tokens[[1]]$type, "LPAREN")
  expect_equal(tokens[[2]]$type, "NAME")
  expect_equal(tokens[[2]]$value, "h")
  expect_equal(tokens[[3]]$type, "INT")
  expect_equal(tokens[[3]]$value, "2")
  expect_equal(tokens[[4]]$type, "RPAREN")
})

test_that("tokenize reports illegal characters with position", {
  expect_error(tokenize("a @ b"), "Illegal character '@' at position 3")
})

test_that("tokenize detects unclosed parenthesis", {
  expect_error(tokenize("(h w"), "Unclosed parenthesis")
})

test_that("tokenize rejects unsupported operators", {
  expect_error(tokenize("a + b"), "Unsupported operator '\\+' at position 3")
})

test_that("tokenize provides accurate 1-based column positions", {
  tokens <- tokenize("ab cd")
  
  expect_equal(tokens[[1]]$start, 1)
  expect_equal(tokens[[1]]$end, 2)
  expect_equal(tokens[[2]]$start, 4)
  expect_equal(tokens[[2]]$end, 5)
})
