test_that("tokenize produces exact ordered token objects with positions", {
    expectedTokens <- TokenSequence(
        NameToken("b", 1, 1),
        NameToken("h", 2, 2),
        NameToken("w", 3, 3),
        ArrowToken(4, 5),
        NameToken("b", 6, 6),
        LParenToken(7, 7),
        NameToken("h", 8, 8),
        NameToken("w", 9, 9),
        RParenToken(10, 10)
    )
    tokens <- tokenize("b h w -> b (h w)")
    expect_equal(tokens, expectedTokens)
})

test_that("tokenize handles whitespace insensitivity", {
    expectedTokens <- TokenSequence(
        NameToken("b", 1, 1),
        NameToken("h", 3, 3),
        NameToken("w", 5, 5)
    )
    
    tokens1 <- tokenize("b h w")
    tokens2 <- tokenize("b  h   w")
    tokens3 <- tokenize(" b h w ")

    # Extract just type and value for comparison
    extract_tokens <- function(tokens) {
        lapply(tokens, function(t) list(type = t$type, value = t$value))
    }
    extract_expected <- function(tokens) {
        lapply(tokens, function(t) list(type = t$type, value = t$value))
    }

    expect_equal(extract_tokens(tokens1), extract_expected(expectedTokens))
    expect_equal(extract_tokens(tokens2), extract_expected(expectedTokens))
    expect_equal(extract_tokens(tokens3), extract_expected(expectedTokens))
})

test_that("tokenize recognizes ellipsis correctly", {
    expectedTokens <- TokenSequence(
        EllipsisToken(1, 3),
        NameToken("h", 5, 5),
        NameToken("w", 7, 7)
    )
    tokens <- tokenize("... h w")
    expect_equal(tokens, expectedTokens)
})

test_that("tokenize fails when two ellipses appear", {
    expect_error(tokenize("... h ... w"), "Only one ellipsis")
})

test_that("tokenize handles numeric literals in parentheses", {
    expectedTokens <- TokenSequence(
        LParenToken(1, 1),
        IntToken("3", 2, 2),
        RParenToken(3, 3)
    )
    tokens <- tokenize("(3)")
    expect_equal(tokens, expectedTokens)
})

test_that("tokenize handles complex numeric expressions", {
    expectedTokens <- TokenSequence(
        LParenToken(1, 1),
        NameToken("h", 2, 2),
        IntToken("2", 4, 4),
        RParenToken(5, 5)
    )
    tokens <- tokenize("(h 2)")
    expect_equal(tokens, expectedTokens)
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
    expectedTokens <- TokenSequence(
        NameToken("ab", 1, 2),
        NameToken("cd", 4, 5)
    )
    tokens <- tokenize("ab cd")
    expect_equal(tokens, expectedTokens)
})

test_that("tokenizer handles complex patterns", {
    pattern1 <- "b c (h1 2) (w1 2) -> b c h1 w1"
    expectedTokens1 <- TokenSequence(
        NameToken("b", 1, 1),
        NameToken("c", 2, 2),
        LParenToken(3, 3),
        NameToken("h1", 4, 5),
        IntToken("2", 6, 6),
        RParenToken(7, 7),
        LParenToken(8, 8),
        NameToken("w1", 9, 10),
        IntToken("2", 11, 11),
        RParenToken(12, 12),
        ArrowToken(13, 14),
        NameToken("b", 15, 15),
        NameToken("c", 16, 16),
        NameToken("h1", 17, 18),
        NameToken("w1", 19, 20)
    )
    tokens1 <- tokenize(pattern1)
    expect_equal(tokens1, expectedTokens1)

    pattern2 <- "b c (h1 h2) (w1 w2) -> b c h1 w1"
    expectedTokens2 <- TokenSequence(
        NameToken("b", 1, 1),
        NameToken("c", 2, 2),
        LParenToken(3, 3),
        NameToken("h1", 4, 5),
        NameToken("h2", 6, 7),
        RParenToken(8, 8),
        LParenToken(9, 9),
        NameToken("w1", 10, 11),
        NameToken("w2", 12, 13),
        RParenToken(14, 14),
        ArrowToken(15, 16),
        NameToken("b", 17, 17),
        NameToken("c", 18, 18),
        NameToken("h1", 19, 20),
        NameToken("w1", 21, 22)
    )
    tokens2 <- tokenize(pattern2)
    expect_equal(tokens2, expectedTokens2)

    # Test case 3: "b c h w -> 1 c 1 1"
    pattern3 <- "b c h w -> 1 c 1 1"
    expectedTokens3 <- TokenSequence(
        NameToken("b", 1, 1),
        NameToken("c", 3, 3),
        NameToken("h", 5, 5),
        NameToken("w", 7, 7),
        ArrowToken(9, 10),
        IntToken("1", 12, 12),
        NameToken("c", 14, 14),
        IntToken("1", 16, 16),
        IntToken("1", 18, 18)
    )
    tokens3 <- tokenize(pattern3)
    expect_equal(tokens3, expectedTokens3)

    # Test case 4: "b c h w -> b c () ()"
    pattern4 <- "b c h w -> b c () ()"
    expectedTokens4 <- TokenSequence(
        NameToken("b", 1, 1),
        NameToken("c", 3, 3),
        NameToken("h", 5, 5),
        NameToken("w", 7, 7),
        ArrowToken(9, 10),
        NameToken("b", 12, 12),
        NameToken("c", 14, 14),
        LParenToken(16, 16),
        RParenToken(17, 17),
        LParenToken(19, 19),
        RParenToken(20, 20)
    )
    tokens4 <- tokenize(pattern4)
    expect_equal(tokens4, expectedTokens4)
})
