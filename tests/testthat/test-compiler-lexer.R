test_that("lex produces exact ordered token objects with positions", {
    expectedTokens <- TokenSequence(
        NameToken("b", 1),
        NameToken("h", 3),
        NameToken("w", 5),
        ArrowToken(7),
        NameToken("b", 10),
        LParenToken(12),
        NameToken("h", 13),
        NameToken("w", 15),
        RParenToken(16)
    )
    tokens <- lex("b h w -> b (h w)")
    expect_equal(tokens, expectedTokens)
})

test_that("lex handles whitespace insensitivity", {
    expectedTokens <- TokenSequence(
        NameToken("b", 1),
        NameToken("h", 3),
        NameToken("w", 5)
    )
    
    tokens1 <- lex("b h w")
    tokens2 <- lex("b  h   w")
    tokens3 <- lex(" b h w ")

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

test_that("lex recognizes ellipsis correctly", {
    expectedTokens <- TokenSequence(
        EllipsisToken(1),
        NameToken("h", 5),
        NameToken("w", 7)
    )
    tokens <- lex("... h w")
    expect_equal(tokens, expectedTokens)
})

test_that("lex handles numeric literals in parentheses", {
    expectedTokens <- TokenSequence(
        LParenToken(1),
        IntToken("3", 2),
        RParenToken(3)
    )
    tokens <- lex("(3)")
    expect_equal(tokens, expectedTokens)
})

test_that("lex handles complex numeric expressions", {
    expectedTokens <- TokenSequence(
        LParenToken(1),
        NameToken("h", 2),
        IntToken("2", 4),
        RParenToken(5)
    )
    tokens <- lex("(h 2)")
    expect_equal(tokens, expectedTokens)
})

test_that("lex provides accurate 1-based column positions", {
    expectedTokens <- TokenSequence(
        NameToken("ab", 1),
        NameToken("cd", 4)
    )
    tokens <- lex("ab cd")
    expect_equal(tokens, expectedTokens)
})

test_that("lexer handles complex patterns", {
    pattern1 <- "b c (h1 2) (w1 2) -> b c h1 w1"
    expectedTokens1 <- TokenSequence(
        NameToken("b", 1),
        NameToken("c", 3),
        LParenToken(5),
        NameToken("h1", 6),
        IntToken("2", 9),
        RParenToken(10),
        LParenToken(12),
        NameToken("w1", 13),
        IntToken("2", 16),
        RParenToken(17),
        ArrowToken(19),
        NameToken("b", 22),
        NameToken("c", 24),
        NameToken("h1", 26),
        NameToken("w1", 29)
    )
    tokens1 <- lex(pattern1)
    expect_equal(tokens1, expectedTokens1)

    pattern2 <- "b c (h1 h2) (w1 w2) -> b c h1 w1"
    expectedTokens2 <- TokenSequence(
        NameToken("b", 1),
        NameToken("c", 3),
        LParenToken(5),
        NameToken("h1", 6),
        NameToken("h2", 9),
        RParenToken(11),
        LParenToken(13),
        NameToken("w1", 14),
        NameToken("w2", 17),
        RParenToken(19),
        ArrowToken(21),
        NameToken("b", 24),
        NameToken("c", 26),
        NameToken("h1", 28),
        NameToken("w1", 31)
    )
    tokens2 <- lex(pattern2)
    expect_equal(tokens2, expectedTokens2)

    pattern3 <- "b c h w -> 1 c 1 1"
    expectedTokens3 <- TokenSequence(
        NameToken("b", 1),
        NameToken("c", 3),
        NameToken("h", 5),
        NameToken("w", 7),
        ArrowToken(9),
        IntToken("1", 12),
        NameToken("c", 14),
        IntToken("1", 16),
        IntToken("1", 18)
    )
    tokens3 <- lex(pattern3)
    expect_equal(tokens3, expectedTokens3)

    pattern4 <- "b c h w -> b c () ()"
    expectedTokens4 <- TokenSequence(
        NameToken("b", 1),
        NameToken("c", 3),
        NameToken("h", 5),
        NameToken("w", 7),
        ArrowToken(9),
        NameToken("b", 12),
        NameToken("c", 14),
        LParenToken(16),
        RParenToken(17),
        LParenToken(19),
        RParenToken(20)
    )
    tokens4 <- lex(pattern4)
    expect_equal(tokens4, expectedTokens4)
})
