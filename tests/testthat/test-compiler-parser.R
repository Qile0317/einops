test_that("a b c -> b c", {

    tokens <- EinopsTokenSequence(
        NameToken("a", 1),
        NameToken("b", 3),
        NameToken("c", 5),
        ArrowToken(7),
        NameToken("b", 10),
        NameToken("c", 12)
    )

    ast <- EinopsAst(
        input_axes = OneSidedAstNode(
            NamedAxisAstNode(
                name = "a",
                src = list(start = 1)
            ),
            NamedAxisAstNode(
                name = "b",
                src = list(start = 3)
            ),
            NamedAxisAstNode(
                name = "c",
                src = list(start = 5)
            )
        ),
        output_axes = OneSidedAstNode(
            NamedAxisAstNode(
                name = "b",
                src = list(start = 10)
            ),
            NamedAxisAstNode(
                name = "c",
                src = list(start = 12)
            )
        ),
        src = list(start = 1)
    )

    expect_identical(parse_einops_ast(tokens), ast)
    expect_identical(to_tokens(ast), tokens)

})

test_that("b c (h1 2) (w1 2) -> b c h1 w1", {

    tokens <- EinopsTokenSequence(
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

    ast <- EinopsAst(
        input_axes = OneSidedAstNode(
            NamedAxisAstNode(
                name = "b",
                src = list(start = 1)
            ),
            NamedAxisAstNode(
                name = "c",
                src = list(start = 3)
            ),
            GroupAstNode(
                children = list(
                    NamedAxisAstNode(
                        name = "h1",
                        src = list(start = 6)
                    ),
                    ConstantAstNode(
                        count = 2L,
                        src = list(start = 9)
                    )
                ),
                src = list(start = 5)
            ),
            GroupAstNode(
                children = list(
                    NamedAxisAstNode(
                        name = "w1",
                        src = list(start = 13)
                    ),
                    ConstantAstNode(
                        count = 2L,
                        src = list(start = 16)
                    )
                ),
                src = list(start = 12)
            )
        ),
        output_axes = OneSidedAstNode(
            NamedAxisAstNode(
                name = "b",
                src = list(start = 22)
            ),
            NamedAxisAstNode(
                name = "c",
                src = list(start = 24)
            ),
            NamedAxisAstNode(
                name = "h1",
                src = list(start = 26)
            ),
            NamedAxisAstNode(
                name = "w1",
                src = list(start = 29)
            )
        ),
        src = list(start = 1)
    )

    expect_identical(parse_einops_ast(tokens), ast)
    expect_identical(to_tokens(ast), tokens)

})

test_that("b c h w -> b c () ()", {

    tokens <- EinopsTokenSequence(
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

    ast <- EinopsAst(
        input_axes = OneSidedAstNode(
            NamedAxisAstNode(
                name = "b",
                src = list(start = 1)
            ),
            NamedAxisAstNode(
                name = "c",
                src = list(start = 3)
            ),
            NamedAxisAstNode(
                name = "h",
                src = list(start = 5)
            ),
            NamedAxisAstNode(
                name = "w",
                src = list(start = 7)
            )
        ),
        output_axes = OneSidedAstNode(
            NamedAxisAstNode(
                name = "b",
                src = list(start = 12)
            ),
            NamedAxisAstNode(
                name = "c",
                src = list(start = 14)
            ),
            GroupAstNode(
                children = list(),
                src = list(start = 16)
            ),
            GroupAstNode(
                children = list(),
                src = list(start = 19)
            )
        ),
        src = list(start = 1)
    )

    expect_identical(parse_einops_ast(tokens), ast)
    expect_identical(to_tokens(ast), tokens)

})

test_that("... h w c -> ... (h w) c", {

    tokens <- EinopsTokenSequence(
        EllipsisToken(1),
        NameToken("h", 5),
        NameToken("w", 7),
        NameToken("c", 9),
        ArrowToken(11),
        EllipsisToken(14),
        LParenToken(18),
        NameToken("h", 19),
        NameToken("w", 21),
        RParenToken(22),
        NameToken("c", 24)
    )

    ast <- EinopsAst(
        input_axes = OneSidedAstNode(
            EllipsisAstNode(
                src = list(start = 1)
            ),
            NamedAxisAstNode(
                name = "h",
                src = list(start = 5)
            ),
            NamedAxisAstNode(
                name = "w",
                src = list(start = 7)
            ),
            NamedAxisAstNode(
                name = "c",
                src = list(start = 9)
            )
        ),
        output_axes = OneSidedAstNode(
            EllipsisAstNode(
                src = list(start = 14)
            ),
            GroupAstNode(
                children = list(
                    NamedAxisAstNode(
                        name = "h",
                        src = list(start = 19)
                    ),
                    NamedAxisAstNode(
                        name = "w",
                        src = list(start = 21)
                    )
                ),
                src = list(start = 18)
            ),
            NamedAxisAstNode(
                name = "c",
                src = list(start = 24)
            )
        ),
        src = list(start = 1)
    )

    expect_identical(parse_einops_ast(tokens), ast)
    expect_identical(to_tokens(ast), tokens)

})

test_that("b h w c -> (b h w c)", {
    
    tokens <- EinopsTokenSequence(
        NameToken("b", 1),
        NameToken("h", 3),
        NameToken("w", 5),
        NameToken("c", 7),
        ArrowToken(9),
        LParenToken(12),
        NameToken("b", 13),
        NameToken("h", 15),
        NameToken("w", 17),
        NameToken("c", 19),
        RParenToken(20)
    )

    ast <- EinopsAst(
        input_axes = OneSidedAstNode(
            NamedAxisAstNode(
                name = "b",
                src = list(start = 1)
            ),
            NamedAxisAstNode(
                name = "h",
                src = list(start = 3)
            ),
            NamedAxisAstNode(
                name = "w",
                src = list(start = 5)
            ),
            NamedAxisAstNode(
                name = "c",
                src = list(start = 7)
            )
        ),
        output_axes = OneSidedAstNode(
            GroupAstNode(
                children = list(
                    NamedAxisAstNode(
                        name = "b",
                        src = list(start = 13)
                    ),
                    NamedAxisAstNode(
                        name = "h",
                        src = list(start = 15)
                    ),
                    NamedAxisAstNode(
                        name = "w",
                        src = list(start = 17)
                    ),
                    NamedAxisAstNode(
                        name = "c",
                        src = list(start = 19)
                    )
                ),
                src = list(start = 12)
            )
        ),
        src = list(start = 1)
    )

    expect_identical(parse_einops_ast(tokens), ast)
    expect_identical(to_tokens(ast), tokens)

})

test_that("a b c -> b 1 ()", {
    tokens <- EinopsTokenSequence(
        NameToken("a", 1),
        NameToken("b", 3),
        NameToken("c", 5),
        ArrowToken(7),
        NameToken("b", 10),
        IntToken("1", 12),
        LParenToken(14),
        RParenToken(15)
    )

    ast <- EinopsAst(
        input_axes = OneSidedAstNode(
            NamedAxisAstNode(
                name = "a",
                src = list(start = 1)
            ),
            NamedAxisAstNode(
                name = "b",
                src = list(start = 3)
            ),
            NamedAxisAstNode(
                name = "c",
                src = list(start = 5)
            )
        ),
        output_axes = OneSidedAstNode(
            NamedAxisAstNode(
                name = "b",
                src = list(start = 10)
            ),
            ConstantAstNode(
                count = 1L,
                src = list(start = 12)
            ),
            GroupAstNode(
                children = list(),
                src = list(start = 14)
            )
        ),
        src = list(start = 1)
    )

    expect_identical(parse_einops_ast(tokens), ast)
    expect_identical(to_tokens(ast), tokens)
})

test_that("a b c d -> (a c) b ...", {

    tokens <- EinopsTokenSequence(
        NameToken("a", 1),
        NameToken("b", 3),
        NameToken("c", 5),
        NameToken("d", 7),
        ArrowToken(9),
        LParenToken(12),
        NameToken("a", 13),
        NameToken("c", 15),
        RParenToken(16),
        NameToken("b", 18),
        EllipsisToken(21)
    )
    
    ast <- EinopsAst(
        input_axes = OneSidedAstNode(
            NamedAxisAstNode(
                name = "a",
                src = list(start = 1)
            ),
            NamedAxisAstNode(
                name = "b",
                src = list(start = 3)
            ),
            NamedAxisAstNode(
                name = "c",
                src = list(start = 5)
            ),
            NamedAxisAstNode(
                name = "d",
                src = list(start = 7)
            )
        ),
        output_axes = OneSidedAstNode(
            GroupAstNode(
                children = list(
                    NamedAxisAstNode(
                        name = "a",
                        src = list(start = 13)
                    ),
                    NamedAxisAstNode(
                        name = "c",
                        src = list(start = 15)
                    )
                ),
                src = list(start = 12)
            ),
            NamedAxisAstNode(
                name = "b",
                src = list(start = 18)
            ),
            EllipsisAstNode(
                src = list(start = 21)
            )
        ),
        src = list(start = 1)
    )

    expect_identical(parse_einops_ast(tokens), ast)
    expect_identical(to_tokens(ast), tokens)

})

test_that("... c -> (c ...)", {
    
    tokens <- EinopsTokenSequence(
        EllipsisToken(1),
        NameToken("c", 5),
        ArrowToken(7),
        LParenToken(10),
        NameToken("c", 11),
        EllipsisToken(13),
        RParenToken(16)
    )

    ast <- EinopsAst(
        input_axes = OneSidedAstNode(
            EllipsisAstNode(
                src = list(start = 1)
            ),
            NamedAxisAstNode(
                name = "c",
                src = list(start = 5)
            )
        ),
        output_axes = OneSidedAstNode(
            GroupAstNode(
                children = list(
                    NamedAxisAstNode(
                        name = "c",
                        src = list(start = 11)
                    ),
                    EllipsisAstNode(
                        src = list(start = 13)
                    )
                ),
                src =  list(start = 10)
            )
        ),
        src = list(start = 1)
    )

    expect_identical(parse_einops_ast(tokens), ast)
    expect_identical(to_tokens(ast), tokens)
})

test_that("parse_onesided_ast handles simple axis names", {
    tokens <- EinopsTokenSequence(
        NameToken("a", 1),
        NameToken("b", 3),
        NameToken("c", 5)
    )
    ast <- OneSidedAstNode(
        NamedAxisAstNode(name = "a", src = list(start = 1)),
        NamedAxisAstNode(name = "b", src = list(start = 3)),
        NamedAxisAstNode(name = "c", src = list(start = 5))
    )
    expect_identical(parse_onesided_ast(tokens), ast)
    expect_identical(to_tokens(ast), tokens)

    tokens <- EinopsTokenSequence(
        NameToken("a", 1),
        NameToken("b", 3),
        UnderscoreToken(5),
        NameToken("c", 7),
        NameToken("d", 9),
        EllipsisToken(11)
    )
    ast <- OneSidedAstNode(
        NamedAxisAstNode(name = "a", src = list(start = 1)),
        NamedAxisAstNode(name = "b", src = list(start = 3)),
        UnderscoreAstNode(src = list(start = 5)),
        NamedAxisAstNode(name = "c", src = list(start = 7)),
        NamedAxisAstNode(name = "d", src = list(start = 9)),
        EllipsisAstNode(src = list(start = 11))
    )
    expect_identical(parse_onesided_ast(tokens), ast)
    expect_identical(to_tokens(ast), tokens)

    tokens <- EinopsTokenSequence(
        LParenToken(1),
        RParenToken(2),
        NameToken("a", 4)
    )
    ast <- OneSidedAstNode(
        GroupAstNode(
            children = list(),
            src = list(start = 1)
        ),
        NamedAxisAstNode(
            name = "a",
            src = list(start = 4)
        )
    )
    expect_identical(parse_onesided_ast(tokens), ast)
    expect_identical(to_tokens(ast), tokens)

    tokens <- EinopsTokenSequence(
        IntToken("3", 1),
        NameToken("c", 3),
        EllipsisToken(5)
    )
    ast <- OneSidedAstNode(
        ConstantAstNode(
            count = 3,
            src = list(start = 1)
        ),
        NamedAxisAstNode(
            name = "c",
            src = list(start = 3)
        ),
        EllipsisAstNode(
            src = list(start = 5)
        )
    )
    expect_identical(parse_onesided_ast(tokens), ast)
    expect_identical(to_tokens(ast), tokens)

    tokens <- EinopsTokenSequence(
        NameToken("a", 1),
        NameToken("b", 3),
        NameToken("c", 5),
        ArrowToken(7)
    )
    ast <- EinopsAst(
        input_axes = OneSidedAstNode(
            NamedAxisAstNode(
                name = "a",
                src = list(start = 1)
            ),
            NamedAxisAstNode(
                name = "b",
                src = list(start = 3)
            ),
            NamedAxisAstNode(
                name = "c",
                src = list(start = 5)
            )
        ),
        output_axes = OneSidedAstNode(
            NothingAstNode()
        ),
        src = list(start = 1)
    )
    expect_identical(parse_einops_ast(tokens), ast)
    expect_identical(to_tokens(ast), tokens)

})
