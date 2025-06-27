test_that("a b c -> b c", {

    tokens <- TokenSequence(
        NameToken("a", 1),
        NameToken("b", 3),
        NameToken("c", 5),
        ArrowToken(7),
        NameToken("b", 10),
        NameToken("c", 12)
    )

    ast <- EinopsAst(
        input_axes = list(
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
        output_axes = list(
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
