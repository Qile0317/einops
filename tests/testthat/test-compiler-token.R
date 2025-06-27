test_that("the constructor works", {
    expect_identical(
        EinopsTokenSequence(
            NameToken("a", 1),
            NameToken("b", 3),
            NameToken("c", 5),
            ArrowToken(7),
            NameToken("b", 10),
            NameToken("c", 12)
        ),
        structure(
            list(
                structure(list(type = "NAME", value = "a", start = 1), class = "EinopsToken"),
                structure(list(type = "NAME", value = "b", start = 3), class = "EinopsToken"),
                structure(list(type = "NAME", value = "c", start = 5), class = "EinopsToken"),
                structure(list(type = "ARROW", value = "->", start = 7), class = "EinopsToken"),
                structure(list(type = "NAME", value = "b", start = 10), class = "EinopsToken"),
                structure(list(type = "NAME", value = "c", start = 12), class = "EinopsToken")
            ),
            class = c("EinopsTokenSequence", "list")
        )
    )
})
