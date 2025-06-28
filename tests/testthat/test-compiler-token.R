test_that("the EinopsTokenSequence constructor works", {

    expected_object <- structure(
        list(
            structure(
                list(type = "NAME", value = "a", start = 1),
                class = c("ParameterizedEinopsToken", "EinopsToken")
            ),
            structure(
                list(type = "NAME", value = "b", start = 3),
                class = c("ParameterizedEinopsToken", "EinopsToken")
            ),
            structure(
                list(type = "NAME", value = "c", start = 5),
                class = c("ParameterizedEinopsToken", "EinopsToken")
            ),
            structure(
                list(type = "ARROW", value = "->", start = 7),
                class = c("SimpleEinopsToken", "EinopsToken")
            ),
            structure(
                list(type = "NAME", value = "b", start = 10),
                class = c("ParameterizedEinopsToken", "EinopsToken")
            ),
            structure(
                list(type = "NAME", value = "c", start = 12),
                class = c("ParameterizedEinopsToken", "EinopsToken")
            )
        ),
        class = c("EinopsTokenSequence", "list")
    )

    # test a flat sequence
    expect_identical(
        EinopsTokenSequence(
            NameToken("a", 1),
            NameToken("b", 3),
            NameToken("c", 5),
            ArrowToken(7),
            NameToken("b", 10),
            NameToken("c", 12)
        ),
        expected_object
    )

    # tokens with token sequences

    expect_identical(
        EinopsTokenSequence(
            NameToken("a", 1),
            NameToken("b", 3),
            NameToken("c", 5),
            ArrowToken(7),
            EinopsTokenSequence(NameToken("b", 10), NameToken("c", 12))
        ),
        expected_object
    )

    expect_identical(
        EinopsTokenSequence(
            NameToken("a", 1),
            EinopsTokenSequence(NameToken("b", 3)),
            NameToken("c", 5),
            ArrowToken(7),
            EinopsTokenSequence(
                NameToken("b", 10),
                NameToken("c", 12)
            )
        ),
        expected_object
    )

    expect_identical(
        EinopsTokenSequence(EinopsTokenSequence(
            NameToken("a", 1),
            NameToken("b", 3),
            NameToken("c", 5),
            ArrowToken(7),
            NameToken("b", 10),
            NameToken("c", 12)
        )),
        expected_object
    )

    expect_identical(
        EinopsTokenSequence(
            NameToken("a", 1),
            NameToken("b", 3),
            EinopsTokenSequence(
                NameToken("c", 5),
                EinopsTokenSequence(ArrowToken(7))
            ),
            EinopsTokenSequence(
                NameToken("b", 10),
                NameToken("c", 12)
            )
        ),
        expected_object
    )

})
