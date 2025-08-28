library(testthat)
library(einops)

if (Sys.getenv("TORCH_TEST", unset = "0") == "1") {
    testthat::test_check("einops")
}
