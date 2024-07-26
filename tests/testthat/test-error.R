# library(testthat)
# source("R/error.R")

test_that("error", {
    error_1 = methods::new("dbError", code=1, message="Error test 1")
    expect_equal(class(error_1)[1], "dbError")
    error_2 = chain(
        error_1,
        methods::new("dbError", code=2, message="Error test 2"))
    expect_equal(class(error_2)[1], "dbError")
    expect_equal(error_2@code, c(1, 2))
    expect_equal(error_2@message, c("Error test 1", "Error test 2"))
})
