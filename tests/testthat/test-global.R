# library(testthat)
# source("R/global.R")

test_that("GLOBAL_df_valid_tables", {
    x = GLOBAL_df_valid_tables()
    expect_equal(class(x), "data.frame")
    expect_equal(x$NAME, c(
        "entries",
        "dates",
        "sites",
        "treatments",
        "traits",
        "abiotics",
        "loci",
        "phenotypes",
        "environments",
        "genotypes"
    ))
})

test_that("GLOBAL_list_required_colnames_per_table", {
    x = GLOBAL_list_required_colnames_per_table()
    expect_equal(class(x), "list")
    expect_equal(names(x), c(
        "entries",
        "dates",
        "sites",
        "treatments",
        "traits",
        "abiotics",
        "loci",
        "phenotypes",
        "environments",
        "additional_IDs"
    ))
})
