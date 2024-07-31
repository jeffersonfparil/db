# library(testthat)
# source("R/import.R")

test_that("fn_check_import_inputs", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
         n_entries=50,
         n_dates=3,
         n_sites=3,
         n_treatments=3,
         n_loci=10e3,
         save_data_tables=TRUE)$list_fnames_tables
    df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    null_error = fn_check_import_inputs(df=df, database=database, table_name="phenotypes", check_UIDs=FALSE)
    non_null_error = fn_check_import_inputs(df=df[, -1:-5], database=database, table_name="phenotypes", check_UIDs=FALSE)
    expect_equal(is.null(null_error), TRUE)
    expect_equal(methods::is(non_null_error, "dbError"), TRUE)
})

test_that("fn_remove_quotes_and_newline_characters_in_data", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
         n_entries=50,
         n_dates=3,
         n_sites=3,
         n_treatments=3,
         n_loci=10e3,
         save_data_tables=TRUE)$list_fnames_tables
    df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    df[1,1] = paste0("'", df[1,1], "'")
    df[2,2] = paste0('"', df[2,2], '"')
    df[3,3] = paste0(df[3,3], "\n")
    df[4,4] = paste0(df[4,4], "\r\n")
    df = fn_remove_quotes_and_newline_characters_in_data(df=df, verbose=TRUE)
    expect_equal(grepl("'", df[1,1]), FALSE)
    expect_equal(grepl('"', df[2,2]), FALSE)
    expect_equal(grepl("\n", df[3,3]), FALSE)
    expect_equal(grepl("\r\n", df[4,4]), FALSE)
})

