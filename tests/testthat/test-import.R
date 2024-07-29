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
    db = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    null_error = fn_check_import_inputs(df=df, db=db, table_name="phenotypes", check_UIDs=FALSE)
    non_null_error = fn_check_import_inputs(df=df[, -1:-5], db=db, table_name="phenotypes", check_UIDs=FALSE)
    expect_equal(is.null(null_error), TRUE)
    expect_equal(methods::is(non_null_error, "dbError"), TRUE)
})
