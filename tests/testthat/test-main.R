# library(testthat)
# source("R/main.R")

test_that("fn_create_database_from_xlsx", {
    set.seed(123)
    fname_xlsx = fn_simulate_tables(
            n_entries=50,
            n_dates=3,
            n_sites=3,
            n_treatments=3,
            n_loci=10e3,
            save_data_tables=TRUE)$list_fnames_tables$fname_data_tables
    ok_1 = fn_create_database_from_xlsx(fname_xlsx=fname_xlsx, fname_db="test.sqlite", fname_genotype_tsv=NULL, overwrite=FALSE, verbose=TRUE)
    ok_2 = fn_create_database_from_xlsx(fname_xlsx=fname_xlsx, fname_db="test.sqlite", overwrite=TRUE)
    err_1 = fn_create_database_from_xlsx(fname_xlsx=fname_xlsx, fname_db="test.sqlite", overwrite=FALSE)
    err_2 = fn_create_database_from_xlsx(fname_xlsx=fname_xlsx, fname_db="/some/dir/which/does/not/exist/test.sqlite")
    expect_equal(ok_1, 0)
    expect_equal(ok_2, 0)
    expect_equal(methods::is(err_1, "dbError"), TRUE)
    expect_equal(methods::is(err_2, "dbError"), TRUE)
    unlink("test.sqlite")
})

test_that("fn_update_database_from_xlsx", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
        n_entries=50,
        n_dates=3,
        n_sites=3,
        n_treatments=3,
        n_loci=10e3,
        save_data_tables=TRUE)$list_fnames_tables
    list_df_initial = list()
    list_df_update = list()
    for (table_name in GLOBAL_df_valid_tables()$NAME[GLOBAL_df_valid_tables()$CLASS=="data"]) {
        df = as.data.frame(readxl::read_excel(
            path=list_fnames_tables$fname_data_tables, 
            sheet=table_name, 
            guess_max=round(0.01*.Machine$integer.max)), 
            check.names=FALSE)
        n = nrow(df)
        eval(parse(text=paste0("list_df_initial$", table_name, " = df[1:ceiling(n/2), ]")))
        eval(parse(text=paste0("list_df_update$", table_name, " = df[(ceiling(n/2)+1):n, ]")))
    }
    fname_xlsx_initial = "test_initial.xlsx"
    fname_xlsx_update = "test_update.xlsx"
    writexl::write_xlsx(x=list_df_initial, path=fname_xlsx_initial)
    writexl::write_xlsx(x=list_df_update, path=fname_xlsx_update)
    ### Initialise the database
    fn_create_database_from_xlsx(fname_xlsx=fname_xlsx_initial)
    ### Update
    fname_xlsx = fname_xlsx_update
    fname_db = gsub(".xlsx$", ".sqlite", fname_xlsx_initial)
    ok = fn_update_database_from_xlsx(fname_xlsx=fname_xlsx, fname_db=fname_db)
    err = fn_update_database_from_xlsx(fname_xlsx="non-existent-file.xlsx", fname_db=fname_db)
    expect_equal(ok, 0)
    expect_equal(methods::is(err, "dbError"), TRUE)
    unlink(fname_xlsx_initial)
    unlink(fname_xlsx_update)
    unlink(fname_db)
})

test_that("fn_create_database_from_tsv", {
    set.seed(123)
})

test_that("fn_update_database_from_tsv", {
    set.seed(123)
})


