# library(testthat)
# source("R/main.R")

test_that("fn_create_database_from_xlsx_or_tsv", {
    set.seed(123)
    list_fnames_tables  = fn_simulate_tables(
            n_entries=50,
            n_dates=3,
            n_sites=3,
            n_treatments=3,
            n_loci=10e3,
            save_data_tables=TRUE)$list_fnames_tables
    fname_xlsx = list_fnames_tables$fname_data_tables
    fname_phenotypes_tsv = list_fnames_tables$fname_phenotypes
    fname_environments_tsv = list_fnames_tables$fname_environments
    fname_genotypes_tsv = list_fnames_tables$fname_genotypes
    ok_1 = fn_create_database_from_xlsx_or_tsv(fname_db="test.sqlite", fname_xlsx=fname_xlsx, fname_genotypes_tsv=NULL, overwrite=FALSE, verbose=TRUE)
    ok_2 = fn_create_database_from_xlsx_or_tsv(fname_db="test.sqlite", fname_xlsx=fname_xlsx, overwrite=TRUE)
    ok_3 = fn_create_database_from_xlsx_or_tsv(fname_db="test.sqlite", fname_xlsx=NULL, fname_phenotypes_tsv=fname_phenotypes_tsv, fname_environments_tsv=fname_environments_tsv, fname_genotypes_tsv=fname_genotypes_tsv, overwrite=TRUE)
    err_1 = fn_create_database_from_xlsx_or_tsv(fname_db="test.sqlite", fname_xlsx=fname_xlsx, overwrite=FALSE)
    err_2 = fn_create_database_from_xlsx_or_tsv(fname_db="/some/dir/which/does/not/exist/test.sqlite", fname_xlsx=fname_xlsx)
    expect_equal(ok_1, 0)
    expect_equal(ok_2, 0)
    expect_equal(ok_3, 0)
    expect_equal(methods::is(err_1, "dbError"), TRUE)
    expect_equal(methods::is(err_2, "dbError"), TRUE)
    unlink("test.sqlite")
})

test_that("fn_update_database_from_xlsx_or_tsv", {
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
        # table_name = GLOBAL_df_valid_tables()$NAME[GLOBAL_df_valid_tables()$CLASS=="data"][1]
        df = as.data.frame(readxl::read_excel(
            path=list_fnames_tables$fname_data_tables, 
            sheet=table_name, 
            guess_max=round(0.01*.Machine$integer.max)), 
            check.names=FALSE)
        n = nrow(df)
        eval(parse(text=paste0("list_df_initial$", table_name, " = df[1:ceiling(n/2), ]")))
        eval(parse(text=paste0("list_df_update$", table_name, " = df[(ceiling(n/2)+1):n, ]")))
        eval(parse(text=paste0("fname_", table_name, "_tsv = 'test_update_", table_name, ".tsv'")))
        eval(parse(text=paste0("write.table(df[(ceiling(n/2)+1):n, ], file=fname_", table_name, "_tsv, sep='\t', row.names=FALSE, col.names=TRUE, quote=FALSE)")))
    }
    fname_xlsx_initial = "test_initial.xlsx"
    fname_xlsx_update = "test_update.xlsx"
    writexl::write_xlsx(x=list_df_initial, path=fname_xlsx_initial)
    writexl::write_xlsx(x=list_df_update, path=fname_xlsx_update)
    ### Initialise the database
    fn_create_database_from_xlsx_or_tsv(fname_xlsx=fname_xlsx_initial)
    ### Update
    fname_xlsx = fname_xlsx_update
    fname_db = gsub(".xlsx$", ".sqlite", fname_xlsx_initial)
    ok_1 = fn_update_database_from_xlsx_or_tsv(fname_db=fname_db, fname_xlsx=fname_xlsx)
    ok_2 = fn_update_database_from_xlsx_or_tsv(fname_db=fname_db, fname_xlsx=NULL, fname_phenotypes_tsv=fname_phenotypes_tsv, fname_environments_tsv=fname_environments_tsv, fname_genotypes_tsv=fname_genotypes_tsv)
    err = fn_update_database_from_xlsx_or_tsv(fname_db=fname_db, fname_xlsx="non-existent-file.xlsx")
    expect_equal(ok_1, 0)
    expect_equal(ok_2, 0)
    expect_equal(methods::is(err, "dbError"), TRUE)
    unlink(fname_xlsx_initial)
    unlink(fname_xlsx_update)
    unlink(fname_phenotypes_tsv)
    unlink(fname_environments_tsv)
    unlink(fname_genotypes_tsv)
    unlink(fname_db)
})


