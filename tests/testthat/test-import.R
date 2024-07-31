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
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
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

test_that("fn_add_POSIX_time", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
         n_entries=50,
         n_dates=3,
         n_sites=3,
         n_treatments=3,
         n_loci=10e3,
         save_data_tables=TRUE)$list_fnames_tables
    df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    vec_POSIX_DATE_TIME = df$POSIX_DATE_TIME
    df = df[, !(colnames(df) %in% "POSIX_DATE_TIME")]
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    df = fn_add_POSIX_time(df=df, database=database, table_name="phenotypes", bool_add_FVI_year_season=TRUE, verbose=TRUE)
    expect_equal(sum(df$POSIX_DATE_TIME == vec_POSIX_DATE_TIME), nrow(df))
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
})

test_that("fn_rename_columns_and_remove_duplicate_columns", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
         n_entries=50,
         n_dates=3,
         n_sites=3,
         n_treatments=3,
         n_loci=10e3,
         save_data_tables=TRUE)$list_fnames_tables
    df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    colnames(df) = tolower(colnames(df))
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    df = fn_rename_columns_and_remove_duplicate_columns(df=df, database=database, table_name="phenotypes", verbose=TRUE)
    expect_equal(sum(colnames(df) == toupper(colnames(df))), ncol(df))
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
})

test_that("fn_define_hash_and_UID_prefix", {
    prefix_1 = fn_define_hash_and_UID_prefix(table_name="phenotypes")
    prefix_2 = fn_define_hash_and_UID_prefix(table_name="entries")
    error = fn_define_hash_and_UID_prefix(table_name="NOT_A_BASE_OR_DATA_TABLE_NAME")
    expect_equal(prefix_1, "PHENOTYPE")
    expect_equal(prefix_2, "ENTRY")
    expect_equal(class(error)[1], "dbError")
})

test_that("fn_add_hash_UID_and_remove_duplicate_rows", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
         n_entries=50,
         n_dates=3,
         n_sites=3,
         n_treatments=3,
         n_loci=10e3,
         save_data_tables=TRUE)$list_fnames_tables
    df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    df = rbind(df[1, ], df)
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    df_with_hash_UID = fn_add_hash_UID_and_remove_duplicate_rows(df=df, database=database, table_name="phenotypes", verbose=TRUE)
    expect_equal(ncol(df_with_hash_UID), ncol(df)+2)
    expect_equal(nrow(df_with_hash_UID), nrow(df)-1)
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
})

test_that("fn_convert_allele_frequency_table_into_blobs_and_dfs", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
         n_entries=50,
         n_dates=3,
         n_sites=3,
         n_treatments=3,
         n_loci=10e3,
         save_data_tables=TRUE)$list_fnames_tables
    df = utils::read.delim(list_fnames_tables$fname_genotypes, header=TRUE)
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    list_df_genotypes_df_loci_df_entries = fn_convert_allele_frequency_table_into_blobs_and_dfs(df=df, database=database, table_name="genotypes", verbose=TRUE)
    expect_equal(length(list_df_genotypes_df_loci_df_entries), 3)
    for (i in sample(4:ncol(df), size=10)) {
        expect_equal(length(list_df_genotypes_df_loci_df_entries$df_genotypes$BLOB[[i-3]]), length(serialize(object=df[, i], connection=NULL)))
    }
    expect_equal(nrow(list_df_genotypes_df_loci_df_entries$df_loci), nrow(df))
    expect_equal(nrow(list_df_genotypes_df_loci_df_entries$df_entries), ncol(df)-3)
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
})

test_that("", {
    set.seed(123)

})

test_that("", {
    set.seed(123)

})

test_that("", {
    set.seed(123)

})

test_that("", {
    set.seed(123)

})

test_that("", {
    set.seed(123)

})

test_that("", {
    set.seed(123)

})

test_that("", {
    set.seed(123)

})
