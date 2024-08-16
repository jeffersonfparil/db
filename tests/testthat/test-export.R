# library(testthat)
# source("R/export.R")

test_that("fn_check_export_inputs", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
         n_entries=50,
         n_dates=3,
         n_sites=3,
         n_treatments=3,
         n_loci=10e3,
         save_data_tables=TRUE)$list_fnames_tables
    df_allele_frequency_table = utils::read.delim(list_fnames_tables$fname_genotypes, header=TRUE, check.names=FALSE)
    list_df_data_tables = list(
        df_phenotypes=utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE),
        df_environments=utils::read.delim(list_fnames_tables$fname_environments, header=TRUE),
        df_genotypes=df_allele_frequency_table)
    fn_initialise_db(fname_db="test.sqlite", list_df_data_tables=list_df_data_tables, verbose=TRUE)
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    list_filters=list(
        REPLICATION="rep_1",
        TREATMENT=sample(unique(list_df_data_tables$df_phenotypes$TREATMENT), size=min(c(2, length(unique(list_df_data_tables$df_phenotypes$TREATMENT)))))
    )
    null_error = fn_check_export_inputs(database=database, table_name="phenotypes", list_filters=list_filters, vec_columns_to_show="*", unique_column_name="ENTRY")
    non_null_error_1 = fn_check_export_inputs(database=database, table_name="non_existent_table", list_filters=list_filters)
    non_null_error_2 = fn_check_export_inputs(database=database, table_name="entries",vec_columns_to_show="non_existent_column")
    expect_equal(is.null(null_error), TRUE)
    expect_equal(is.null(non_null_error_1), FALSE)
    expect_equal(is.null(non_null_error_2), FALSE)
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
})

test_that("fn_query_and_left_join_tables", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
        n_entries=50,
        n_dates=3,
        n_sites=3,
        n_treatments=3,
        n_loci=10e3,
        save_data_tables=TRUE)$list_fnames_tables
    df_allele_frequency_table = utils::read.delim(list_fnames_tables$fname_genotypes, header=TRUE, check.names=FALSE)
    list_df_data_tables = list(
        df_phenotypes=utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE),
        df_environments=utils::read.delim(list_fnames_tables$fname_environments, header=TRUE),
        df_genotypes=df_allele_frequency_table)
    fn_initialise_db(fname_db="test.sqlite", list_df_data_tables=list_df_data_tables, verbose=TRUE)
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    list_tables_and_filters = list(
        entries=list(
            key_names=c("ENTRY_UID"), 
            column_names=c("*"),
            list_filters=list(
                ENTRY=sample(unique(list_df_data_tables$df_phenotypes$ENTRY), size=min(c(10, length(unique(list_df_data_tables$df_phenotypes$ENTRY))))))
        ),
        phenotypes=list(
            key_names=c("ENTRY_UID", "REPLICATION", "TREATMENT", "SITE", "FVI_YEAR_SEASON"),
            column_names=c("*"),
            list_filters=list(
                REPLICATION="rep_1",
                SITE=list_df_data_tables$df_phenotypes$SITE[1],
                POSIX_DATE_TIME=list_df_data_tables$df_phenotypes$POSIX_DATE_TIME[1],
                TREATMENT=sample(unique(list_df_data_tables$df_phenotypes$TREATMENT), size=min(c(1, length(unique(list_df_data_tables$df_phenotypes$TREATMENT))))))
        ),
        genotypes=list(
            key_names=c("ENTRY_UID"),
            column_names=c("*"),
            list_filters=NULL
        )
    )
    df_query_1 = fn_query_and_left_join_tables(database=database, list_tables_and_filters=list_tables_and_filters, unique_column_name=NULL)
    expect_equal(nrow(df_query_1) - length(unique(df_query_1$ENTRY)) >= 0, TRUE )
    list_tables_and_filters$entries$list_filters$POPULATION = list_df_data_tables$df_phenotypes$POPULATION[1]
    df_query_2 = fn_query_and_left_join_tables(database=database, list_tables_and_filters=list_tables_and_filters, unique_column_name="ENTRY_UID")
    expect_equal(length(unique(df_query_2$ENTRY)), nrow(df_query_2))
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
})

test_that("fn_assess_df_subsets", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
        n_entries=50,
        n_dates=3,
        n_sites=3,
        n_treatments=3,
        n_loci=10e3,
        save_data_tables=TRUE)$list_fnames_tables
    fname_db = "test.sqlite"
    df_allele_frequency_table = utils::read.delim(list_fnames_tables$fname_genotypes, header=TRUE, check.names=FALSE)
    list_df_data_tables = list(
        df_phenotypes=utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE),
        df_environments=utils::read.delim(list_fnames_tables$fname_environments, header=TRUE),
        df_genotypes=df_allele_frequency_table)
    fn_initialise_db(fname_db=fname_db, list_df_data_tables=list_df_data_tables, verbose=TRUE)
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname=fname_db)
    table_name = "phenotypes"
    list_filters=list(
                REPLICATION="*",
                MONTH=c(1, 12),
                TREATMENT=sample(unique(list_df_data_tables$df_phenotypes$TREATMENT), size=min(c(2, length(unique(list_df_data_tables$df_phenotypes$TREATMENT)))))
                )
    list_counts = fn_assess_df_subsets(database=database, table_name=table_name, list_filters=list_filters, vec_column_names_to_count=NULL, verbose=TRUE)
    expect_equal(
        nrow(list_counts$df_all_possible_combinations) * length(GLOBAL_list_required_colnames_per_table()$phenotypes), 
        length(list_counts$list_per_combination)
    )
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
})

test_that("fn_deserialise_genotype_data", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
         n_entries=50,
         n_dates=3,
         n_sites=3,
         n_treatments=3,
         n_loci=10e3,
         save_data_tables=TRUE)$list_fnames_tables
    fname_db = "test.sqlite"
    df_allele_frequency_table = utils::read.delim(list_fnames_tables$fname_genotypes, header=TRUE, check.names=FALSE)
    list_df_data_tables = list(
        df_phenotypes=utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE),
        df_environments=utils::read.delim(list_fnames_tables$fname_environments, header=TRUE),
        df_genotypes=df_allele_frequency_table)
    fn_initialise_db(fname_db=fname_db, list_df_data_tables=list_df_data_tables, verbose=TRUE)
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname=fname_db)
    df_genotypes = DBI::dbGetQuery(conn=database, statement="SELECT * FROM genotypes")
    df_allele_frequency_table_SERDE = fn_deserialise_genotype_data(database=database, df_genotypes=df_genotypes, verbose=TRUE)
    expect_equal(df_allele_frequency_table_SERDE, df_allele_frequency_table)
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
})
